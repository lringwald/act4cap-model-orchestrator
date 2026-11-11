#!/usr/bin/env bash
# =============================================================================
# entrypoint.sh — Act4Cap Integrated Downscaling & Environmental Routine
# =============================================================================
# Entrypoint for the WKUBE routine container. Sources config defaults, parses
# environment variable overrides, validates TASK, and invokes R/orchestrator.R.
# =============================================================================
set -euo pipefail
umask 0000

# --- help dispatch ---
if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  echo "Act4Cap Integrated Modelling Orchestrator"
  echo "Usage: entrypoint.sh [TASK]"
  echo "Tasks: trial | downscale | swat | biodiversity | postprocess | all"
  echo "Environment variables configure execution; see docs/routine_config.schema.json"
  exit 0
fi

if [ "${1:-}" = "--version" ]; then
  cat .build_id 2>/dev/null || echo "development"
  exit 0
fi

if [ -n "${1:-}" ] && [[ "$1" != -* ]] && [[ "$1" != *=* ]]; then
  echo "ERROR: Positional task argument '$1' is deprecated. Pass TASK=$1 as an environment variable or config." >&2
  exit 1
fi

TASKS="trial downscale swat biodiversity postprocess all"
is_task() { case " $TASKS " in *" $1 "*) return 0 ;; *) return 1 ;; esac; }

# --- load knob defaults from generated registry ---
[ -f config/knobs.generated.sh ] && . config/knobs.generated.sh

KN=(); KV=(); KS=()
knob() {
  local n="$1" env_var def blank v s
  eval "env_var=\${KNOB_ENV_$n:-$n}; def=\${KNOB_DEFAULT_$n-}; blank=\${KNOB_BLANK_$n-}"
  v="${!env_var-}"
  if [ -n "$v" ]; then s="environment"
  elif [ -n "${!env_var+x}" ]; then v="$blank"; s="blank -> registry"
  else v="$def"; s="default"; fi
  printf -v "$n" '%s' "$v"
  KN+=("$n"); KV+=("$v"); KS+=("$s")
}

if [ ${#KNOB_REGISTRY[@]} -gt 0 ]; then
  for n in "${KNOB_REGISTRY[@]}"; do knob "$n"; done
else
  echo "ERROR: config/knobs.generated.sh missing or empty. Run: Rscript tools/gen_config.R" >&2; exit 1
fi

# --- normalise boolean / optional switches ---
off() { case "$(printf '%s' "${1:-}" | tr 'A-Z' 'a-z')" in ""|0|no|off|false|auto|default) return 0;; *) return 1;; esac; }
for v in TRIAL FORCE; do
  if off "${!v:-}"; then export "$v=0"; else export "$v=1"; fi
done

export TASK MACRO_MODEL INPUT_FILE CUSTOM_INPUT_FILE DOWNSCALE_OUTPUT SCENARIO POSTMODELS RUN_SWAT RUN_BIODIVERSITY YEARS RESOLUTION NCORES WORK_DIR OUTPUT_DIR INPUT_DIR CASCADING_GAMBLE_DIR CASCADE_ENGINE_SOURCE SWATPLUS_DIR BIODIVERSITY_LINK_DIR BIODIVERSITY_MAPPING_FILE POSTMODEL_MODE TRIAL FORCE RUN_DATE VERSION_TAG

if ! is_task "$TASK"; then
  echo "ERROR: unknown TASK '$TASK'. Allowed: $TASKS" >&2; exit 1
fi

# --- banner ---
BUILD_ID="unknown"
[ -f .build_id ] && BUILD_ID="$(cat .build_id)"
echo "============================================================================="
echo ">>> Act4Cap Integrated Modelling Orchestrator"
echo ">>> Build ID:     $BUILD_ID"
echo ">>> Working Dir:  $PWD"
echo ">>> Task:         $TASK"
echo ">>> Macro Model:  $MACRO_MODEL"
echo ">>> Scenario:     $SCENARIO"
echo ">>> SWAT+:        $RUN_SWAT"
echo ">>> Biodiversity: $RUN_BIODIVERSITY"
echo ">>> Cores:        $NCORES"
echo "============================================================================="
echo ">>> Active Knob Configuration:"
for i in "${!KN[@]}"; do
  printf "    %-22s = %-30s (%s)\n" "${KN[$i]}" "${KV[$i]}" "${KS[$i]}"
done
echo "-----------------------------------------------------------------------------"

# --- persistent storage redirection & unified pipeline consolidation (FUSE mount) ---
if [ -n "$WORK_DIR" ] && [ -d "$(dirname "$WORK_DIR")" ]; then
  WDRV_BASE="$(dirname "$WORK_DIR")"
  mkdir -p "$WORK_DIR/00_inputs/macro_models" \
           "$WORK_DIR/00_inputs/swatplus_assets" \
           "$WORK_DIR/00_inputs/biodiversity_assets" \
           "$WORK_DIR/01_interimmodel/gamble-cascade" \
           "$WORK_DIR/01_interimmodel/snapshots" \
           "$WORK_DIR/02_postmodels/swatplus" \
           "$WORK_DIR/02_postmodels/biodiversity_link" \
           "$WORK_DIR/03_outputs/downscaling" \
           "$WORK_DIR/$OUTPUT_DIR" \
           "$WORK_DIR/99_archive" \
           "$WORK_DIR/logs"

  # Sync or source gamble-cascade and its submodules (cascade-core, gamble-core)
  GAMBLE_TARGET="$WORK_DIR/01_interimmodel/gamble-cascade"
  SNAPSHOT_MODE="${CASCADE_ENGINE_SOURCE:-live}"
  if [ "$SNAPSHOT_MODE" = "live" ] || [ "$SNAPSHOT_MODE" = "freeze" ]; then
    echo ">>> Provisioning live gamble-cascade and engine submodules (cascade-core, gamble-core) in /tmp/gamble-cascade-live..."
    rm -rf /tmp/gamble-cascade-live 2>/dev/null || true
    mkdir -p /tmp/gamble-cascade-live
    if [ -d "/app/vendor/gamble-cascade" ]; then
      cp -rf /app/vendor/gamble-cascade/. /tmp/gamble-cascade-live/
      echo ">>> Seeded /tmp/gamble-cascade-live from container image (/app/vendor/gamble-cascade)"
    fi
    # Optional non-interactive refresh from GitHub if credentials or public access are available
    if GIT_TERMINAL_PROMPT=0 GIT_LFS_SKIP_SMUDGE=1 git clone --depth 1 --recursive https://github.com/lringwald/gamble-cascade.git /tmp/gamble-cascade-git 2>/dev/null; then
      cp -rf /tmp/gamble-cascade-git/. /tmp/gamble-cascade-live/
      rm -rf /tmp/gamble-cascade-git
      echo ">>> Refreshed /tmp/gamble-cascade-live from live GitHub repository"
    fi
    if [ -f "/tmp/gamble-cascade-live/pipelines/AGMIP/run_downscaling.R" ]; then
      mkdir -p /tmp/gamble-cascade-live/data/intermediate
      if [ -d "$GAMBLE_TARGET/data" ]; then
        for d_sub in "$GAMBLE_TARGET/data"/*; do
          [ -e "$d_sub" ] || continue
          b_sub="$(basename "$d_sub")"
          if [ "$b_sub" = "intermediate" ]; then
            # Symlink existing cached .rds/.qs files into local writable /tmp/gamble-cascade-live/data/intermediate
            for f_int in "$d_sub"/*; do
              [ -e "$f_int" ] || continue
              ln -sf "$f_int" "/tmp/gamble-cascade-live/data/intermediate/$(basename "$f_int")" 2>/dev/null || true
            done
          else
            rm -rf "/tmp/gamble-cascade-live/data/$b_sub" 2>/dev/null || true
            ln -s "$d_sub" "/tmp/gamble-cascade-live/data/$b_sub"
          fi
        done
      fi
      export CASCADING_GAMBLE_DIR="/tmp/gamble-cascade-live"
      echo ">>> Activated gamble-cascade in /tmp/gamble-cascade-live (linked data from $GAMBLE_TARGET/data)"
    else
      echo ">>> WARNING: Could not provision /tmp/gamble-cascade-live; falling back to existing $GAMBLE_TARGET"
    fi
  elif [[ "$SNAPSHOT_MODE" == snapshot:* ]]; then
    SNAP_NAME="${SNAPSHOT_MODE#snapshot:}"
    SNAP_DIR="$WORK_DIR/01_interimmodel/snapshots/$SNAP_NAME"
    if [ -d "$SNAP_DIR" ]; then
      echo ">>> [Reproducibility] Activating frozen snapshot: $SNAP_DIR"
      export CASCADING_GAMBLE_DIR="$SNAP_DIR"
    else
      echo "WARNING: Requested snapshot '$SNAP_NAME' not found in $WORK_DIR/01_interimmodel/snapshots/."
    fi
  fi

  # Consolidate legacy loose directories on /mnt/wdrv into ACT4CAP-model-pipeline if present
  if [ "$WDRV_BASE" = "/mnt/wdrv" ]; then
    if [ -d "/mnt/wdrv/model_outputs" ]; then
      cp -rn /mnt/wdrv/model_outputs/* "$WORK_DIR/00_inputs/macro_models/" 2>/dev/null || true
      rm -rf /mnt/wdrv/model_outputs 2>/dev/null || true
    fi
    if [ -d "/mnt/wdrv/swatplus_runs/default_hru_shp" ]; then
      mkdir -p "$WORK_DIR/00_inputs/swatplus_assets/default_hru_shp"
      cp -rn /mnt/wdrv/swatplus_runs/default_hru_shp/* "$WORK_DIR/00_inputs/swatplus_assets/default_hru_shp/" 2>/dev/null || true
    fi
    if [ -d "/mnt/wdrv/swatplus_runs" ]; then
      cp -rn /mnt/wdrv/swatplus_runs/*.py "$WORK_DIR/02_postmodels/swatplus/" 2>/dev/null || true
      rm -rf /mnt/wdrv/swatplus_runs 2>/dev/null || true
    fi
    if [ -d "/mnt/wdrv/act4cap/outputs" ]; then
      cp -rn /mnt/wdrv/act4cap/outputs/* "$WORK_DIR/$OUTPUT_DIR/" 2>/dev/null || true
    fi
    if [ -d "/mnt/wdrv/act4cap/swatplus_staging" ]; then
      cp -rn /mnt/wdrv/act4cap/swatplus_staging/* "$WORK_DIR/02_postmodels/swatplus/" 2>/dev/null || true
    fi
    [ -d "/mnt/wdrv/act4cap" ] && rm -rf /mnt/wdrv/act4cap 2>/dev/null || true
    for arch_dir in /mnt/wdrv/enriched_with_bii /mnt/wdrv/*_backup_*; do
      if [ -d "$arch_dir" ]; then
        mv "$arch_dir" "$WORK_DIR/99_archive/" 2>/dev/null || rm -rf "$arch_dir" 2>/dev/null || true
      fi
    done
    [ -d "$WORK_DIR/02_postmodels/swatplus/default_hru_shp" ] && rm -rf "$WORK_DIR/02_postmodels/swatplus/default_hru_shp" 2>/dev/null || true
  fi

  chmod -R a+rwX "$WORK_DIR/$OUTPUT_DIR" "$WORK_DIR/02_postmodels" 2>/dev/null || true
  if [ ! -L "$OUTPUT_DIR" ]; then
    rm -rf "$OUTPUT_DIR"
    ln -s "$WORK_DIR/$OUTPUT_DIR" "$OUTPUT_DIR"
    echo ">>> Linked $OUTPUT_DIR -> $WORK_DIR/$OUTPUT_DIR"
  fi
fi

# --- sync agent rulebooks if available ---
if [ -f "tools/update_rules.sh" ]; then
  bash tools/update_rules.sh 2>/dev/null || true
fi

# --- dispatch execution to R orchestrator ---
echo ">>> Starting R orchestrator..."
exec Rscript R/orchestrator.R "$@"

#!/usr/bin/env bash
# =============================================================================
# entrypoint.sh — routine entrypoint for Act4Cap Orchestrator
# =============================================================================
# Dispatches a containerised Act4Cap execution routine on WKUBE cluster.
# Coordinates: Cascading Gamble Downscaling -> SWAT+ -> Biodiversity Link
# =============================================================================
set -eo pipefail

# --- resolve application root directory ---
if [ -z "${APP_DIR:-}" ]; then
  _d="$(cd "$(dirname "$0")" 2>/dev/null && pwd || echo .)"
  if   [ -f "$_d/R/orchestrator.R" ]; then APP_DIR="$_d"
  elif [ -f /app/R/orchestrator.R  ]; then APP_DIR=/app
  else APP_DIR="$_d"; fi
fi
cd "$APP_DIR" 2>/dev/null || { echo "ERROR: APP_DIR '$APP_DIR' is not a directory" >&2; exit 1; }
if [ ! -f R/orchestrator.R ]; then
  echo "ERROR: no R/orchestrator.R found under '$PWD'. Set APP_DIR to the repo root." >&2
  exit 1
fi

TASKS="trial downscale swat biodiversity postprocess all smoke"
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
for v in TRIAL SMOKE FORCE; do
  if off "${!v:-}"; then export "$v=0"; else export "$v=1"; fi
done

export TASK MACRO_MODEL INPUT_FILE SCENARIO POSTMODELS RUN_SWAT RUN_BIODIVERSITY YEARS RESOLUTION NCORES WORK_DIR OUTPUT_DIR INPUT_DIR CASCADING_GAMBLE_DIR SWATPLUS_DIR BIODIVERSITY_LINK_DIR BIODIVERSITY_MAPPING_FILE POSTMODEL_MODE TRIAL FORCE

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

# --- persistent storage redirection (FUSE mount) ---
if [ -n "$WORK_DIR" ] && [ -d "$(dirname "$WORK_DIR")" ]; then
  mkdir -p "$WORK_DIR/$OUTPUT_DIR" "$WORK_DIR/logs"
  if [ ! -L "$OUTPUT_DIR" ]; then
    rm -rf "$OUTPUT_DIR"
    ln -s "$WORK_DIR/$OUTPUT_DIR" "$OUTPUT_DIR"
    echo ">>> Linked $OUTPUT_DIR -> $WORK_DIR/$OUTPUT_DIR"
  fi
fi

# --- dispatch execution to R orchestrator ---
echo ">>> Starting R orchestrator..."
exec Rscript R/orchestrator.R "$@"

# Act4Cap Integrated Modelling Orchestrator

A containerised orchestration routine designed for the **Act4Cap Project** to execute spatial downscaling via **Cascading Gamble** and subsequent environmental impact post-modelling (**SWAT+** water quality and **Biodiversity Link** ecological integrity) on a WKUBE Kubernetes cluster or locally.

---

## Quickstart: WKUBE Routine Execution

The routine is fully configured by `config.json` adhering to `docs/routine_config.schema.json`.

### Configuration Schema Knobs (`config.json`)

| Knob | Type | Default | Description |
| :--- | :---: | :---: | :--- |
| `TASK` | enum | `all` | `all`, `downscale`, `swat`, `biodiversity`, `postprocess`, `smoke` |
| `MACRO_MODEL` | enum | `globiom` | `globiom`, `capri`, `image`, `magnet`, `clumondo`, `all` |
| `SCENARIO` | enum | `base` | `base`, `all` |
| `RUN_SWAT` | enum | `1` | `1` to run SWAT+ water quality model, `0` to skip |
| `RUN_BIODIVERSITY` | enum | `1` | `1` to run Biodiversity Link ecological model, `0` to skip |
| `YEARS` | string | `all` | `all` (2000-2050) or comma-separated years |
| `RESOLUTION` | enum | `10km` | `10km` (standard EEA EPSG:3035) or `1km` |
| `NCORES` | string | `4` | Parallel CPU cores |
| `SMOKE` | enum | `0` | Set to `1` for ~1-minute plumbing proof run |
| `FORCE` | enum | `0` | Set to `1` to overwrite completed stages |
| `WORK_DIR` | string | `/mnt/wdrv/act4cap` | Cluster FUSE mount path |
| `OUTPUT_DIR` | string | `outputs` | Output folder relative to WORK_DIR |
| `CASCADING_GAMBLE_DIR` | string | `/app/cascadinggamble-core` | Path to Cascading Gamble downscaling engine |

---

## Running Locally

```bash
# 1. Install dependencies
Rscript init.R

# 2. Re-generate schema and shell registry if knobs change
Rscript tools/gen_config.R

# 3. Execute smoke test
TASK=smoke MACRO_MODEL=globiom bash entrypoint.sh

# 4. Execute production run
TASK=all MACRO_MODEL=globiom bash entrypoint.sh
```

---

## Building Container for WKUBE

```bash
docker build -t act4cap-orchestrator:latest .
```

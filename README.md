# Act4Cap Integrated Modelling Orchestrator

A containerised orchestration routine designed for the **Act4Cap Project** coupling macro-economic agricultural models (GLOBIOM, CAPRI, IMAGE, MAGNET, CLUMONDO) with environmental post-processors (**SWAT+** water quality and **Biodiversity Link** ecological integrity).

The **Cascade Interface** ([`lringwald/gamble-cascade`](https://github.com/lringwald/gamble-cascade)) acts as the **necessary spatial intermediary and mediator**—translating aggregate regional/national economic supply balance sheets into mass-conserved 10km grid-cell distributions that directly enable downstream hydrological catchment routing and biodiversity assessments.

---

## Quickstart: WKUBE Routine Execution

The routine is fully configured by `config.json` adhering to `docs/routine_config.schema.json`.

### Configuration Schema Knobs (`config.json`)

| Knob | Type | Default | Description |
| :--- | :---: | :---: | :--- |
| `TASK` | enum | `all` | `all`, `downscale` (mediator), `swat`, `biodiversity`, `postprocess`, `trial` |
| `MACRO_MODEL` | enum | `globiom` | `globiom`, `capri`, `image`, `magnet`, `clumondo`, `all` |
| `SCENARIO` | enum | `base` | `base`, `all` |
| `RUN_SWAT` | enum | `1` | `1` to run SWAT+ water quality model, `0` to skip |
| `RUN_BIODIVERSITY` | enum | `1` | `1` to run Biodiversity Link ecological model, `0` to skip |
| `YEARS` | string | `all` | `all` (2000-2050) or comma-separated years |
| `RESOLUTION` | enum | `10km` | `10km` (standard EEA EPSG:3035) or `1km` |
| `NCORES` | string | `4` | Parallel CPU cores |
| `TRIAL` | enum | `0` | Set to `1` for fast verification trial run (~1-minute plumbing proof) |
| `FORCE` | enum | `0` | Set to `1` to overwrite completed stages |
| `WORK_DIR` | string | `/mnt/wdrv/act4cap` | Cluster FUSE mount path |
| `OUTPUT_DIR` | string | `outputs` | Output folder relative to WORK_DIR |
| `CASCADING_GAMBLE_DIR` | string | `/app/gamble-cascade` | Path to Cascade Interface downscaling suite (spatial mediator) |
| `SWATPLUS_DIR` | string | `/mnt/wdrv/act4cap/swatplus_runs` | SWAT+ pipeline scripts (0-6) & HRU datasets (`swatplus_runs` locally) |
| `BIODIVERSITY_LINK_DIR` | string | `vendor/Biodiversity_Link` | IIASA Biodiversity_Link repository path (`6cc4443f`) |
| `BIODIVERSITY_MAPPING_FILE` | string | `config/mapping_agmip_to_biodiversity_link.csv` | Crosswalk between AgMIP downscaled classes and IIASA Biodiversity Link categories |
| `POSTMODEL_MODE` | enum | `tentative` | `tentative` (integrated proxy), `full` (native cluster runs), `trial` |
| `RUN_DATE` | string | `auto` | Run date stamp (`YYYYMMDD` or `auto` for execution date) |
| `VERSION_TAG` | string | `""` | Optional version suffix appended to output directories and files |

---

## Output Architecture & Storage Formats

All outputs for a given run are organized into a self-contained, reproducible job folder (`outputs/<tag>/`):
- **Regional / Policy Aggregates (`.csv`)**:
  - Regional summaries aggregated by NUTS2 / country code (`Ns_reg`) and simulation year (`year`) are stored strictly as `.csv` files for direct human readability in Excel / spreadsheet applications.
  - Examples: `swat_plus_water_quality_summary_<tag>.csv`, `biodiversity_link_bii_summary_<tag>.csv`, `biodiversity_link_csar_summary_<tag>.csv`, `biodiversity_link_msa_summary_<tag>.csv`, and `act4cap_integrated_synthesis_<tag>.csv`.
- **Pixel / Cell-Level Spatial Tables (`.parquet`)**:
  - Cell-level spatial tables containing 10km grid cell identifiers (`ns`), NUTS units, and detailed biophysical/hydrological/ecological indicators strictly enforce the **Apache Parquet standard** (`.parquet`) with Snappy compression via `arrow`.
  - Parquet reduces file sizes by ~80% compared to uncompressed CSVs, preserves strict column datatypes, and accelerates loading into R/Python by ~10x.
  - Examples: `swat_plus_cells_<tag>.parquet`, `biodiversity_link_cells_<tag>.parquet`, and mediated land-use allocations `DS_ACT4CAP_AgMIP_<tag>_lu_<scenario>.parquet`.
- **Spatial Continuous Grids (`.tif`)**:
  - Standard European GeoTIFF rasters (`EPSG:3035`) with LZW compression for direct GIS inspection (QGIS, ArcGIS).
- **In-Memory Bundles (`.rds`)**:
  - Full R composite objects containing tabular summaries, raster paths, and run provenance metadata.
- **Run Manifests (`manifest.json`)**:
  - Self-describing execution metadata and artifact inventories.

---

## Running Locally

```bash
# 1. Install dependencies
Rscript init.R

# 2. Re-generate schema and shell registry if knobs change
Rscript tools/gen_config.R

# 3. Execute fast trial run
TASK=trial MACRO_MODEL=globiom bash entrypoint.sh

# 4. Execute production run
TASK=all MACRO_MODEL=globiom bash entrypoint.sh
```

---

## Building Container for WKUBE

```bash
docker build -t act4cap-orchestrator:latest .
```

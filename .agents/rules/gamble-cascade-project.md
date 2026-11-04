---
# Claude Code Path Trigger:
paths:
  # gamble-cascade (data, pipelines, orchestration)
  - "pipelines/**/*.{R,py,ps1}"
  - "scripts/**/*.{R,py}"
  - "Snakefile"
  - "workflow/Snakefile"
  - "**/*.smk"
  - "data/aux_files/**/*.csv"
  # gamble-core / cascade-core (R package layout)
  - "R/**/*.R"
  - "src/**/*.{cpp,cc,c,h,hpp}"
  - "inst/include/**/*.{h,hpp}"
  - "tests/testthat/**/*.R"
  # Vendored engine packages
  - "external/{gamble-core,cascade-core}/**/*.{R,cpp,cc,c,h,hpp,stan}"
# Antigravity Model Decision Trigger:
trigger: model_decision
description: "Project architecture for the gamble-cascade / gamble-core / cascade-core ecosystem: repository roles and code ownership, package loading, data roots and pixel identifiers, and the mapping from the general spatial and statistical rulebooks to concrete functions, arguments, files, and pipelines."
---

# GAMBLE–CASCADE Project Architecture

This rulebook is **project-specific**. It maps the general invariants in `spatial-downscaling.md` and `statistical-modeling.md` onto this project's repositories, packages, functions, and file layout. When the two disagree, the general rulebooks define *what* must hold; this file defines *where and how* it is implemented here.

---

## 1. Repositories & Division of Labor

The pipeline spans three repositories with strict boundaries:

1. **`gamble-cascade`: Data Engineering, Harmonization & Orchestration Hub**
   - Owns the master spatial data assets and grid mappings, the raw-to-processed grid pipelines, covariate harmonization (climate, terrain, soils, accessibility, socio-economic statistics), and target harmonization of macro-model outputs. It also owns the execution runners (`Snakefile`, `pipelines/<PROJECT>/run_*.R`, `scripts/core_orchestrator.R`).
   - One folder per study under `pipelines/<PROJECT>/` (e.g. `LAMASUS_GLOBIOM`, `BMLEH_Los1_CAPRI`, `BIOCLIMA3_CAPRI`, `AGMIP`), each with its own `config.R`. Shared steps live in `scripts/core/`.
2. **`gamble-core`: Statistical Prior Estimation Engine**
   - Owns the Bayesian econometrics: spatial multinomial logit (MNL) discrete-choice models, nested-cut trees (`nested_cut.R`), livestock Poisson/hurdle count models (`count_rcpp.R`), and the Pólya-Gamma MCMC C++ samplers.
3. **`cascade-core`: Numerical Downscaling Engine**
   - Owns the constrained allocation algorithms: generalized IPF, `nloptr` BOBYQA/COBYLA optimization, sequential target boosts, and residual mop-up. It also owns the start-map harmonization primitives (gross transitions, spatial transition allocation, zonal swapping).

**Ownership rules:**
- `gamble-cascade` owns the DATA and the pipeline wiring. `gamble-core` owns the STATISTICAL MODEL CODE, and `cascade-core` owns the ALLOCATION / HARMONIZATION ALGORITHMS.
- Never duplicate or fork model or algorithm code into `gamble-cascade`. A fix goes into the engine package, followed by a bump of the pinned submodule. Project-level redefinitions of engine functions (e.g. a script that redefines `allocate_transitions_spatial`) are forbidden. If an experimental variant is needed, give it a distinct name and select it explicitly in `config.R`.
- Steps that exist in several pipelines (start-map compilation, harmonization, transitions) must implement the same policy. Differences between pipelines go in `config.R` as parameters, not in divergent copies of the script.

---

## 2. Package Loading

- The engines are vendored as git submodules under `external/gamble-core` and `external/cascade-core`. Resolve them deterministically: the project-pinned submodule first, a sibling checkout (`../cascade-core`) only as an explicit developer override, and the installed package last.
- Load each engine **once** per session (`devtools::load_all(<path>)` or `library()`), and log the resolved path and git SHA. Never `source()` a script after loading that redefines exported engine functions.
- Record the engine SHAs in the run lineage and in every node-store fingerprint (spatial rulebook, Principle 4).

---

## 3. Data Roots, Grids & Identifiers

- **Data roots:** parameterize storage via `GAMBLE_DATA_ROOTS` (ordered search path), `GAMBLE_HOME`, and `GAMBLE_GRIDWORK_DIR`. Never hardcode user paths (`/Users/...`, `P:/...`). Path fallbacks inside scripts must come from `config.R`.
- **Stage directories:** `data/input/` (raw), `data/02_intermediate/` (processed, via `save_intermediate()` / `read_intermediate()`), and `data/aux_files/` (mappings, crosswalks, protected-area shares).
- **Master grid mapping:** `data/aux_files/one_kmID_master_mapping_*.parquet` (latest version via `get_latest_file_aux()`).
- **Pixel identifiers:**
  - `EEA_1kmID`: the permanent 1 km pixel key (EPSG:3035) used for all joins.
  - `INSPIRE_Europe_buffer_1kmID`: source-side INSPIRE key, mapped to `EEA_1kmID` through the master mapping.
  - `ns`: the pixel key at the downscaling resolution (`downscale_resolution`, e.g. `"5km"`), with coordinates `x_ns`, `y_ns`.
- **Class crosswalks:** `data/aux_files/<PROJECT>_thematic_mapping.csv` (e.g. `LAMASUS_LUM_thematic_mapping.csv`). These are the versioned crosswalk matrices required by the spatial rulebook (Principle 2, item 5). Register every input with `register_source_file()` so it appears in lineage.

---

## 4. Concept Map: General Rulebooks → Implementation

| General concept (rulebook) | Implementation in this project |
| :--- | :--- |
| Estimation engine (spatial Principle 3) | `gamble-core` |
| Nested-cut MNL tree | `gamble-core`: `nested_cut.R`; runner `scripts/core/run_prior_nested_cut.R` |
| Livestock count models | `gamble-core`: `count_rcpp.R`; priors `pipelines/<PROJECT>/compute_LSU_priors.R` |
| Automated constant-sum detection | `const_sum_blocks = "auto"` |
| In-sweep RE recentering (with intercept compensation) | `re_center = TRUE` |
| Streaming posterior serialization | `stream_disk`; batch files `posterior_batch_*.qs` / `.rds` |
| Resumable node store | `store_dir/nodes/<node_path>.rds` |
| Downscaling engine (spatial Principle 5) | `cascade-core`; runner `pipelines/<PROJECT>/run_downscaling.R` |
| Solver options (lineage) | `downscale_control()` |
| Start-map compilation (harmonization step 1) | `pipelines/<PROJECT>/compile_*_start_map.R` → intermediate `pre_harm_start_map` |
| Start-map harmonization (steps 2–8) | `pipelines/<PROJECT>/process_*start_map_harmonization.R` → intermediate `processed_ds_start_map` |
| Harmonization zones | LAMASUS_GLOBIOM: `GLOB_country × GLOB_LUID × GLOB_AEZ`. CAPRI and AgMIP pipelines: `Ns_reg` (standardized via `standardize_ns_reg_col()`) |
| Balancing class $b$ | currently `NODATA` (LAMASUS_GLOBIOM) or `Natural_unmanaged` (CAPRI, AgMIP); to be unified (see §6) |
| Transition prior $\Pi$ | NNLS `beta_matrix` → `empirical_prior` (`from_class`, `to_class`, `value`) |
| Gross transitions (Fienberg / IPF) | `cascade-core`: `solve_gross_transitions(from, to, prior, keep_areas)` → `fienberg()` |
| Gravity-seeded spatial allocation | `cascade-core`: `allocate_transitions_spatial(pixel_dt, shares_dt, zone_cols, ..., block_pixels, gravity_floor)` |
| Quantum hygiene (snap, protect rare classes, rescue pixels) | `cascade-core`: `execute_zonal_swaps(mode = "decimal", threshold = 0.01)` (protection of rare classes is the "Ark" protocol) |
| Harmonization validation | `scripts/core/aux_codes/start_target_map_harmonization_check.R` (`check_stage_name` = `"Pre-Swap"` / `"Post-Swap"`) |
| Static regions (spatial Principle 5, item 1) | `execute_luc_downscaling(write_static_regions = TRUE)`; rows flagged `downscaling_area_flag = 2L` |
| Zones kept as `_other` | intermediate `outside_model_zones` (`record_outside_model_zones()`); their targets are dropped in `adapt_targets()` |
| Outside-model relabeling (spatial Principle 5, item 1) | `relabel_outside_model()` and `keep_outside_model_zones()` in `scripts/core/aux_codes.R`; `skipped_zone_handling` and `outside_model_primary_rules` in `config.R`. LAMASUS: `relabel_outside_globiom()` wrapper (zones with zero target area, pixels with `TERRA_GLOBIOM == FALSE`). Carried through downscaling by `downscale()`'s stay rows for classes without targets (`missing_luc`) |
| Area tolerance | `tol_rel` (soft tier: messages and warnings) and `area_gap_stop_rel` (stop tier, relative, default 1%) via `stop_on_excessive_area_gap()` |
| Period pairing (spatial Principle 6, item 1) | `build_period_pairs()` / `lag_by_period()` in `scripts/core/aux_codes.R`; optional cadence check `expected_period_step` in `config.R` |
| Scenario resolution | `resolve_scenario()` in `scripts/core/aux_codes.R` (exact, or unique partial match) |
| Harmonization coverage report | `report_harmonization_coverage()` → `<temp>/<label>_harmonization_coverage.csv` |
| Transition targets (spatial Principle 6, item 3) | `pipelines/<PROJECT>/compute_*_transitions.R` (`solve_gross_transitions(..., keep_areas = "both")`) → intermediate `LUC_targets_full` (`from_class`, `to_class`, `value` per region, scenario, period) |
| Multi-period chain driver | `scripts/core/process_lu_downscaling.R` → `cascade-core`: `execute_luc_downscaling()` → `downscale()`, looping over ordered `times` |
| Chaining update (period start = previous result) | `downscale(areas_update_fun = areas_sum_to)` |
| Dynamic land-use covariates | `xmat_coltypes` = `"dynamic"` for land-use classes, updated by `xmat_dyn_fun = xmat_share_to` (own-pixel class shares) |
| Projected exogenous covariates | intermediate `high_res_xmat_proj` → `xmat_proj_full` / `xmat_proj` |
| Pixel restrictions | `cfg$restriction_intermediates(_res)` → `restrictions` (`ns`, `from_class`, `to_class`, `times`, `scenario`, `value` = restricted share). The solver (`solve_biascorr_lu()`) supports `from_class`-specific rows and the wildcard `__NA_LU__`; omit `times` to apply rows to every period. On `main`, `execute_luc_downscaling()` drops `from_class` and requires binary values (see §6) |
| Chronological validity check | `execute_luc_downscaling()`: warning for targets from classes that do not exist yet |
| Dynamic feasibility pre-check | `check_start_map_target_alignment()` in `scripts/core/process_lu_downscaling.R` → `files$harmonization_check_output` |
| Unmet-demand handling | `downscale_control(rebalance_targets = TRUE)` → `solve_biascorr_lu()` caps restricted targets and scales a from-class's targets down proportionally; flags (`initial_area_insufficient`, `restriction_blocking`, `rebalancing_applied`, `final_deviation`) go to `files$diag_output` (`outputs/downscaled_results_diag.csv`) |

---

## 5. Project Profile: Thresholds & Evaluation Choices

Project-specific values for the recommended defaults of the general rulebooks (statistical rulebook, "How to Read the Numbers in This Rulebook"). Where this profile is silent, the defaults apply. Add a line, with a short reason, whenever the project deviates.

| Topic | Project choice | Reason / source |
| :--- | :--- | :--- |
| Convergence ($\hat{R}$) | Target $\le 1.01$; soft zone $1.01$–$1.1$ interpreted with diagnostics; hard limit $> 1.1$ | Project decision, 2026-11-04 (also the general rule) |
| Effective sample size | Relative to post-warmup draws: target $\text{ESS}/S \ge 0.10$, soft zone $0.01$–$0.10$, hard limit $< 0.01$ (bulk and tail); MCSE/SD $< 0.05$ for reported summaries | Project decision, 2026-11-04 (also the general rule) |
| Uncertainty propagation | Declared point estimate by default (posterior mean of the quantities passed downstream); ensembles optional | Project decision, 2026-11-04 |
| Area checks in compilation/harmonization | Two-tier: existing messages kept; stop above `area_gap_stop_rel` = 1% | Rounding noise is expected; only excessive gaps stop |
| Coverage screen for harmonization | 95% (CAPRI pipelines), 50% (AgMIP, default countries always harmonized) | Existing pipeline settings |
| Skipped zones | `skipped_zone_handling`: `"keep"` (BMLEH_Los1, BIOCLIMA3), `"other"` (AgMIP) | Grid-edge slivers and inexact border mapping in AgMIP |
| Model–map mismatch | Reported (unmet demand), not resolved by default | Project decision, 2026-11-03 |
| Period cadence | `expected_period_step = 10` for LAMASUS_GLOBIOM | GLOBIOM reports 10-year steps |
| Year fallbacks | Allowed when logged | Project decision, 2026-11-03 |
| Predictive measures (statistical C.3) | Defaults; not yet tailored | To be set per model family |

---

## 6. Known Deviations from the General Rulebooks (status 2026-11-04)

Alignment items found in the current code. Each entry is **open**, **fixed** (merged into `main`, 2026-11-04), or **accepted** (intended behaviour, decided 2026-11-03). Fix open items when you touch the file, and remove an entry once its fix is merged.

- **Balancing policy differs between pipelines** (spatial Principle 5, item 2):
  - `LAMASUS_GLOBIOM/process_start_map_harmonization.R` handles only a start-map surplus (to `NODATA`). A deficit falls through to `keep_areas = "from"`, which rescales every target in the zone.
  - `BIOCLIMA3_CAPRI/process_CAPRI_start_map_harmonization.R` adds the deficit to the *start* side of `Natural_unmanaged`.
  - `BMLEH_Los1_CAPRI/process_CAPRI_start_map_harmonization.R` reduces the `Natural_unmanaged` target with `max(0, …)`.
- **Year fallbacks** (*accepted*): closest-year start-map selection in `LAMASUS_GLOBIOM/compile_DS_start_map.R` and the earliest-year fallback in the CAPRI and AgMIP harmonization scripts stay, with their log messages.
- **Ambiguous scenario matching** (*fixed: lringwald/gamble-cascade#2*): `BMLEH_Los1_CAPRI/process_CAPRI_start_map_harmonization.R` resolved several partial matches by row order (`matched_scens[1]`). It now uses `resolve_scenario()`: an exact match, or a unique partial match; ambiguous matches stop.
- **Skipped zones** (*fixed: lringwald/gamble-cascade#2*):
  - `report_harmonization_coverage()` now writes `<temp>/<label>_harmonization_coverage.csv` (zone, start-map area, model area, ratio, status) and logs the area affected.
  - **Handling** (*fixed: lringwald/gamble-cascade#2*): all pipelines now keep non-harmonized zones via `keep_outside_model_zones()`. Previously BIOCLIMA3 (below 95%) and AgMIP (below 50%, unless a default country) dropped them from the harmonized map.
    - Zones without targets (e.g. the German city states in CAPRI) are always relabelled to `<primary>_other`.
    - Zones below the coverage screen follow `skipped_zone_handling` in `config.R`: `"keep"` (unharmonized, BMLEH_Los1 and BIOCLIMA3) or `"other"` (AgMIP: grid-edge slivers and inexact border mapping).
- **Regions without targets disappear from the outputs** (*fixed: lringwald/cascade-core#2 writes them as static regions; lringwald/gamble-cascade#2 drops the targets of zones kept as `_other`*).
  - The BMLEH passthrough exists so that regions CAPRI does not model (e.g. Berlin, Hamburg, Bremen) keep their observed land cover in the final rasters.
  - The passthrough keeps them in `processed_ds_start_map`, but `execute_luc_downscaling()` loops only over region × scenario pairs that have targets (`ds_grid`, `R/execute_luc_downscaling.R:297`). The rasters are built from `files$lu_output` only (`scripts/core/core_orchestrator.R:771-790`), so such regions are missing from the land-use outputs and rasters.
  - A synthetic run on `cascade-core` `main` confirms it: a region without targets is absent from the land-use output, while `_other` pixels *inside* a region with targets are carried unchanged through all periods. Not yet confirmed on a real BMLEH raster.
  - *Fix:*
    - `execute_luc_downscaling()` writes start-map regions without targets (decided per scenario, before the year and country filters) unchanged: LU rows for every run period plus the base year, and LUC stay rows, with `downscaling_area_flag = 2L`. `write_static_regions = FALSE` restores the old behaviour.
    - Zones kept as `_other` are saved as the intermediate `outside_model_zones` (`record_outside_model_zones()`), and `adapt_targets()` drops their targets (`drop_outside_model_targets()`). This keeps AgMIP slivers from reporting their whole country demand as unmet.
    - BIOCLIMA3 and AgMIP keep these zones instead of dropping them (see Skipped zones).
- **Outside-model relabeling only in LAMASUS** (*fixed: lringwald/gamble-cascade#2*): the shared helper `relabel_outside_model()` (`scripts/core/aux_codes.R`) is used by all four pipelines. `relabel_outside_globiom()` wraps it with identical output.
  - The primary class is the first token of the class name (`Cropland_arable_barley` → `Cropland_other`, `CRP|ARA` → `CRP_other`); `outside_model_primary_rules` in `config.R` can override it.
  - The new `_other` class names (e.g. `Cropland_other`, `Grassland_other`, `Forests_other` in BMLEH_Los1; `CRP_other`, `FOR_other`, `BUI_other` in AgMIP) must exist in each pipeline's reporting legends and raster mappings.
- **Area checks** (*accepted as two-tier; stop tier fixed: lringwald/gamble-cascade#2*):
  - The existing `message()` / `warning()` checks stay, because some rounding noise is expected (zone aggregates use `round(..., 2)`); their absolute use of `tol_rel` is kept.
  - `stop_on_excessive_area_gap()` adds a stop when the relative gap exceeds `area_gap_stop_rel` (default 1%). It runs at the start-map compilation checks (LAMASUS, AgMIP) and the LAMASUS harmonization checks.
- **Duplicate allocator** (*open*): `scripts/core/gravity_allocator.R` (sourced through the `scripts/gravity_allocator.R` shim from `scripts/core/aux_codes.R` and the LAMASUS harmonization script) redefines `cascade-core`'s `allocate_transitions_spatial`.
  - The computation is identical; the override is a performance rewrite (pre-cast wide lookup tables instead of keyed joins in the inner loop).
  - *Fix:* move it into `cascade-core`, delete the override (§1), and confirm identical output on one zone.
- **Prior permits every transition:** `beta_matrix + 0.001` gives every class pair positive weight. Add structural zeros for impossible transitions, and use the same hierarchy bonus in all pipelines (currently only `BMLEH_Los1_CAPRI` adds `+10` for coarse → fine paths).
- **Lag construction assumes uniform steps** (spatial Principle 6, item 1). *Status: fixed (lringwald/gamble-cascade#2). `build_period_pairs()` / `lag_by_period()` pair periods per scenario (per model × scenario for AgMIP), and `expected_period_step = 10` (LAMASUS_GLOBIOM) checks the GLOBIOM cadence. For regular 10-year data the targets are identical to the old `year + 10` join. `compute_FM_transitions.R` already lags over a dense period grid and is unchanged.*
  - *Where:* `BMLEH_Los1_CAPRI/compute_CAPRI_transitions.R:71-74` and `BIOCLIMA3_CAPRI/compute_CAPRI_transitions.R:34-37` set `time_step <- available_years[2] - available_years[1]` and build the lag as `year + time_step`; `LAMASUS_GLOBIOM/compute_ACR_transitions.R:83` uses `year + 10`. The merge is `all = TRUE`, followed by `area_t_lag := 0` for unmatched rows.
  - *Failure:* with periods 2020, 2025, 2030, 2050 the step is 5, so the lag rows land on 2025, 2030, 2035, 2055. Period 2050 finds no lag row, so every class gets `area_t_lag = 0`. The whole region area then counts as `macro_inflow` and enters as `Natural_unmanaged` on the from-side (`:157`). The 2050 targets become "convert the entire region from `Natural_unmanaged`", and the real 2030→2050 change is lost. No error or warning is raised.
  - *Fix:* pair periods by position in the ordered vector, as `AGMIP/compute_agmip_transitions.R:63-82` does (`year_pairs`). Assert that every period except the first has exactly one predecessor. `LAMASUS_GLOBIOM/compute_FM_transitions.R:55` (`shift()` by group) is only correct if every group has a row in every period; check that too.
- **Transition margins come from model levels, not achieved levels** (Principle 6, item 3). *Status: accepted (intended). A model–map mismatch is reported (unmet demand in the diagnostics), not resolved by default. Rebuilding from achieved totals or carrying the gap forward may be added later as an optional strategy.*
  - *Where:* the lag in every reviewed `compute_*_transitions.R` script is the *model's* area at $t-1$ (e.g. `BMLEH_Los1_CAPRI/compute_CAPRI_transitions.R:74`). The resulting `LUC_targets_full` is fixed before downscaling starts. `downscale()` then runs the periods on the *achieved* map (`areas_update_fun = areas_sum_to`).
  - *Failure:* in 2030 the target `Natural_unmanaged → Cropland` is 100 km², but restrictions allow only 60 km². The 2030 map holds 40 km² less cropland than the model. The 2040 transitions were computed from the model's 2030 level, so nothing recovers the 40 km², and later cropland outflows are drawn from area that is not on the map (capped again). The gap persists or grows over the chain. `check_start_map_target_alignment()` (`scripts/core/process_lu_downscaling.R:370-418`) accumulates the *targets'* net changes, so it cannot see this either; it only warns.
  - *Fix:* write per-period achieved region totals. Then either rebuild the next period's transitions from them, or carry the unmet amount forward under a documented policy, and report the cumulative gap to the model per region and class.
- **Single-year fallback applies the base-year change twice.** *Status: accepted, not changed. It only triggers when the targets contain a single year at or after the base year.*
  - *Where:* `BMLEH_Los1_CAPRI/compute_CAPRI_transitions.R:25-68` (the case of only one target year at or after the base year). The lag is taken from `pre_harm_start_map` (unharmonized). Meanwhile `process_CAPRI_start_map_harmonization.R:39-49` falls back to the earliest available target year when the base year is missing, i.e. the same single year.
  - *Failure:* say the observed map has 1,000 km² of cropland and the only CAPRI year (2030) has 1,100 km². Harmonization lifts the start map to 1,100 km², and the transitions (observed 1,000 → 1,100) add another 100 km² on top. Downscaled 2030 cropland is then 1,200 km², unless the cap hits. The harmonization check passes because it compares against 2030. The defaults also differ: `base_yr_val` is 2020 at `:14` but 2018 at `:28` when `current_model_cfg$base_year` is unset.
  - *Fix:* in the fallback, build transitions from `processed_ds_start_map`, or stop when the base year is missing. Use one base-year default from `config.R`.
- **Small flows dropped after fitting** (*open; to be handled with the allowed-transition matrix*).
  - *Where:* units are km², so the threshold is 0.1 ha.
    - `transitions[value > 0.001 & from_class != to_class]`: `BMLEH_Los1_CAPRI/compute_CAPRI_transitions.R:181`, `BIOCLIMA3_CAPRI/compute_CAPRI_transitions.R:143`.
    - `transitions[value > 0.001]`: `AGMIP/compute_agmip_transitions.R:218`.
    - `abs(value) > 0.001` or set to 0: `LAMASUS_GLOBIOM/compute_ACR_transitions.R:391,407`, `compute_FM_transitions.R:332,350`, `compute_Pasture_transitions.R:356,395`.
  - *Failure:* the fitted flows match both margins exactly. The `+ 0.001` prior smoothing gives every class pair a small positive flow, so many sub-threshold flows exist, and removing them lowers both the outflow of their from-class and the inflow of their to-class. Each dropped flow is under 0.001 km², so the loss is below $0.001 \cdot K(K-1)$ km² per region and period (under 0.38 km² for $K = 20$). It is small but systematic, accumulates over regions and periods, and is not logged.
  - *Fix:* zero implausible pairs in the prior (structural zeros) and re-fit, or remove sub-threshold flows and re-fit on the remaining support. Log the area removed.
- **Unmet demand is flagged but not quantified.** *Status: fixed (lringwald/cascade-core#1; diagnostics gain `target_requested`, `target_capped`, `achieved`, `unmet`).*
  - *Where:* `execute_luc_downscaling()` calls `downscale_control(rebalance_targets = TRUE)` (`R/execute_luc_downscaling.R:662`, `:686`). In `R/solve_biascorr_lu.R:336-365`, a target exceeding the area its restrictions allow is capped to that area. If a from-class's targets together exceed its current area, all of them are scaled down by the same factor.
  - *What is recorded:* per target, `initial_area_insufficient`, `restriction_blocking`, `rebalancing_applied` and `final_deviation` go to `outputs/downscaled_results_diag.csv`. `final_deviation` is computed against the *capped* targets (`calculate_target_deviation(colSums(res$out_mu), curr_targets_vec, ...)` after `curr_targets_vec` was overwritten), so a capped target reports near-zero deviation.
  - *Missing:* the requested target, the capped target, and the unmet amount. Nothing in the outputs shows how many km² of model demand were not allocated.
  - *Fix:* keep the original target vector, write `target_requested`, `target_capped`, `achieved` and `unmet = target_requested - achieved` to the diagnostics, and apply one documented unmet-demand policy.
- **Projected covariates: dead code path, and no future data behind it.**
  - *Where:* `R/execute_luc_downscaling.R:437-441` prepares `xmat_proj_region`, but `:656` passes `#xmat_proj = xmat_proj_region` (commented out since the package's initial commit, 2026-07-24). `downscale()` therefore updates only the dynamic land-use columns (`xmat_share_to`), and every other covariate keeps its base-year value in every period.
  - *Re-enabling it as-is would not add climate or socio-economic change:*
    - `scripts/gamble_core/process_prior_models.R:652-711` builds `high_res_xmat_proj` for 2010 and 2020 only, as copies of the base-year covariates. Its land-use share columns are reconstructed from zone-level targets: one share per zone assigned to all its pixels, summed over scenarios.
    - The future climate and socio-economic file `data/02_intermediate/prior_model_1km_future_inputs.parquet` (written by `scripts/core/merge_future_projections.R`) is not read by any R script.
    - The BMLEH pipeline writes no projection xmat at all.
    - `downscale()` applies `xmat_proj[times == curr_time]` *after* solving `curr_time`, so projected values would take effect one period late.
  - *Fix:* decide explicitly. Either declare the priors static in lineage (Principle 6, item 6), or build a scenario- and period-indexed projection xmat from the future-inputs file and pass it. In the second case, check the one-period offset and the extrapolation range.
- **Pair-specific restrictions are flattened, and BMLEH restrictions are probably never applied.** *Status: fixed (lringwald/cascade-core#1: keeps `from_class`, scenario wildcard `__NA_SCEN__`, key validation, fractional caps in all solver stages; lringwald/gamble-cascade#1: producer encoding, scored balancing class, post-solve compliance gate). Reproduced with synthetic tests; not yet run on BMLEH data.*
  - *Where:* `pipelines/BMLEH_Los1_CAPRI/compile_CAPRI_priors_and_restrictions.R:101-133` is the only pipeline that supplies restrictions (`restriction_intermediates_res = "BMLEH_Los1_restrictions"`). It builds *pair* rules: "prohibit intensification", `to_score > from_score`, per pixel with value = restricted share. It tags every row `times = "ALL"` and `scenario = "ALL"`.
  - *Flattening:* `R/execute_luc_downscaling.R:238` keeps only `ns`, `to_class`, `times`, the scenario ID, spatial keys and `value`, so it drops `from_class`. `:409-410` then takes the `max` per `ns` and `to_class`, and the solver treats the rows as applying to every from-class (wildcard `__NA_LU__`). With the 43 fine classes in `BMLEH_Los1_thematic_mapping.csv`, the scores are: `Artificial` 6; 33 `Cropland_arable_*` / `Cropland_permanent_*` classes 5; `Grassland_intensive` 4; `Grassland_extensive` 3; `Forests` 2; four `Natural_*` classes 1; `Waterbodies_inland` and `Wetlands_natural` 0. Every class with score $\ge 1$ is the destination of at least one prohibited pair, so after flattening it is restricted for **every** source.
    - Crop-to-crop shifts of equal intensity, which the rule allows (e.g. `Cropland_arable_barley → Cropland_arable_softwheat`), are blocked.
    - De-intensification is blocked too (e.g. `Cropland_arable_softwheat → Natural_unmanaged_fallow`, because `Waterbodies_inland → Natural_*` is prohibited).
    - Within each pixel's restricted share, only conversions into `Waterbodies_inland` or `Wetlands_natural` remain. "No intensification" becomes "almost no change".
    - The solver itself does support `from_class`-specific rows (`R/solve_biascorr_lu.R:170-173`).
  - *Not applied at all (from code reading, not yet confirmed in a run):* `"ALL"` is not a wildcard anywhere; the engine's placeholders are `__NA_TIME__` / `__NA_LU__` (`R/aux_func.R:5-8`). `apply_composite_scenario()` returns the rows unchanged. `execute_luc_downscaling():316` keeps `restrictions[.SCEN_ID. == temp_scen]`, which is empty unless a scenario is literally named `ALL`. `downscale()` also filters `restrictions[times == curr_time]`, which `"ALL"` never matches.
  - *Check:* in a BMLEH run, print `nrow(temp_restrict)` and `nrow(restrict_region)` per region.
  - *Fix:* drop the `times` column (`complete_restrictions()` in `R/aux_func.R:374-381` then replicates rows over all target periods) or write explicit periods. Write one row per real scenario (a scenario column is mandatory; `execute_luc_downscaling()` stops without one at `:237`). Add `"from_class"` to `keep_cols_restr` at `:238`, and add a post-solve check that no prohibited pair received area.
- **Double counting in the BMLEH policy aggregates** (*open*; data-pipelines Principle 13).
  - *Where:* `pipelines/BMLEH_Los1_CAPRI/process_policy_aggregates.R:226-235`. The crop and livestock economics are collected with `list.files(paths$temp, pattern = "^production_downscaled_.*\\.qs$")` and `"^lsu_production_downscaled_.*\\.qs$"` from the shared `paths$temp`, with no run filter, and with a silent fallback to `out_dir` when none match. `:259` sums by `ns, NUTS3, scenario, Indicator`, which collapses rows from different runs.
  - *Effect (as reported from the 2026-11 report review):* every run's intermediates accumulate in the shared folder, so monetary totals were doubled in the second run and tripled in the third (e.g. baseline net farm profit around 44 and 66 bn € instead of around 22 bn €). Areas, read from the run-specific `out_dir` (`:268`), were correct, so per-hectare values were inflated while relative shares were not.
  - *Fix:*
    - resolve the files of the current run explicitly (run ID required; stop if it is missing);
    - assert the expected file count and a single run ID;
    - assert unique keys before the sum at `:259`;
    - reconcile control totals against the producing step.
- **`read_intermediate()` reads from a shared folder** (*open, risk*): `get_intermediate_path()` (`scripts/core/aux_codes.R:84-96`) picks the newest matching file in `paths$temp` by the date in its name.
  - It cannot double count, but if the current run did not write an intermediate, it silently reads an earlier run's file.
  - Its fallback pattern drops the pipeline name, so it can match another pipeline's file.
  - Reusing intermediates across runs is intended (skipping steps), but the run whose file was used should be logged and recorded in the run manifest.


suppressPackageStartupMessages(library(data.table))
current_model_cfg <- list(
  project_name     = "ACT4CAP",
  macro_model_name = "AgMIP",
  project_model    = "ACT4CAP_AgMIP",
  input_dir        = if (nzchar(Sys.getenv("AGMIP_INPUT_DIR", "")) && dir.exists(Sys.getenv("AGMIP_INPUT_DIR"))) Sys.getenv("AGMIP_INPUT_DIR")
                     else if (nzchar(Sys.getenv("INPUT_DIR", "")) && dir.exists(Sys.getenv("INPUT_DIR"))) Sys.getenv("INPUT_DIR")
                     else if (dir.exists("P:/globiom/Projects/DS_AgMIP/AgMIP_output/")) "P:/globiom/Projects/DS_AgMIP/AgMIP_output/"
                     else if (dir.exists("/mnt/pdrv/globiom/Projects/DS_AgMIP/AgMIP_output/")) "/mnt/pdrv/globiom/Projects/DS_AgMIP/AgMIP_output/"
                     else "data/macro_models",
  output_dir       = if (nzchar(Sys.getenv("AGMIP_OUTPUT_DIR", ""))) Sys.getenv("AGMIP_OUTPUT_DIR")
                     else if (nzchar(Sys.getenv("OUTPUT_DIR", ""))) Sys.getenv("OUTPUT_DIR")
                     else if (dir.exists("P:/globiom/Projects/DS_AgMIP/DS_results/outputs")) "P:/globiom/Projects/DS_AgMIP/DS_results/outputs"
                     else "data/outputs",
  temp_dir         = if (nzchar(Sys.getenv("AGMIP_TEMP_DIR", ""))) Sys.getenv("AGMIP_TEMP_DIR")
                     else if (nzchar(Sys.getenv("TEMP_DIR", ""))) Sys.getenv("TEMP_DIR")
                     else if (nzchar(Sys.getenv("AGMIP_OUTPUT_DIR", ""))) file.path(Sys.getenv("AGMIP_OUTPUT_DIR"), "intermediate")
                     else if (nzchar(Sys.getenv("OUTPUT_DIR", ""))) file.path(Sys.getenv("OUTPUT_DIR"), "intermediate")
                     else if (.Platform$OS.type == "windows" && dir.exists("P:/globiom/Projects/DS_AgMIP/DS_results/temp/")) "P:/globiom/Projects/DS_AgMIP/DS_results/temp/"
                     else if (dir.exists("/mnt/pdrv/globiom/Projects/DS_AgMIP/DS_results/temp/")) "/mnt/pdrv/globiom/Projects/DS_AgMIP/DS_results/temp/"
                     else "data/intermediate",

  input_files      = list(
    main   = if (nzchar(Sys.getenv("AGMIP_INPUT_FILE", ""))) Sys.getenv("AGMIP_INPUT_FILE")
             else if (nzchar(Sys.getenv("INPUT_FILE", ""))) Sys.getenv("INPUT_FILE")
             else "20260916_compr_scens_merge.csv",
    forest = NULL
  ),

  # =========================================================================
  # 2. Spatial & Column Mappings
  # =========================================================================
  spatial_key      = "GLOB_country",
  country_col      = "GLOB_country",
  lu_class_col     = "lu_class",
  area_col         = "area_km2",
  region_col       = "Ns_reg",
  scen_col         = "scenario",
  native_scen_cols = "scenario",
  time_col         = "year",

  base_year         = if ((exists("OVERRIDE_AGMIP_MODEL") && tolower(OVERRIDE_AGMIP_MODEL) == "capri") ||
                          (nzchar(Sys.getenv("AGMIP_MODEL")) && tolower(Sys.getenv("AGMIP_MODEL")) == "capri")) 2020
                      else if ((exists("OVERRIDE_AGMIP_MODEL") && tolower(OVERRIDE_AGMIP_MODEL) == "magnet") ||
                               (nzchar(Sys.getenv("AGMIP_MODEL")) && tolower(Sys.getenv("AGMIP_MODEL")) == "magnet")) 2017
                      else 2000,
  start_map_prefix  = "LUM_fit_with_energy_levels_and_new_FM_",
  base_scen         = if (nzchar(Sys.getenv("SCENARIO", ""))) tolower(Sys.getenv("SCENARIO")) else "base",
  skipped_zone_handling = "other",  # slivers at the grid edge / inexact border mapping become exogenous _other land
  # Intermediates (get_intermediate_path): the run's own file first; reuse of an earlier
  # run's file is logged and recorded in <run>_intermediate_resolutions.csv
  intermediate_reuse            = TRUE,   # FALSE: stop when this run did not write the intermediate
  intermediate_unnamed_fallback = FALSE,  # TRUE: legacy lookup without the pipeline name (can match other pipelines)
  scenario_mapping  = data.table(
    scenario = if (nzchar(Sys.getenv("SCENARIO", ""))) tolower(Sys.getenv("SCENARIO")) else "base"
  ),
  scen_filter       = if (nzchar(Sys.getenv("SCENARIO", ""))) tolower(Sys.getenv("SCENARIO")) else c("base"),
  year_filter       = NULL,
  country_filter    = NULL,
  default_countries = c(
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "CzechRep", "Denmark",
    "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
    "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
    "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "UK"
  ),

  # =========================================================================
  # 4. Resolution Settings
  # =========================================================================
  pixel_res              = if (nzchar(Sys.getenv("RESOLUTION", ""))) {
                             res_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", Sys.getenv("RESOLUTION"))))
                             if (!is.na(res_num) && res_num %in% c(1L, 2L, 3L, 4L, 5L, 10L, 20L, 30L, 40L, 50L, 100L, 200L)) res_num else 10L
                           } else 10L,
  pixel_intersect_col    = NULL,
  macro_model_resolution = "Ns_reg",

  # =========================================================================
  # 5. Downscaling Engine Integration
  # =========================================================================
  raw_targets_intermediates = NULL,
  raw_area_col              = NULL,
  prior_grouping_cols       = c("GLOB_country"),
  keep_cols                 = c("GLOB_country"),
  prior_intermediates       = NULL,
  prior_intermediates_res   = NULL,
  restriction_intermediates = NULL,
  restriction_intermediates_res = NULL,
  run_lsu_default           = FALSE,

  # =========================================================================
  # 6. Execution Scripts (The Pipeline Entry Points)
  # =========================================================================
  step1_scripts     = c("compile_agmip_csv_data.R"),
  step2_scripts     = c("compile_agmip_start_map.R"),
  harmonize_scripts = c("process_agmip_start_map_harmonization.R"),
  step3_scripts     = c("compute_agmip_transitions.R"),
  prior_scripts     = NULL,  # Skipped: downscaling without priors
  step4_scripts     = c("process_lu_downscaling.R"),
  step4.5_scripts   = NULL,
  step5_scripts     = NULL,
  step5a_scripts    = c("compute_agmip_inputs_downscaling.R"),
  step5b_scripts    = NULL,

  # =========================================================================
  # 7. Diagnostics & Reporting
  # =========================================================================
  calc_prior_impact_metric = FALSE,
  generate_indicators      = TRUE,
  generate_report          = TRUE,

  # =========================================================================
  # 8. Model-Specific Tunings
  # =========================================================================
  agmip_model_name     = if (exists("OVERRIDE_AGMIP_MODEL") && !is.null(OVERRIDE_AGMIP_MODEL)) OVERRIDE_AGMIP_MODEL else if (nzchar(Sys.getenv("AGMIP_MODEL"))) Sys.getenv("AGMIP_MODEL") else "globiom",
  classes_to_harmonize = NULL,
  yield_downscale_method = "cross_entropy"
)

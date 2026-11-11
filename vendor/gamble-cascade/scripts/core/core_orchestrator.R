#' ---
#' title: "LAMASUS Downscaling Pipeline"
#' author: "LAMASUS Downscaling Team"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     theme: flatly
#' ---
#'
#' # LAMASUS Downscaling Orchestrator
#'
#' This interactive notebook orchestrates the full pipeline from raw model data (GDX/CSV) down to high-resolution spatial TIFFs and CSVs.
#'
#' ## 1. Global Configuration
#'
# ===============================================================
# MASTER SCRIPT: LAMASUS Downscaling Pipeline
# ===============================================================
# This script orchestrates the full pipeline from raw GDX data
# to high-resolution downscaled spatial maps (LU, LUC, LSU).
# ===============================================================
# Clear workspace except for step control overrides passed by a caller script

# ===============================================================
# SCRIPT: run_downscaling.R
# ===============================================================
source_script <- function(script_name, model_dir) {
  # Remove 'codes/' prefix if present
  base_name <- basename(script_name)

  wd <- getwd()
  paths_to_try <- unique(c(
    file.path(wd, script_name),
    script_name,
    file.path(wd, "pipelines", model_dir, script_name),
    file.path("pipelines", model_dir, script_name),
    file.path(wd, "pipelines", model_dir, base_name),
    file.path("pipelines", model_dir, base_name),
    file.path(wd, "scripts", "core", script_name),
    file.path("scripts", "core", script_name),
    file.path(wd, "scripts", "core", base_name),
    file.path("scripts", "core", base_name),
    file.path(wd, "scripts", model_dir, script_name),
    file.path("scripts", model_dir, script_name),
    file.path(wd, "scripts", model_dir, base_name),
    file.path(wd, "scripts", base_name),
    file.path(wd, "scripts", script_name),
    file.path("scripts", model_dir, base_name),
    file.path("scripts", base_name),
    file.path("scripts", script_name)
  ))

  for (p in paths_to_try) {
    if (file.exists(p)) {
      cat("Sourcing:", p, "\n")
      source(p, local = FALSE)
      return(invisible(NULL))
    }
  }

  cat("DEBUG: getwd() =", getwd(), "\n")
  cat("DEBUG: list.files('scripts') =", paste(list.files("scripts"), collapse=", "), "\n")
  stop(sprintf("Cannot find script: %s (tried: %s)", script_name, paste(paths_to_try, collapse=", ")))
}


#'
#+ setup, message=FALSE, warning=FALSE
# ---------------------------------------------------------------
# Library & Environment Setup (Lightweight packages first)
# ---------------------------------------------------------------
suppressMessages({
  library(data.table)
  library(terra)
  library(cli)
  library(optparse)
})

# Load the cascade-core engine once for the session (pinned submodule, or CASCADE_CORE_PATH)
# and record which copy (scripts/core/run_context.R)
source_script("run_context.R", model_name)
ensure_cascade_engine()

# ---------------------------------------------------------------
# Synchronize agent rulebooks from agent-rules-hub before execution
# ---------------------------------------------------------------
rule_update_script <- c("scripts/core/update_agent_rules.R", "scripts/update_agent_rules.R")
rule_update_script <- rule_update_script[file.exists(rule_update_script)][1]
if (!is.na(rule_update_script)) {
  tryCatch({
    source(rule_update_script, local = TRUE)
    if (exists("update_agent_rules", mode = "function")) {
      update_agent_rules()
    }
  }, error = function(e) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_warning(sprintf("Rulebook update check encountered an issue: %s", conditionMessage(e)))
    }
  })
}

# Load the metadata tracker
source_script("metadata_tracker.R", model_name)

# Disable scientific notation for cleaner CSV writing across the entire pipeline
options(scipen = 999)

# Prevent 'Unable to allocate TMP for items in parallel batch counting' error
# by limiting data.table threads to a safe number that won't exhaust Windows memory
setDTthreads(1)

#'
#' ## 2. Configuration & Model Registry
# ---------------------------------------------------------------
# Configuration & Model Registry
# ---------------------------------------------------------------
# Master framework variables to retain across all step memory cleanups
protected_vars <- c("model_name", "model_dir_name", "current_model_cfg", "paths", "files",
                    "downscale_resolution", "mapping_grid_path", "PIXEL_RES", "PIXEL_INTERSECT_COL",
                    "MACRO_MODEL_RESOLUTION", "RUN_COMPILATION", "RUN_HARMONIZATION", "RUN_TRANSITIONS",
                    "RUN_PRIORS", "RUN_DS_LU", "RUN_DS_LSU", "RUN_DS_ECONOMICS", "RUN_CHECKS", "RUN_DS_AGGREGATION",
                    "DOWNSCALE_MODE", "scen_chunk", "safe_scen_chunk", "scen_filter", "country_filter",
                    "year_filter", "active_scenarios", "run_basename", "output_prefix", "RUN_DATE", "RUN_DATE_SOURCE", "OVERRIDE_DATE", "cur_run_date",
                    "solve_cross_entropy_yield", "align_production_cross_entropy_dt",
                    "intermediate_file_name", "get_intermediate_path", "save_intermediate", "read_intermediate")


.NATIVE_SCEN_COLS <- if (!is.null(current_model_cfg$native_scen_cols)) current_model_cfg$native_scen_cols else "scenario"

BASE_YEAR <- if (!is.null(current_model_cfg$base_year)) current_model_cfg$base_year else 2000

# --- ACTIVE STEP CONTROL FLAGS ---
if (!exists("RUN_COMPILATION"))   RUN_COMPILATION   <- TRUE # Step 1: Extract model data from GDX
if (!exists("RUN_HARMONIZATION")) RUN_HARMONIZATION <- TRUE  # Step 2: Prepare base maps & harmonize
if (!exists("RUN_TRANSITIONS"))   RUN_TRANSITIONS   <- TRUE  # Step 3: Compute LU transition priors
if (!exists("RUN_PRIORS"))        RUN_PRIORS        <- TRUE  # Step 3.5: Prior Module Construction
if (!exists("RUN_DS_LU"))         RUN_DS_LU         <- TRUE  # Step 4: Run Land Use downscaling (LUC)
if (!exists("RUN_DS_LSU"))        RUN_DS_LSU        <- current_model_cfg$run_lsu_default # Step 5: Run Livestock downscaling (LSU)
if (!exists("RUN_CHECKS"))        RUN_CHECKS        <- TRUE  # Step 6: Post-processing & Validation
if (!exists("RUN_DS_AGGREGATION")) RUN_DS_AGGREGATION <- TRUE  # Step 7: Hierarchical TIFF Aggregation
if (!exists("USE_START_MAP_COV"))  USE_START_MAP_COV  <- if (!is.null(current_model_cfg$USE_START_MAP_COV)) current_model_cfg$USE_START_MAP_COV else FALSE
# --- DOWNSCALING OPTIMIZATION MODE ---
if (!exists("DOWNSCALE_MODE"))    DOWNSCALE_MODE    <- "fast" # Options: "exact", "fast"

# Covariate selection for prior models (MNL)
covariate_filter <- NULL
USE_START_MAP_COV <- TRUE

# --- DERIVED RESOLUTION SETTINGS ---
PIXEL_RES              <- current_model_cfg$pixel_res
PIXEL_INTERSECT_COL    <- current_model_cfg$pixel_intersect_col
MACRO_MODEL_RESOLUTION <- current_model_cfg$macro_model_resolution
downscale_resolution   <- paste0(PIXEL_RES, "km", if (!is.null(PIXEL_INTERSECT_COL)) paste0("_by_", PIXEL_INTERSECT_COL))
REPORTING_INTENSITY_UNIT <- "km2"

#'
#' ## 3. Paths & Scoping
#+ paths
# --- PATH CONFIGURATION ---
paths <- list(
  input        = "input/",
  temp         = if (!is.null(current_model_cfg$temp_dir)) current_model_cfg$temp_dir else stop("[core_orchestrator] temp_dir not set in model config. Please define temp_dir in your config.R."),
  output_dir   = if (!is.null(current_model_cfg$output_dir)) current_model_cfg$output_dir else stop("[core_orchestrator] output_dir not set in model config. Please define output_dir in your config.R."),
  aux_files    = "aux_files",
  codes        = "codes",
  gridwork_dir = "data/aux_files/",
  input_dir    = current_model_cfg$input_dir
)

if (!is.null(current_model_cfg$run_basename)) {
  run_basename <- current_model_cfg$run_basename
} else if (!is.null(current_model_cfg$project_model) && !is.null(current_model_cfg$agmip_model_name)) {
  run_basename <- paste0(current_model_cfg$project_model, "_", toupper(current_model_cfg$agmip_model_name))
} else if (!is.null(current_model_cfg$agmip_model_name)) {
  run_basename <- paste0("AgMIP_", toupper(current_model_cfg$agmip_model_name))
} else if (!is.null(current_model_cfg$input_files$main)) {
  run_basename <- gsub("\\.gdx$", "", current_model_cfg$input_files$main)
  run_basename <- gsub("^final_", "", run_basename)
  run_basename <- gsub("\\.csv$", "", run_basename)
} else {
  run_basename <- paste0(model_name, "_run")
}
# Determine run date with persistent session lockfile support
lockfile_path <- file.path(paths$temp, paste0(".run_session_", run_basename, "_", PIXEL_RES, "km.rds"))

# Run date: declared OVERRIDE_DATE > config.R run_date > this run's session lockfile within
# run_resume_days (config, default 5) > today. No "latest output directory" guess.
rd <- resolve_run_date(lockfile_path, current_model_cfg,
                       declared = if (exists("OVERRIDE_DATE") && !is.null(OVERRIDE_DATE)) OVERRIDE_DATE else NULL)
RUN_DATE <- rd$run_date
RUN_DATE_SOURCE <- rd$source
cli_alert_info(sprintf("Run date %s (%s)", RUN_DATE, RUN_DATE_SOURCE))

OVERRIDE_DATE <- RUN_DATE
output_prefix <- paste0("DS_", run_basename, "_", PIXEL_RES, "km_", RUN_DATE)
# Run-level fields for the raster JSON sidecars (results_to_raster(metadata = ...)).
r2r_meta_base <- list(
  run_id = output_prefix,
  version = as.character(RUN_DATE),
  config_hash = if (exists("current_model_cfg") && requireNamespace("digest", quietly = TRUE)) digest::digest(current_model_cfg) else NULL
)

# Save or refresh session lockfile
if (!dir.exists(paths$temp)) dir.create(paths$temp, recursive = TRUE)
session_info <- list(
  model_name    = model_name,
  pixel_res     = paste0(PIXEL_RES, "km"),
  run_basename  = run_basename,
  run_date      = RUN_DATE,
  output_prefix = output_prefix,
  output_dir    = file.path(paths$output_dir, output_prefix),
  updated_at    = Sys.time()
)
try(saveRDS(session_info, lockfile_path), silent = TRUE)

# Update output directory to include the prefix (run name and date)
paths$output_dir <- file.path(paths$output_dir, output_prefix)
if (!dir.exists(paths$output_dir)) dir.create(paths$output_dir, recursive = TRUE)

# Run manifest <output_prefix>_run_manifest.json: identity, run date and its source, code
# versions (pipeline, engine, submodule pins, gamble-core), environment, config hash
init_run_manifest(RUN_DATE_SOURCE)

# Output file definitions
files <- list(
  lu_output   = file.path(paths$output_dir, paste0(output_prefix, "_lu.parquet")),
  luc_output  = file.path(paths$output_dir, paste0(output_prefix, "_luc.parquet")),
  lsu_output  = file.path(paths$output_dir, paste0(output_prefix, "_lsu.parquet")),
  diag_output = file.path(paths$output_dir, paste0(output_prefix, "_diagnostics.csv")),
  report_output = file.path(paths$output_dir, paste0(output_prefix, "_target_performance_report.csv")),
  pre_ds_loss_output = file.path(paths$output_dir, paste0(output_prefix, "_pre_ds_target_loss.csv")),
  harmonization_check_output = file.path(paths$output_dir, paste0(output_prefix, "_harmonization_check.csv")),
  area_conservation_output = file.path(paths$output_dir, paste0(output_prefix, "_area_conservation.csv")),
  metadata_output = file.path(paths$output_dir, paste0(output_prefix, "_sources_metadata.csv")),
  prior_impact_output = file.path(paths$output_dir, paste0(output_prefix, "_prior_impact_regional.csv")),
  prior_impact_pixels_output = file.path(paths$output_dir, paste0(output_prefix, "_prior_impact_pixels.csv"))
)

tol_rel <- 0.1
area_conv_factor <- 10 # 1000ha = 10km2

# Set spatial filter (e.g., "Germany"). Set to NULL for all default_countries.
country_filter <- if (!is.null(current_model_cfg$country_filter)) current_model_cfg$country_filter else current_model_cfg$default_countries

# Temporal & Scenario scope
year_filter   <- current_model_cfg$year_filter

# Only use config scen_filter if command-line scen_filter wasn't provided (i.e. is NULL)
if (!exists("scen_filter") || is.null(scen_filter)) {
  scen_filter <- current_model_cfg$scen_filter
}

if (identical(scen_filter, "ALL")) {
  scen_filter <- NULL
}

if (!is.null(scen_filter)) {
  native_cols <- if (!is.null(current_model_cfg$native_scen_cols)) current_model_cfg$native_scen_cols else "scenario"
  if (length(native_cols) > 1 && !is.null(current_model_cfg$scenario_mapping)) {
    primary_col <- native_cols[1]
    mapping <- current_model_cfg$scenario_mapping
    if (primary_col %in% names(mapping)) {
      matched <- mapping[get(primary_col) %in% scen_filter]
      if (nrow(matched) > 0) {
        scen_filter <- matched[, do.call(paste, c(.SD, sep = "::")), .SDcols = native_cols]
      }
    }
  }
}

#'
#' ## 4. Core Grid Processing
#+ grid
# ---------------------------------------------------------------
# Mappings & Initial Factor Setups
# ---------------------------------------------------------------
message("\n>>> Initialising Pipeline Context...")

# Load the master grid mapping (dynamically pick the most recent)
mapping_files_pq <- list.files(paths$gridwork_dir, pattern = "one_kmID_master_mapping_.*\\.parquet", full.names = TRUE)
if(length(mapping_files_pq) > 0) {
  mapping_grid_path_parquet <- sort(mapping_files_pq, decreasing = TRUE)[1]
  mapping_grid_path_rds     <- sub("\\.parquet$", ".rds", mapping_grid_path_parquet)
} else {
  mapping_grid_path_parquet <- file.path(paths$gridwork_dir, "one_kmID_master_mapping_2026-05-28.parquet")
  mapping_grid_path_rds     <- file.path(paths$gridwork_dir, "one_kmID_master_mapping_2026-05-28.rds")
}
mapping_grid_path <- if (file.exists(mapping_grid_path_parquet)) mapping_grid_path_parquet else mapping_grid_path_rds

# Build columns to keep dynamically
keep_cols <- c(
  "INSPIRE_Europe_buffer_1kmID", "EEA_1kmID", "x", "y"
)
if (length(current_model_cfg$keep_cols) > 0) {
  keep_cols <- c(keep_cols, current_model_cfg$keep_cols)
}

# Add dynamic resolution columns
keep_cols <- c(keep_cols,
               paste0("x_", paste0(PIXEL_RES,"km"), "ID"),
               paste0("y_", paste0(PIXEL_RES,"km"), "ID"),
               paste0("EEA_", paste0(PIXEL_RES,"km"), "ID"))

# Add dynamic intersection column if applicable
if (!is.null(PIXEL_INTERSECT_COL)) {
  keep_cols <- c(keep_cols, PIXEL_INTERSECT_COL)
}

if (file.exists(mapping_grid_path_parquet)) {
  message(">>> Loading master grid mapping from Parquet (memory-efficient)...")
  require(arrow)
  if (!exists("is_accelerator", mode = "function")) {
    is_accelerator <- function() nzchar(Sys.getenv("PROJECT_SLUG", ""))
  }
  # Determine available columns in parquet file to intersect with keep_cols
  pq_cols <- arrow::open_dataset(mapping_grid_path_parquet)$schema$names
  load_cols <- intersect(keep_cols, pq_cols)
  mapping_grid <- as.data.table(arrow::read_parquet(
    mapping_grid_path_parquet,
    col_select = all_of(load_cols),
    memory_map = if (is_accelerator()) FALSE else TRUE
  ))
} else if (file.exists(mapping_grid_path_rds)) {
  message(">>> Loading master grid mapping from RDS...")
  mapping_grid <- readRDS(mapping_grid_path_rds)
} else {
  stop("Master grid mapping not found: ", mapping_grid_path_rds)
}

if (exists("mapping_grid")) {
  # Apply model-specific spatial filtering and scope detection
  if (current_model_cfg$macro_model_name == "LAMASUS_GLOBIOM") {
    # Apply country scoping using memory-efficient data.table filtering
    if (!is.null(country_filter)) {
      target_countries <- unique(c(country_filter, "GLOB_NA"))
      mapping_grid <- mapping_grid[GLOB_country %in% target_countries]
    }

    # Derive TERRA_GLOBIOM flag if missing
    if (!"TERRA_GLOBIOM" %in% names(mapping_grid)) {
      mapping_grid$TERRA_GLOBIOM <- (mapping_grid$GLOB_SimUID != 0 & mapping_grid$GLOB_SimUID != "GLOB_NA")
    }

    # Apply strict spatial filtering based on the loaded start map scope
    if (!is.null(country_filter)) {
      valid_sim_countries <- as.character(unique(mapping_grid[TERRA_GLOBIOM == TRUE]$GLOB_country))
      old_count <- length(country_filter)
      country_filter <- intersect(country_filter, c(valid_sim_countries, "GLOB_NA"))
      message(sprintf(">>> Strict spatial filter: Kept %d valid countries (removed %d out-of-scope countries).", length(country_filter), old_count - length(country_filter)))
    }
  } else if (!is.null(current_model_cfg$agmip_model_name)) {
    # --- AgMIP CSV models: use GLOB_country for country-border-respecting downscaling ---
    # GLOB_country is loaded from the parquet and used as Ns_reg
    if ("GLOB_country" %in% names(mapping_grid)) {
      # Apply country filter
      if (!is.null(country_filter)) {
        target_countries <- unique(c(country_filter, "GLOB_NA"))
        mapping_grid <- mapping_grid[GLOB_country %in% target_countries]
        message(sprintf(">>> AgMIP country filter: Kept %d countries.", length(unique(mapping_grid$GLOB_country))))
      }
    } else {
      warning(">>> GLOB_country not found in mapping grid for AgMIP model. Country filtering skipped.")
    }
  }

  # Wrap for legacy downstream script expectations
  mappings <- list(mapping_grid = mapping_grid)

  # Ensure keep_cols contains only existing columns
  actual_keep <- intersect(keep_cols, names(mappings$mapping_grid))

  # Assign fractional area to each 1km pixel based on how many regions it intersects
  mappings$mapping_grid[, fractional_area := 1 / .N, by = INSPIRE_Europe_buffer_1kmID]

  # Construct unique mapping table and compute area
  grid_dt <- mappings$mapping_grid[, .(ns_area_km2 = sum(fractional_area)), by = actual_keep]

  # Add coordinates for spatial ID
  grid_dt[, x_ns := get(paste0("x_", paste0(PIXEL_RES,"km"), "ID"))]
  grid_dt[, y_ns := get(paste0("y_", paste0(PIXEL_RES,"km"), "ID"))]

  # Define compound ID (ns)
  if (!is.null(PIXEL_INTERSECT_COL) && PIXEL_INTERSECT_COL %in% names(grid_dt)) {
    grid_dt[, ns := paste0(get(paste0("EEA_", paste0(PIXEL_RES,"km"), "ID")), "_", get(PIXEL_INTERSECT_COL))]
  } else {
    grid_dt[, ns := as.character(get(paste0("EEA_", paste0(PIXEL_RES,"km"), "ID")))]
  }

  mappings$mapping_grid <- grid_dt
  rm(mapping_grid, grid_dt)

  # Generalize spatial key to 'Ns_reg' for all downstream operations
  if (current_model_cfg$spatial_key %in% names(mappings$mapping_grid)) {
    mappings$mapping_grid[, Ns_reg := get(current_model_cfg$spatial_key)]
  }

  # Pre-calculate regional area weights for the performance report later
  # (Because process_lu_downscaling.R will trim columns to save memory)
  if (current_model_cfg$macro_model_name == "LAMASUS_GLOBIOM") {
    reg_area_weights <- mappings$mapping_grid[, .(area = sum(GLOB_5arcminID_area_km2, na.rm=TRUE)), by = .(Ns_reg, GLOB_country)]
    reg_area_weights[, weight := area / sum(area), by = Ns_reg]
  }

  # Reclaim unused memory
  gc(verbose = FALSE)

  # Load heavier packages now that the grid footprint is minimized
  message(">>> Loading runtime environments...")
  require(gamstransfer)
  require(cli)

  # Source core logic and utilities
  source_script("aux_codes.R", model_dir_name)
  source_script("results_to_raster.R", model_dir_name)

  cli_h1("Pipeline Context Initialised")
} else {
  stop("Master grid mapping not found: ", mapping_grid_path_rds)
}

#'
#' ## 5. Model Execution Steps
# ---------------------------------------------------------------
# 3. Pipeline Execution
# ---------------------------------------------------------------
cli_h1("Executing Pipeline Steps")

#'
#' ### Step 1: Model Data Compilation
#' Extract model data from source (e.g., GDX) into standard RDS structures.
#+ step1
# --- Model Data Compilation ---
if (RUN_COMPILATION) {
  cli_h2("Data Compilation (GDX -> RDS)")

  env_pre_step1 <- ls(envir = .GlobalEnv)

  if (!is.null(current_model_cfg$step1_scripts)) {
    lapply(current_model_cfg$step1_scripts, function(x) source_script(x, model_dir_name))
  }

  env_post_step1 <- ls(envir = .GlobalEnv)
  new_objs <- setdiff(env_post_step1, c(env_pre_step1, "env_pre_step1", "env_post_step1", protected_vars))

  if (length(new_objs) > 0) {
    message(">>> Removing ", length(new_objs), " intermediate objects created during Step 1...")
    rm(list = new_objs, envir = .GlobalEnv)
  }
  gc(verbose = FALSE)
  cli_alert_success("Step 1 memory cleaned up.")
}

#'
#' ### Step 2: Base Map Compilation
#' Create the high-resolution start map natively or from compiled datasets.
#+ step2
# --- Spatial Harmonization ---
if (RUN_HARMONIZATION) {
  cli_h2("Base Map Compilation")
  env_pre_step2 <- ls(envir = .GlobalEnv)

  if (!is.null(current_model_cfg$step2_scripts)) {
    lapply(current_model_cfg$step2_scripts, function(x) source_script(x, model_dir_name))
  }
}

#'
#' ### Step 2b: Spatial Harmonization
#' Align base maps with macro model totals.
#+ step2b
# --- Spatial Harmonization ---
if (RUN_HARMONIZATION) {
  cli_h2("Spatial Harmonization (Base Map alignment)")
  if (!is.null(current_model_cfg$harmonize_scripts)) {
    lapply(current_model_cfg$harmonize_scripts, function(x) source_script(x, model_dir_name))
  }
  write_ipf_diagnostics("start-map harmonization")

  env_post_step2 <- ls(envir = .GlobalEnv)
  new_objs <- setdiff(env_post_step2, c(env_pre_step2, "env_pre_step2", "env_post_step2", "ds_start_map", protected_vars))

  if (length(new_objs) > 0) {
    message(">>> Removing ", length(new_objs), " intermediate objects created during Step 2...")
    rm(list = new_objs, envir = .GlobalEnv)
  }
  gc(verbose = FALSE)
  cli_alert_success("Step 2 memory cleaned up (ds_start_map retained).")
}


# Snapshot the environment before Steps 3 run to cleanly purge all leftovers afterward
env_pre_step3 <- ls(envir = .GlobalEnv)

#'
#' ### Step 3: Transition Priors
#' Compute transition priors based on model outputs.
#+ step3
# --- Transition Priors ---
if (RUN_TRANSITIONS) {
  cli_h2("Computing Transition Priors & Restrictions")
  if (!is.null(current_model_cfg$step3_scripts)) {
    lapply(current_model_cfg$step3_scripts, function(x) source_script(x, model_dir_name))
  }
  write_ipf_diagnostics("transition targets")
}

#'
#' ### Step 3.5: Prior Module Construction
#+ step3_5
# --- Prior Module Construction ---
if ((RUN_TRANSITIONS || RUN_DS_LU) && RUN_PRIORS) {
  cli_h2("Prior Module Construction")
  if (!is.null(current_model_cfg$prior_scripts)) {
    for (scr in current_model_cfg$prior_scripts) {
      gc()
      source_script(scr, model_dir_name)
    }
  }
}

# Clean up all intermediate objects created during Step 3 by checking against the pre-step snapshot
env_post_step3 <- ls(envir = .GlobalEnv)
new_objs <- setdiff(env_post_step3, c(env_pre_step3, "env_pre_step3", "env_post_step3", protected_vars))

if (length(new_objs) > 0) {
  message(">>> Removing ", length(new_objs), " intermediate objects created during Step 3...")
  rm(list = new_objs, envir = .GlobalEnv)
}
gc(verbose = FALSE)

# Snapshot the environment before Step 4
# (Moved inside the loop below)

#'
#' ### Step 4: Land Use Downscaling
#+ step4
# --- Land Use Downscaling ---
cat("DEBUG RUN_DS_LU:", RUN_DS_LU, "RUN_DS_LSU:", RUN_DS_LSU, "exists(RUN_DS_ECONOMICS):", exists("RUN_DS_ECONOMICS"), "\n")
if (exists("RUN_DS_ECONOMICS")) cat("DEBUG RUN_DS_ECONOMICS:", RUN_DS_ECONOMICS, "\n")
if (RUN_DS_LU || RUN_DS_LSU || (exists("RUN_DS_ECONOMICS") && RUN_DS_ECONOMICS)) {
  cli_h1("=== STARTING DOWN-SCALING ENGINE (SCENARIO CHUNKED) ===")
  # Determine active scenarios from targets (compiled in Step 1/2)
  targets_full_path <- tryCatch(get_intermediate_path("LUC_targets_full"), error = function(e) NULL)
  if (!is.null(targets_full_path) && file.exists(targets_full_path)) {
    active_scenarios <- unique(if(grepl("\\.qs$", targets_full_path)) qs2::qs_read(targets_full_path)$scenario else readRDS(targets_full_path)$scenario)
  } else {
    active_scenarios <- if (!is.null(scen_filter)) scen_filter else "BASE"
  }

  if (!is.null(scen_filter) && !("ALL" %in% scen_filter)) {
    active_scenarios <- intersect(active_scenarios, scen_filter)
  }

  # Enforce concrete scenario ordering if specified in current_model_cfg$scenario_mapping
  if (!is.null(current_model_cfg$scenario_mapping)) {
    ordered_scens <- if ("LAMAScen" %in% names(current_model_cfg$scenario_mapping)) {
      current_model_cfg$scenario_mapping$LAMAScen
    } else {
      current_model_cfg$scenario_mapping[[1]]
    }
    active_scenarios <- c(intersect(ordered_scens, active_scenarios), setdiff(active_scenarios, ordered_scens))
  }

  # Helper to provide dynamic template custom for rasterization
  ensure_template_custom <- function() {
    if (exists("template_custom", envir = .GlobalEnv) && !is.null(get("template_custom", envir = .GlobalEnv))) {
      return(get("template_custom", envir = .GlobalEnv))
    }
    
    template_cache_file <- file.path(paths$output_dir, paste0("template_custom_", PIXEL_RES, "km.tif"))
    if (file.exists(template_cache_file)) {
      tmpl <- terra::rast(template_cache_file)
      assign("template_custom", tmpl, envir = .GlobalEnv)
      return(tmpl)
    }
    
    buffer_1km_path <- file.path(paths$gridwork_dir, "../aux_files/INSPIRE_Europe_buffer_1km.tif")
    if (!file.exists(buffer_1km_path)) {
      buffer_1km_path <- file.path(paths$gridwork_dir, "INSPIRE_Europe_buffer_1km.tif")
    }
    
    if (file.exists(buffer_1km_path) && exists("mapping_grid_path_parquet") && file.exists(mapping_grid_path_parquet)) {
      cli::cli_alert_info("Generating specialized template from mapping grid...")
      reso_num <- PIXEL_RES
      base_id_col <- paste0("EEA_", reso_num, "kmID")
      x_col <- paste0("x_", reso_num, "kmID")
      y_col <- paste0("y_", reso_num, "kmID")
      
      mg_mask <- arrow::read_parquet(
        mapping_grid_path_parquet,
        col_select = tidyselect::any_of(c(base_id_col, "GLOB_country", "NUTS0", "CAPRI_NUTS", x_col, y_col)),
        memory_map = if (exists("is_accelerator", mode = "function") && is_accelerator()) FALSE else TRUE
      )
      
      mg_coords <- data.frame(
        x = mg_mask[[x_col]],
        y = mg_mask[[y_col]],
        z = mg_mask[[base_id_col]]
      )
      tmpl <- terra::rast(mg_coords, type = "xyz")
      names(tmpl) <- base_id_col
      
      if (!is.null(country_filter)) {
        is_target <- mg_mask$GLOB_country %in% c(country_filter, "GLOB_NA")
        if ("NUTS0" %in% names(mg_mask)) {
          is_target <- is_target | (mg_mask$NUTS0 %in% country_filter)
        }
        if ("CAPRI_NUTS" %in% names(mg_mask)) {
          is_target <- is_target | (substr(mg_mask$CAPRI_NUTS, 1, 2) %in% country_filter)
        }
        target_ids <- unique(mg_mask[is_target, ][[base_id_col]])
        
        template_vals <- terra::values(tmpl)
        template_vals[!(template_vals %in% target_ids)] <- NA
        terra::values(tmpl) <- template_vals
      }
      
      try(terra::writeRaster(tmpl, template_cache_file, overwrite = TRUE), silent = TRUE)
      assign("template_custom", tmpl, envir = .GlobalEnv)
      return(tmpl)
    }
    return(NULL)
  }

  # One-time cleanup of cumulative multi-scenario output files if starting fresh from the first scenario
  if (RUN_DS_LU && length(active_scenarios) > 0) {
    first_safe_scen <- gsub(":", "_", active_scenarios[1])
    first_lu_exists <- file.exists(file.path(paths$output_dir, paste0(output_prefix, "_lu_", first_safe_scen, ".parquet"))) ||
                       file.exists(file.path(paths$output_dir, paste0(output_prefix, "_lu_", first_safe_scen, ".csv")))
    if (!first_lu_exists) {
      for (f in c("diag_output", "report_output", "area_conservation_output", "prior_impact_output", "prior_impact_pixels_output")) {
        if (!is.null(files[[f]]) && file.exists(files[[f]])) file.remove(files[[f]])
      }
    }
  }

  for (scen_chunk in active_scenarios) {
    cli_alert_info(sprintf(">>> Executing Optimization Chunk for Scenario: %s", scen_chunk))
    # Override scen_filter for this iteration so inner scripts only load/process one scenario at a time
    scen_filter <- scen_chunk

    # Inject dynamic scenario filenames (detect existing parquet/csv or default to parquet)
    safe_scen_chunk <- gsub(":", "_", scen_chunk)
    lu_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen_chunk, ".parquet"))
    lu_csv <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen_chunk, ".csv"))
    files$lu_output   <- if (file.exists(lu_pq)) lu_pq else if (file.exists(lu_csv)) lu_csv else lu_pq

    luc_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_luc_", safe_scen_chunk, ".parquet"))
    luc_csv <- file.path(paths$output_dir, paste0(output_prefix, "_luc_", safe_scen_chunk, ".csv"))
    files$luc_output  <- if (file.exists(luc_pq)) luc_pq else if (file.exists(luc_csv)) luc_csv else luc_pq

    lsu_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_lsu_", safe_scen_chunk, ".parquet"))
    lsu_csv <- file.path(paths$output_dir, paste0(output_prefix, "_lsu_", safe_scen_chunk, ".csv"))
    files$lsu_output  <- if (file.exists(lsu_pq)) lsu_pq else if (file.exists(lsu_csv)) lsu_csv else lsu_pq

    if (RUN_DS_LU) {
      env_pre_step4 <- ls(envir = .GlobalEnv)
      if (file.exists(files$lu_output) && file.exists(files$luc_output)) {
        cli_alert_info(">>> Step 4: Final LU/LUC outputs already exist. Skipping optimization.")
      } else {
        message("\n>>> Preparing data for subsequent steps...")
        latest_start_map <- get_intermediate_path("processed_ds_start_map", downscale_resolution)
        if (!is.null(current_model_cfg$step4_scripts)) {
          lapply(current_model_cfg$step4_scripts, function(x) source_script(x, model_dir_name))
        }

        # Cleanup intermediate objects created during Step 4
        env_post_step4 <- ls(envir = .GlobalEnv)
        new_objs <- setdiff(env_post_step4, c(env_pre_step4, "env_pre_step4", "env_post_step4", protected_vars))
        if (length(new_objs) > 0) rm(list = new_objs, envir = .GlobalEnv)
        gc(verbose = FALSE)
        cli_alert_success("Step 4 memory cleaned up.")
      }
    }


# Snapshot the environment before Step 4.5
env_pre_step45 <- ls(envir = .GlobalEnv)

#'
#' ### Step 4.5: High-Res Dynamic LSU Priors
#+ step4_5
# --- High-Res Dynamic LSU Priors ---
if (RUN_DS_LSU && !is.null(current_model_cfg$step4.5_scripts)) {
  cli_h2("High-Res Dynamic LSU Priors")
  lapply(current_model_cfg$step4.5_scripts, function(x) source_script(x, model_dir_name))

  # Cleanup intermediate objects created during Step 4.5
  env_post_step45 <- ls(envir = .GlobalEnv)
  new_objs <- setdiff(env_post_step45, c(env_pre_step45, "env_pre_step45", "env_post_step45", protected_vars))
  if (length(new_objs) > 0) rm(list = new_objs, envir = .GlobalEnv)
  gc(verbose = FALSE)
  cli_alert_success("Step 4.5 memory cleaned up.")
}

# Snapshot the environment before Step 5
env_pre_step5 <- ls(envir = .GlobalEnv)

#'
#' ### Step 5: Livestock Optimization (LSU)
#+ step5
# --- Livestock Downscaling ---
message("DEBUG: RUN_DS_LSU = ", RUN_DS_LSU, ", step5_scripts = ", paste(current_model_cfg$step5_scripts, collapse=", "))
if (RUN_DS_LSU && !is.null(current_model_cfg$step5_scripts)) {
  if (file.exists(files$lsu_output) && !(exists("FORCE_REDO_LSU") && isTRUE(FORCE_REDO_LSU))) {
    cli_alert_info(">>> Step 5: Final LSU outputs already exist. Skipping optimization.")
  } else {
    cli_h2("Livestock Optimization (LSU)")
    lapply(current_model_cfg$step5_scripts, function(x) source_script(x, model_dir_name))

    # Cleanup intermediate objects created during Step 5
    env_post_step5 <- ls(envir = .GlobalEnv)
    new_objs <- setdiff(env_post_step5, c(env_pre_step5, "env_pre_step5", "env_post_step5", protected_vars))
    if (length(new_objs) > 0) rm(list = new_objs, envir = .GlobalEnv)
    gc(verbose = FALSE)
    cli_alert_success("Step 5 memory cleaned up.")
  }
}

cur_run_date <- if (exists("RUN_DATE") && !is.null(RUN_DATE)) RUN_DATE else if (exists("OVERRIDE_DATE") && !is.null(OVERRIDE_DATE)) OVERRIDE_DATE else format(Sys.Date(), "%Y%m%d")

# Snapshot the environment before Step 5a
env_pre_step5a <- ls(envir = .GlobalEnv)

#'
#' ### Step 5a: Production Downscaling
#+ step5a
# --- Production Downscaling ---
if (!exists("RUN_DS_ECONOMICS")) RUN_DS_ECONOMICS <- RUN_DS_LU

if (RUN_DS_ECONOMICS && !is.null(current_model_cfg$step5a_scripts)) {
  # Skip only if THIS run already wrote the file (no reuse of earlier runs here)
  if (current_run_intermediate_exists(paste0("production_downscaled_", safe_scen_chunk))) {
    cli_alert_info(">>> Step 5a: Final Production outputs already exist for current run date. Skipping optimization.")
  } else {
    cli_h2("Production Downscaling")
    lapply(current_model_cfg$step5a_scripts, function(x) source_script(x, model_dir_name))

    # Cleanup intermediate objects created during Step 5a
    env_post_step5a <- ls(envir = .GlobalEnv)
    new_objs <- setdiff(env_post_step5a, c(env_pre_step5a, "env_pre_step5a", "env_post_step5a", protected_vars))
    if (length(new_objs) > 0) rm(list = new_objs, envir = .GlobalEnv)
    gc(verbose = FALSE)
    cli_alert_success("Step 5a memory cleaned up.")
  }
}

# Snapshot the environment before Step 5b
env_pre_step5b <- ls(envir = .GlobalEnv)

#'
#' ### Step 5b: Livestock Economics & Emissions Downscaling
#+ step5b
# --- Livestock Economics & Emissions Downscaling ---
if (RUN_DS_LSU && !is.null(current_model_cfg$step5b_scripts)) {
  if (!exists("cur_run_date") || is.null(cur_run_date)) {
    cur_run_date <- if (exists("RUN_DATE") && !is.null(RUN_DATE)) RUN_DATE else if (exists("OVERRIDE_DATE") && !is.null(OVERRIDE_DATE)) OVERRIDE_DATE else format(Sys.Date(), "%Y%m%d")
  }
  if (current_run_intermediate_exists(paste0("lsu_production_downscaled_", safe_scen_chunk)) && !(exists("FORCE_REDO_LSU") && isTRUE(FORCE_REDO_LSU))) {
    cli_alert_info(">>> Step 5b: Final LSU Economics outputs already exist for current run date. Skipping optimization.")
  } else {
    lapply(current_model_cfg$step5b_scripts, function(x) source_script(x, model_dir_name))

    # Cleanup intermediate objects created during Step 5b
    env_post_step5b <- ls(envir = .GlobalEnv)
    new_objs <- setdiff(env_post_step5b, c(env_pre_step5b, "env_pre_step5b", "env_post_step5b", protected_vars))
    if (length(new_objs) > 0) rm(list = new_objs, envir = .GlobalEnv)
    gc(verbose = FALSE)
    cli_alert_success("Step 5b memory cleaned up.")
  }
}

  cli_alert_success(sprintf("=== COMPLETED DOWN-SCALING ENGINE CHUNK FOR SCENARIO: %s ===", scen_chunk))

  # Optional: Generate TIFF rasters chunk-by-chunk if RUN_DS_CHUNK_TIFFS is TRUE
  if (exists("RUN_DS_CHUNK_TIFFS") && isTRUE(RUN_DS_CHUNK_TIFFS)) {
    cli_h2(sprintf("Generating All TIFF Rasters for Scenario: %s", scen_chunk))
    TARGET_SCENARIOS <- scen_chunk
    TARGET_SCENARIOS_FILTER <- scen_chunk

    # 1. Rasterize Land Use for scen_chunk if missing
    if (file.exists(files$lu_output)) {
      if (!exists("results_to_raster", mode = "function")) {
        source_script("results_to_raster.R", model_dir_name)
      }
      if (!exists("template_custom") || is.null(template_custom)) {
        template_custom <- ensure_template_custom()
      }
      if (!is.null(template_custom)) {
        lu_ds_chunk <- read_output_table(files$lu_output)
        if ("year" %in% names(lu_ds_chunk) && !"times" %in% names(lu_ds_chunk)) setnames(lu_ds_chunk, "year", "times")
        chunk_years <- if ("times" %in% names(lu_ds_chunk)) sort(unique(lu_ds_chunk$times)) else c(2000, 2010, 2020, 2030, 2040, 2050)
        for (yr in chunk_years) {
          out_lu_tif <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen_chunk, "_", yr, ".tif"))
          if (!file.exists(out_lu_tif)) {
            sub_chunk <- lu_ds_chunk[times == as.integer(yr)]
            if (nrow(sub_chunk) > 0) {
              sub_chunk_copy <- data.table::copy(sub_chunk)
              sub_chunk_copy[, times := NULL]
              cli_alert_info("Rasterizing Land Use for {scen_chunk} ({yr})...")
              results_to_raster(
                results = sub_chunk_copy,
                template_path = template_custom,
                class_col = "lu",
                output_path = out_lu_tif,
                check_area = TRUE,
                max_pixel_area = PIXEL_RES^2,
                value_type = "area", units = "km2",
                metadata = c(r2r_meta_base, list(product = paste0(output_prefix, "_lu"), scenario = scen_chunk,
                                                 period = as.character(yr),
                                                 description = "Downscaled land-use area per pixel and class"))
              )
              rm(sub_chunk_copy)
            }
          }
        }
        rm(lu_ds_chunk); gc(verbose = FALSE)
      }
    }

    # 2. Dual Hierarchical Aggregation for Land Use and Livestock
    agg_tiff_script <- c("scripts/core/aggregate_tiff_hierarchical.R", "scripts/aggregate_tiff_hierarchical.R")
    agg_tiff_script <- agg_tiff_script[file.exists(agg_tiff_script)][1]
    if (!is.na(agg_tiff_script)) {
      tryCatch(source(agg_tiff_script), error = function(e) {
        cli_alert_warning(sprintf("aggregate_tiff_hierarchical.R failed for %s: %s", scen_chunk, conditionMessage(e)))
      })
    }

    # 3. LSU Rasters & Indicators
    lsu_script <- c("pipelines/LAMASUS_GLOBIOM/generate_lsu_tiffs.R", "scripts/LAMASUS_GLOBIOM/generate_lsu_tiffs.R")
    lsu_script <- lsu_script[file.exists(lsu_script)][1]
    if (!is.na(lsu_script)) {
      tryCatch(source(lsu_script), error = function(e) {
        cli_alert_warning(sprintf("generate_lsu_tiffs.R failed for %s: %s", scen_chunk, conditionMessage(e)))
      })
    }

    # 4. Crop Production Rasters & Hierarchical Aggregates
    prod_script <- c("pipelines/LAMASUS_GLOBIOM/generate_production_hierarchical_tiffs.R", "scripts/LAMASUS_GLOBIOM/generate_production_hierarchical_tiffs.R")
    prod_script <- prod_script[file.exists(prod_script)][1]
    if (!is.na(prod_script)) {
      tryCatch(source(prod_script), error = function(e) {
        cli_alert_warning(sprintf("generate_production_hierarchical_tiffs.R failed for %s: %s", scen_chunk, conditionMessage(e)))
      })
    }

    # 5. Full Agricultural Production Indicators (Crop + Livestock)
    full_prod_script <- c("pipelines/LAMASUS_GLOBIOM/generate_full_production_tiffs.R", "scripts/LAMASUS_GLOBIOM/generate_full_production_tiffs.R")
    full_prod_script <- full_prod_script[file.exists(full_prod_script)][1]
    if (!is.na(full_prod_script)) {
      tryCatch(source(full_prod_script), error = function(e) {
        cli_alert_warning(sprintf("generate_full_production_tiffs.R failed for %s: %s", scen_chunk, conditionMessage(e)))
      })
    }

    # 6. Categorization into 5 viewer subfolders, JSON metadata, and schema validation
    cat_script <- c("pipelines/LAMASUS_GLOBIOM/categorize_and_generate_indicator_tiffs.R", "scripts/LAMASUS_GLOBIOM/categorize_and_generate_indicator_tiffs.R")
    cat_script <- cat_script[file.exists(cat_script)][1]
    if (!is.na(cat_script)) {
      tryCatch(source(cat_script), error = function(e) {
        cli_alert_warning(sprintf("categorize_and_generate_indicator_tiffs.R failed for %s: %s", scen_chunk, conditionMessage(e)))
      })
    }
    if (exists("TARGET_SCENARIOS_FILTER")) rm(TARGET_SCENARIOS_FILTER)
  }
} # END SCENARIO LOOP
} # END RUN_DS_LU || RUN_DS_LSU

#'
#' ## 6. Post-processing & Validation
#+ post_processing
# ---------------------------------------------------------------
# 4. Post-processing & Visualization
# ---------------------------------------------------------------
if (RUN_CHECKS) {
  cli_h1("4. Post-processing & Validation")
  source_script("results_to_raster.R", model_dir_name)

  # 4.1 Create Dynamic Template from Buffer
  if (!exists("template_custom") || is.null(template_custom)) {
    template_custom <- ensure_template_custom()
  }

  # ---------------------------------------------------------------
  # Shared Helpers
  # ---------------------------------------------------------------

  # Extract scenarios and years from the dataset
  get_scens_years_from_tifs <- function(prefix, type, ds, has_scen, has_times) {
    # Always scan the dataset to ensure we don't miss scenarios that haven't been rasterized yet
    scens <- if (has_scen) ds |> dplyr::select(scenario) |> dplyr::distinct() |> dplyr::collect() |> dplyr::pull() else "BASE"
    yrs <- if (has_times) ds |> dplyr::select(times) |> dplyr::distinct() |> dplyr::collect() |> dplyr::pull() else "ALL"
    return(list(scenarios = scens, years = yrs))
  }

  # Helper function to map grid IDs to intersection column using dplyr/arrow
  add_intersect_col_arrow <- function(ds, res_col = PIXEL_INTERSECT_COL) {
    first_ns <- ds |> head(1) |> dplyr::collect() |> dplyr::pull(ns)
    if (any(grepl("_", first_ns))) {
      ds <- ds |> dplyr::mutate(!!rlang::sym(res_col) := sub("^[0-9]+_", "", ns))
    } else if (exists("mappings") && !is.null(mappings$mapping_grid)) {
      join_col <- paste0("EEA_", PIXEL_RES, "kmID")
      if (!join_col %in% names(mappings$mapping_grid) && "ns" %in% names(mappings$mapping_grid)) {
        join_col <- "ns"
      }
      map_slice <- unique(mappings$mapping_grid[, .(join_id = as.character(get(join_col)), intersect_val = get(res_col))])
      map_slice_df <- as.data.frame(map_slice)
      ds <- ds |>
        dplyr::mutate(join_id = as.character(ns)) |>
        dplyr::left_join(map_slice_df, by = "join_id") |>
        dplyr::mutate(!!rlang::sym(res_col) := intersect_val) |>
        dplyr::select(-join_id, -intersect_val)
    }
    return(ds)
  }

  # ---------------------------------------------------------------
  # 4.2 Land Use (LU) Outputs
  # ---------------------------------------------------------------
  lu_chunk_files <- list.files(paths$output_dir, pattern = paste0("^", output_prefix, "_lu_.*\\.(parquet|csv)$"), full.names = TRUE)
  # Filter out Land Use Change (luc) and Livestock (lsu) files from the Land Use (lu) matches
  lu_chunk_files <- lu_chunk_files[!grepl("_luc|_lsu|aggregated|check|loss|target", basename(lu_chunk_files))]
  if (length(lu_chunk_files) > 0) {
    message(">>> Processing Land Use (LU) Outputs...")
    scenarios <- unique(gsub(paste0("^", output_prefix, "_lu_|\\.(parquet|csv)$"), "", basename(lu_chunk_files)))
    first_dt <- read_output_table(lu_chunk_files[1])
    if ("year" %in% names(first_dt) && !"times" %in% names(first_dt)) setnames(first_dt, "year", "times")
    years <- if ("times" %in% names(first_dt)) sort(unique(first_dt$times)) else c(2000, 2010, 2020, 2030, 2040, 2050)
    rm(first_dt); gc(verbose = FALSE)

    # -- 4.2.1 LU Rasters --
    if (!exists("SKIP_LU_RASTERS") || !SKIP_LU_RASTERS) {
      message(">>> Generating Land Use Rasters...")
      for (scen in scenarios) {
        safe_scen <- gsub("[^A-Za-z0-9_-]", "_", scen)
        missing_years <- c()
        for (yr in years) {
          out_path <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen, "_", yr, ".tif"))
          if (!file.exists(out_path)) missing_years <- c(missing_years, yr)
        }
        if (length(missing_years) == 0) {
          message(sprintf(">>> -> Skipping existing LU rasters for scenario: %s", scen))
          next
        }
        scen_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen, ".parquet"))
        scen_csv <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen, ".csv"))
        scen_f   <- if (file.exists(scen_pq)) scen_pq else if (file.exists(scen_csv)) scen_csv else NULL
        if (is.null(scen_f)) next

        scen_dt <- read_output_table(scen_f)
        if ("year" %in% names(scen_dt) && !"times" %in% names(scen_dt)) setnames(scen_dt, "year", "times")
        for (yr in missing_years) {
          out_path <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen, "_", yr, ".tif"))
          sub_res <- scen_dt[times == as.integer(yr)]
          if (nrow(sub_res) > 0) {
            message(sprintf(">>> -> Generating raster for scenario: %s, year: %s", scen, yr))
            sub_res_copy <- data.table::copy(sub_res)
            sub_res_copy[, times := NULL]
            results_to_raster(
              results = sub_res_copy,
              template_path = template_custom,
              class_col = "lu",
              output_path = out_path,
              check_area = TRUE,
              max_pixel_area = PIXEL_RES^2,
              value_type = "area", units = "km2",
              metadata = c(r2r_meta_base, list(product = paste0(output_prefix, "_lu"), scenario = scen,
                                               period = as.character(yr),
                                               description = "Downscaled land-use area per pixel and class"))
            )
            rm(sub_res_copy)
          }
        }
        rm(scen_dt); gc(verbose = FALSE)
      }

      # Generate Colormap CSV
      message(">>> Generating LUM Colormap CSV...")
      sub_samp <- read_output_table(lu_chunk_files[1])
      unique_classes <- sort(unique(sub_samp$lu))
      rm(sub_samp)
      default_colors <- c("Cropland"="#954535", "Cropland_other"="#B56555", "Cropland_IR"="#C57565",
                          "Grassland"="#7BB369", "Pasture"="#7BB369", "Forests"="#228B22",
                          "Forests_unmanaged"="#006400", "Natural_unmanaged"="#BDB76B",
                          "Wetlands"="#4682B4", "Urban"="#696969")

      colormap <- data.table(layer_value = 1:length(unique_classes), `name in viewer` = unique_classes, color = NA_character_)
      for (i in seq_along(unique_classes)) {
        cls <- unique_classes[i]
        matched_col <- "#808080"
        for (pref in names(default_colors)) {
          if (grepl(pref, cls, ignore.case = TRUE)) {
            matched_col <- default_colors[pref]
            break
          }
        }
        colormap[i, color := matched_col]
      }

      colormap_path <- file.path(paths$output_dir, paste0("DS_LUM_colormap_", run_basename, ".csv"))
      fwrite(colormap, colormap_path)
      message(">>> Colormap saved to: ", colormap_path)
      rm(unique_classes, colormap)
    }

    # -- 4.2.2 LU CSV Aggregation --
    if (!is.null(PIXEL_INTERSECT_COL)) {
      for (scen in scenarios) {
        safe_scen <- gsub("[^A-Za-z0-9_-]", "_", scen)
        agg_out_path <- file.path(paths$output_dir, paste0("downscaled_results_lu_aggregated_", PIXEL_INTERSECT_COL, "_", safe_scen, ".csv"))
        if (file.exists(agg_out_path)) {
          message(">>> Skipping existing aggregated LU CSV for scenario ", scen, " at ", PIXEL_INTERSECT_COL, " level...")
          next
        }
        scen_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen, ".parquet"))
        scen_csv <- file.path(paths$output_dir, paste0(output_prefix, "_lu_", safe_scen, ".csv"))
        scen_f   <- if (file.exists(scen_pq)) scen_pq else if (file.exists(scen_csv)) scen_csv else NULL
        if (is.null(scen_f)) next

        message(">>> Saving aggregated Land Use results to CSV for scenario ", scen, " at ", PIXEL_INTERSECT_COL, " level...")
        scen_dt <- read_output_table(scen_f)
        if ("year" %in% names(scen_dt) && !"times" %in% names(scen_dt)) setnames(scen_dt, "year", "times")
        
        if (any(grepl("_", scen_dt$ns))) {
          scen_dt[, (PIXEL_INTERSECT_COL) := sub("^[0-9]+_", "", ns)]
        } else if (exists("mappings") && !is.null(mappings$mapping_grid)) {
          join_col <- paste0("EEA_", PIXEL_RES, "kmID")
          if (!join_col %in% names(mappings$mapping_grid) && "ns" %in% names(mappings$mapping_grid)) join_col <- "ns"
          map_slice <- unique(mappings$mapping_grid[, .(join_id = as.character(get(join_col)), intersect_val = get(PIXEL_INTERSECT_COL))])
          scen_dt[, join_id := as.character(ns)]
          scen_dt <- merge(scen_dt, map_slice, by = "join_id", all.x = TRUE)
          scen_dt[, (PIXEL_INTERSECT_COL) := intersect_val]
          scen_dt[, c("join_id", "intersect_val") := NULL]
        }
        
        if (PIXEL_INTERSECT_COL %in% names(scen_dt)) {
          agg_dt <- scen_dt[, .(value = sum(value, na.rm = TRUE)), by = c("lu", PIXEL_INTERSECT_COL, "times")]
          if (!"scenario" %in% names(agg_dt)) agg_dt[, scenario := as.character(scen)]
          lu_agg_std <- data.table(
            layer = "level", model = current_model_cfg$macro_model_name, scenario = agg_dt$scenario, region = agg_dt[[PIXEL_INTERSECT_COL]],
            variable = "Area", item = agg_dt$lu, year = as.integer(agg_dt$times), unit = "km2", value = agg_dt$value
          )
          fwrite(unpack_composite_scenario(lu_agg_std, current_model_cfg), agg_out_path)
          message(">>> Aggregated standard LU results for scenario ", scen, " saved to: ", agg_out_path)
          rm(agg_dt, lu_agg_std)
        }
        rm(scen_dt); gc(verbose = FALSE)
      }
    }
  }

  # ---------------------------------------------------------------
  # 4.3 Land Use Change (LUC) Outputs
  # ---------------------------------------------------------------
  luc_chunk_files <- list.files(paths$output_dir, pattern = paste0("^", output_prefix, "_luc_.*\\.(parquet|csv)$"), full.names = TRUE)
  luc_chunk_files <- luc_chunk_files[!grepl("aggregated|check|loss|target", basename(luc_chunk_files))]
  if (length(luc_chunk_files) > 0) {
    message(">>> Processing Land Use Change (LUC) Outputs...")
    scenarios <- unique(gsub(paste0("^", output_prefix, "_luc_|\\.(parquet|csv)$"), "", basename(luc_chunk_files)))
    first_dt <- read_output_table(luc_chunk_files[1])
    if ("year" %in% names(first_dt) && !"times" %in% names(first_dt)) setnames(first_dt, "year", "times")
    years <- if ("times" %in% names(first_dt)) sort(unique(first_dt$times)) else c(2000, 2010, 2020, 2030, 2040, 2050)
    rm(first_dt); gc(verbose = FALSE)

    # -- 4.3.1 LUC Rasters --
    message(">>> Generating Land Use Change (LUC) Rasters...")
    for (scen in scenarios) {
      if (exists("SKIP_LUC_RASTERS") && isTRUE(SKIP_LUC_RASTERS)) {
        message(sprintf(">>> -> Skipping LUC raster generation for scenario %s as requested.", scen))
        next
      }
      safe_scen <- gsub("[^A-Za-z0-9_-]", "_", scen)
      missing_years <- c()
      for (yr in years) {
        fname_suffix <- paste0("_", safe_scen, "_", yr)
        if (!file.exists(file.path(paths$output_dir, paste0(output_prefix, "_luc", fname_suffix, ".tif")))) {
          missing_years <- c(missing_years, yr)
        }
      }
      if (length(missing_years) == 0) {
        message(sprintf(">>> -> Skipping existing LUC rasters for scenario: %s", scen))
        next
      }

      scen_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_luc_", safe_scen, ".parquet"))
      scen_csv <- file.path(paths$output_dir, paste0(output_prefix, "_luc_", safe_scen, ".csv"))
      scen_f   <- if (file.exists(scen_pq)) scen_pq else if (file.exists(scen_csv)) scen_csv else NULL
      if (is.null(scen_f)) next

      message(sprintf(">>> -> Reading LUC data from disk for scenario: %s", scen))
      sub_res_scen <- read_output_table(scen_f)
      if ("year" %in% names(sub_res_scen) && !"times" %in% names(sub_res_scen)) setnames(sub_res_scen, "year", "times")
      sub_res_scen[, luc_label := paste(from_class, to_class, sep = "_to_")]

      for (yr in missing_years) {
        sub_res <- sub_res_scen[times == as.integer(yr)]
        if (nrow(sub_res) > 0) {
          fname_suffix <- paste0("_", safe_scen, "_", yr)
          out_path <- file.path(paths$output_dir, paste0(output_prefix, "_luc", fname_suffix, ".tif"))
          if (file.exists(out_path)) next
          message(sprintf(">>> -> Generating LUC raster for scenario: %s, year: %s", scen, yr))

          sub_res_copy <- data.table::copy(sub_res)
          sub_res_copy[, times := NULL]

          results_to_raster(
            results = sub_res_copy,
            template_path = template_custom,
            class_col = "luc_label",
            output_path = out_path,
            check_area = TRUE,
            max_pixel_area = PIXEL_RES^2,
            value_type = "area", units = "km2",
            metadata = c(r2r_meta_base, list(product = paste0(output_prefix, "_luc"), scenario = scen,
                                             period = as.character(yr),
                                             description = "Downscaled land-use change area per pixel and transition (from -> to)"))
          )
          rm(sub_res_copy)
        }
        rm(sub_res)
        gc(verbose = FALSE)
      }
      rm(sub_res_scen)
      gc(verbose = FALSE)
    }

    # -- 4.3.2 LUC CSV Aggregation --
    if (!is.null(PIXEL_INTERSECT_COL)) {
      agg_out_path <- file.path(paths$output_dir, paste0("downscaled_results_luc_aggregated_", PIXEL_INTERSECT_COL, ".csv"))
      if (file.exists(agg_out_path)) {
        message(">>> Skipping existing aggregated LUC CSV at ", PIXEL_INTERSECT_COL, " level...")
      } else {
        message(">>> Saving aggregated Land Use Change results to CSV at ", PIXEL_INTERSECT_COL, " level...")
        agg_list <- list()
        for (scen in scenarios) {
          safe_scen <- gsub("[^A-Za-z0-9_-]", "_", scen)
          scen_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_luc_", safe_scen, ".parquet"))
          scen_csv <- file.path(paths$output_dir, paste0(output_prefix, "_luc_", safe_scen, ".csv"))
          scen_f   <- if (file.exists(scen_pq)) scen_pq else if (file.exists(scen_csv)) scen_csv else NULL
          if (is.null(scen_f)) next

          scen_dt <- read_output_table(scen_f)
          if ("year" %in% names(scen_dt) && !"times" %in% names(scen_dt)) setnames(scen_dt, "year", "times")
          if (any(grepl("_", scen_dt$ns))) {
            scen_dt[, (PIXEL_INTERSECT_COL) := sub("^[0-9]+_", "", ns)]
          } else if (exists("mappings") && !is.null(mappings$mapping_grid)) {
            join_col <- paste0("EEA_", PIXEL_RES, "kmID")
            if (!join_col %in% names(mappings$mapping_grid) && "ns" %in% names(mappings$mapping_grid)) join_col <- "ns"
            map_slice <- unique(mappings$mapping_grid[, .(join_id = as.character(get(join_col)), intersect_val = get(PIXEL_INTERSECT_COL))])
            scen_dt[, join_id := as.character(ns)]
            scen_dt <- merge(scen_dt, map_slice, by = "join_id", all.x = TRUE)
            scen_dt[, (PIXEL_INTERSECT_COL) := intersect_val]
            scen_dt[, c("join_id", "intersect_val") := NULL]
          }

          if (PIXEL_INTERSECT_COL %in% names(scen_dt)) {
            chunk_agg <- scen_dt[, .(value = sum(value, na.rm = TRUE)), by = c("from_class", "to_class", PIXEL_INTERSECT_COL, "times")]
            if (!"scenario" %in% names(chunk_agg)) chunk_agg[, scenario := as.character(scen)]
            agg_list[[scen]] <- chunk_agg
          }
          rm(scen_dt); gc(verbose = FALSE)
        }
        if (length(agg_list) > 0) {
          luc_agg <- data.table::rbindlist(agg_list)
          luc_agg[, item_name := paste0(from_class, "_to_", to_class)]
          luc_agg_std <- data.table(
            layer = "transition", model = current_model_cfg$macro_model_name, scenario = luc_agg$scenario, region = luc_agg[[PIXEL_INTERSECT_COL]],
            variable = "Area", item = luc_agg$item_name, year = as.integer(luc_agg$times), unit = "km2", value = luc_agg$value
          )
          fwrite(unpack_composite_scenario(luc_agg_std, current_model_cfg), agg_out_path)
          message(">>> Aggregated standard LUC results saved to: ", agg_out_path)
          rm(luc_agg, luc_agg_std)
        }
        rm(agg_list)
      }
    }
  }

  # ---------------------------------------------------------------
  # 4.4 Livestock (LSU) Outputs
  # ---------------------------------------------------------------
  lsu_chunk_files <- list.files(paths$output_dir, pattern = paste0("^", output_prefix, "_lsu_[A-Za-z0-9_]+\\.(parquet|csv)$"), full.names = TRUE)
  lsu_chunk_files <- lsu_chunk_files[!grepl("_production|_target|_performance|aggregated", basename(lsu_chunk_files))]
  if (length(lsu_chunk_files) > 0) {
    message(">>> Processing Livestock (LSU) Outputs...")
    scenarios <- unique(gsub(paste0("^", output_prefix, "_lsu_|\\.(parquet|csv)$"), "", basename(lsu_chunk_files)))
    first_dt <- read_output_table(lsu_chunk_files[1])
    if ("year" %in% names(first_dt) && !"times" %in% names(first_dt)) setnames(first_dt, "year", "times")
    years <- if ("times" %in% names(first_dt)) sort(unique(first_dt$times)) else c(2000, 2010, 2020, 2030, 2040, 2050)
    rm(first_dt); gc(verbose = FALSE)

    # -- 4.4.1 LSU Rasters --
    message(">>> Generating Livestock Rasters...")
    for (scen in scenarios) {
      safe_scen <- gsub("[^A-Za-z0-9_-]", "_", scen)
      missing_years <- c()
      for (yr in years) {
        fname_suffix <- paste0("_", safe_scen, "_", yr)
        if (!file.exists(file.path(paths$output_dir, paste0(output_prefix, "_lsu", fname_suffix, ".tif")))) {
          missing_years <- c(missing_years, yr)
        }
      }
      if (length(missing_years) == 0) {
        message(sprintf(">>> -> Skipping existing LSU rasters for scenario: %s", scen))
        next
      }

      scen_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_lsu_", safe_scen, ".parquet"))
      scen_csv <- file.path(paths$output_dir, paste0(output_prefix, "_lsu_", safe_scen, ".csv"))
      scen_f   <- if (file.exists(scen_pq)) scen_pq else if (file.exists(scen_csv)) scen_csv else NULL
      if (is.null(scen_f)) next

      message(sprintf(">>> -> Reading LSU data from disk for scenario: %s", scen))
      sub_res_scen <- read_output_table(scen_f)
      if ("year" %in% names(sub_res_scen) && !"times" %in% names(sub_res_scen)) setnames(sub_res_scen, "year", "times")

      for (yr in missing_years) {
        sub_res <- sub_res_scen[times == as.integer(yr)]
        if (nrow(sub_res) > 0) {
          fname_suffix <- paste0("_", safe_scen, "_", yr)
          out_path <- file.path(paths$output_dir, paste0(output_prefix, "_lsu", fname_suffix, ".tif"))
          if (file.exists(out_path)) next
          message(sprintf(">>> -> Generating raster for scenario: %s, year: %s", scen, yr))

          sub_res_copy <- data.table::copy(sub_res)
          sub_res_copy[, times := NULL]

          results_to_raster(
            results = sub_res_copy,
            template_path = template_custom,
            class_col = "pop_type",
            output_path = out_path,
            value_type = "count", units = "LSU",
            metadata = c(r2r_meta_base, list(product = paste0(output_prefix, "_lsu"), scenario = scen,
                                             period = as.character(yr),
                                             description = "Downscaled livestock units per pixel and livestock type"))
          )
          rm(sub_res_copy)
        }
        rm(sub_res)
        gc(verbose = FALSE)
      }
      rm(sub_res_scen)
      gc(verbose = FALSE)
    }

    # -- 4.4.2 LSU CSV Aggregation --
    if (!is.null(PIXEL_INTERSECT_COL)) {
      agg_out_path <- file.path(paths$output_dir, paste0("downscaled_results_lsu_aggregated_", PIXEL_INTERSECT_COL, ".csv"))
      if (file.exists(agg_out_path)) {
        message(">>> Skipping existing aggregated LSU CSV at ", PIXEL_INTERSECT_COL, " level...")
      } else {
        message(">>> Saving aggregated Livestock results to CSV at ", PIXEL_INTERSECT_COL, " level...")
        agg_list <- list()
        for (scen in scenarios) {
          safe_scen <- gsub("[^A-Za-z0-9_-]", "_", scen)
          scen_pq  <- file.path(paths$output_dir, paste0(output_prefix, "_lsu_", safe_scen, ".parquet"))
          scen_csv <- file.path(paths$output_dir, paste0(output_prefix, "_lsu_", safe_scen, ".csv"))
          scen_f   <- if (file.exists(scen_pq)) scen_pq else if (file.exists(scen_csv)) scen_csv else NULL
          if (is.null(scen_f)) next

          scen_dt <- read_output_table(scen_f)
          if ("year" %in% names(scen_dt) && !"times" %in% names(scen_dt)) setnames(scen_dt, "year", "times")
          if (any(grepl("_", scen_dt$ns))) {
            scen_dt[, (PIXEL_INTERSECT_COL) := sub("^[0-9]+_", "", ns)]
          } else if (exists("mappings") && !is.null(mappings$mapping_grid)) {
            join_col <- paste0("EEA_", PIXEL_RES, "kmID")
            if (!join_col %in% names(mappings$mapping_grid) && "ns" %in% names(mappings$mapping_grid)) join_col <- "ns"
            map_slice <- unique(mappings$mapping_grid[, .(join_id = as.character(get(join_col)), intersect_val = get(PIXEL_INTERSECT_COL))])
            scen_dt[, join_id := as.character(ns)]
            scen_dt <- merge(scen_dt, map_slice, by = "join_id", all.x = TRUE)
            scen_dt[, (PIXEL_INTERSECT_COL) := intersect_val]
            scen_dt[, c("join_id", "intersect_val") := NULL]
          }

          if (PIXEL_INTERSECT_COL %in% names(scen_dt)) {
            chunk_agg <- scen_dt[, .(value = sum(value, na.rm = TRUE)), by = c("pop_type", PIXEL_INTERSECT_COL, "times")]
            if (!"scenario" %in% names(chunk_agg)) chunk_agg[, scenario := as.character(scen)]
            agg_list[[scen]] <- chunk_agg
          }
          rm(scen_dt); gc(verbose = FALSE)
        }
        if (length(agg_list) > 0) {
          lsu_agg <- data.table::rbindlist(agg_list)
          lsu_agg_std <- data.table(
            layer = "level", model = current_model_cfg$macro_model_name, scenario = lsu_agg$scenario, region = lsu_agg[[PIXEL_INTERSECT_COL]],
            variable = "Livestock", item = lsu_agg$pop_type, year = as.integer(lsu_agg$times), unit = "TLU", value = lsu_agg$value
          )
          fwrite(unpack_composite_scenario(lsu_agg_std, current_model_cfg), agg_out_path)
          message(">>> Aggregated standard LSU results saved to: ", agg_out_path)
          rm(lsu_agg, lsu_agg_std)
        }
        rm(agg_list)
      }
    }
  }

  # ---------------------------------------------------------------
  # 4.4.5 Prior Impact (Pixel-level) Outputs
  # ---------------------------------------------------------------
  if (exists("files") && !is.null(files$prior_impact_pixels_output) && file.exists(files$prior_impact_pixels_output)) {
    message(">>> Processing Prior Impact Pixel Diagnostics...")
    impact_ds <- arrow::open_dataset(files$prior_impact_pixels_output, format = "csv")
    ds_names <- names(impact_ds)
    if ("year" %in% ds_names && !"times" %in% ds_names) {
      impact_ds <- impact_ds |> dplyr::rename(times = year)
      ds_names <- names(impact_ds)
    }
    actual_scen_col <- if (!is.null(current_model_cfg$scen_col)) current_model_cfg$scen_col else "scenario"
    if (actual_scen_col %in% ds_names && actual_scen_col != "scenario") {
      impact_ds <- impact_ds |> dplyr::rename(scenario = !!dplyr::sym(actual_scen_col))
      ds_names <- names(impact_ds)
    }
    if (".SCEN_ID." %in% ds_names) {
      impact_ds <- impact_ds |> dplyr::rename(scenario = .SCEN_ID.)
      ds_names <- names(impact_ds)
    }
    impact_ds <- impact_ds |> dplyr::mutate(luc_label = paste(from_class, to_class, sep = "_to_"))
    has_scen <- "scenario" %in% ds_names
    has_times <- "times" %in% ds_names

    sy <- get_scens_years_from_tifs(output_prefix, "lu", impact_ds, has_scen, has_times)
    scenarios <- sy$scenarios
    years <- sy$years

    message(">>> Generating Prior Impact Rasters...")
    for (scen in scenarios) {
      missing_any <- FALSE
      for (yr in years) {
        fname_suffix <- ""
        if (has_scen) fname_suffix <- paste0(fname_suffix, "_", scen)
        if (has_times && yr != "ALL") fname_suffix <- paste0(fname_suffix, "_", yr)
        if (!file.exists(file.path(paths$output_dir, paste0(output_prefix, "_prior_impact", fname_suffix, ".tif")))) {
          missing_any <- TRUE
          break
        }
      }
      if (!missing_any) next

      sub_res_scen_query <- impact_ds
      if (has_scen) sub_res_scen_query <- sub_res_scen_query |> dplyr::filter(scenario == !!as.character(scen))

      sub_res_scen <- sub_res_scen_query |> dplyr::collect() |> data.table::setDT()

      for (yr in years) {
        if (has_times && yr != "ALL") {
          sub_res <- sub_res_scen[times == yr]
        } else {
          sub_res <- sub_res_scen
        }

        if (nrow(sub_res) > 0) {
          fname_suffix <- ""
          if (has_scen) fname_suffix <- paste0(fname_suffix, "_", scen)
          if (has_times && yr != "ALL") fname_suffix <- paste0(fname_suffix, "_", yr)

          out_path <- file.path(paths$output_dir, paste0(output_prefix, "_prior_impact", fname_suffix, ".tif"))
          if (file.exists(out_path)) next
          message(sprintf(">>> -> Generating raster for scenario: %s, year: %s", scen, yr))

          if (has_times) sub_res[, times := NULL]

          results_to_raster(
            results = sub_res,
            template_path = template_custom,
            class_col = "luc_label",
            value_col = "shift_km2",
            output_path = out_path,
            check_area = FALSE,
            value_type = "area", units = "km2",
            metadata = c(r2r_meta_base, list(product = paste0(output_prefix, "_prior_impact"), scenario = scen,
                                             period = as.character(yr),
                                             description = "Prior impact: shift of allocated area per pixel and transition relative to a flat-prior run"))
          )
        }
        rm(sub_res)
        gc(verbose = FALSE)
      }
      rm(sub_res_scen, sub_res_scen_query)
      gc(verbose = FALSE)
    }
  }

  # ---------------------------------------------------------------
  # 4.5 Unified Indicator Raster Generation
  # ---------------------------------------------------------------
  if (isTRUE(current_model_cfg$generate_indicators)) {
    # Steps 5a/5b save one intermediate per scenario chunk; process them one at a time to bound memory
    ind_scens <- if (exists("active_scenarios")) active_scenarios else character(0)
    prod_files <- get_scenario_intermediate_paths("production_downscaled", ind_scens)
    lsu_prod_files <- get_scenario_intermediate_paths("lsu_production_downscaled", ind_scens)

    if (length(prod_files) > 0) {
      message(">>> Generating Unified Economic & Emissions Rasters...")
      for (prod_key in names(prod_files)) {
      prod_file <- prod_files[[prod_key]]
      prod_ds <- if (grepl("\\.qs$", prod_file)) qs2::qs_read(prod_file) else readRDS(prod_file)

      has_scen <- "scenario" %in% names(prod_ds)
      has_times <- "year" %in% names(prod_ds)

      scenarios <- if (has_scen) unique(prod_ds$scenario) else "BASE"
      years <- if (has_times) unique(prod_ds$year) else "ALL"

      # Matching LSU dataset (same scenario, else a combined "ALL" file)
      lsu_prod_file <- if (prod_key %in% names(lsu_prod_files)) lsu_prod_files[[prod_key]] else if ("ALL" %in% names(lsu_prod_files)) lsu_prod_files[["ALL"]] else NULL
      lsu_ds <- if (!is.null(lsu_prod_file)) { if (grepl("\\.qs$", lsu_prod_file)) qs2::qs_read(lsu_prod_file) else readRDS(lsu_prod_file) } else NULL

      lsu_indicator_whitelist <- c(
        "area_lsu",
        # Products, feed intake, manure/nitrogen (actual GLOBIOM column names)
        "BVMEAT", "BVMILK", "SGMEAT", "SGMILK", "PGMEAT", "PTMEAT", "PTEGGS",
        "GRAZING", "Manure", "N_Excretion",
        # Emissions
        "Entferm_CH4", "ManmgtTot_CH4", "ManmgtTot_N2O", "ManaplTot_N2O",
        "ManprpTot_N2O", "MansolTot_N2O",
        # Legacy names kept for other model vintages
        "MEAT_t_unit", "MILK_t_unit", "EGGS_t_unit", "GRAZING_t_unit",
        "Manure_yield_t_unit", "N_excretion_kg_unit",
        "Cost", "Cost_usd", "Cost_eur", "N_input", "N_input_kg", "P_input", "P_input_kg", "K_input", "K_input_kg", "Water", "Water_m3",
        "N_input_kg_lsu_per_head", "P_input_kg_lsu_per_head", "K_input_kg_lsu_per_head", "Water_m3_lsu_per_head",
        # Livestock-relevant producer prices ONLY — a blanket ^Price_ grep pulled
        # all 54 commodity prices (wood, biomass, fuels) into per-class raster
        # stacks and exhausted memory (std::bad_alloc)
        "Price_ALMILK", "Price_BVMEAT", "Price_SGMEAT", "Price_PGMEAT", "Price_PTMEAT", "Price_PTEGGS",
        grep("^CAP_", if (!is.null(lsu_ds)) names(lsu_ds) else character(0), value = TRUE),
        paste0(grep("^CAP_", if (!is.null(lsu_ds)) names(lsu_ds) else character(0), value = TRUE), "_lsu_per_head"),
        "Cost_lsu_per_head", "Cost_usd_lsu_per_head", "Cost_eur_lsu_per_head",
        # CAPRI per-head economics (process_CAPRI_lsu_production_downscaling.R)
        "prod_t_lsu_per_head", "Revenue_usd_lsu_per_head", "Profit_usd_lsu_per_head", "Subsidy_usd_lsu_per_head",
        "Revenue_eur_lsu_per_head", "Profit_eur_lsu_per_head", "Subsidy_eur_lsu_per_head"
      )

      for (scen in scenarios) {
        for (yr in years) {
          fname_suffix <- ""
          if (has_scen) fname_suffix <- paste0(fname_suffix, "_", scen)
          if (has_times && yr != "ALL") fname_suffix <- paste0(fname_suffix, "_", yr)
          out_path <- file.path(paths$output_dir, paste0(output_prefix, "_indicators", fname_suffix, ".tif"))

          if (file.exists(out_path)) {
            message(sprintf(">>> -> Skipping existing unified indicator raster for scenario: %s, year: %s", scen, yr))
            next
          }

          sub_prod <- prod_ds
          if (has_scen) sub_prod <- sub_prod[scenario == scen]
          if (has_times) sub_prod <- sub_prod[year == yr]

          # Economic composites (cost, revenue, subsidies, profit) are already
          # absolute USD per pixel-row; Price_usd_t is an intensity aggregated
          # production-weighted below.

          # Compute per km2 economic rates
          if ("Total_COST_USD" %in% names(sub_prod)) sub_prod[, Cost_usd_km2 := fifelse(area_km2 > 1e-6, Total_COST_USD / area_km2, 0)]
          if ("Revenue_USD" %in% names(sub_prod)) sub_prod[, Revenue_usd_km2 := fifelse(area_km2 > 1e-6, Revenue_USD / area_km2, 0)]
          if ("total_subsidy_usd" %in% names(sub_prod)) sub_prod[, Subsidy_usd_km2 := fifelse(area_km2 > 1e-6, total_subsidy_usd / area_km2, 0)]
          if ("Profit_USD" %in% names(sub_prod)) sub_prod[, Profit_usd_km2 := fifelse(area_km2 > 1e-6, Profit_USD / area_km2, 0)]
          
          if ("Total_COST_EUR" %in% names(sub_prod)) sub_prod[, Cost_eur_km2 := fifelse(area_km2 > 1e-6, Total_COST_EUR / area_km2, 0)]
          if ("Revenue_EUR" %in% names(sub_prod)) sub_prod[, Revenue_eur_km2 := fifelse(area_km2 > 1e-6, Revenue_EUR / area_km2, 0)]
          if ("total_subsidy_eur" %in% names(sub_prod)) sub_prod[, Subsidy_eur_km2 := fifelse(area_km2 > 1e-6, total_subsidy_eur / area_km2, 0)]
          if ("Profit_EUR" %in% names(sub_prod)) sub_prod[, Profit_eur_km2 := fifelse(area_km2 > 1e-6, Profit_EUR / area_km2, 0)]

          # Compute per km2 environmental rates
          if ("N_input_kg" %in% names(sub_prod)) sub_prod[, N_input_kg_km2 := N_input_kg]
          if ("P_input_kg" %in% names(sub_prod)) sub_prod[, P_input_kg_km2 := P_input_kg]
          if ("K_input_kg" %in% names(sub_prod)) sub_prod[, K_input_kg_km2 := K_input_kg]
          if ("Water_m3" %in% names(sub_prod)) sub_prod[, Water_m3_km2 := Water_m3]

          crop_val_cols <- intersect(c("area_km2", "yield_t_km2", "prod_t", "scale_factor",
                                       "Total_COST_USD", "Revenue_USD", "total_subsidy_usd",
                                       "Profit_USD", "Price_usd_t",
                                       "Cost_usd_km2", "Revenue_usd_km2", "Subsidy_usd_km2", "Profit_usd_km2",
                                       "Total_COST_EUR", "Revenue_EUR", "total_subsidy_eur",
                                       "Profit_EUR", "Price_eur_t",
                                       "Cost_eur_km2", "Revenue_eur_km2", "Subsidy_eur_km2", "Profit_eur_km2",
                                       "Total_N_input_kg", "Total_P_input_kg", "Total_K_input_kg", "Total_Water_m3",
                                       "N_input_kg_km2", "P_input_kg_km2", "K_input_kg_km2", "Water_m3_km2",
                                       "multi_cropping_index"), names(sub_prod))

          # Initialize vector to store all generated indicator rasters
          unified_temp_files <- c()

          # Compute ns_template once to speed up aggregation
          sub_prod[, ns_template := as.integer(gsub("_.*", "", ns))]

          cell_values <- terra::values(template_custom[[1]], mat = FALSE)
          mask_target <- !is.na(cell_values)

          # Memory Safe Processing: Loop over Crop Indicators
          for (ind in crop_val_cols) {
            # Convert intensive variables to extensive (absolute totals) for spatial accumulation
            # This fixes the bug where rates were summed across NUTS3 borders causing spikes
            if (ind %in% c("area_km2", "prod_t", "Total_COST_USD", "Revenue_USD", "total_subsidy_usd", "Profit_USD", "Total_COST_EUR", "Revenue_EUR", "total_subsidy_eur", "Profit_EUR", "Total_N_input_kg", "Total_P_input_kg", "Total_K_input_kg", "Total_Water_m3")) {
              # Already-absolute (extensive) values: plain spatial sum
              ind_agg <- sub_prod[!is.na(get(ind)) & get(ind) != 0, .(value = sum(get(ind), na.rm=TRUE)), by = .(ns_template, GLOB_CTYP, GLOB_CSYS)]
            } else if (ind %in% c("Price_usd_t", "Price_eur_t")) {
              # Producer price: production-weighted mean (economically correct
              # average price); falls back to area weights where prod is 0
              ind_agg <- sub_prod[!is.na(get(ind)) & get(ind) != 0,
                                  .(value = {
                                    w <- if (sum(prod_t, na.rm=TRUE) > 0) prod_t else area_km2
                                    sum(get(ind) * w, na.rm=TRUE) / max(sum(w, na.rm=TRUE), 1e-12)
                                  }), by = .(ns_template, GLOB_CTYP, GLOB_CSYS)]
            } else if (ind %in% c("yield_t_km2", "scale_factor", "multi_cropping_index", "Cost_usd_km2", "Revenue_usd_km2", "Subsidy_usd_km2", "Profit_usd_km2", "Cost_eur_km2", "Revenue_eur_km2", "Subsidy_eur_km2", "Profit_eur_km2", "N_input_kg_km2", "P_input_kg_km2", "K_input_kg_km2", "Water_m3_km2")) {
              ind_agg <- sub_prod[!is.na(get(ind)) & get(ind) != 0, .(value = {
                tot_a <- sum(area_km2, na.rm=TRUE)
                if (tot_a > 1e-6) sum(get(ind) * area_km2, na.rm=TRUE) / tot_a else 0
              }), by = .(ns_template, GLOB_CTYP, GLOB_CSYS)]
            } else {
              ind_agg <- sub_prod[!is.na(get(ind)) & get(ind) != 0, .(value = sum(get(ind) * area_km2, na.rm=TRUE)), by = .(ns_template, GLOB_CTYP, GLOB_CSYS)]
            }

            if (nrow(ind_agg) == 0) next

            ind_agg[is.na(GLOB_CSYS), GLOB_CSYS := ""]
            ind_agg[is.na(GLOB_CTYP), GLOB_CTYP := ""]

            present_classes <- unique(ind_agg[, .(GLOB_CSYS, GLOB_CTYP)])
            valid_classes <- character()
            valid_vals <- list()

            for (i in seq_len(nrow(present_classes))) {
              csys <- present_classes$GLOB_CSYS[i]
              ctyp <- present_classes$GLOB_CTYP[i]

              lu_name <- paste("cropland", tolower(csys), tolower(ctyp), tolower(ind), sep = "_")
              lu_name <- gsub("_unit$", "", lu_name)
              lu_name <- gsub("^_|_$", "", gsub("_+", "_", lu_name))

              res_sub <- ind_agg[GLOB_CSYS == csys & GLOB_CTYP == ctyp]
              if (nrow(res_sub) == 0) next

              match_idx <- match(cell_values, res_sub$ns_template)
              vals <- res_sub$value[match_idx]
              vals[mask_target & (is.na(vals) | is.infinite(vals))] <- 0

              valid_classes <- c(valid_classes, lu_name)
              valid_vals[[length(valid_vals) + 1]] <- vals
            }

            if (length(valid_classes) > 0) {
              r_ind_stack <- terra::rast(template_custom[[1]], nlyrs = length(valid_classes))
              names(r_ind_stack) <- valid_classes
              terra::values(r_ind_stack) <- do.call(cbind, valid_vals)

              tmp_out <- file.path(tempdir(), paste0("crop_", tolower(ind), "_", scen, "_", yr, ".tif"))
              terra::writeRaster(r_ind_stack, tmp_out, overwrite = TRUE)
              unified_temp_files <- c(unified_temp_files, tmp_out)
              rm(r_ind_stack, valid_vals)
            }
            rm(ind_agg)
            gc(verbose = FALSE)
          }
          rm(sub_prod)
          gc(verbose = FALSE)

          if (!is.null(lsu_ds)) {
            sub_lsu <- lsu_ds
            if (has_scen) sub_lsu <- sub_lsu[scenario == scen]
            if (has_times) sub_lsu <- sub_lsu[year == yr]

            if (nrow(sub_lsu) > 0) {
              # Ensure lsu_heads column exists (fallback to value or area_lsu)
              if (!"lsu_heads" %in% names(sub_lsu)) {
                if ("value" %in% names(sub_lsu)) {
                  sub_lsu[, lsu_heads := as.numeric(value)]
                } else if ("area_lsu" %in% names(sub_lsu)) {
                  sub_lsu[, lsu_heads := as.numeric(area_lsu)]
                } else {
                  sub_lsu[, lsu_heads := 1]
                }
              }
              if (!"GLOB_LSSYS" %in% names(sub_lsu)) sub_lsu[, GLOB_LSSYS := "ALL"]
              if (!"GLOB_LSTYP" %in% names(sub_lsu)) {
                sub_lsu[, GLOB_LSTYP := if ("pop_type" %in% names(sub_lsu)) pop_type else "ALL"]
              }

              # Compute per lsu economic rates (safe against division by zero)
              if ("Cost_usd" %in% names(sub_lsu)) sub_lsu[, Cost_usd_lsu_per_head := fifelse(lsu_heads > 0, Cost_usd / lsu_heads, 0)]
              if ("Cost_eur" %in% names(sub_lsu)) sub_lsu[, Cost_eur_lsu_per_head := fifelse(lsu_heads > 0, Cost_eur / lsu_heads, 0)]
              if ("Cost" %in% names(sub_lsu)) sub_lsu[, Cost_lsu_per_head := fifelse(lsu_heads > 0, Cost / lsu_heads, 0)]
              if ("N_input_kg" %in% names(sub_lsu)) sub_lsu[, N_input_kg_lsu_per_head := fifelse(lsu_heads > 0, N_input_kg / lsu_heads, 0)]
              if ("P_input_kg" %in% names(sub_lsu)) sub_lsu[, P_input_kg_lsu_per_head := fifelse(lsu_heads > 0, P_input_kg / lsu_heads, 0)]
              if ("K_input_kg" %in% names(sub_lsu)) sub_lsu[, K_input_kg_lsu_per_head := fifelse(lsu_heads > 0, K_input_kg / lsu_heads, 0)]
              if ("Water_m3" %in% names(sub_lsu)) sub_lsu[, Water_m3_lsu_per_head := fifelse(lsu_heads > 0, Water_m3 / lsu_heads, 0)]

              cap_cols_lsu <- grep("^CAP_", names(sub_lsu), value = TRUE)
              for (cap_col in cap_cols_lsu) {
                if (!grepl("_lsu_per_head$", cap_col)) {
                  rate_col <- paste0(cap_col, "_lsu_per_head")
                  sub_lsu[, (rate_col) := fifelse(lsu_heads > 0, get(cap_col) / lsu_heads, 0)]
                }
              }

              lsu_val_cols <- intersect(lsu_indicator_whitelist, names(sub_lsu))
              lsu_val_cols <- lsu_val_cols[vapply(sub_lsu[, ..lsu_val_cols], is.numeric, logical(1))]

              sub_lsu[, ns_template := as.integer(gsub("_.*", "", ns))]

              # Memory Safe Processing: Loop over LSU Indicators
              for (ind in lsu_val_cols) {
                if (ind %in% c("lsu_heads", "value", "area_lsu")) {
                  ind_agg <- sub_lsu[!is.na(get(ind)) & get(ind) != 0, .(value = sum(get(ind), na.rm=TRUE)), by = .(ns_template, GLOB_LSSYS, GLOB_LSTYP)]
                } else if (grepl("^Price_", ind) || grepl("_lsu_per_head$", ind)) {
                  ind_agg <- sub_lsu[!is.na(get(ind)) & get(ind) != 0, .(value = sum(get(ind) * lsu_heads, na.rm=TRUE) / pmax(sum(lsu_heads, na.rm=TRUE), 1e-9)), by = .(ns_template, GLOB_LSSYS, GLOB_LSTYP)]
                } else {
                  ind_agg <- sub_lsu[!is.na(get(ind)) & get(ind) != 0, .(value = sum(get(ind), na.rm=TRUE)), by = .(ns_template, GLOB_LSSYS, GLOB_LSTYP)]
                }

                if (nrow(ind_agg) == 0) next

                ind_agg[is.na(GLOB_LSSYS), GLOB_LSSYS := ""]
                ind_agg[is.na(GLOB_LSTYP), GLOB_LSTYP := ""]

                present_classes <- unique(ind_agg[, .(GLOB_LSSYS, GLOB_LSTYP)])
                valid_classes <- character()
                valid_vals <- list()

                for (i in seq_len(nrow(present_classes))) {
                  lsys <- present_classes$GLOB_LSSYS[i]
                  ltyp <- present_classes$GLOB_LSTYP[i]

                  lu_name <- paste("livestock", tolower(lsys), tolower(ltyp), tolower(ind), sep = "_")
                  lu_name <- gsub("_unit$", "", lu_name)
                  lu_name <- gsub("^_|_$", "", gsub("_+", "_", lu_name))

                  res_sub <- ind_agg[GLOB_LSSYS == lsys & GLOB_LSTYP == ltyp]
                  if (nrow(res_sub) == 0) next

                  match_idx <- match(cell_values, res_sub$ns_template)
                  vals <- res_sub$value[match_idx]
                  vals[mask_target & is.na(vals)] <- 0

                  valid_classes <- c(valid_classes, lu_name)
                  valid_vals[[length(valid_vals) + 1]] <- vals
                }

                if (length(valid_classes) > 0) {
                  r_ind_stack <- terra::rast(template_custom[[1]], nlyrs = length(valid_classes))
                  names(r_ind_stack) <- valid_classes
                  terra::values(r_ind_stack) <- do.call(cbind, valid_vals)

                  tmp_out <- file.path(tempdir(), paste0("lsu_", tolower(ind), "_", scen, "_", yr, ".tif"))
                  terra::writeRaster(r_ind_stack, tmp_out, overwrite = TRUE)
                  unified_temp_files <- c(unified_temp_files, tmp_out)
                  rm(r_ind_stack, valid_vals)
                }
                rm(ind_agg)
                gc(verbose = FALSE)
              }
            }
            rm(sub_lsu)
            gc(verbose = FALSE)
          }

          message(sprintf(">>> -> Stacking unified indicator raster for scenario: %s, year: %s", scen, yr))

          if (length(unified_temp_files) > 0) {
            r_all <- terra::rast(unified_temp_files)
            terra::writeRaster(r_all, out_path, overwrite = TRUE)
            rm(r_all)
            file.remove(unified_temp_files)
          } else {
            message(">>> -> No valid indicator data found to generate raster.")
          }
          gc(verbose = FALSE)
        }
      }
      rm(prod_ds, lsu_ds)
      gc(verbose = FALSE)
      } # END per-scenario production files

      source_script("aggregate_tiff_hierarchical_indicators.R", model_dir_name)
    }

    rm(list = intersect(ls(), c("prod_ds", "prod_long", "lsu_long", "unified_long", "lsu_ds", "prod_file", "lsu_prod_file", "lsu_indicator_whitelist")))
    gc(verbose = FALSE)
  }

  # ---------------------------------------------------------------
  # 5. Diagnostic Reporting
  # ---------------------------------------------------------------
  if (isTRUE(current_model_cfg$generate_report)) {
    cli_h2("Diagnostic Reporting")
    rmd_diag_cands <- c("scripts/core/report_results.Rmd", "scripts/report_results.Rmd")
    rmd_diag_path <- rmd_diag_cands[file.exists(rmd_diag_cands)][1]
    if (!is.na(rmd_diag_path)) {
      message(">>> Rendering Diagnostic Report...")
    scenarios <- "All"
    if (file.exists(files$diag_output)) {
      diag_results_tmp <- tryCatch(fread(files$diag_output, select = "scenario"), error = function(e) {
        # Try the model's native scen_col
        scen_col_name <- if (!is.null(current_model_cfg$scen_col)) current_model_cfg$scen_col else "scenario"
        tryCatch(fread(files$diag_output, select = scen_col_name), error = function(e2) NULL)
      })
      if (!is.null(diag_results_tmp) && ncol(diag_results_tmp) > 0) {
        scenarios <- unique(diag_results_tmp[[1]])
      }
    }

    for (scen in scenarios) {
      if (scen == "All") {
        lu_rast <- paste0(output_prefix, "_lu.tif")
        lsu_rast <- paste0(output_prefix, "_lsu.tif")
        out_name <- paste0(output_prefix, "_diagnostics.html")
      } else {
        lu_rast <- paste0(output_prefix, "_lu_", scen, ".tif")
        lsu_rast <- paste0(output_prefix, "_lsu_", scen, ".tif")
        out_name <- paste0(output_prefix, "_diagnostics_", scen, ".html")
      }

      message(sprintf(">>> Rendering report for scenario: %s", scen))
      tryCatch({
        rmarkdown::render(
          rmd_diag_path,
          params = list(
            lu_raster  = normalizePath(file.path(paths$output_dir, lu_rast), mustWork = FALSE),
            lsu_raster = normalizePath(file.path(paths$output_dir, lsu_rast), mustWork = FALSE),
            diag_csv   = normalizePath(files$diag_output, mustWork = FALSE),
            scenario   = scen
          ),
          output_dir  = paths$output_dir,
          output_file = out_name,
          intermediates_dir = paths$temp,
          quiet = FALSE
        )
      }, error = function(e) {
        cli_alert_warning("Could not render spatial diagnostic report for {scen}: {e$message}")
      })
    }
    rm(list = intersect(ls(), c("diag_results_tmp")))
  }
}
}
# ---------------------------------------------------------------
# 7. Post-Processing Performance Reporting
# ---------------------------------------------------------------
if (RUN_CHECKS) {
  cli_alert_info("Aggregating Downscaling Performance Report...")

  if (file.exists(files$report_output)) {
    perf_report <- fread(files$report_output)

    if (!is.null(files$pre_ds_loss_output) && file.exists(files$pre_ds_loss_output)) {
      pre_loss <- fread(files$pre_ds_loss_output)
    } else {
      pre_loss <- data.table(scenario = character(), region = character(),
                             area_raw = numeric(), area_ds_targets = numeric(), diff_area = numeric())
    }

    # --- Normalize scenario column ---
    scen_col_name <- if (!is.null(current_model_cfg$scen_col)) current_model_cfg$scen_col else "scenario"
    if (scen_col_name %in% names(perf_report) && scen_col_name != "scenario") setnames(perf_report, scen_col_name, "scenario")
    if (".SCEN_ID." %in% names(perf_report)) setnames(perf_report, ".SCEN_ID.", "scenario")
    if (scen_col_name %in% names(pre_loss) && scen_col_name != "scenario") setnames(pre_loss, scen_col_name, "scenario")

    # --- Normalize region column to generic 'region' ---
    # For perf_report: try .REGION_ID. → Ns_reg → GLOB_country → region
    if (".REGION_ID." %in% names(perf_report) && !"region" %in% names(perf_report)) setnames(perf_report, ".REGION_ID.", "region")
    if ("Ns_reg" %in% names(perf_report) && !"region" %in% names(perf_report)) setnames(perf_report, "Ns_reg", "region")
    if ("GLOB_country" %in% names(perf_report) && !"region" %in% names(perf_report)) setnames(perf_report, "GLOB_country", "region")

    # For pre_loss: try Ns_reg → GLOB_country → region
    if ("Ns_reg" %in% names(pre_loss) && !"region" %in% names(pre_loss)) setnames(pre_loss, "Ns_reg", "region")
    if ("GLOB_country" %in% names(pre_loss) && !"region" %in% names(pre_loss)) setnames(pre_loss, "GLOB_country", "region")

    # --- Attempt NUTS → Country aggregation for models without GLOB_country ---
    # If reg_area_weights exists (GLOBIOM), use it to map Ns_reg→GLOB_country
    # Otherwise try to derive country from NUTS codes (first 2 chars)
    if (exists("reg_area_weights") && "GLOB_country" %in% names(reg_area_weights)) {
      # GLOBIOM path: merge weights and re-apportion
      if ("region" %in% names(perf_report)) {
        perf_report <- merge(perf_report, reg_area_weights[, .(region = Ns_reg, GLOB_country, weight)], by = "region", all.x = TRUE, allow.cartesian = TRUE)
        if ("diff" %in% names(perf_report)) perf_report[, diff := diff * weight]
        if ("value" %in% names(perf_report)) perf_report[, value := value * weight]
        perf_report[, region := GLOB_country][, c("GLOB_country", "weight") := NULL]
      }
      if ("region" %in% names(pre_loss) && nrow(pre_loss) > 0) {
        pre_loss <- merge(pre_loss, reg_area_weights[, .(region = Ns_reg, GLOB_country, weight)], by = "region", all.x = TRUE, allow.cartesian = TRUE)
        if ("area_raw" %in% names(pre_loss)) pre_loss[, area_raw := area_raw * weight]
        if ("area_ds_targets" %in% names(pre_loss)) pre_loss[, area_ds_targets := area_ds_targets * weight]
        if ("diff_area" %in% names(pre_loss)) pre_loss[, diff_area := area_ds_targets - area_raw]
        pre_loss[, region := GLOB_country][, c("GLOB_country", "weight") := NULL]
      }
    } else {
      # CAPRI / other models: derive country from NUTS code (first 2 characters)
      perf_report[, country := substr(region, 1, 2)]
      if (nrow(pre_loss) > 0) pre_loss[, country := substr(region, 1, 2)]
    }

    # Determine the aggregation key for the summary report
    agg_key <- if ("country" %in% names(perf_report)) c("scenario", "country") else c("scenario", "region")

    # --- Summarize ACR Integration Loss ---
    if (nrow(pre_loss) > 0 && "area_raw" %in% names(pre_loss)) {
      pre_agg_key <- intersect(agg_key, names(pre_loss))
      pre_loss_agg <- pre_loss[, .(
        Target_Area_Raw = sum(area_raw, na.rm = TRUE),
        ACR_Integration_Loss = sum(diff_area[diff_area > 0], na.rm = TRUE)
      ), by = pre_agg_key]
    } else {
      pre_loss_agg <- data.table(region = character(), scenario = character(),
                                  Target_Area_Raw = numeric(), ACR_Integration_Loss = numeric())
      setnames(pre_loss_agg, "region", agg_key[2])
    }

    # --- Summarize Downscaling Loss (with graceful column handling) ---
    has_col <- function(dt, col) col %in% names(dt)

    perf_agg <- perf_report[, {
      tgt <- if (has_col(.SD, "value")) sum(value, na.rm = TRUE) else 0
      missed <- if (has_col(.SD, "diff")) sum(diff, na.rm = TRUE) else 0

      start_loss <- if (has_col(.SD, "initial_area_insufficient") && has_col(.SD, "rebalancing_applied")) {
        sum(diff[initial_area_insufficient == TRUE | rebalancing_applied == TRUE], na.rm = TRUE)
      } else 0

      restrict_loss <- if (has_col(.SD, "restriction_blocking")) {
        sum(diff[initial_area_insufficient == FALSE & rebalancing_applied == FALSE & restriction_blocking == TRUE], na.rm = TRUE)
      } else 0

      solver_loss <- missed - start_loss - restrict_loss

      .(Target_Area_DS = tgt,
        Missed_Area_DS = missed,
        Start_Area_Insufficient_Loss = start_loss,
        Restriction_Blocking_Loss = restrict_loss,
        Competition_Solver_Loss = solver_loss)
    }, by = agg_key]

    # --- Merge and compute final summary ---
    summary_report <- merge(pre_loss_agg, perf_agg, by = agg_key, all = TRUE)

    summary_report[, Total_Target_Area := fcoalesce(Target_Area_Raw, Target_Area_DS)]
    summary_report[, Total_Missed_Area := fcoalesce(ACR_Integration_Loss, 0) + fcoalesce(Missed_Area_DS, 0)]
    summary_report[, Percent_Missed := round((Total_Missed_Area / pmax(Total_Target_Area, 1e-12)) * 100, 2)]

    # Rename aggregation key to 'region' for the output CSV (standardized)
    region_col_out <- agg_key[2]
    if (region_col_out != "region") setnames(summary_report, region_col_out, "region")

    out_summary_path <- file.path(paths$output_dir, paste0(output_prefix, "_performance_summary.csv"))
    fwrite(summary_report, out_summary_path)

    cli_alert_success("Performance summary saved to: {out_summary_path}")

    # Print top problematic regions
    top_issues <- summary_report[order(-Percent_Missed)][1:min(10, .N)]
    cli_h2("Top 10 Regions by Missing Target Area (%)")
    print(knitr::kable(top_issues[, .(Scenario = scenario, Region = region, Total_Target_Area, Percent_Missed)]))

    # Render neat HTML report
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
      cli_alert_info("Rendering HTML performance reports...")

      # Ensure interactive reporting dependencies are installed
      report_pkgs <- c("plotly", "DT", "bslib", "htmltools")
      missing_pkgs <- report_pkgs[!(report_pkgs %in% installed.packages()[,"Package"])]
      if (length(missing_pkgs) > 0) {
        cli_alert_info(paste("Installing missing packages for interactive HTML report:", paste(missing_pkgs, collapse = ", ")))
        install.packages(missing_pkgs, repos = "https://cloud.r-project.org", quiet = TRUE)
      }

      tryCatch({
        # Save metadata registry
        if (exists("pipeline_metadata") && nrow(pipeline_metadata) > 0) {
          fwrite(pipeline_metadata, files$metadata_output)
        }

        perf_scens <- if ("scenario" %in% names(summary_report)) unique(c("ALL", summary_report$scenario)) else "ALL"
        rmd_perf_cands <- c("scripts/core/render_performance_report.Rmd", "scripts/render_performance_report.Rmd")
        rmd_perf_path <- rmd_perf_cands[file.exists(rmd_perf_cands)][1]
        for (scen in perf_scens) {
          scen_suffix <- if (scen == "ALL") "" else paste0("_", scen)
          html_report_path <- file.path(paths$output_dir, paste0(output_prefix, "_performance_report", scen_suffix, ".html"))

          rmarkdown::render(rmd_perf_path,
                            output_file = html_report_path,
                            params = list(diag_file = out_summary_path,
                                          harmonization_file = files$harmonization_check_output,
                                          area_conservation_file = files$area_conservation_output,
                                          report_file = files$report_output,
                                          metadata_file = if (file.exists(files$metadata_output)) files$metadata_output else NULL,
                                          prior_impact_file = if (!is.null(files$prior_impact_output) && file.exists(files$prior_impact_output)) files$prior_impact_output else NULL,
                                          model_name = model_name,
                                          target_scenario = scen),
                            intermediates_dir = paths$output_dir,
                            quiet = TRUE)
          cli_alert_success("HTML report generated for {scen}: {html_report_path}")
        }
      }, error = function(e) {
        cli_alert_warning("Could not render HTML report: {e$message}")
      })
    } else {
      cli_alert_warning("Skipped HTML report generation because the 'rmarkdown' package is not installed.")
    }
  }

  # Cleanup performance report temporaries
  rm(list = intersect(ls(), c("perf_report", "pre_loss", "pre_loss_agg", "perf_agg",
                              "summary_report", "top_issues", "reg_area_weights")))
  gc(verbose = FALSE)
}

# --- Hierarchical TIFF Aggregation ---
if (RUN_DS_AGGREGATION) {
  cli_h2("Hierarchical TIFF Aggregation")
  source_script("aggregate_tiff_hierarchical.R", model_dir_name)
}

cli_alert_success("PIPELINE COMPLETED SUCCESSFULLY.")

# Close the run manifest (package versions, end time); run scripts may close it again later
finalize_run_manifest("orchestrator")

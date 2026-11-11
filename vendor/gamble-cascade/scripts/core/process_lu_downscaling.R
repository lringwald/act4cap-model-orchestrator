# ===============================================================
# process_lu_downscaling.R: Land Use (LUC) Downscaling Optimization
# ===============================================================
# What this script does
# -------------------------------------------------------------
# Every macro model (LAMASUS_GLOBIOM, BIOCLIMA_CAPRI, ...) produces its own
# native intermediates upstream (compile_*_gdx_data.R, process_*_start_map_
# harmonization.R, compute_*_transitions.R, ...). This script's only job is to
# turn whatever those intermediates look like into FOUR standardized inputs —
# mapping_grid, targets, start_map, and the priors/covariates bundle — and hand
# them to the one generic optimization engine (execute_luc_downscaling).
#
# Design: model-specific knowledge lives ONLY inside the "MODEL ADAPTERS"
# functions below. Each adapter takes `cfg` (= current_model_cfg, the active
# model's model_config entry from master_downscaling.R) and returns data in a
# standardized schema — with two exceptions that are genuinely per-model, not
# fixed: the scenario and time column NAMES. Those are never hardcoded (e.g.
# as "scenario"/"year") anywhere in this file; adapt_targets() resolves them
# from cfg$scen_col/cfg$time_col (defaulting only if the model doesn't set
# them) and returns the resolved names alongside `targets`, and the PIPELINE
# section passes those same resolved names straight into
# execute_luc_downscaling()'s scen_cols/time_col arguments — exactly like
# region_cols is already dynamically resolved from cfg$spatial_key upstream
# (see master_downscaling.R's "Ns_reg" bridging). A model with no scenario
# dimension at all still declares a scen_col name in its config; adapt_targets()
# injects a constant value under that name rather than inventing "scenario".
# The PIPELINE section never branches on model identity or config presence
# itself — so a new macro model is added purely by extending model_config,
# with no changes below PIPELINE. See each adapter's header for the exact cfg
# fields it reads.
# -------------------------------------------------------------
require(tidyverse)
require(progress)
require(cli)
require(knitr)
# Engine: loaded once per session, pinned submodule or CASCADE_CORE_PATH (scripts/core/run_context.R).
# A second load_all() here used to decide which copy was live (and a hard-coded "../cascade-core"
# once replaced the pinned engine silently); ensure_cascade_engine() is a no-op after the first load.
if (!exists("ensure_cascade_engine", mode = "function")) source("scripts/core/run_context.R")
ensure_cascade_engine()

# Source core engine and local modifications
aux_cands <- c("scripts/core/aux_codes.R", "scripts/aux_codes.R")
source(aux_cands[file.exists(aux_cands)][1])

options(cli.progress_show_after = 0)

if (!exists("current_model_cfg")) {
  stop("process_lu_downscaling.R requires `current_model_cfg` to be set — source master_downscaling.R first.")
}

# ===============================================================
# MODEL ADAPTERS
# ===============================================================
# Each adapter is the ONLY place in this file allowed to read `cfg`. Every
# other function/section operates purely on the standardized schema.

#' Load and scope the master grid mapping.
#' Scope a region table to `country_filter`.
#'
#' Two vocabularies are in play. A country-keyed model (GLOBIOM, AGMIP) holds country NAMES in the
#' column and `country_filter` names them directly. A CAPRI-keyed model holds REGION CODES ("DE11")
#' and `country_filter` carries the country code ("DE"), which is a PREFIX of them, never a value.
#'
#' Exact matching alone therefore selected nothing for the CAPRI models and adapt_mapping_grid()
#' returned an EMPTY grid -- not an error, just a run with no pixels, after which every downstream
#' step merely looks small rather than broken.
#'
#' Resolution order, and it never guesses:
#'   1. exact match, when the column already speaks the filter's vocabulary  (GLOBIOM/AGMIP -- the
#'      existing behaviour, bit for bit)
#'   2. prefix match, for hierarchical region codes                          (CAPRI: DE -> DE11, DE2, ...)
#'   3. neither matches -> hard ERROR. An empty table is exactly the silent failure being fixed, so
#'      it must not be a reachable outcome.
.filter_regions_by_country <- function(dt, col, country_filter, quiet = FALSE) {
  if (is.null(country_filter) || !length(country_filter) || !col %in% names(dt)) return(dt)
  vals <- as.character(dt[[col]])
  cf   <- as.character(country_filter)

  keep <- vals %in% cf
  if (any(keep)) return(dt[keep])

  keep <- Reduce(`|`, lapply(cf, function(cc) startsWith(vals, cc)))
  if (!any(keep))
    stop(sprintf(paste0("country_filter %s matches no value of `%s`, exactly or by prefix (column looks like: %s). ",
                        "Refusing to continue: the alternative is an empty table and a run that looks merely small."),
                 paste(sQuote(cf), collapse = ", "), col,
                 paste(head(unique(vals), 3), collapse = ", ")), call. = FALSE)
  if (!quiet)
    message(sprintf(">>> country scope: `%s` starting with %s -- %s of %s rows",
                    col, paste(cf, collapse = ", "),
                    format(sum(keep), big.mark = ","), format(length(keep), big.mark = ",")))
  dt[keep]
}

adapt_mapping_grid <- function(cfg, mappings, mapping_grid_path, countries_to_run,
                                country_filter, downscale_resolution) {
  country_col <- if (!is.null(cfg$country_col)) cfg$country_col else "Ns_reg"
  universal_cols <- c("Ns_reg", "ns", "x_ns", "y_ns", paste0("EEA_", downscale_resolution, "ID"))
  model_cols <- if (length(cfg$keep_cols) > 0) cfg$keep_cols else country_col
  keep_cols <- unique(c(universal_cols, model_cols))

  if (is.null(mappings)) {
    message(">>> Loading master grid mapping...")
    if (grepl("\\.parquet$", mapping_grid_path)) {
      require(arrow)
      mapping_grid <- as.data.table(arrow::read_parquet(
        mapping_grid_path,
        memory_map = if (nzchar(Sys.getenv("PROJECT_SLUG", ""))) FALSE else TRUE
      ))
    } else {
      mapping_grid <- readRDS(mapping_grid_path)
    }

    if (!is.null(countries_to_run) && !is.null(country_col) && country_col %in% names(mapping_grid)) {
      target_countries <- unique(c(countries_to_run, "GLOB_NA"))
      mapping_grid <- mapping_grid[mapping_grid[[country_col]] %in% target_countries, ]
    }
    if (!"TERRA_GLOBIOM" %in% names(mapping_grid)) {
      mapping_grid$TERRA_GLOBIOM <- (mapping_grid$GLOB_SimUID != 0 & mapping_grid$GLOB_SimUID != "GLOB_NA")
    }
    mappings <- list(mapping_grid = mapping_grid)
  }

  actual_keep <- intersect(keep_cols, names(mappings$mapping_grid))
  if (!is.null(country_filter) && !is.null(country_col) && country_col %in% names(mappings$mapping_grid)) {
    unique(.filter_regions_by_country(mappings$mapping_grid, country_col, country_filter)[, ..actual_keep])
  } else {
    unique(mappings$mapping_grid[, ..actual_keep])
  }
}

#' Optional diagnostic: compare integrated targets against a model's "raw" (pre-
#' integration) targets, when the model defines one (cfg$raw_targets_intermediates).
#' Models without an equivalent representation (e.g. CAPRI) are skipped entirely —
#' this is informational only and never affects `targets`. Writes files$pre_ds_loss_output.
check_target_integration_loss <- function(cfg, targets, files, scen_col, time_col) {
  raw_names <- cfg$raw_targets_intermediates
  if (is.null(files) || is.null(files$pre_ds_loss_output) || length(raw_names) == 0) return(invisible(NULL))

  cli_alert_info("Computing pre-downscale target integration loss against raw macro-model targets...")
  raw_targets <- NULL
  for (nm in raw_names) {
    raw_targets <- tryCatch(as.data.table(read_intermediate(nm)), error = function(e) NULL)
    if (!is.null(raw_targets)) break
  }
  if (is.null(raw_targets) || nrow(raw_targets) == 0) return(invisible(NULL))

  raw_area_col <- cfg$raw_area_col
  if (!is.null(raw_area_col) && raw_area_col %in% names(raw_targets)) setnames(raw_targets, raw_area_col, "area_raw")
  # Bridge the model's native spatial key (untouched in its GDX/native extraction script) to Ns_reg
  spatial_key <- if (!is.null(cfg$spatial_key)) cfg$spatial_key else "Ns_reg"
  if (spatial_key %in% names(raw_targets) && !"Ns_reg" %in% names(raw_targets)) setnames(raw_targets, spatial_key, "Ns_reg")

  group_cols <- c(scen_col, "Ns_reg", "from_class", "to_class", time_col)

  targets_agg_temp <- copy(targets)
  
  # --- GLOBIOM-specific normalization ---
  if (!is.null(cfg$macro_model_name) && cfg$macro_model_name == "LAMASUS_GLOBIOM") {
    targets_agg_temp[grepl("^Cropland_", from_class), from_class := "Cropland"]
    targets_agg_temp[grepl("^Cropland_", to_class), to_class := "Cropland"]
    targets_agg_temp[grepl("^Pasture_", from_class), from_class := "Pasture"]
    targets_agg_temp[grepl("^Pasture_", to_class), to_class := "Pasture"]
  }

  targets_agg <- targets_agg_temp[value != 0 & from_class != to_class, .(area_ds_targets = sum(value)), by = group_cols]
  raw_agg <- raw_targets[area_raw != 0 & from_class != to_class, .(area_raw = sum(area_raw)), by = group_cols]

  pre_ds_loss <- merge(raw_agg, targets_agg, by = group_cols, all = TRUE)
  pre_ds_loss[is.na(area_raw), area_raw := 0]
  pre_ds_loss[is.na(area_ds_targets), area_ds_targets := 0]
  pre_ds_loss[, diff_area := area_ds_targets - area_raw]

  # Only keep actual discrepancies (allowing tiny float errors)
  pre_ds_loss <- pre_ds_loss[abs(diff_area) > 1e-4]
  fwrite(pre_ds_loss, file = files$pre_ds_loss_output, append = file.exists(files$pre_ds_loss_output), col.names = !file.exists(files$pre_ds_loss_output))
  invisible(NULL)
}

#' Load transition targets and inject zero-value base-year rows so every
#' region/transition combination is represented in the output even where no
#' change occurred. Runs the optional integration-loss diagnostic as a side
#' effect. The scenario/time column NAMES are resolved dynamically from
#' cfg$scen_col/cfg$time_col (this model's actual native names) rather than
#' assumed — a model with no scenario dimension at all still gets a
#' cfg$scen_col-named column injected with a constant value, never a
#' hardcoded "scenario".
#' Returns list(targets=, scen_col=, time_col=) — targets keeps whatever
#' native column names this model uses; scen_col/time_col tell the caller
#' what those names are so it can pass them straight to
#' execute_luc_downscaling() without this file ever hardcoding them.
  adapt_targets <- function(cfg, files) {
    targets <- as.data.table(read_intermediate("LUC_targets_full"))
    default_scen <- if (!is.null(cfg$scen_filter) && length(cfg$scen_filter) > 0) cfg$scen_filter[1] else "BASE"
    targets <- apply_composite_scenario(targets, cfg, default_scen)
    scen_col <- "scenario"

  time_col <- if (!is.null(cfg$time_col)) cfg$time_col else "year"
  if (!time_col %in% names(targets)) {
    stop(sprintf("adapt_targets: configured time_col '%s' not found in LUC_targets_full.", time_col))
  }

  # Keep only non-zero transitions that represent actual change
  targets <- targets[value != 0 & from_class != to_class, ]
  targets <- targets[from_class != "NODATA" & to_class != "NODATA", ]

  # Zones kept as exogenous `_other` land during harmonization (e.g. AgMIP slivers at the
  # grid edge) have no endogenous area left: drop their targets, so the engine writes them
  # unchanged as static regions instead of reporting their whole demand as unmet.
  outside_zones <- tryCatch(as.data.table(read_intermediate("outside_model_zones", apply_filters = FALSE)),
                            error = function(e) NULL)
  targets <- drop_outside_model_targets(targets, outside_zones)

  # Country scoping. Was a hardcoded "Germany" -> ^DE; now the shared resolver, so the vocabulary
  # lives in the cfg (BMLEH scopes on the CAPRI code "DE") instead of in three separate magic strings.
  if (!is.null(cfg$country_filter) && "Ns_reg" %in% names(targets)) {
    targets <- .filter_regions_by_country(targets, "Ns_reg", cfg$country_filter, quiet = TRUE)
  }

  check_target_integration_loss(cfg, targets, files, scen_col, time_col)

  avail_years <- sort(unique(targets[[time_col]]))
  base_yr <- if (!is.null(cfg$base_year)) {
    as.integer(cfg$base_year)
  } else if (length(avail_years) > 0) {
    min(avail_years)
  } else {
    2000
  }
  cli_alert_info(sprintf("Injecting Year %s zero-transitions...", base_yr))
  country_col <- if (!is.null(cfg$country_col)) cfg$country_col else "Ns_reg"
  group_cols <- intersect(names(targets), c(scen_col, "Ns_reg", country_col))

  targets_base <- targets[, .SD[1], by = group_cols]
  targets_base[, (time_col) := base_yr]
  targets_base[, value := 0]
  targets <- rbindlist(list(targets, targets_base), use.names = TRUE, fill = TRUE)

  list(targets = targets, scen_col = scen_col, time_col = time_col)
}

#' Load priors, restrictions, and exogenous priors for the downscaling engine.
#'
#' Entirely config-driven — the model config declares which intermediates to
#' load via `prior_intermediates`, `prior_intermediates_res`, and
#' `restriction_intermediates`, `restriction_intermediates_res`.  Each entry is tried with `tryCatch`; if the
#' intermediate was not produced by the model's step-3 scripts it is silently
#' skipped.
#'
#' @return list(priors=, restrictions=, exo_priors_all=) — any element NULL
#'   when the model produces no corresponding intermediates.
load_priors_and_restrictions <- function(cfg, downscale_resolution) {
  default_scen <- if (!is.null(cfg$scen_filter) && length(cfg$scen_filter) > 0) cfg$scen_filter[1] else "BASE"

  # --- Generic intermediate loader ---
  # Reads an intermediate, standardizes the time column, applies scenario mapping.
  # Returns NULL silently when the intermediate does not exist.
  .load_and_standardize <- function(name, resolution = NULL) {
    dt <- tryCatch({
      if (!is.null(resolution)) read_intermediate(name, resolution) else read_intermediate(name)
    }, error = function(e) NULL)
    if (is.null(dt)) return(NULL)
    if ("year" %in% names(dt) && !"times" %in% names(dt)) {
      dt[, times := as.character(year)]
      dt[, year := NULL]
    } else if ("times" %in% names(dt)) {
      dt[, times := as.character(times)]
    }
    apply_composite_scenario(dt, cfg, default_scen)
  }

  # --- Priors ---
  # cfg$prior_intermediates:     character vector of intermediate names (no resolution suffix)
  # cfg$prior_intermediates_res: character vector of intermediate names that use downscale_resolution
  prior_parts <- c(
    lapply(cfg$prior_intermediates,     .load_and_standardize),
    lapply(cfg$prior_intermediates_res, .load_and_standardize, resolution = downscale_resolution)
  )
  prior_parts <- Filter(Negate(is.null), prior_parts)
  priors <- if (length(prior_parts) > 0) rbindlist(prior_parts, use.names = TRUE, fill = TRUE) else NULL

  # --- Exogenous priors (file-pattern based) ---
  exo_priors_all <- NULL
  exo_files <- list.files(paths$temp,
    pattern = paste0("^exogenous_priors_.*_", run_basename, "_\\d{8}\\.(rds|qs)$"),
    full.names = TRUE)
  if (length(exo_files) > 0) {
    cli_alert_info(sprintf("Loading %d exogenous prior datasets...", length(exo_files)))
    exo <- rbindlist(lapply(exo_files, function(f)
      if (grepl("\\.qs$", f)) qs2::qs_read(f) else readRDS(f)),
      use.names = TRUE, fill = TRUE)
    exo_priors_all <- apply_composite_scenario(exo, cfg, default_scen)
  }

  # --- Restrictions ---
  restrict_parts <- c(
    lapply(cfg$restriction_intermediates, .load_and_standardize),
    lapply(cfg$restriction_intermediates_res, .load_and_standardize, resolution = downscale_resolution)
  )
  restrict_parts <- Filter(Negate(is.null), restrict_parts)
  restrictions <- if (length(restrict_parts) > 0) rbindlist(restrict_parts, use.names = TRUE, fill = TRUE) else NULL

  list(priors = priors, restrictions = restrictions, exo_priors_all = exo_priors_all)
}

#' Load the high-resolution covariates/betas produced by the prior model steps,
#' when available. Model-agnostic: absence just means no high-res priors, no
#' `cfg` needed.
#' Standardized schema: list(xmat_full=, xmat_proj_full=, betas_full=) (all NULL if unavailable).
load_covariates_and_betas <- function(downscale_resolution) {
  xmat_path  <- tryCatch(get_intermediate_path("high_res_xmat", downscale_resolution), error = function(e) NULL)
  xmat_proj_path <- tryCatch(get_intermediate_path("high_res_xmat_proj", downscale_resolution), error = function(e) NULL)
  betas_path <- tryCatch(get_intermediate_path("high_res_betas", downscale_resolution), error = function(e) NULL)

  if (is.null(xmat_path) || is.null(betas_path) || !file.exists(xmat_path) || !file.exists(betas_path)) {
    return(list(xmat_full = NULL, xmat_proj_full = NULL, betas_full = NULL))
  }

  cli_alert_info("Loading high-res prior models (Resolution: {downscale_resolution})...")
  list(
    xmat_full = if (grepl("\\.qs$", xmat_path)) qs2::qs_read(xmat_path) else readRDS(xmat_path),
    xmat_proj_full = if (!is.null(xmat_proj_path) && file.exists(xmat_proj_path)) { if (grepl("\\.qs$", xmat_proj_path)) qs2::qs_read(xmat_proj_path) else readRDS(xmat_proj_path) } else NULL,
    betas_full = if (grepl("\\.qs$", betas_path)) qs2::qs_read(betas_path) else readRDS(betas_path)
  )
}

#' Load the harmonized start map and bridge it to the GLOBIOM-native column
#' names the rest of the pipeline (and execute_luc_downscaling) is written
#' against, via cfg$lu_class_col / cfg$area_col.
#' Standardized schema: ns, Ns_reg, from_class, value.
adapt_start_map <- function(cfg, downscale_resolution) {
  start_map <- read_intermediate("processed_ds_start_map", downscale_resolution)

  if (!is.null(cfg$lu_class_col) && !"GLOB_lu_class" %in% names(start_map) && cfg$lu_class_col %in% names(start_map)) {
    setnames(start_map, cfg$lu_class_col, "GLOB_lu_class")
  }
  if (!is.null(cfg$area_col) && !"LUM_area_km2" %in% names(start_map) && cfg$area_col %in% names(start_map)) {
    setnames(start_map, cfg$area_col, "LUM_area_km2")
  }
  
  # Country scoping -- see adapt_targets(); same resolver, same vocabulary.
  if (!is.null(cfg$country_filter) && "Ns_reg" %in% names(start_map)) {
    start_map <- .filter_regions_by_country(start_map, "Ns_reg", cfg$country_filter, quiet = TRUE)
  }
  
  if (!"GLOB_CSYS" %in% names(start_map)) start_map[, GLOB_CSYS := ""]
  if (!"GLOB_CTYP" %in% names(start_map)) start_map[, GLOB_CTYP := ""]

  start_map[, from_class := paste(GLOB_lu_class, GLOB_CSYS, GLOB_CTYP, sep = "_")]
  start_map[, from_class := gsub("_+", "_", from_class)]
  start_map[, from_class := gsub("_$", "", from_class)]
  start_map <- start_map[!is.na(from_class) & from_class != "" & LUM_area_km2 > 0]
  start_map[, .(value = sum(LUM_area_km2)), by = .(ns, Ns_reg, from_class)]
}

# ===============================================================
# ORCHESTRATION
# ===============================================================
#' Prepare the four standardized inputs execute_luc_downscaling needs, for
#' whichever macro model is active, plus the resolved region/scenario/time
#' column NAMES this model actually uses (region_col, scen_col, time_col) —
#' these are aggregated from `cfg` here so the PIPELINE section below never
#' needs to read `cfg` itself. This is the single "abstracted" entry point:
#' every macro-model difference is resolved by the adapters it calls.

#' Check how well the harmonized start map aligns with the target demand.
#' Compares available area per (Ns_reg, from_class) in the start map against
#' total demanded transition area per (Ns_reg, from_class) in the targets.
#' Regions where demand > supply are flagged — the solver will necessarily
#' under-allocate there. Writes files$harmonization_check_output.
check_start_map_target_alignment <- function(start_map, targets, files, scen_col, time_col) {
  if (is.null(files) || is.null(files$harmonization_check_output)) return(invisible(NULL))
  
  cli_alert_info("Checking start map vs. target area alignment...")
  
  available <- start_map[, .(available_area = sum(value, na.rm = TRUE)), by = .(Ns_reg, class = from_class)]
  
  dem_in <- targets[from_class != to_class, .(area_in = sum(value, na.rm = TRUE)), 
                    by = c("Ns_reg", "to_class", scen_col, time_col)]
  setnames(dem_in, "to_class", "class")
  
  dem_out <- targets[from_class != to_class, .(area_out = sum(value, na.rm = TRUE)), 
                     by = c("Ns_reg", "from_class", scen_col, time_col)]
  setnames(dem_out, "from_class", "class")
  
  timeline <- merge(dem_in, dem_out, by = c("Ns_reg", "class", scen_col, time_col), all = TRUE)
  timeline[is.na(area_in), area_in := 0]
  timeline[is.na(area_out), area_out := 0]
  
  setorderv(timeline, c(scen_col, "Ns_reg", "class", time_col))
  
  timeline[, net_change := area_in - area_out]
  timeline[, cum_net_change := data.table::shift(cumsum(net_change), 1L, fill = 0), by = c(scen_col, "Ns_reg", "class")]
  
  timeline <- merge(timeline, available, by = c("Ns_reg", "class"), all.x = TRUE)
  timeline[is.na(available_area), available_area := 0]
  
  timeline[, current_available := available_area + cum_net_change]
  timeline[, deficit := area_out - current_available]
  
  alignment <- timeline[, .(deficit = max(deficit, 0, na.rm = TRUE), max_demanded = max(area_out, na.rm=TRUE)), by = c(scen_col, "Ns_reg", "class")]
  alignment <- merge(alignment, available, by = c("Ns_reg", "class"), all.x = TRUE)
  alignment[is.na(available_area), available_area := 0]
  alignment[, pct_deficit := round(deficit / pmax(max_demanded, 1e-12) * 100, 2)]
  setnames(alignment, "class", "from_class")
  
  fwrite(alignment, files$harmonization_check_output)
  
  n_deficit <- sum(alignment$deficit > 1e-4)
  if (n_deficit > 0) {
    cli_alert_warning("{n_deficit} region-class combinations have true sequential start map area deficits vs. target demand")
    top_deficit <- alignment[deficit > 1e-4][order(-deficit)][1:min(5, .N)]
    print(top_deficit)
  } else {
    cli_alert_success("Start map and dynamic accumulation fully cover target demand in all regions.")
  }
  
  invisible(alignment)
}

prepare_luc_downscaling_inputs <- function(cfg, mappings, mapping_grid_path, countries_to_run,
                                            country_filter, downscale_resolution, files) {
  mapping_grid <- adapt_mapping_grid(cfg, mappings, mapping_grid_path, countries_to_run, country_filter, downscale_resolution)
  targets_result <- adapt_targets(cfg, files)
  priors_bundle <- load_priors_and_restrictions(cfg, downscale_resolution)
  covariates_bundle <- load_covariates_and_betas(downscale_resolution)
  start_map <- adapt_start_map(cfg, downscale_resolution)
  region_col <- if (!is.null(cfg$region_col)) cfg$region_col else "Ns_reg"
  country_col <- if (!is.null(cfg$country_col)) cfg$country_col else "Ns_reg"

  # Run harmonization alignment check (writes CSV as side effect)
  check_start_map_target_alignment(start_map, targets_result$targets, files,
                                    targets_result$scen_col, targets_result$time_col)

  list(
    mapping_grid = mapping_grid,
    targets = targets_result$targets,
    start_map = start_map,
    priors = priors_bundle$priors,
    restrictions = priors_bundle$restrictions,
    exo_priors_all = priors_bundle$exo_priors_all,
    xmat_full = covariates_bundle$xmat_full,
    xmat_proj_full = covariates_bundle$xmat_proj_full,
    betas_full = covariates_bundle$betas_full,
    region_col = region_col,
    country_col = country_col,
    scen_col = targets_result$scen_col,
    time_col = targets_result$time_col
  )
}

# ===============================================================
# PIPELINE
# ===============================================================
# Nothing from here on inspects current_model_cfg — every macro-model
# difference has already been resolved by the adapters above.

run_luc_optimization <- function() {
  if (!exists("country_filter", envir = .GlobalEnv)) country_filter <- NULL else country_filter <- get("country_filter", envir = .GlobalEnv)
  if (!exists("year_filter", envir = .GlobalEnv))    year_filter    <- NULL else year_filter <- get("year_filter", envir = .GlobalEnv)
  if (!exists("scen_filter", envir = .GlobalEnv))    scen_filter    <- NULL else scen_filter <- get("scen_filter", envir = .GlobalEnv)
  if (!exists("downscale_resolution", envir = .GlobalEnv)) downscale_resolution <- "5km" else downscale_resolution <- get("downscale_resolution", envir = .GlobalEnv)
  
  if (!exists("files", envir = .GlobalEnv)) {
    files <- list(
      lu_output  = "outputs/downscaled_results_lu.csv",
      luc_output = "outputs/downscaled_results_luc.csv"
    )
  } else {
    files <- get("files", envir = .GlobalEnv)
  }
  
  if (!exists("mapping_grid_path", envir = .GlobalEnv)) stop("mapping_grid_path not found")
  mapping_grid_path <- get("mapping_grid_path", envir = .GlobalEnv)
  
  if (!exists("countries_to_run", envir = .GlobalEnv)) countries_to_run <- NULL else countries_to_run <- get("countries_to_run", envir = .GlobalEnv)
  if (!exists("current_model_cfg", envir = .GlobalEnv)) stop("current_model_cfg not found")
  current_model_cfg <- get("current_model_cfg", envir = .GlobalEnv)
  
  cli_h1("1. Preparing Data for LUC Optimization")
  inputs <- prepare_luc_downscaling_inputs(
    cfg = current_model_cfg,
    mappings = if (exists("mappings", envir = .GlobalEnv)) get("mappings", envir = .GlobalEnv) else NULL,
    mapping_grid_path = mapping_grid_path,
    countries_to_run = countries_to_run,
    country_filter = country_filter,
    downscale_resolution = downscale_resolution,
    files = files
  )
  
  # For Step 4.5+ scripts that expect these in global, we will assign them back at the very end
  # but ONLY the specific objects they need. Actually, Step 4.5 reads from disk if possible,
  # or we can export a tiny subset. For now, execute_luc_downscaling saves to disk.
  
  # Setup execution files (redirect to fast local SSD temporary CSVs if output is parquet)
  is_pq_lu  <- !is.null(files$lu_output)  && grepl("\\.parquet$", files$lu_output)
  is_pq_luc <- !is.null(files$luc_output) && grepl("\\.parquet$", files$luc_output)
  
  staging_dir <- if (exists("paths") && !is.null(paths$temp) && dir.exists(paths$temp)) paths$temp else tempdir()
  tmp_lu_csv  <- if (is_pq_lu)  file.path(staging_dir, paste0("tmp_lu_", Sys.getpid(), "_", basename(sub("\\.parquet$", ".csv", files$lu_output)))) else files$lu_output
  tmp_luc_csv <- if (is_pq_luc) file.path(staging_dir, paste0("tmp_luc_", Sys.getpid(), "_", basename(sub("\\.parquet$", ".csv", files$luc_output)))) else files$luc_output
  
  exec_files <- files
  exec_files$lu_output  <- tmp_lu_csv
  exec_files$luc_output <- tmp_luc_csv
  
  # Ensure clean slate for current scenario chunk outputs
  for (f in c(files$lu_output, files$luc_output, tmp_lu_csv, tmp_luc_csv)) {
    if (!is.null(f) && file.exists(f)) file.remove(f)
  }
  
  # execute_luc_downscaling() renames columns of its inputs in place (setnames), so keep
  # an untouched copy of the restrictions for the post-solve compliance check.
  restrictions_for_check <- if (!is.null(inputs$restrictions)) copy(inputs$restrictions) else NULL

  cli_h1("2. Calling Generalized Downscaling Function")
  execute_luc_downscaling(
    targets = inputs$targets,
    start_map = inputs$start_map,
    mapping_grid = inputs$mapping_grid,
    priors = inputs$priors,
    restrictions = inputs$restrictions,
    xmat_full = inputs$xmat_full,
    xmat_proj_full = inputs$xmat_proj_full,
    betas_full = inputs$betas_full,
    exo_priors_all = inputs$exo_priors_all,
    region_cols = inputs$region_col,
    scen_cols = inputs$scen_col,
    time_col = inputs$time_col,
    country_col = inputs$country_col,
    files = exec_files,
    downscale_resolution = downscale_resolution,
    scen_filter = scen_filter,
    year_filter = year_filter,
    country_filter = country_filter,
    countries_to_run = countries_to_run,
    calc_prior_impact_metric = if (!is.null(current_model_cfg$calc_prior_impact_metric)) current_model_cfg$calc_prior_impact_metric else FALSE,
    base_year = if (!is.null(current_model_cfg$base_year)) current_model_cfg$base_year else 2000
  )
  
  # Convert staged CSVs to compressed Parquet if requested
  if (is_pq_lu && file.exists(tmp_lu_csv)) {
    cli_alert_info("Writing Land Use output to compressed Parquet: {basename(files$lu_output)}")
    lu_dt <- data.table::fread(tmp_lu_csv)
    arrow::write_parquet(lu_dt, files$lu_output, compression = "zstd")
    file.remove(tmp_lu_csv)
  }
  if (is_pq_luc && file.exists(tmp_luc_csv)) {
    cli_alert_info("Writing Land Use Change output to compressed Parquet: {basename(files$luc_output)}")
    luc_dt <- data.table::fread(tmp_luc_csv)
    arrow::write_parquet(luc_dt, files$luc_output, compression = "zstd")
    file.remove(tmp_luc_csv)
  }
  
  run_restriction_compliance_check(restrictions_for_check, files, inputs$scen_col)

  cli_alert_success("Optimization chunk completed and flushed to disk.")
}

#' Post-solve gate: no restricted transition may exceed its cap (spatial rulebook,
#' checklist "Non-Negativity & Restriction Compliance"). Writes all violations to
#' files$restriction_check_output (default: next to the LUC output) and stops the run
#' when there are any.
run_restriction_compliance_check <- function(restrictions, files, scen_col) {
  if (is.null(restrictions) || nrow(restrictions) == 0) return(invisible(NULL))
  if (is.null(files$luc_output) || !file.exists(files$luc_output)) {
    cli_alert_warning("Restriction compliance check skipped: no LUC output at {files$luc_output}.")
    return(invisible(NULL))
  }
  if (!exists("check_restriction_compliance")) source("scripts/core/aux_codes/check_restriction_compliance.R")
  luc <- if (grepl("\\.parquet$", files$luc_output)) as.data.table(arrow::read_parquet(files$luc_output)) else fread(files$luc_output)
  sc <- if (!is.null(scen_col) && scen_col %in% names(luc)) scen_col else "scenario"
  res <- check_restriction_compliance(luc, restrictions, scen_col = sc)
  out <- if (!is.null(files$restriction_check_output)) files$restriction_check_output else
    file.path(dirname(files$luc_output), "restriction_compliance_violations.csv")
  fwrite(res$violations, out)
  if (res$n_violations > 0) {
    worst <- res$violations[1]
    stop(sprintf(paste0("Restriction compliance: %d of %d restricted flows exceed their cap ",
                        "(worst: %s -> %s in pixel %s, %s, excess %.6f km2). Details: %s"),
                 res$n_violations, res$n_checked, worst$from_class, worst$to_class, worst$ns,
                 worst$times, worst$excess, out), call. = FALSE)
  }
  cli_alert_success("Restriction compliance: {res$n_checked} restricted flows checked, no cap exceeded.")
  invisible(res)
}

# Execute and natively garbage collect
run_luc_optimization()
cli_alert_success("process_lu_downscaling.R completed.")

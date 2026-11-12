# Engine: loaded once per session, pinned submodule or CASCADE_CORE_PATH (scripts/core/run_context.R)
if (!exists("ensure_cascade_engine", mode = "function")) source("scripts/core/run_context.R")
ensure_cascade_engine()
if (file.exists("scripts/core/gravity_allocator.R")) {
  source("scripts/core/gravity_allocator.R")
} else if (file.exists("scripts/gravity_allocator.R")) {
  source("scripts/gravity_allocator.R")
}


# =========================================================================
# Platform detection: Accelerator (DeepOrigin / Celery / WKUBE) jobs or FUSE mounts
# =========================================================================
is_accelerator <- function() {
  nzchar(Sys.getenv("PROJECT_SLUG", "")) || dir.exists("/mnt/wdrv") || .Platform$OS.type == "unix"
}

get_latest_file_aux <- function(files) {
  if (length(files) == 0) return(NULL)
  
  # Default robust fallback: alphabetical sorting (lexicographical sorting 
  # works perfectly for both YYYYMMDD and YYYY-MM-DD dates)
  sorted_files <- sort(files, decreasing = TRUE)
  return(sorted_files[1])
}

# Standardize regional column to Ns_reg
standardize_ns_reg_col <- function(dt) {
  if (!is.data.frame(dt)) return(dt)
  cfg <- if (exists("current_model_cfg")) current_model_cfg else NULL
  
  if (!is.null(cfg) && !is.null(cfg$macro_model_resolution)) {
    macro_col <- cfg$macro_model_resolution
    if (macro_col %in% names(dt) && !"Ns_reg" %in% names(dt)) {
      setnames(dt, macro_col, "Ns_reg")
      return(dt)
    }
  }
  
  # Global Fallbacks
  fallback_cols <- c("CAPRI_region", "CAPRI_NUTS", "GLOB_ColRow30", "GLOB_country")
  for (col in fallback_cols) {
    if (col %in% names(dt) && !"Ns_reg" %in% names(dt)) {
      setnames(dt, col, "Ns_reg")
      return(dt)
    }
  }
  return(dt)
}

# Exact path of an intermediate file for the current run: <prefix>[_<res>][_<model_name>]_<run_basename>_<run_date>.qs
# in paths$temp. Single source of truth for the naming used by save_intermediate() and
# resolve_run_scenario_inputs(); run_date defaults to the run's RUN_DATE / OVERRIDE_DATE.
intermediate_file_name <- function(prefix, downscale_resolution = NULL, run_date = NULL) {
  res_str <- if(!is.null(downscale_resolution)) paste0("_", downscale_resolution) else ""
  pipeline_str <- if (exists("model_name", envir = .GlobalEnv)) paste0("_", get("model_name", envir = .GlobalEnv)) else ""
  run_date_str <- if (!is.null(run_date)) run_date else if (exists("RUN_DATE") && !is.null(RUN_DATE)) RUN_DATE else if (exists("OVERRIDE_DATE") && !is.null(OVERRIDE_DATE)) OVERRIDE_DATE else format(Sys.Date(), "%Y%m%d")
  run_basename_str <- if (exists("run_basename", envir = .GlobalEnv)) get("run_basename", envir = .GlobalEnv) else "manual_run"
  file.path(paths$temp, paste0(prefix, res_str, pipeline_str, "_", run_basename_str, "_", run_date_str, ".qs"))
}

# Helper to save intermediate files with version-aware naming
save_intermediate <- function(object, prefix, downscale_resolution = NULL) {
  restore_cols <- NULL
  
  if (is.data.frame(object)) {
    native_cols <- if (exists(".NATIVE_SCEN_COLS")) .NATIVE_SCEN_COLS else "LAMAScen"
    
    if (length(native_cols) == 1 && native_cols != "scenario" && native_cols %in% names(object)) {
      setnames(object, native_cols, "scenario")
      restore_cols <- native_cols
    } else if (length(native_cols) > 1 && all(native_cols %in% names(object))) {
      object[, scenario := do.call(paste, c(.SD, sep = "::")), .SDcols = native_cols]
      restore_cols <- native_cols
    }
  }
  
  file_name <- intermediate_file_name(prefix, downscale_resolution)
  if (!dir.exists(dirname(file_name))) dir.create(dirname(file_name), recursive = TRUE, showWarnings = FALSE)
  qs2::qs_save(object, file_name)
  
  if (!is.null(restore_cols)) {
    if (length(restore_cols) == 1) {
      setnames(object, "scenario", restore_cols)
    } else {
      object[, scenario := NULL]
    }
  }
  return(file_name)
}

# Path of the intermediate <prefix> for the current run (data-pipelines rulebook,
# Principles 2, 8 and 13):
#   1. the current run's own file (exact name from intermediate_file_name(), the
#      run's RUN_DATE), in .qs or .rds;
#   2. otherwise, if reuse is allowed (cfg$intermediate_reuse, default TRUE), the
#      newest file of the same pipeline and run name with a date not later than
#      the run's date. Every reuse is logged once and recorded in
#      <output_prefix>_intermediate_resolutions.csv and the sources metadata;
#   3. otherwise stop.
# Files dated after the run's date belong to later runs and are never used. Two
# formats with the same date stop the run. The legacy lookup without the
# pipeline name is used only with cfg$intermediate_unnamed_fallback = TRUE.
get_intermediate_path <- function(prefix, downscale_resolution = NULL) {
  cfg <- if (exists("current_model_cfg", envir = .GlobalEnv)) get("current_model_cfg", envir = .GlobalEnv) else list()
  allow_reuse <- !isFALSE(cfg$intermediate_reuse)
  allow_unnamed <- isTRUE(cfg$intermediate_unnamed_fallback)

  exact_qs <- intermediate_file_name(prefix, downscale_resolution)
  run_date <- sub("^.*_(\\d{8})\\.qs$", "\\1", basename(exact_qs))
  exact <- c(exact_qs, sub("\\.qs$", ".rds", exact_qs))
  exact <- exact[file.exists(exact)]
  if (length(exact) > 1) {
    stop(sprintf("Intermediate '%s': both .qs and .rds exist for run date %s (%s). Remove the stale one.",
                 prefix, run_date, paste(basename(exact), collapse = ", ")))
  }
  if (length(exact) == 1) {
    .record_intermediate_resolution(prefix, exact, run_date, run_date, "current_run")
    return(exact)
  }

  res_str <- if (!is.null(downscale_resolution)) paste0("_", downscale_resolution) else ""
  pipeline_str <- if (exists("model_name", envir = .GlobalEnv)) paste0("_", get("model_name", envir = .GlobalEnv)) else ""
  run_basename_str <- if (exists("run_basename", envir = .GlobalEnv)) get("run_basename", envir = .GlobalEnv) else "manual_run"
  pattern <- paste0("^", prefix, res_str, pipeline_str, "_", run_basename_str, "_\\d{8}\\.(rds|qs)$")
  files <- list.files(paths$temp, pattern = pattern, full.names = TRUE)
  source_kind <- "earlier_run"
  if (length(files) == 0 && nchar(pipeline_str) > 0 && allow_unnamed) {
    fallback_pattern <- paste0("^", prefix, res_str, "_", run_basename_str, "_\\d{8}\\.(rds|qs)$")
    files <- list.files(paths$temp, pattern = fallback_pattern, full.names = TRUE)
    source_kind <- "earlier_run_unnamed"
  }
  if (length(files) == 0) stop("No intermediate file found for pattern: ", pattern)

  dates <- sub("^.*_(\\d{8})\\.(rds|qs)$", "\\1", basename(files))
  later <- files[dates > run_date]
  files <- files[dates <= run_date]; dates <- dates[dates <= run_date]
  if (length(files) == 0) {
    stop(sprintf("Intermediate '%s': no file for run date %s or earlier; only later runs have one (%s).",
                 prefix, run_date, paste(basename(later), collapse = ", ")))
  }
  newest <- max(dates)
  pick <- files[dates == newest]
  if (length(pick) > 1) {
    stop(sprintf("Intermediate '%s': both .qs and .rds exist for date %s (%s). Remove the stale one.",
                 prefix, newest, paste(basename(pick), collapse = ", ")))
  }
  if (!allow_reuse) {
    stop(sprintf("Intermediate '%s' was not written by this run (run date %s); newest earlier file: %s. ",
                 prefix, run_date, basename(pick)),
         "Rerun the producing step, or set intermediate_reuse = TRUE in config.R.")
  }
  .record_intermediate_resolution(prefix, pick, newest, run_date, source_kind)
  pick
}

# TRUE if the current run itself wrote the intermediate <prefix> (exact name for the
# run's date, .qs or .rds). For "already done in this run?" checks; never reuses.
current_run_intermediate_exists <- function(prefix, downscale_resolution = NULL) {
  exact_qs <- intermediate_file_name(prefix, downscale_resolution)
  any(file.exists(c(exact_qs, sub("\\.qs$", ".rds", exact_qs))))
}

# Label for a gross-transition fit inside a data.table `by` group, e.g.
# "BMLEH_Los1 transitions: scenario=S1, Ns_reg=DE11, year=2030" (pass .BY as `by`).
ipf_label <- function(step, by = NULL) {
  if (is.null(by) || !length(by)) return(step)
  paste0(step, ": ", paste(names(by), vapply(by, as.character, character(1)), sep = "=", collapse = ", "))
}

# Write the gross-transition fit diagnostics of this session (cascade-core
# ipf_diagnostics()) to <output_prefix>_ipf_diagnostics.csv in the run directory and
# log a summary: fits, infeasible margins, fits that miss their margins.
write_ipf_diagnostics <- function(step = "") {
  if (!exists("ipf_diagnostics", mode = "function")) return(invisible(NULL))
  d <- ipf_diagnostics()
  if (!nrow(d)) return(invisible(d))
  if (!exists("output_prefix", envir = .GlobalEnv) || !exists("paths", envir = .GlobalEnv) || is.null(paths$output_dir)) {
    return(invisible(d))
  }
  out <- file.path(paths$output_dir, paste0(get("output_prefix", envir = .GlobalEnv), "_ipf_diagnostics.csv"))
  tmp <- paste0(out, ".tmp")
  data.table::fwrite(d, tmp)
  file.rename(tmp, out)
  n_bad <- sum(!d$converged, na.rm = TRUE); n_inf <- sum(!d$feasible, na.rm = TRUE)
  msg <- sprintf("Gross-transition fits%s: %d recorded, %d with infeasible margins, %d missing their margins (max relative error %.3g). See %s",
                 if (nzchar(step)) paste0(" after ", step) else "", nrow(d), n_inf, n_bad,
                 max(d$margin_error, na.rm = TRUE), basename(out))
  if (requireNamespace("cli", quietly = TRUE)) {
    if (n_bad + n_inf > 0) cli::cli_alert_warning(msg) else cli::cli_alert_info(msg)
  } else message(msg)
  invisible(d)
}

# Registry of intermediate resolutions for the run manifest. Reuses of earlier
# runs' files are announced once per prefix and file, and registered as sources.
.record_intermediate_resolution <- function(prefix, path, file_date, run_date, kind) {
  reg <- if (exists(".intermediate_resolutions", envir = .GlobalEnv)) get(".intermediate_resolutions", envir = .GlobalEnv) else NULL
  if (!is.null(reg) && nrow(reg[reg$prefix == prefix & reg$file == basename(path), ]) > 0) return(invisible(NULL))
  row <- data.table::data.table(prefix = prefix, file = basename(path), file_run_date = file_date,
                                run_date = run_date, source = kind, resolved_at = format(Sys.time()))
  reg <- data.table::rbindlist(list(reg, row))
  assign(".intermediate_resolutions", reg, envir = .GlobalEnv)
  if (kind != "current_run") {
    msg <- sprintf("Reusing intermediate '%s' from run date %s (current run %s): %s", prefix, file_date, run_date, basename(path))
    if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_warning(msg) else message(msg)
    if (exists("register_source_file", mode = "function")) {
      register_source_file(paste0("Intermediate reused (", kind, ")"), paste0(prefix, " from run date ", file_date), path)
    }
  }
  if (exists("output_prefix", envir = .GlobalEnv) && exists("paths", envir = .GlobalEnv) && !is.null(paths$output_dir) &&
      dir.exists(paths$output_dir) && basename(paths$output_dir) == get("output_prefix", envir = .GlobalEnv)) {
    out <- file.path(paths$output_dir, paste0(get("output_prefix", envir = .GlobalEnv), "_intermediate_resolutions.csv"))
    tmp <- paste0(out, ".tmp")
    data.table::fwrite(reg, tmp)
    file.rename(tmp, out)
  }
  invisible(NULL)
}

# Paths of per-scenario intermediates (<prefix>_<scenario>), named by scenario tag.
# Falls back to a single unsuffixed <prefix> intermediate, named "ALL".
get_scenario_intermediate_paths <- function(prefix, scenarios, downscale_resolution = NULL) {
  scen_tags <- gsub(":", "_", scenarios)
  found <- lapply(scen_tags, function(s) tryCatch(get_intermediate_path(paste0(prefix, "_", s), downscale_resolution), error = function(e) NULL))
  names(found) <- scen_tags
  found <- unlist(found)
  if (length(found) == 0) {
    combined <- tryCatch(get_intermediate_path(prefix, downscale_resolution), error = function(e) NULL)
    if (!is.null(combined)) found <- c(ALL = combined)
  }
  found
}

# Resolve exactly one input file per expected scenario for the CURRENT run, without
# globbing shared directories (rulebook data-pipelines, Principle 13).
#   prefix     e.g. "production_downscaled"; files are <prefix>_<scenario tag>
#   scenarios  the run's scenarios (e.g. active_scenarios); ":" -> "_" as in the orchestrator
#   run_date   the run's RUN_DATE (required, no "latest file" resolution)
#   csv_dir    optional run output directory holding <prefix>_<tag>.csv copies, used per
#              scenario only when the run's .qs intermediate is absent (logged)
# Stops if any scenario has no input. Returns data.table(scenario_tag, path, source).
resolve_run_scenario_inputs <- function(prefix, scenarios, run_date, csv_dir = NULL,
                                        downscale_resolution = NULL, label = prefix) {
  if (length(scenarios) == 0) stop("resolve_run_scenario_inputs(", label, "): no scenarios given.")
  if (is.null(run_date) || !nzchar(run_date)) stop("resolve_run_scenario_inputs(", label, "): run_date is required.")
  tags <- unique(gsub(":", "_", scenarios))
  res <- data.table::rbindlist(lapply(tags, function(tag) {
    qs_path <- intermediate_file_name(paste0(prefix, "_", tag), downscale_resolution, run_date = run_date)
    csv_path <- if (!is.null(csv_dir)) file.path(csv_dir, paste0(prefix, "_", tag, ".csv")) else NA_character_
    if (file.exists(qs_path)) {
      data.table::data.table(scenario_tag = tag, path = qs_path, source = "temp_qs")
    } else if (!is.na(csv_path) && file.exists(csv_path)) {
      data.table::data.table(scenario_tag = tag, path = csv_path, source = "run_output_csv")
    } else {
      data.table::data.table(scenario_tag = tag, path = NA_character_, source = "missing")
    }
  }))
  missing <- res[source == "missing", scenario_tag]
  if (length(missing) > 0) {
    stop(sprintf("resolve_run_scenario_inputs(%s): no input for run date %s and scenario(s): %s. Expected e.g. %s",
                 label, run_date, paste(missing, collapse = ", "),
                 basename(intermediate_file_name(paste0(prefix, "_", missing[1]), downscale_resolution, run_date = run_date))))
  }
  n_csv <- sum(res$source == "run_output_csv")
  if (n_csv > 0 && requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_warning(sprintf("%s: %d of %d scenario(s) read from the run output CSV (no .qs intermediate for run date %s): %s",
                                   label, n_csv, nrow(res), run_date, paste(res[source == "run_output_csv", scenario_tag], collapse = ", ")))
  }
  res
}

# Stop if `keys` do not identify rows of `dt` uniquely (guard before aggregation).
assert_unique_keys <- function(dt, keys, label = "table") {
  dup <- dt[, .N, by = keys][N > 1]
  if (nrow(dup) > 0) {
    stop(sprintf("%s: %d duplicated key combination(s) on (%s), e.g. %s. Inputs overlap; refusing to aggregate.",
                 label, nrow(dup), paste(keys, collapse = ", "),
                 paste(utils::capture.output(print(utils::head(dup, 3))), collapse = " | ")))
  }
  invisible(TRUE)
}

# Helper to load the latest intermediate file matching the version
read_intermediate <- function(prefix, downscale_resolution = NULL, apply_filters = TRUE) {
  latest_file <- get_intermediate_path(prefix, downscale_resolution)
  if (grepl("\\.qs$", latest_file)) {
    dt <- qs2::qs_read(latest_file)
  } else {
    dt <- readRDS(latest_file)
  }
  
  if (apply_filters && is.data.frame(dt) && exists("apply_globiom_filters", mode = "function")) {
    dt <- apply_globiom_filters(dt)
  }
  
  return(dt)
}

# ---------------------------------------------------------------
# Runtime Subsetting and Cache Validation
# ---------------------------------------------------------------

apply_globiom_filters <- function(dt, c_filter = NULL, y_filter = NULL, s_filter = NULL) {
  if (!is.data.frame(dt)) return(dt)
  
  if (exists("country_filter") && is.null(c_filter)) c_filter <- country_filter
  if (exists("year_filter") && is.null(y_filter)) y_filter <- year_filter
  if (exists("scen_filter") && is.null(s_filter)) {
    # Extract original filter before composite expansion if possible
    s_filter <- if (exists("current_model_cfg") && !is.null(current_model_cfg$scen_filter) && !identical(current_model_cfg$scen_filter, "ALL")) current_model_cfg$scen_filter else scen_filter
  }
  
  if (!is.null(c_filter) && "GLOB_country" %in% names(dt)) {
    if (!all(dt$GLOB_country %in% c("UNKNOWN", NA))) {
      dt <- dt[!is.na(GLOB_country) & GLOB_country %in% c_filter]
    }
  }
    
  if (!is.null(y_filter) && "year" %in% names(dt)) {
    b_y <- if (exists("base_year")) base_year else 2000
    dt <- dt[!is.na(year) & year %in% union(b_y, y_filter)]
  }
  
  if (!is.null(s_filter) && !identical(s_filter, "ALL")) {
    native_cols <- if (exists(".NATIVE_SCEN_COLS")) .NATIVE_SCEN_COLS else "LAMAScen"
    primary_col <- native_cols[1]
    
    b_s <- if (exists("baseline_scen")) baseline_scen else "BASE"
    
    if (primary_col %in% names(dt)) {
      dt <- dt[!is.na(get(primary_col)) & get(primary_col) %in% union(b_s, s_filter)]
    } else if ("ForestScen" %in% names(dt) && exists("scenario_mapping")) {
      temp_sf <- scenario_mapping[scenario_mapping[[primary_col]] %in% union(b_s, s_filter), .(ForestScen)]
      dt <- dt[!is.na(ForestScen) & ForestScen %in% temp_sf$ForestScen]
    } else if ("scenario" %in% names(dt)) {
      # If s_filter contains composite keys (e.g. "Base_CPFRT::RCP4p5") but dt only has "scenario" 
      # which is likely the primary_col value ("Base_CPFRT"), extract the primary_col part.
      s_filter_base <- sapply(strsplit(s_filter, "::"), function(x) x[1])
      dt <- dt[!is.na(scenario) & scenario %in% union(b_s, union(s_filter, s_filter_base))]
    }
  }
  
  if (ncol(dt) > 0) {
    is_fac <- vapply(dt, is.factor, logical(1))
    if (any(is_fac)) {
      factor_cols <- names(dt)[is_fac]
      for (col in factor_cols) set(dt, j = col, value = as.character(dt[[col]]))
    }
  }
  return(dt)
}

validate_cache_subsets <- function(dt, c_filter = NULL, y_filter = NULL, s_filter = NULL) {
  if (exists("country_filter") && is.null(c_filter)) c_filter <- country_filter
  if (exists("year_filter") && is.null(y_filter)) y_filter <- year_filter
  if (exists("scen_filter") && is.null(s_filter)) {
    s_filter <- if (exists("current_model_cfg") && !is.null(current_model_cfg$scen_filter) && !identical(current_model_cfg$scen_filter, "ALL")) current_model_cfg$scen_filter else scen_filter
  }
  
  is_valid <- TRUE
  
  # Check Countries
  if (!is.null(c_filter) && "GLOB_country" %in% names(dt)) {
    # GLOB_NA is a placeholder/spillover and never present in GDX extracts
    expected_c <- setdiff(c_filter, "GLOB_NA")
    missing_c <- setdiff(expected_c, unique(dt$GLOB_country))
    if (length(missing_c) > 0) {
       message(">>> Cache invalid: missing countries (", paste(missing_c, collapse=", "), ")")
       is_valid <- FALSE
    }
  }
  
  # Check Years
  if (!is.null(y_filter) && "year" %in% names(dt)) {
    # If the user provides a large sequence (e.g. 2000:2100), they just want a range, 
    # not strict enforcement of every single year (GDX only has decadal data).
    # We also do not enforce the presence of base_year (e.g. 2000) because it is 
    # injected later by adapt_targets and is typically absent from raw GDX transition data.
    if (length(y_filter) > 10) {
      expected_y <- c() # Just validate the range conceptually, skip strict checking
    } else {
      expected_y <- y_filter
    }
    
    if (length(expected_y) > 0) {
      missing_y <- setdiff(expected_y, unique(dt$year))
      if (length(missing_y) > 0) {
         message(">>> Cache invalid: missing years (", paste(missing_y, collapse=", "), ")")
         is_valid <- FALSE
      }
    }
  }
  
  # Check Scenarios
  if (!is.null(s_filter)) {
    b_s <- if (exists("baseline_scen")) baseline_scen else "BASE"
    required_s <- union(b_s, s_filter)
    
    native_cols <- if (exists(".NATIVE_SCEN_COLS")) .NATIVE_SCEN_COLS else "LAMAScen"
    primary_col <- native_cols[1]
    
    avail_scens <- character(0)
    if (primary_col %in% names(dt)) {
      avail_scens <- unique(dt[[primary_col]])
    } else if ("scenario" %in% names(dt)) {
      avail_scens <- unique(dt$scenario)
    } else if ("ForestScen" %in% names(dt) && exists("scenario_mapping")) {
      avail_scens <- scenario_mapping[ForestScen %in% unique(dt$ForestScen)][[primary_col]]
    }
    
    if (length(avail_scens) > 0) {
       missing_s <- setdiff(required_s, avail_scens)
       if (length(missing_s) > 0) {
          message(">>> Cache invalid: missing scenarios (", paste(missing_s, collapse=", "), ")")
          is_valid <- FALSE
       }
    }
  }
  
  return(is_valid)
}

# General function to dynamically rename and standardize GLOBIOM GDX columns
standardize_globiom_columns <- function(dt, region_col_name = "GLOB_REGION59", val_col_name = "value", expected_cols = NULL) {
  # If the parameter lacks domain names (has uni_X columns), fallback to positional mapping if provided
  if (any(grepl("^records\\.", names(dt)))) {
    setnames(dt, names(dt), gsub("^records\\.", "", names(dt)))
  }
  
  run_dropped <- FALSE
  for (col in names(dt)) {
    if (is.character(dt[[col]]) || is.factor(dt[[col]])) {
      u_vals <- as.character(unique(dt[[col]]))
        # Check if the single unique value strongly matches the run name
        if (all(grepl("^output_", u_vals))) {
          dt[, (col) := NULL]
          run_dropped <- TRUE
          break # Only drop the first matching column to protect single-value scenario columns
        }
      }
    
  }
  
  # Adjust expected_cols since the run column has been dropped from the table (or was missing natively)
  if (!is.null(expected_cols) && "GLOB_run" %in% expected_cols) {
    if (run_dropped || ncol(dt) == length(expected_cols) - 1) {
      expected_cols <- setdiff(expected_cols, "GLOB_run")
    }
  }

  # Anonymous columns processing (e.g. uni_1, uni_2)
  if (!is.null(expected_cols) && any(grepl("^uni_[0-9]+$", names(dt)))) {
    if (length(expected_cols) == ncol(dt)) {
      setnames(dt, expected_cols)
    } else {
      warning("expected_cols length does not match ncol(dt). Falling back to generic mapping.")
    }
  }
  
  # Dynamically map AllColRow depending on content
  colrow_names <- c("AllColRow", "ALLColRow", "ALLCOLROW")
  matched_colrow <- intersect(colrow_names, names(dt))
  if (length(matched_colrow) > 0) {
    target_col <- matched_colrow[1]
    # Check if ANY element in the column contains "CR"
    if (any(grepl("CR", dt[[target_col]], ignore.case = TRUE))) {
      setnames(dt, target_col, "GLOB_ColRow30")
    } else {
      setnames(dt, target_col, "GLOB_LUID")
    }
  }
  
  # Dictionary of typical raw GDX column names to standard LAMASUS names
  col_map <- list(
    "ANYREGION" = region_col_name,
    "COUNTRY" = "GLOB_country",
    "LUID" = "GLOB_LUID",
    "ALTI" = "GLOB_alti",
    "SLP" = "GLOB_slope",
    "SOIL" = "GLOB_soil",
    "AEZ" = "GLOB_AEZ",
    "AEZCLASS" = "GLOB_AEZ",
    "LC_TYPES_EPIC" = "GLOB_lu_class",
    "LCTYP" = "GLOB_lu_class",
    "LC_TYPE" = "GLOB_lu_class", 
    "ALLTECH" = "GLOB_CSYS",
    "LIVE_SYSTEM" = "GLOB_LSSYS",
    "ANIMAL" = "GLOB_LSTYP",
    "CROP" = "GLOB_CTYP",
    "SPECIES" = "GLOB_CTYP",
    "ALLITEM" = "ALLITEM",
    "LCTYPF" = "from_class",
    "LCTYPT" = "to_class",
    "FOREST_TYPES" = "GLOBFOR_lum_class",
    "ForestScen" = "ForestScen",
    "FORESTSCEN" = "ForestScen",
    "ALLIEA_SCEN" = "ForestScen",
    "MACROSCEN" = "MacroScen",
    "AllMacroScen" = "MacroScen",
    "MacroScen" = "MacroScen",
    "INDEX" = "index",
    "index" = "index",
    "uni" = "GLOBFOR_lum_class",
    "SIMU" = "GLOB_ColRow30",
    "AllScenYear" = "year",
    "AllScenyear" = "year",
    "ScenYear" = "year",
    "YEAR" = "year",
    "Year" = "year",
    "ALLYEAR" = "year",
    "AllYear" = "year",
    "scenario" = "LAMAScen",
    "LAMAScen" = "LAMAScen"
  )
  
  safe_rename <- function(old, new) {
    if (old %in% names(dt)) {
      if (new %in% names(dt) && old != new) {
        dt[, (old) := NULL]
      } else if (old != new) {
        setnames(dt, old, new)
      }
    }
  }

  for (raw_col in names(col_map)) {
    safe_rename(raw_col, col_map[[raw_col]])
  }
  
  # Ensure Gamstransfer's default "value" column is renamed if needed
  if ("value" %in% names(dt) && val_col_name != "value") {
    safe_rename("value", val_col_name)
  }
  
  # Handle scenario columns gracefully (fallback to the deepest available ALLSCEN)
  if ("ALLSCEN3" %in% names(dt)) {
    safe_rename("ALLSCEN3", "LAMAScen")
    safe_rename("ALLSCEN2", "RCPScen")
    safe_rename("ALLSCEN1", "MacroScen")
  } else if ("ALLSCEN2" %in% names(dt)) {
    safe_rename("ALLSCEN2", "LAMAScen")
    safe_rename("ALLSCEN1", "MacroScen")
  } else if ("ALLSCEN1" %in% names(dt)) {
    safe_rename("ALLSCEN1", "LAMAScen")
  }
  
  # Standardize types
  if (any(duplicated(names(dt)))) {
    dt <- dt[, unique(names(dt)), with = FALSE]
  }
  
  if ("year" %in% names(dt)) {
    dt[, year := as.integer(as.character(year))]
  }
  
  if (val_col_name %in% names(dt)) {
    dt[, (val_col_name) := as.numeric(get(val_col_name))]
  }
  
  return(dt)
}

recode_lu <- function(x) {
  lookup <- c(
    "CrpLnd" = "Cropland",
    "OthAgri" = "Cropland_other",
    "OagLnd" = "Cropland_other",
    "GrsLnd" = "Pasture",
    "Grass" = "Pasture",
    "PriFor" = "Forests_primary",
    "MngFor" = "Forests_managed",
    "NatLnd" = "Natural_unmanaged",
    "PltFor" = "Forests_SR",
    "Forest" = "Forests",
    "OthNatLnd" = "Natural_unmanaged",
    "WetLnd" = "Wetlands_natural",
    "ResOrg" = "Wetlands_restored",
    "NotRel" = "Natural_other",
    "G4MAfr" = "Forests_unmanaged",
#    "Affor" = "Forests_unmanaged",
#    "Defor" = "Forests_unmanaged",
    "Cur0" = "Forests_unmanaged",
    "CurS" = "Forests_protected",
    "CurC_M" = "Forests_MI_coniferous",
    "CurNC_M" = "Forests_MI",
    "CurC_L" = "Forests_LI_coniferous",
    "CurNC_L" = "Forests_LI",
    "CurC_H" = "Forests_HI_coniferous",
    "CurNC_H" = "Forests_HI",
    "CurC_HN" = "Forests_SR_coniferous",
    "CurNC_HN" = "Forests_SR",
    "CurC" = "Forests_HIG_coniferous",
    "CurNC" = "Forests_HIG",
    "CurC_R" = "Forests_restored_coniferous",
    "CurNC_R" = "Forests_restored",
    "CurC_LR" = "Forests_LI_restored_coniferous",
    "CurNC_LR" = "Forests_LI_restored",
    "SimUarea" = "GLOB_LUID_AEZ_TOT_AREA"
  )
  x_char <- as.character(x)
  res <- unname(lookup[x_char])
  ifelse(is.na(res), x_char, res)
}

#' Apply Composite Scenario logic to a data.table
#' Concatenates multiple scenario columns into a single 'scenario' column.
#' If only one column is specified, renames it to 'scenario'.
apply_composite_scenario <- function(dt, cfg, default_scen_val = "BASE") {
  if (is.null(dt) || nrow(dt) == 0) return(dt)
  
  native_cols <- if (!is.null(cfg$native_scen_cols)) cfg$native_scen_cols else
                 if (!is.null(cfg$scen_col)) cfg$scen_col else "scenario"
  
  # If there is no scenario mapping or only 1 native col expected, return basic mapping
  if (length(native_cols) <= 1) {
    if ("scenario" %in% names(dt)) return(dt)
    if (length(native_cols) == 1 && native_cols %in% names(dt)) setnames(dt, native_cols, "scenario")
    return(dt)
  }
  
  # For composite scenarios, if dt ONLY has 'scenario' (which happens when save_intermediate renames primary_col),
  # temporarily rename it back to primary native col to allow joining with scenario mapping
  primary_col <- native_cols[1]
  if ("scenario" %in% names(dt) && !primary_col %in% names(dt) && !any(grepl("::", dt$scenario[1]))) {
    setnames(dt, "scenario", primary_col)
  }
  
  # Fill missing native dimensions via scenario_mapping cross-join
  existing_native <- intersect(native_cols, names(dt))
  missing_native <- setdiff(native_cols, existing_native)
  
  if (!is.null(cfg$scenario_mapping) && length(missing_native) > 0) {
    map_dt <- as.data.table(cfg$scenario_mapping)
    if (length(existing_native) > 0) {
      join_cols <- intersect(names(map_dt), existing_native)
      if (length(join_cols) > 0) {
        dt <- merge(dt, map_dt, by = join_cols, all.x = TRUE, allow.cartesian = TRUE)
      }
    } else {
      # No native columns, cross join everything
      dt[, dummy := 1]
      map_dt[, dummy := 1]
      dt <- merge(dt, map_dt, by = "dummy", allow.cartesian = TRUE)
      dt[, dummy := NULL]
      map_dt[, dummy := NULL]
    }
  }
  
  # Fallback: fill any still-missing columns with default_scen_val
  missing_native2 <- setdiff(native_cols, names(dt))
  if (length(missing_native2) > 0) {
    for (c in missing_native2) set(dt, j = c, value = default_scen_val)
  }
  
  # Build composite string
  dt[, scenario := do.call(paste, c(.SD, sep = "::")), .SDcols = native_cols]
  
  return(dt)
}

#' Unpack Composite Scenario logic from a data.table
#' Splits a single 'scenario' column back into the original multi-columns defined in cfg
unpack_composite_scenario <- function(dt, cfg) {
  if (is.null(dt)) return(dt)
  native_cols <- if (!is.null(cfg$native_scen_cols)) cfg$native_scen_cols else NULL
  
  if (!is.null(native_cols) && length(native_cols) > 1 && "scenario" %in% names(dt)) {
     dt[, (native_cols) := data.table::tstrsplit(scenario, "::", fixed = TRUE)]
     dt[, scenario := NULL]
     cols <- names(dt)
     new_cols <- c(setdiff(cols, native_cols), native_cols)
     data.table::setcolorder(dt, new_cols)
  }
  return(dt)
}

#' Locate the cascade_indicators Directory Dynamically
#' Resolves across environment variables, R options, and candidate drive letters.
get_cascade_indicators_dir <- function(create_if_missing = FALSE, fallback = NULL) {
  # 1. Environment Variable
  env_p <- Sys.getenv("CASCADE_INDICATORS_DIR", unset = "")
  if (nzchar(env_p) && dir.exists(env_p)) {
    return(normalizePath(env_p, winslash = "/", mustWork = FALSE))
  }
  
  # 2. R Option
  opt_p <- getOption("cascade_indicators_dir", default = NULL)
  if (!is.null(opt_p) && nzchar(opt_p) && dir.exists(opt_p)) {
    return(normalizePath(opt_p, winslash = "/", mustWork = FALSE))
  }
  
  # Helper to ensure target directory is responsive and writable
  is_responsive_and_writable <- function(d) {
    if (!dir.exists(d)) return(FALSE)
    tf <- file.path(d, paste0(".probe_write_", Sys.getpid(), ".tmp"))
    ok <- tryCatch({
      suppressWarnings(writeLines("probe", tf))
      if (file.exists(tf)) unlink(tf)
      TRUE
    }, error = function(e) FALSE)
    isTRUE(ok)
  }

  # 3. Dynamic Drive Discovery (safe search on active drives only to prevent hangs on disconnected network mounts)
  if (.Platform$OS.type == "windows") {
    cand_drives <- c("C:/cascade_indicators", "D:/cascade_indicators", "G:/cascade_indicators", "P:/cascade_indicators")
    for (d in cand_drives) {
      if (is_responsive_and_writable(d)) {
        return(normalizePath(d, winslash = "/", mustWork = FALSE))
      }
    }
  } else {
    cand_mounts <- c("/mnt/cascade_indicators", "/media/cascade_indicators", "~/cascade_indicators")
    for (m in cand_mounts) {
      if (is_responsive_and_writable(m)) return(normalizePath(m, winslash = "/", mustWork = FALSE))
    }
  }
  
  # 4. Fallback or NULL
  if (!is.null(fallback) && is_responsive_and_writable(fallback)) {
    return(normalizePath(fallback, winslash = "/", mustWork = FALSE))
  }
  
  return(NULL)
}

#' Read output table transparently from .parquet or .csv
read_output_table <- function(path_or_prefix, ...) {
  if (file.exists(path_or_prefix)) {
    if (grepl("\\.parquet$", path_or_prefix, ignore.case = TRUE)) {
      return(tryCatch(
        data.table::as.data.table(arrow::read_parquet(path_or_prefix, memory_map = FALSE, ...)),
        error = function(e) data.table::fread(path_or_prefix, ...)
      ))
    } else {
      return(data.table::fread(path_or_prefix, ...))
    }
  }
  # If base path without extension was passed
  pq_cand <- paste0(path_or_prefix, ".parquet")
  csv_cand <- paste0(path_or_prefix, ".csv")
  if (file.exists(pq_cand)) {
    return(tryCatch(
      data.table::as.data.table(arrow::read_parquet(pq_cand, memory_map = FALSE, ...)),
      error = function(e) data.table::fread(pq_cand, ...)
    ))
  } else if (file.exists(csv_cand)) {
    return(data.table::fread(csv_cand, ...))
  }
  stop("Output file not found: ", path_or_prefix, " (.parquet or .csv)")
}


# =========================================================================
# Period pairing for transition targets
# =========================================================================
# Transitions for period t are built from the area at t and at its predecessor.
# The predecessor is the previous entry in the ORDERED period list of the same
# group (scenario, or model x scenario), never `year + step`:
#   - irregular reporting years (2020, 2025, 2030, 2050) pair correctly;
#   - sparse data (zero-area classes have no row) still reads as area 0 after
#     the full outer join in the callers, as with the old `year + step` join;
#   - groups with different year sets each get their own pairs.
# `expected_step` keeps a known model cadence as a check: e.g. 10 for GLOBIOM
# stops the run when any other step appears, instead of silently pairing a stray
# year. `window` restricts the periods considered (base year inclusive).
build_period_pairs <- function(dt, group_cols = "scenario", year_col = "year",
                               window = NULL, expected_step = NULL, label = "") {
  yrs <- unique(dt[, c(group_cols, year_col), with = FALSE])
  setnames(yrs, year_col, ".yr")
  yrs[, .yr := as.integer(.yr)]
  if (!is.null(window)) yrs <- yrs[.yr >= window[1] & .yr <= window[2]]
  setorderv(yrs, c(group_cols, ".yr"))
  pairs <- yrs[, .(year_lag = .yr[-.N], year = .yr[-1]), by = group_cols]
  if (nrow(pairs) == 0) {
    stop(sprintf("build_period_pairs%s: fewer than two periods per group%s.",
                 if (nzchar(label)) paste0(" [", label, "]") else "",
                 if (!is.null(window)) sprintf(" within %d-%d", window[1], window[2]) else ""), call. = FALSE)
  }
  pairs[, step := year - year_lag]
  if (!is.null(expected_step)) {
    bad <- pairs[step != expected_step]
    if (nrow(bad) > 0) {
      ex <- bad[1]
      stop(sprintf(paste0("build_period_pairs%s: expected a %d-year step, found %d (%d -> %d) in %d pair(s). ",
                          "Check the target years, or adjust `expected_period_step` in config.R."),
                   if (nzchar(label)) paste0(" [", label, "]") else "",
                   expected_step, ex$step, ex$year_lag, ex$year, nrow(bad)), call. = FALSE)
    }
  }
  steps <- unique(pairs$step)
  message(sprintf(">>> Period pairs%s: %s", if (nzchar(label)) paste0(" [", label, "]") else "",
                  paste(unique(paste0(pairs$year_lag, "->", pairs$year)), collapse = ", ")))
  if (length(steps) > 1) message(sprintf(">>> Note: irregular period lengths (%s years).", paste(sort(steps), collapse = ", ")))
  pairs[]
}

# Lagged copy of `dt`: each row's year is moved to its successor period, and
# `value_col` is renamed to `lag_col`. Rows in the last period (no successor)
# are dropped, as are rows outside the pairs.
lag_by_period <- function(dt, pairs, group_cols = "scenario", year_col = "year",
                          value_col, lag_col) {
  lag <- copy(dt)
  setnames(lag, c(year_col, value_col), c(".yr_lag", lag_col))
  lag[, .yr_lag := as.integer(.yr_lag)]
  map <- pairs[, c(group_cols, "year_lag", "year"), with = FALSE]
  lag <- merge(lag, map, by.x = c(group_cols, ".yr_lag"), by.y = c(group_cols, "year_lag"))
  lag[, .yr_lag := NULL]
  if (year_col != "year") setnames(lag, "year", year_col)
  lag[]
}

# =========================================================================
# Scenario resolution
# =========================================================================
# Exact match first. Otherwise a partial match (either direction, e.g. a run
# prefix) is accepted only when it is unique: several candidates used to be
# resolved by row order (`[1]`), which could harmonize against the wrong scenario.
resolve_scenario <- function(target_scen, avail_scens, label = "") {
  if (target_scen %in% avail_scens) return(target_scen)
  matched <- avail_scens[vapply(avail_scens, function(s)
    grepl(target_scen, s, fixed = TRUE) || grepl(s, target_scen, fixed = TRUE), logical(1))]
  tag <- if (nzchar(label)) paste0(" [", label, "]") else ""
  if (length(matched) == 1) {
    message("  Note", tag, ": exact scenario '", target_scen, "' not found. Using unique partial match: '", matched, "'")
    return(matched)
  }
  if (length(matched) > 1) {
    stop("Scenario '", target_scen, "'", tag, " is ambiguous: partial matches ", paste(matched, collapse = ", "),
         ". Set the exact name in config.R.", call. = FALSE)
  }
  stop("Scenario '", target_scen, "'", tag, " not found in targets (exact or partial). Available: ",
       paste(avail_scens, collapse = ", "), call. = FALSE)
}

# =========================================================================
# Excessive area gaps
# =========================================================================
# The existing start-map and harmonization checks report small area differences
# as messages/warnings: some rounding noise is expected there. This second tier
# stops the run only when the relative gap is excessive. Threshold:
# current_model_cfg$area_gap_stop_rel (default 0.01, i.e. 1% of the expected total).
stop_on_excessive_area_gap <- function(got, expected, label) {
  thr <- if (exists("current_model_cfg") && !is.null(current_model_cfg$area_gap_stop_rel)) current_model_cfg$area_gap_stop_rel else 0.01
  if (!is.finite(got) || !is.finite(expected) || expected <= 0) {
    stop(sprintf("%s: area check not computable (got %s, expected %s).", label, format(got), format(expected)), call. = FALSE)
  }
  rel <- abs(got - expected) / expected
  if (rel > thr) {
    stop(sprintf("%s: area differs by %.4f km2 (%.3f%% of %.2f km2), above the stop threshold of %.3f%% (area_gap_stop_rel).",
                 label, abs(got - expected), 100 * rel, expected, 100 * thr), call. = FALSE)
  }
  invisible(rel)
}

# =========================================================================
# Harmonization coverage report
# =========================================================================
# Which zones were harmonized, and what happened to the others. `skipped_status`
# names the script's handling of skipped zones ("passthrough" = kept unharmonized
# in the map, "dropped" = removed from the harmonized map). Writes
# <paths$temp>/<label>_harmonization_coverage.csv and logs the area affected.
report_harmonization_coverage <- function(start_area, target_area, harmonized, skipped_status, label,
                                          no_target_status = paste0("no_target_", skipped_status)) {
  cov <- merge(start_area, target_area, by = "Ns_reg", all = TRUE)
  cov[, ratio := fifelse(!is.na(tg_area) & tg_area > 0, sm_area / tg_area, NA_real_)]
  cov[, status := fifelse(Ns_reg %in% harmonized, "harmonized",
                   fifelse(is.na(tg_area), no_target_status,
                   fifelse(is.na(sm_area), "no_start_map", skipped_status)))]
  setorder(cov, status, ratio)
  out_dir <- if (exists("paths") && !is.null(paths$temp)) paths$temp else "temp"
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(out_dir, paste0(gsub("[^A-Za-z0-9]+", "_", label), "_harmonization_coverage.csv"))
  fwrite(cov, out)
  skipped <- cov[status != "harmonized"]
  if (nrow(skipped) > 0) {
    message(sprintf(">>> Harmonization coverage [%s]: %d of %d zones not harmonized (%s), %.1f km2 of start-map area (%.2f%%). Details: %s",
                    label, nrow(skipped), nrow(cov),
                    paste(sprintf("%s: %d", names(table(skipped$status)), as.integer(table(skipped$status))), collapse = ", "),
                    sum(skipped$sm_area, na.rm = TRUE),
                    100 * sum(skipped$sm_area, na.rm = TRUE) / max(sum(cov$sm_area, na.rm = TRUE), 1e-12), out))
  } else {
    message(sprintf(">>> Harmonization coverage [%s]: all %d zones harmonized. Details: %s", label, nrow(cov), out))
  }
  invisible(cov)
}

# =========================================================================
# Outside-model area: exogenous `<primary>_other` classes
# =========================================================================
# Land the macro model does not cover keeps its observed start-map land cover,
# relabelled from its endogenous class to the exogenous counterpart of its primary
# class (spatial rulebook, Principle 5, item 1). `_other` classes get no targets,
# so the downscaling carries them unchanged in every period.

# Primary class: the first token of the class name ("Cropland_arable_barley" ->
# "Cropland", "CRP|ARA" -> "CRP", "Forests_SR" -> "Forests"). `primary_rules` is a
# named character vector of regex -> primary overrides, applied in order
# (e.g. c("^Plantation_shortrotation$" = "Forests")).
outside_model_primary <- function(classes, primary_rules = NULL) {
  prim <- sub("[_|].*$", "", classes)
  if (length(primary_rules)) {
    for (pat in names(primary_rules)) prim[grepl(pat, classes)] <- primary_rules[[pat]]
  }
  prim
}

# Relabel the endogenous classes of the selected rows to `<primary><suffix>`.
#   rows        logical vector (or row indices) of the land outside the model
#   endogenous  class names the model determines (e.g. classes in its targets), or
#   endogenous_pattern  a regex for them; if both are NULL every class is treated
#               as endogenous. Exogenous classes (water, urban, ...) are left alone.
#   clear_cols  sub-class columns to blank on the selected rows (e.g. management)
# Classes already ending in `suffix` are left unchanged. Modifies `dt` by reference.
relabel_outside_model <- function(dt, class_col, rows, endogenous = NULL, endogenous_pattern = NULL,
                                  primary_rules = NULL, suffix = "_other", clear_cols = NULL, label = "") {
  if (is.numeric(rows)) { idx <- rows; rows <- logical(nrow(dt)); rows[idx] <- TRUE }
  rows[is.na(rows)] <- FALSE
  if (!any(rows)) return(invisible(dt))
  cls <- as.character(dt[[class_col]])
  endo <- if (!is.null(endogenous)) cls %in% endogenous else if (!is.null(endogenous_pattern)) grepl(endogenous_pattern, cls) else rep(TRUE, length(cls))
  hit <- which(rows & endo & !is.na(cls) & nzchar(cls) & !endsWith(cls, suffix))
  if (length(hit)) set(dt, i = hit, j = class_col, value = paste0(outside_model_primary(cls[hit], primary_rules), suffix))
  for (cc in intersect(clear_cols, names(dt))) set(dt, i = which(rows), j = cc, value = "")
  if (nzchar(label)) {
    message(sprintf(">>> Outside-model relabeling [%s]: %d of %d selected rows relabelled to *%s.",
                    label, length(hit), sum(rows), suffix))
  }
  invisible(dt)
}

# Start-map rows of zones that are not harmonized, prepared for the harmonized map.
#   coverage   the table returned by report_harmonization_coverage()
#   handling   for zones skipped by the coverage screening that DO have targets:
#              "keep"  = keep their endogenous classes unharmonized (downscaled as is)
#              "other" = relabel to `_other` (exogenous; e.g. slivers of countries at
#                        the grid edge, or residuals of inexact border mapping)
# Zones without any targets are always relabelled to `_other`.
keep_outside_model_zones <- function(start_rows, coverage, class_col, endogenous,
                                     handling = c("keep", "other"), primary_rules = NULL, label = "") {
  handling <- match.arg(handling)
  rows <- copy(start_rows)
  if (!nrow(rows)) return(rows)
  no_target <- coverage[is.na(tg_area), Ns_reg]
  skipped   <- coverage[status != "harmonized" & !is.na(tg_area), Ns_reg]
  sel <- rows$Ns_reg %in% no_target | (handling == "other" & rows$Ns_reg %in% skipped)
  relabel_outside_model(rows, class_col, sel, endogenous = endogenous, primary_rules = primary_rules, label = label)
  rows[]
}

# Zones whose whole start-map land cover is kept as exogenous `_other` land: zones
# without targets, plus zones below the coverage screen when handling == "other".
# Saved as the intermediate "outside_model_zones" (always written, possibly empty, so a
# re-run never reads a stale list) and read by the downscaling driver, which drops
# their targets so the engine writes them unchanged as static regions.
record_outside_model_zones <- function(coverage, handling = c("keep", "other"), label = "") {
  handling <- match.arg(handling)
  zones <- coverage[is.na(tg_area) | (handling == "other" & status != "harmonized"), .(Ns_reg, sm_area, tg_area, status)]
  zones <- zones[!is.na(sm_area)]
  if (exists("save_intermediate")) save_intermediate(zones, "outside_model_zones")
  message(sprintf(">>> Outside-model zones%s: %d zone(s) kept as exogenous _other land (%.1f km2).",
                  if (nzchar(label)) paste0(" [", label, "]") else "", nrow(zones), sum(zones$sm_area, na.rm = TRUE)))
  invisible(zones)
}

# Drop targets for zones kept as exogenous `_other` land (record_outside_model_zones()).
# Region keys are matched case-insensitively (AgMIP country codes differ in casing
# between targets and start map).
drop_outside_model_targets <- function(targets, zones, region_col = "Ns_reg", value_col = "value") {
  if (is.null(zones) || !nrow(zones) || !region_col %in% names(targets)) return(targets)
  hit <- toupper(targets[[region_col]]) %in% toupper(zones$Ns_reg)
  if (any(hit)) {
    message(sprintf(">>> Dropping targets of %d outside-model zone(s): %d target row(s), %.1f km2 of transitions. They are written as static regions.",
                    uniqueN(targets[[region_col]][hit]), sum(hit),
                    if (value_col %in% names(targets)) sum(abs(targets[[value_col]][hit]), na.rm = TRUE) else NA_real_))
  }
  targets[!hit]
}

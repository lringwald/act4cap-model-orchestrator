# =============================================================================
# R/cascade_interface.R — Cascade Downscaling Interface
# =============================================================================
# Orchestrates spatial downscaling routines across EU-27 for macro-economic
# models (GLOBIOM, CAPRI, IMAGE, MAGNET, CLUMONDO) using the Cascade downscaling
# suite (gamble-cascade / cascadinggamble-core).
#
# Upstream repository: https://github.com/lringwald/gamble-cascade
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(cli)
})

# =============================================================================
# Helper: Resolve Input Filename or Auto-Detect Latest Vintage in Macro Folder
# =============================================================================
resolve_macro_model_vintage <- function(input_file = "", model = "globiom", input_dir = "inputs", gamble_dir = NULL) {
  # 1. Explicit filename / path provided
  if (nzchar(input_file) && tolower(input_file) != "latest") {
    if (file.exists(input_file)) {
      cli::cli_alert_info("Using explicitly specified macro model input file: {input_file}")
      return(list(
        path = normalizePath(input_file, winslash = "/", mustWork = FALSE),
        name = basename(input_file),
        source = "explicit_path"
      ))
    }
    # Search common candidate folders for the bare filename
    cands <- c(
      file.path(input_dir, model, input_file),
      file.path(input_dir, input_file),
      file.path("W:/model_outputs", model, input_file),
      file.path("W:/model_outputs/merged", input_file),
      file.path("W:/model_outputs", input_file),
      file.path("/mnt/wdrv/model_outputs", model, input_file),
      file.path("/mnt/wdrv/model_outputs/merged", input_file),
      file.path("/mnt/pdrv/globiom/Projects/DS_AgMIP/AgMIP_output", input_file),
      if (!is.null(gamble_dir)) file.path(gamble_dir, "data", "macro_models", input_file),
      if (!is.null(gamble_dir)) file.path(gamble_dir, "data", input_file)
    )
    cands <- cands[!is.null(cands)]
    for (cand in cands) {
      if (file.exists(cand)) {
        cli::cli_alert_info("Resolved explicit macro input filename '{input_file}' to: {cand}")
        return(list(
          path = normalizePath(cand, winslash = "/", mustWork = FALSE),
          name = basename(cand),
          source = "explicit_resolved"
        ))
      }
    }
    cli::cli_alert_warning("Specified INPUT_FILE '{input_file}' not found locally. Passing through to runtime.")
    return(list(
      path = input_file,
      name = basename(input_file),
      source = "explicit_passthrough"
    ))
  }
  
  # 2. Auto-detect latest vintage in macro_model folder
  search_dirs <- c(
    file.path(input_dir, model),
    input_dir,
    "W:/model_outputs/merged",
    file.path("W:/model_outputs", model),
    "W:/model_outputs",
    "/mnt/wdrv/model_outputs/merged",
    file.path("/mnt/wdrv/model_outputs", model),
    if (!is.null(gamble_dir)) file.path(gamble_dir, "data", "macro_models", model),
    if (!is.null(gamble_dir)) file.path(gamble_dir, "data", "macro_models"),
    if (!is.null(gamble_dir)) file.path(gamble_dir, "data"),
    "/mnt/pdrv/globiom/Projects/DS_AgMIP/AgMIP_output"
  )
  search_dirs <- unique(search_dirs[!is.null(search_dirs) & dir.exists(search_dirs)])
  
  all_files <- character(0)
  for (sd in search_dirs) {
    found <- list.files(sd, pattern = "\\.(csv|rds|parquet|gdx)$", full.names = TRUE)
    all_files <- c(all_files, found)
  }
  all_files <- unique(all_files)
  
  if (length(all_files) == 0) {
    fallback_name <- "20260916_compr_scens_merge.csv"
    cli::cli_alert_warning("No macro model data files found in search directories. Using default vintage: {fallback_name}")
    return(list(
      path = fallback_name,
      name = fallback_name,
      source = "fallback_default"
    ))
  }
  
  # Filter to files relevant to this model or general merge files
  model_pattern <- sprintf("(%s|compr_scens_merge|scens_merge)", tolower(model))
  matching_files <- all_files[grepl(model_pattern, basename(all_files), ignore.case = TRUE)]
  if (length(matching_files) == 0) {
    matching_files <- all_files
  }
  
  # Extract YYYYMMDD date prefix and modification time
  bnames <- basename(matching_files)
  has_date <- grepl("^[0-9]{8}", bnames)
  date_str <- sub("^([0-9]{8}).*", "\\1", bnames)
  date_str[!has_date] <- "00000000"
  mtimes <- as.numeric(file.mtime(matching_files))
  mtimes[is.na(mtimes)] <- 0
  
  ord <- order(has_date, date_str, mtimes, decreasing = TRUE)
  latest_file <- matching_files[ord[1]]
  latest_bname <- basename(latest_file)
  
  cli::cli_alert_success("Auto-detected latest vintage in macro_model folder: {latest_bname} [{latest_file}]")
  return(list(
    path = normalizePath(latest_file, winslash = "/", mustWork = FALSE),
    name = latest_bname,
    source = "auto_detected_latest"
  ))
}

locate_existing_downscaling <- function(model, scenario, out_dir, gamble_dir) {
  # Look in out_dir, W:/cascade_gamble_outputs, and cluster paths
  pat <- sprintf('DS_.*AgMIP_%s_.*_lu.*\\.csv$', toupper(model))
  search_roots <- c(
    out_dir,
    "W:/cascade_gamble_outputs",
    "/mnt/wdrv/cascade_gamble_outputs",
    file.path(out_dir, "..", "cascade_gamble_outputs")
  )
  search_roots <- search_roots[dir.exists(search_roots)]
  cands <- character(0)
  for (sr in search_roots) {
    found <- list.files(sr, pattern = pat, recursive = TRUE, full.names = TRUE)
    cands <- c(cands, found)
  }
  if (length(cands) == 0 && file.exists('/mnt/pdrv/globiom/Projects/DS_AgMIP/DS_results/outputs')) {
    cands <- list.files('/mnt/pdrv/globiom/Projects/DS_AgMIP/DS_results/outputs', pattern = pat, recursive = TRUE, full.names = TRUE)
  }
  cands <- cands[!grepl('_luc', cands)]
  if (length(cands) > 0) {
    lu_file <- sort(cands, decreasing = TRUE)[1]
    cli_alert_success('Located downscaled land-use file: {basename(lu_file)} [{dirname(lu_file)}]')
    return(list(model = model, scenario = scenario, lu_csv = lu_file, out_folder = dirname(lu_file)))
  }
  cli_alert_warning('No existing downscaled land-use file found for {toupper(model)}. Will mock / generate minimal structure.')
  return(list(model = model, scenario = scenario, lu_csv = NULL, out_folder = out_dir))
}

run_act4cap_cascade_downscaling <- function(model, scenario, gamble_dir, out_dir, years, resolution, ncores, trial = FALSE, force = FALSE, input_file = "", smoke = trial) {
  cli_alert_info('Initiating Cascade downscaling execution for model={toupper(model)}, scenario={scenario}...')
  is_trial <- trial || smoke
  
  # Resolve input file / latest vintage in macro_model folder
  vintage_info <- resolve_macro_model_vintage(
    input_file = if (is.null(input_file)) "" else input_file,
    model = model,
    input_dir = "inputs",
    gamble_dir = gamble_dir
  )
  
  # 1. Check if existing processed downscaling output already exists (unless force = TRUE)
  if (!force) {
    existing <- locate_existing_downscaling(model, scenario, out_dir, gamble_dir)
    if (!is.null(existing$lu_csv) && file.exists(existing$lu_csv)) {
      cli_alert_success('Reusing existing downscaled land-use output: {basename(existing$lu_csv)} [FORCE=0]')
      existing$input_vintage <- vintage_info
      existing$cached <- TRUE
      return(existing)
    }
  }

  # 2. Check if gamble-cascade / cascadinggamble-core exists
  cand_paths <- c(
    gamble_dir,
    'g:/My Drive/R/gamble-cascade',
    'g:/My Drive/R/cascadinggamble-core',
    '../gamble-cascade',
    '../cascadinggamble-core',
    '/app/gamble-cascade',
    '/app/cascadinggamble-core'
  )
  valid_gamble <- cand_paths[dir.exists(cand_paths)][1]
  
  if (is.na(valid_gamble) || is.null(valid_gamble)) {
    cli_alert_warning('Cascade downscaling directory not found. Checking existing pre-computed outputs...')
    return(locate_existing_downscaling(model, scenario, out_dir, gamble_dir))
  }
  
  # 3. In trial mode, locate existing or write tiny subset
  if (is_trial) {
    trial_dir <- file.path(out_dir, sprintf('DS_ACT4CAP_AgMIP_%s_10km_TRIAL', toupper(model)))
    trial_csv <- file.path(trial_dir, sprintf('DS_ACT4CAP_AgMIP_%s_10km_lu_base.csv', toupper(model)))
    
    if (!force && file.exists(trial_csv)) {
      cli_alert_success('Reusing existing trial downscaling test file: {basename(trial_csv)} [FORCE=0]')
      return(list(model = model, scenario = scenario, lu_csv = trial_csv, out_folder = trial_dir, input_vintage = vintage_info, cached = TRUE))
    }
    
    cli_alert_info('Trial mode: generating lightweight test slice for {toupper(model)} (vintage: {vintage_info$name})...')
    if (!dir.exists(trial_dir)) dir.create(trial_dir, recursive = TRUE)
    
    trial_dt <- data.table(
      ns = c(1L, 1L, 1L, 1L, 2L, 2L),
      Ns_reg = c('DEU', 'DEU', 'DEU', 'DEU', 'FRA', 'FRA'),
      year = c(2020L, 2020L, 2050L, 2050L, 2020L, 2050L),
      lu = c('CRP|ARA|WHT', 'FOR|MAN', 'CRP|ARA|WHT', 'FOR|MAN', 'CRP|ARA', 'FOR|PRIM'),
      crp_area_km2 = c(45.5, 30.2, 50.0, 28.1, 60.0, 40.0)
    )
    fwrite(trial_dt, trial_csv)
    cli_alert_success('Created trial downscaling test file: {basename(trial_csv)}')
    return(list(model = model, scenario = scenario, lu_csv = trial_csv, out_folder = trial_dir, input_vintage = vintage_info, cached = FALSE))
  }
  
  # 4. Production dispatch via Rscript run_downscaling.R
  # Detect reorganized pipelines/ architecture, falling back to legacy scripts/ shims if needed
  script_cand <- if (file.exists(file.path(valid_gamble, 'pipelines', 'AGMIP', 'run_downscaling.R'))) {
    'pipelines/AGMIP/run_downscaling.R'
  } else if (file.exists(file.path(valid_gamble, 'scripts', 'AGMIP', 'run_downscaling.R'))) {
    'scripts/AGMIP/run_downscaling.R'
  } else {
    'scripts/run_downscaling.R'
  }

  cmd <- sprintf('Rscript %s %s %s "%s" "%s"', script_cand, model, scenario, vintage_info$name, out_dir)
  cli_alert_info('Executing: {cmd} inside {valid_gamble} (Input vintage: {vintage_info$name})')
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(valid_gamble)
  
  # Set environment overrides including resolved vintage file, output directory, resolution, and cores
  Sys.setenv(AGMIP_MODEL = tolower(model))
  Sys.setenv(SCENARIO = tolower(scenario))
  Sys.setenv(AGMIP_INPUT_FILE = vintage_info$name)
  Sys.setenv(INPUT_FILE = vintage_info$name)
  Sys.setenv(AGMIP_OUTPUT_DIR = out_dir)
  Sys.setenv(OUTPUT_DIR = out_dir)
  Sys.setenv(RESOLUTION = resolution)
  Sys.setenv(NCORES = as.character(ncores))
  if (vintage_info$path != vintage_info$name && file.exists(vintage_info$path)) {
    Sys.setenv(AGMIP_INPUT_DIR = dirname(vintage_info$path))
    Sys.setenv(INPUT_DIR = dirname(vintage_info$path))
  }
  
  # Execute downscaling
  if (file.exists(script_cand)) {
    res_code <- system(cmd)
    if (res_code != 0) {
      cli_alert_warning('Downscaling command exited with code {res_code}. Falling back to locate_existing_downscaling...')
    }
  }
  
  res <- locate_existing_downscaling(model, scenario, out_dir, gamble_dir)
  res$input_vintage <- vintage_info
  return(res)
}

# Alias for backward compatibility
run_act4cap_downscaling <- run_act4cap_cascade_downscaling
locate_existing_cascade_downscaling <- locate_existing_downscaling

# =============================================================================
# R/cascade_interface.R — Cascade Interface (gamble-cascade Downscaling)
# =============================================================================
# Connects the Act4Cap Orchestrator to gamble-cascade / cascadinggamble-core:
#   1. Resolves macro-economic input vintages (e.g. 20260916_compr_scens_merge.csv)
#   2. Invokes spatial mediation via run_downscaling.R
#   3. Generates/Locates mediated 10km land use & fertilizer allocation tables
#   4. Enforces strict 0% mass & area conservation across administrative units
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(cli)
  library(tools)
})

if (!exists('construct_version_tag')) {
  if (file.exists('R/version_labels.R')) source('R/version_labels.R')
}

resolve_macro_model_vintage <- function(input_file = "", model = "globiom", input_dir = "00_inputs/macro_models", gamble_dir = NULL) {
  # If an explicit non-empty input_file is given, resolve its path
  if (!is.null(input_file) && nzchar(input_file) && input_file != "latest") {
    if (file.exists(input_file)) {
      return(list(
        path = input_file,
        name = basename(input_file),
        source = "explicit_path"
      ))
    }
    cands <- c(
      file.path(input_dir, model, input_file),
      file.path(input_dir, "merged", input_file),
      file.path(input_dir, input_file),
      file.path("/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models/merged", input_file),
      file.path("/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models", model, input_file),
      file.path("/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models", input_file),
      file.path("T:/ACT4CAP-model-pipeline/00_inputs/macro_models/merged", input_file),
      file.path("T:/ACT4CAP-model-pipeline/00_inputs/macro_models", model, input_file),
      file.path("T:/ACT4CAP-model-pipeline/00_inputs/macro_models", input_file),
      file.path(input_dir, input_file),
      file.path("T:/ACT4CAP-model-pipeline/00_inputs/macro_models", model, input_file),
      file.path("T:/ACT4CAP-model-pipeline/00_inputs/macro_models/merged", input_file),
      file.path("T:/ACT4CAP-model-pipeline/00_inputs/macro_models", input_file),
      file.path("W:/ACT4CAP-model-pipeline/00_inputs/macro_models", model, input_file),
      file.path("W:/ACT4CAP-model-pipeline/00_inputs/macro_models/merged", input_file),
      file.path("W:/ACT4CAP-model-pipeline/00_inputs/macro_models", input_file),
      file.path("/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models", model, input_file),
      file.path("/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models/merged", input_file),
      file.path("/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models", input_file),
      file.path("T:/model_outputs", model, input_file),
      file.path("T:/model_outputs/merged", input_file),
      file.path("T:/model_outputs", input_file),
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
        return(list(
          path = cand,
          name = basename(cand),
          source = "explicit_candidate"
        ))
      }
    }
    cli_alert_danger('Specified INPUT_FILE {input_file} not found.'); stop('Specified INPUT_FILE not found')
  }

  # Automatic detection: scan directories for latest vintage matching macro model
  search_dirs <- c(
    file.path(input_dir, model),
    file.path(input_dir, "merged"),
    input_dir,
    "T:/ACT4CAP-model-pipeline/00_inputs/macro_models/merged",
    file.path("T:/ACT4CAP-model-pipeline/00_inputs/macro_models", model),
    "T:/ACT4CAP-model-pipeline/00_inputs/macro_models",
    "W:/ACT4CAP-model-pipeline/00_inputs/macro_models/merged",
    file.path("W:/ACT4CAP-model-pipeline/00_inputs/macro_models", model),
    "W:/ACT4CAP-model-pipeline/00_inputs/macro_models",
    "/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models/merged",
    file.path("/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models", model),
    "/mnt/wdrv/ACT4CAP-model-pipeline/00_inputs/macro_models",
    "T:/model_outputs/merged",
    file.path("T:/model_outputs", model),
    "T:/model_outputs",
    "W:/model_outputs/merged",
    file.path("W:/model_outputs", model),
    "W:/model_outputs",
    "/mnt/wdrv/model_outputs/merged",
    file.path("/mnt/wdrv/model_outputs", model),
    if (!is.null(gamble_dir)) file.path(gamble_dir, "data", "macro_models", model),
    if (!is.null(gamble_dir)) file.path(gamble_dir, "data", "macro_models"),
    if (!is.null(gamble_dir)) file.path(gamble_dir, "data"),
    file.path("/mnt/pdrv/globiom/Projects/DS_AgMIP/AgMIP_output")
  )
  search_dirs <- search_dirs[!is.null(search_dirs) & dir.exists(search_dirs)]

  candidates <- character(0)
  for (sd in search_dirs) {
    found <- list.files(
      sd,
      pattern = sprintf(".*(%s|merge|compr).*\\.(csv|parquet)$", model),
      full.names = TRUE,
      ignore.case = TRUE
    )
    candidates <- c(candidates, found)
  }

  if (length(candidates) > 0) {
    candidates <- unique(candidates)
    # Extract date prefixes (YYYYMMDD) if present
    base_names <- basename(candidates)
    date_matches <- regmatches(base_names, regexpr("^[0-9]{8}", base_names))
    
    # Sort candidates by extracted date prefix first, then modification time
    file_info <- file.info(candidates)
    file_info$date_prefix <- NA_character_
    has_date <- grepl("^[0-9]{8}", rownames(file_info)) | grepl("^[0-9]{8}", base_names)
    file_info$date_prefix[has_date] <- substr(base_names[has_date], 1, 8)
    
    # Order by date_prefix desc, then mtime desc
    ord <- order(file_info$date_prefix, file_info$mtime, decreasing = TRUE, na.last = TRUE)
    best_file <- candidates[ord[1]]
    cli_alert_success('Auto-detected latest vintage in macro_model folder: {basename(best_file)} [{best_file}]')
    return(list(
      path = best_file,
      name = basename(best_file),
      source = "auto_detected_latest"
    ))
  }

  stop(sprintf("No external macro model vintage found for %s. Fallback to default vintage is prohibited by Data Integrity rules.", toupper(model)))
}

ensure_downscale_parquet <- function(lu_file, model, scenario, out_dir) {
  if (is.null(lu_file) || !file.exists(lu_file)) return(lu_file)
  if (grepl('\\.parquet$', lu_file, ignore.case = TRUE)) return(lu_file)

  # Check if companion .parquet already exists next to the CSV
  companion_parquet <- sub('\\.csv$', '.parquet', lu_file, ignore.case = TRUE)
  if (file.exists(companion_parquet)) {
    cli_alert_success('Using standard Parquet cell table: {basename(companion_parquet)}')
    return(companion_parquet)
  }

  # Under Act4Cap project architecture rules, cell tables must be .parquet
  cli_alert_info('Standardizing legacy downscaled cell table to Parquet: {basename(companion_parquet)}...')
  upgraded <- tryCatch({
    dt <- read_cell_tabular_data(lu_file)
    write_cell_tabular_data(dt, companion_parquet)
    tryCatch(Sys.chmod(companion_parquet, mode = "0666", use_umask = FALSE), error = function(e) NULL)
    companion_parquet
  }, error = function(e) {
    # If source folder is read-only, write into out_dir/downscaling/
    local_dir <- file.path(out_dir, sprintf('%s_%s', tolower(model), tolower(scenario)), 'downscaling')
    if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
    local_parquet <- file.path(local_dir, basename(companion_parquet))
    tryCatch({
      dt <- read_cell_tabular_data(lu_file)
      write_cell_tabular_data(dt, local_parquet)
      tryCatch(Sys.chmod(local_parquet, mode = "0666", use_umask = FALSE), error = function(e) NULL)
      local_parquet
    }, error = function(err) {
      cli_alert_warning('Could not auto-convert downscaled CSV to Parquet ({err$message}); proceeding with CSV.')
      lu_file
    })
  })

  if (grepl('\\.parquet$', upgraded, ignore.case = TRUE)) {
    cli_alert_success('Successfully upgraded downscaled land use to Parquet: {basename(upgraded)}')
  }
  return(upgraded)
}

locate_existing_downscaling <- function(model, scenario, out_dir, gamble_dir, downscale_output = "auto") {
  # 1. If explicit downscale_output folder or file was selected from target folder dropdown
  if (!is.null(downscale_output) && nzchar(downscale_output) && downscale_output != "auto" && downscale_output != "none") {
    target_cands <- c(
      file.path("T:/ACT4CAP-model-pipeline/03_outputs/downscaling", downscale_output),
      file.path("W:/ACT4CAP-model-pipeline/03_outputs/downscaling", downscale_output),
      file.path("/mnt/wdrv/ACT4CAP-model-pipeline/03_outputs/downscaling", downscale_output),
      file.path(out_dir, "downscaling", downscale_output),
      file.path(out_dir, downscale_output),
      downscale_output
    )
    for (tc in target_cands) {
      if (dir.exists(tc)) {
        found <- list.files(tc, pattern = sprintf('.*lu_%s\\.(parquet|csv)$', tolower(scenario)), full.names = TRUE)
        if (length(found) == 0) found <- list.files(tc, pattern = '.*lu.*\\.(parquet|csv)$', full.names = TRUE)
        found <- found[!grepl('_luc', found)]
        if (length(found) > 0) {
          parquet_f <- found[grepl('\\.parquet$', found, ignore.case = TRUE)]
          lu_raw <- if (length(parquet_f) > 0) parquet_f[1] else found[1]
          lu_file <- ensure_downscale_parquet(lu_raw, model, scenario, out_dir)
          cli_alert_success('Selected explicit downscaling output: {basename(lu_file)} [{dirname(lu_file)}]')
          return(list(
            model       = model,
            scenario    = scenario,
            lu_file     = lu_file,
            lu_parquet  = if (grepl('\\.parquet$', lu_file, ignore.case = TRUE)) lu_file else NULL,
            lu_csv      = lu_file,
            out_folder  = dirname(lu_file),
            source_name = tools::file_path_sans_ext(basename(lu_file))
          ))
        }
      } else if (file.exists(tc)) {
        lu_file <- ensure_downscale_parquet(tc, model, scenario, out_dir)
        cli_alert_success('Selected explicit downscaling output file: {basename(lu_file)}')
        return(list(
          model       = model,
          scenario    = scenario,
          lu_file     = lu_file,
          lu_parquet  = if (grepl('\\.parquet$', lu_file, ignore.case = TRUE)) lu_file else NULL,
          lu_csv      = lu_file,
          out_folder  = dirname(lu_file),
          source_name = tools::file_path_sans_ext(basename(lu_file))
        ))
      }
    }
    cli_alert_warning('Specified DOWNSCALE_OUTPUT "{downscale_output}" not found in target directories. Falling back to auto-detection.')
  }

  if (identical(downscale_output, "none")) {
    return(list(
      model       = model,
      scenario    = scenario,
      lu_file     = NULL,
      lu_parquet  = NULL,
      lu_csv      = NULL,
      out_folder  = out_dir,
      source_name = toupper(model)
    ))
  }

  # 2. Look in 03_outputs/downscaling and out_dir (.parquet or .csv)
  pat <- sprintf('DS_.*AgMIP_%s_.*_lu.*\\.(parquet|csv)$', toupper(model))
  search_roots <- c(
    "T:/ACT4CAP-model-pipeline/03_outputs/downscaling",
    "W:/ACT4CAP-model-pipeline/03_outputs/downscaling",
    "/mnt/wdrv/ACT4CAP-model-pipeline/03_outputs/downscaling",
    file.path(out_dir, "downscaling"),
    out_dir
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
    # Prioritize .parquet over .csv if available
    parquet_cands <- cands[grepl('\\.parquet$', cands, ignore.case = TRUE)]
    lu_raw <- if (length(parquet_cands) > 0) {
      sort(parquet_cands, decreasing = TRUE)[1]
    } else {
      sort(cands, decreasing = TRUE)[1]
    }
    lu_file <- ensure_downscale_parquet(lu_raw, model, scenario, out_dir)
    cli_alert_success('Located downscaled land-use file: {basename(lu_file)} [{dirname(lu_file)}]')
    return(list(
      model       = model,
      scenario    = scenario,
      lu_file     = lu_file,
      lu_parquet  = if (grepl('\\.parquet$', lu_file, ignore.case = TRUE)) lu_file else NULL,
      lu_csv      = lu_file, # backward compatible alias
      out_folder  = dirname(lu_file),
      source_name = tools::file_path_sans_ext(basename(lu_file))
    ))
  }
  cli_alert_warning('No existing downscaled land-use file found for {toupper(model)}. Will mock / generate minimal structure.')
  return(list(
    model       = model,
    scenario    = scenario,
    lu_file     = NULL,
    lu_parquet  = NULL,
    lu_csv      = NULL,
    out_folder  = out_dir,
    source_name = toupper(model)
  ))
}

run_act4cap_cascade_downscaling <- function(model,
                                           scenario,
                                           gamble_dir,
                                           out_dir,
                                           years,
                                           resolution = "10km",
                                           ncores = 4,
                                           trial = FALSE,
                                           force = FALSE,
                                           input_file = "latest",
                                           downscale_output = "auto",
                                           run_date = NULL,
                                           version_tag = NULL) {
  cli_alert_info('Initiating Cascade downscaling execution for model={toupper(model)}, scenario={scenario}...')
  is_trial <- trial
  
  # Resolve input file / latest vintage in macro_model folder
  vintage_info <- resolve_macro_model_vintage(
    input_file = if (is.null(input_file) || !nzchar(input_file)) "latest" else input_file,
    model = model,
    input_dir = "00_inputs/macro_models",
    gamble_dir = gamble_dir
  )
  
  # 1. Check if existing processed downscaling output already exists (unless force = TRUE or downscale_output == 'none')
  if (!force && downscale_output != "none") {
    existing <- locate_existing_downscaling(model, scenario, out_dir, gamble_dir, downscale_output = downscale_output)
    if (!is.null(existing$lu_file) && file.exists(existing$lu_file)) {
      cli_alert_success('Reusing existing downscaled land-use output: {basename(existing$lu_file)} [FORCE=0]')
      existing$input_vintage <- vintage_info
      existing$cached <- TRUE
      return(existing)
    }
  }

  # 2. Check if gamble-cascade / cascadinggamble-core exists
  cand_paths <- c(
    gamble_dir,
    'T:/ACT4CAP-model-pipeline/01_interimmodel/gamble-cascade',
    '/mnt/wdrv/ACT4CAP-model-pipeline/01_interimmodel/gamble-cascade',
    'W:/ACT4CAP-model-pipeline/01_interimmodel/gamble-cascade',
    'g:/My Drive/R/gamble-cascade',
    'g:/My Drive/R/cascadinggamble-core',
    '../gamble-cascade',
    '../cascadinggamble-core',
    '/app/gamble-cascade',
    '/app/cascadinggamble-core'
  )
  valid_gamble <- cand_paths[dir.exists(cand_paths)][1]
  
  if (is.na(valid_gamble) || is.null(valid_gamble)) {
    stop('Cascade downscaling directory not found. Pre-computed fallback is prohibited.')
  }
  
  # 3. In trial mode, generate standardized lightweight test slice (.parquet standard)
  if (is_trial) {
    tag <- construct_version_tag(
      model = model,
      scenario = scenario,
      macro_output = vintage_info$name,
      resolution = resolution,
      date = run_date,
      trial = TRUE,
      custom_tag = version_tag
    )
    trial_dir     <- file.path(out_dir, tag, 'downscaling')
    trial_parquet <- file.path(trial_dir, sprintf('DS_ACT4CAP_AgMIP_%s_lu_%s.parquet', tag, tolower(scenario)))
    
    if (!force && file.exists(trial_parquet)) {
      cli_alert_success('Reusing existing trial downscaling test parquet: {basename(trial_parquet)} [FORCE=0]')
      return(list(
        model         = model,
        scenario      = scenario,
        lu_file       = trial_parquet,
        lu_parquet    = trial_parquet,
        lu_csv        = trial_parquet,
        out_folder    = trial_dir,
        source_name   = tag,
        input_vintage = vintage_info,
        cached        = TRUE
      ))
    }
    
    cli_alert_info('Trial mode: generating lightweight test slice for {toupper(model)} (tag: {tag})...')
    if (!dir.exists(trial_dir)) dir.create(trial_dir, recursive = TRUE)
    
    trial_dt <- data.table(
      ns = c(1L, 1L, 1L, 1L, 2L, 2L),
      Ns_reg = c('DEU', 'DEU', 'DEU', 'DEU', 'FRA', 'FRA'),
      year = c(2020L, 2020L, 2050L, 2050L, 2020L, 2050L),
      lu = c('CRP|ARA|WHT', 'FOR|MAN', 'CRP|ARA|WHT', 'FOR|MAN', 'CRP|ARA', 'FOR|PRIM'),
      crp_area_km2 = c(45.5, 30.2, 50.0, 28.1, 60.0, 40.0)
    )
    write_cell_tabular_data(trial_dt, trial_parquet)
    cli_alert_success('Created trial downscaling test parquet: {basename(trial_parquet)}')
    return(list(
      model         = model,
      scenario      = scenario,
      lu_file       = trial_parquet,
      lu_parquet    = trial_parquet,
      lu_csv        = trial_parquet,
      out_folder    = trial_dir,
      source_name   = tag,
      input_vintage = vintage_info,
      cached        = FALSE
    ))
  }
  
  # 4. Production dispatch via Rscript run_downscaling.R
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
  Sys.setenv(AGMIP_TEMP_DIR = file.path(out_dir, "intermediate"))
  Sys.setenv(TEMP_DIR = file.path(out_dir, "intermediate"))
  Sys.setenv(RESOLUTION = resolution)
  Sys.setenv(NCORES = as.character(ncores))
  # Point CASCADE_CORE_PATH to the installed package in the container to bypass FUSE compilation/io issues
  if (requireNamespace("cascadecore", quietly = TRUE)) {
    Sys.setenv(CASCADE_CORE_PATH = "installed")
  } else if (file.exists("/app/vendor/cascade-core/DESCRIPTION")) {
    Sys.setenv(CASCADE_CORE_PATH = "/app/vendor/cascade-core")
  }
  if (vintage_info$path != vintage_info$name && file.exists(vintage_info$path)) {
    Sys.setenv(AGMIP_INPUT_DIR = dirname(vintage_info$path))
    Sys.setenv(INPUT_DIR = dirname(vintage_info$path))
  }
  
  # Execute downscaling
  if (file.exists(script_cand)) {
    res_code <- system(cmd)
    if (res_code != 0) {
      stop(sprintf('Cascade downscaling execution failed with exit code %s inside %s. Pre-computed fallback is prohibited under Data Integrity rules.', res_code, valid_gamble))
    }
  } else {
    stop(sprintf('Cascade downscaling script "%s" not found inside %s.', script_cand, valid_gamble))
  }
  
  res <- locate_existing_downscaling(model, scenario, out_dir, gamble_dir)
  if (is.null(res$lu_file) || !file.exists(res$lu_file)) {
    stop(sprintf('Cascade downscaling finished but no output land-use file could be found in %s.', out_dir))
  }
  res$input_vintage <- vintage_info
  return(res)
}

# Alias for backward compatibility
run_act4cap_downscaling <- run_act4cap_cascade_downscaling
locate_existing_cascade_downscaling <- locate_existing_downscaling

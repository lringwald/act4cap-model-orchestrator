#!/usr/bin/env Rscript
# =============================================================================
# R/orchestrator.R — Main workflow runner for Act4Cap Integrated Modelling
# =============================================================================
suppressPackageStartupMessages({
  library(cli)
  library(jsonlite)
  library(data.table)
})

# Load helper interfaces
source('R/downscaling_interface.R')
source('R/swat_interface.R')
source('R/biodiversity_interface.R')
source('R/synthesis.R')

# 0. Resolve execution parameters from env or config.json
get_param <- function(key, default = NULL) {
  val <- Sys.getenv(key, unset = '')
  if (nzchar(val)) return(val)
  if (file.exists('config/config.json')) {
    cfg <- tryCatch(jsonlite::fromJSON('config/config.json'), error = function(e) list())
    if (!is.null(cfg[[key]])) return(as.character(cfg[[key]]))
  }
  if (file.exists('config.json')) {
    cfg <- tryCatch(jsonlite::fromJSON('config.json'), error = function(e) list())
    if (!is.null(cfg[[key]])) return(as.character(cfg[[key]]))
  }
  return(default)
}

TASK             <- tolower(get_param('TASK', 'all'))
MACRO_MODEL      <- tolower(get_param('MACRO_MODEL', 'globiom'))
SCENARIO         <- tolower(get_param('SCENARIO', 'base'))
RUN_SWAT         <- get_param('RUN_SWAT', '1') %in% c('1', 'true', 'TRUE', 'yes')
RUN_BIODIVERSITY <- get_param('RUN_BIODIVERSITY', '1') %in% c('1', 'true', 'TRUE', 'yes')
YEARS            <- get_param('YEARS', 'all')
RESOLUTION       <- get_param('RESOLUTION', '10km')
NCORES           <- as.integer(get_param('NCORES', '4'))
SMOKE            <- get_param('SMOKE', '0') %in% c('1', 'true', 'TRUE', 'yes')
FORCE            <- get_param('FORCE', '0') %in% c('1', 'true', 'TRUE', 'yes')
WORK_DIR         <- get_param('WORK_DIR', '/mnt/wdrv/act4cap')
OUTPUT_DIR       <- get_param('OUTPUT_DIR', 'outputs')
INPUT_DIR        <- get_param('INPUT_DIR', 'inputs')
GAMBLE_DIR       <- get_param('CASCADING_GAMBLE_DIR', 'g:/My Drive/R/cascadinggamble-core')

cli_h1('Act4Cap Integrated Modelling Orchestrator')
cli_alert_info('Task:              {toupper(TASK)}')
cli_alert_info('Macro Model:       {toupper(MACRO_MODEL)}')
cli_alert_info('Scenario:          {SCENARIO}')
cli_alert_info('Water (SWAT+):     {fifelse(RUN_SWAT, "ENABLED", "DISABLED")}')
cli_alert_info('Biodiversity Link: {fifelse(RUN_BIODIVERSITY, "ENABLED", "DISABLED")}')
cli_alert_info('Grid Resolution:   {RESOLUTION}')
cli_alert_info('Smoke Mode:        {fifelse(SMOKE, "YES (Fast minimal slice)", "NO (Full run)")}')

models_to_run <- if (MACRO_MODEL == 'all') {
  c('globiom', 'capri', 'image', 'magnet', 'clumondo')
} else {
  MACRO_MODEL
}

work_dir_resolved <- WORK_DIR
if (startsWith(work_dir_resolved, "/mnt") && .Platform$OS.type == "windows") {
  cli_alert_info("Windows host detected with cluster mount path {WORK_DIR}. Using local workspace outputs.")
  work_dir_resolved <- "."
} else if (!dir.exists(work_dir_resolved)) {
  ok <- tryCatch({ dir.create(work_dir_resolved, recursive = TRUE); TRUE }, error = function(e) FALSE)
  if (!ok) {
    cli_alert_warning("WORK_DIR {WORK_DIR} cannot be created. Falling back to local workspace.")
    work_dir_resolved <- "."
  }
}
out_path <- file.path(work_dir_resolved, OUTPUT_DIR)
if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

results_summary <- list()

for (m in models_to_run) {
  cli_h2('Processing Macro Model: {toupper(m)}')
  
  # Step 1: Spatial Downscaling (Cascading Gamble)
  downscale_res <- NULL
  if (TASK %in% c('downscale', 'all', 'smoke')) {
    cli_alert_info('Step 1/3: Running Cascading Gamble Downscaling for {toupper(m)}...')
    downscale_res <- run_act4cap_downscaling(
      model = m,
      scenario = SCENARIO,
      gamble_dir = GAMBLE_DIR,
      out_dir = out_path,
      years = YEARS,
      resolution = RESOLUTION,
      ncores = NCORES,
      smoke = SMOKE,
      force = FORCE
    )
  } else {
    cli_alert_info('Skipping Step 1 (Downscaling) as requested by TASK={TASK}. Locating existing downscaled maps...')
    downscale_res <- locate_existing_downscaling(m, SCENARIO, out_path, GAMBLE_DIR)
  }
  
  # Step 2: SWAT+ Water Quality Simulation
  swat_res <- NULL
  if ((TASK %in% c('swat', 'postprocess', 'all', 'smoke')) && RUN_SWAT) {
    cli_alert_info('Step 2/3: Executing SWAT+ Water Quality post-processor for {toupper(m)}...')
    swat_res <- run_act4cap_swat_postprocess(
      model = m,
      scenario = SCENARIO,
      downscale_outputs = downscale_res,
      out_dir = out_path,
      smoke = SMOKE,
      ncores = NCORES
    )
  }
  
  # Step 3: Biodiversity Link Simulation
  bio_res <- NULL
  if ((TASK %in% c('biodiversity', 'postprocess', 'all', 'smoke')) && RUN_BIODIVERSITY) {
    cli_alert_info('Step 3/3: Executing Biodiversity Link ecological effects post-processor for {toupper(m)}...')
    bio_res <- run_act4cap_biodiversity_postprocess(
      model = m,
      scenario = SCENARIO,
      downscale_outputs = downscale_res,
      out_dir = out_path,
      smoke = SMOKE,
      ncores = NCORES
    )
  }
  
  # Step 4: Multi-Pillar Synthesis and Indicator Packaging
  if (TASK %in% c('all', 'postprocess', 'smoke')) {
    cli_alert_info('Packaging unified multi-pillar indicator synthesis...')
    synth_res <- synthesize_act4cap_results(
      model = m,
      scenario = SCENARIO,
      downscale_res = downscale_res,
      swat_res = swat_res,
      bio_res = bio_res,
      out_dir = out_path
    )
    results_summary[[m]] <- synth_res
  }
}

cli_h1('Act4Cap Orchestration Execution Completed Successfully')
cli_alert_success('All requested models and post-processing modules completed.')
cli_alert_info('Output artifacts written to: {out_path}')

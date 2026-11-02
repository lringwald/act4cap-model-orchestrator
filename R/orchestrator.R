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
source('R/cascade_interface.R')
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
INPUT_FILE       <- get_param('INPUT_FILE', '')
SCENARIO         <- tolower(get_param('SCENARIO', 'base'))
POSTMODELS       <- tolower(get_param('POSTMODELS', 'all'))
RUN_SWAT_PARAM   <- get_param('RUN_SWAT', '1')
RUN_BIO_PARAM    <- get_param('RUN_BIODIVERSITY', '1')
RUN_SWAT         <- RUN_SWAT_PARAM %in% c('1', 'true', 'TRUE', 'yes')
RUN_BIODIVERSITY <- RUN_BIO_PARAM %in% c('1', 'true', 'TRUE', 'yes')
YEARS            <- get_param('YEARS', 'all')
RESOLUTION       <- get_param('RESOLUTION', '10km')
NCORES           <- as.integer(get_param('NCORES', '4'))
SMOKE            <- (get_param('SMOKE', '0') %in% c('1', 'true', 'TRUE', 'yes')) || (TASK == 'smoke')
FORCE            <- get_param('FORCE', '0') %in% c('1', 'true', 'TRUE', 'yes')
WORK_DIR         <- get_param('WORK_DIR', '/mnt/wdrv/act4cap')
OUTPUT_DIR       <- get_param('OUTPUT_DIR', 'outputs')
INPUT_DIR        <- get_param('INPUT_DIR', 'inputs')
GAMBLE_DIR       <- get_param('CASCADING_GAMBLE_DIR', 'g:/My Drive/R/cascadinggamble-core')
SWATPLUS_DIR     <- get_param('SWATPLUS_DIR', '/mnt/wdrv/act4cap/swatplus_runs')
BIODIVERSITY_DIR <- get_param('BIODIVERSITY_LINK_DIR', 'vendor/Biodiversity_Link')
BIODIVERSITY_MAPPING_FILE <- get_param('BIODIVERSITY_MAPPING_FILE', 'config/mapping_agmip_to_biodiversity_link.csv')
POSTMODEL_MODE   <- tolower(get_param('POSTMODEL_MODE', 'tentative'))

# Auto-resolve downscaling suite directory
gamble_dir_resolved <- GAMBLE_DIR
if (!dir.exists(gamble_dir_resolved)) {
  for (cand in c('g:/My Drive/R/gamble-cascade', 'g:/My Drive/R/cascadinggamble-core', 'Q:/gamble-cascade', 'Q:/cascadinggamble-core', '../gamble-cascade', '../cascadinggamble-core')) {
    if (dir.exists(cand)) { gamble_dir_resolved <- cand; break }
  }
}

# Auto-resolve Windows host paths if cluster FUSE mount path was passed
swat_dir_resolved <- SWATPLUS_DIR
if (.Platform$OS.type == "windows") {
  if (!dir.exists(swat_dir_resolved) && dir.exists("swatplus_runs")) {
    swat_dir_resolved <- "swatplus_runs"
  }
}

bio_dir_resolved <- BIODIVERSITY_DIR
if (!dir.exists(bio_dir_resolved)) {
  if (dir.exists("vendor/Biodiversity_Link")) {
    bio_dir_resolved <- "vendor/Biodiversity_Link"
  }
}

bio_map_resolved <- BIODIVERSITY_MAPPING_FILE
if (!file.exists(bio_map_resolved)) {
  for (cand in c('config/mapping_agmip_to_biodiversity_link.csv', file.path(bio_dir_resolved, 'mapping_agmip_to_biodiversity_link.csv'))) {
    if (file.exists(cand)) { bio_map_resolved <- cand; break }
  }
}

# 1. Resolve Active Subprocessing Models (POSTMODELS knob)
post_tokens <- trimws(unlist(strsplit(POSTMODELS, '[,+ ]')))
post_tokens <- post_tokens[nzchar(post_tokens)]
swat_requested <- ('all' %in% post_tokens || 'swat' %in% post_tokens) && !('none' %in% post_tokens)
bio_requested  <- ('all' %in% post_tokens || 'biodiversity' %in% post_tokens || 'bio' %in% post_tokens) && !('none' %in% post_tokens)
run_swat_active <- swat_requested && RUN_SWAT
run_bio_active  <- bio_requested && RUN_BIODIVERSITY

# 2. Resolve Macro Models (MACRO_MODEL knob)
macro_tokens <- trimws(unlist(strsplit(MACRO_MODEL, '[,+ ]')))
macro_tokens <- macro_tokens[nzchar(macro_tokens)]
all_macro_models <- c('globiom', 'capri', 'image', 'magnet', 'clumondo')
models_to_run <- if ('all' %in% macro_tokens) all_macro_models else unique(macro_tokens)

# 3. Resolve Scenarios (SCENARIO knob)
scenario_tokens <- trimws(unlist(strsplit(SCENARIO, '[,+ ]')))
scenario_tokens <- scenario_tokens[nzchar(scenario_tokens)]
all_scenarios <- c('base', 'mitigation', 'diet', 'biodiversity')
scenarios_to_run <- if ('all' %in% scenario_tokens) all_scenarios else unique(scenario_tokens)

# 4. Preview / Resolve input vintage
vintage_preview <- resolve_macro_model_vintage(INPUT_FILE, model = models_to_run[1], input_dir = INPUT_DIR, gamble_dir = gamble_dir_resolved)

cli_h1('Act4Cap Integrated Modelling Orchestrator')
cli_alert_info('Task:              {toupper(TASK)}')
cli_alert_info('Macro Model(s):    {paste(toupper(models_to_run), collapse=", ")}')
cli_alert_info('Macro Input File:  {vintage_preview$name} [{vintage_preview$source}]')
cli_alert_info('Scenario(s):       {paste(scenarios_to_run, collapse=", ")}')
cli_alert_info('Spatial Mediator:  Cascade Interface [{gamble_dir_resolved}]')
cli_alert_info('Subprocess Models: POSTMODELS={POSTMODELS} -> Water(SWAT+)={fifelse(run_swat_active, "ON", "OFF")}, Biodiversity={fifelse(run_bio_active, "ON", "OFF")}')
cli_alert_info('Post-Model Mode:   {toupper(POSTMODEL_MODE)}')
cli_alert_info('Grid Resolution:   {RESOLUTION}')
cli_alert_info('Smoke Mode:        {fifelse(SMOKE, "YES (Fast minimal slice)", "NO (Full run)")}')
cli_alert_info('Force Re-run:      {fifelse(FORCE, "YES (Recomputing & overwriting)", "NO (Auto-resolving & reusing existing step outputs)")}')
cli_alert_info('Allocated Cores:   {NCORES}')

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

# 5. Construct execution grid
job_grid <- expand.grid(
  model = models_to_run,
  scenario = scenarios_to_run,
  stringsAsFactors = FALSE
)
total_jobs <- nrow(job_grid)
n_workers <- min(NCORES, total_jobs)
inner_cores <- max(1L, floor(NCORES / n_workers))

cli_alert_info('Job Grid:          {total_jobs} total job(s) ({length(models_to_run)} model(s) x {length(scenarios_to_run)} scenario(s))')
cli_alert_info('Worker Allocation: {n_workers} concurrent worker(s), each with {inner_cores} core(s)')

# 6. Worker execution function for a single (model, scenario) job
run_act4cap_pipeline_job <- function(m, sc, cores = 1L) {
  cli::cli_h2('Executing Job: Model [{toupper(m)}] x Scenario [{sc}]')
  
  # Step 1: Spatial Mediation & Downscaling (Cascade Interface)
  downscale_res <- NULL
  if (TASK %in% c('downscale', 'all', 'smoke')) {
    cli::cli_alert_info('  [Step 1/3] Running Cascade Spatial Mediation (Downscaling) for {toupper(m)} ({sc})...')
    downscale_res <- run_act4cap_cascade_downscaling(
      model = m,
      scenario = sc,
      gamble_dir = gamble_dir_resolved,
      out_dir = out_path,
      years = YEARS,
      resolution = RESOLUTION,
      ncores = cores,
      smoke = SMOKE,
      force = FORCE,
      input_file = INPUT_FILE
    )
  } else {
    cli::cli_alert_info('  [Step 1/3] Skipping Spatial Mediation (TASK={TASK}). Locating existing mediated maps...')
    downscale_res <- locate_existing_downscaling(m, sc, out_path, gamble_dir_resolved)
  }
  
  # Step 2: Environmental Post-Modelling (Side-by-Side Twin Post-Models)
  swat_res <- NULL
  if ((TASK %in% c('swat', 'postprocess', 'all', 'smoke')) && run_swat_active) {
    cli::cli_alert_info('  [Step 2A] Executing SWAT+ Water Quality for {toupper(m)} ({sc})...')
    swat_res <- run_act4cap_swat_postprocess(
      model = m,
      scenario = sc,
      downscale_outputs = downscale_res,
      out_dir = out_path,
      swat_dir = swat_dir_resolved,
      postmodel_mode = POSTMODEL_MODE,
      smoke = SMOKE,
      force = FORCE,
      ncores = cores
    )
  }
  
  bio_res <- NULL
  if ((TASK %in% c('biodiversity', 'postprocess', 'all', 'smoke')) && run_bio_active) {
    cli::cli_alert_info('  [Step 2B] Executing Biodiversity Link for {toupper(m)} ({sc})...')
    bio_res <- run_act4cap_biodiversity_postprocess(
      model = m,
      scenario = sc,
      downscale_outputs = downscale_res,
      out_dir = out_path,
      bio_dir = bio_dir_resolved,
      mapping_file = bio_map_resolved,
      postmodel_mode = POSTMODEL_MODE,
      smoke = SMOKE,
      force = FORCE,
      ncores = cores
    )
  }
  
  # Step 3: Multi-Pillar Synthesis and Indicator Packaging (converging 2A & 2B)
  synth_res <- NULL
  if (TASK %in% c('all', 'postprocess', 'smoke')) {
    cli::cli_alert_info('  [Step 3/3] Packaging unified multi-pillar indicator synthesis for {toupper(m)} ({sc})...')
    synth_res <- synthesize_act4cap_results(
      model = m,
      scenario = sc,
      downscale_res = downscale_res,
      swat_res = swat_res,
      bio_res = bio_res,
      out_dir = out_path,
      force = FORCE
    )
  }
  
  list(
    model = m,
    scenario = sc,
    downscale = downscale_res,
    swat = swat_res,
    bio = bio_res,
    synthesis = synth_res,
    status = "SUCCESS"
  )
}

# 6. Execute job grid (Parallel across workers if n_workers > 1)
results_summary <- list()

if (n_workers > 1 && requireNamespace("future.apply", quietly = TRUE) && requireNamespace("future", quietly = TRUE)) {
  cli_alert_info('Starting parallel execution via future.apply across {n_workers} worker processes...')
  future::plan(future::multisession, workers = n_workers)
  on.exit(future::plan(future::sequential), add = TRUE)
  
  results_list <- future.apply::future_lapply(seq_len(total_jobs), function(idx) {
    suppressPackageStartupMessages({
      library(cli)
      library(data.table)
    })
    m  <- job_grid$model[idx]
    sc <- job_grid$scenario[idx]
    run_act4cap_pipeline_job(m, sc, cores = inner_cores)
  }, future.seed = TRUE, future.packages = c("cli", "data.table"))
  
  for (res in results_list) {
    job_key <- paste0(res$model, "_", res$scenario)
    results_summary[[job_key]] <- res$synthesis
  }
} else {
  cli_alert_info('Executing {total_jobs} job(s) sequentially...')
  for (idx in seq_len(total_jobs)) {
    m  <- job_grid$model[idx]
    sc <- job_grid$scenario[idx]
    res <- run_act4cap_pipeline_job(m, sc, cores = NCORES)
    job_key <- paste0(m, "_", sc)
    results_summary[[job_key]] <- res$synthesis
  }
}

cli_h1('Act4Cap Orchestration Execution Completed Successfully')
cli_alert_success('All {total_jobs} requested job(s) completed across models [{paste(models_to_run, collapse=", ")}] and scenarios [{paste(scenarios_to_run, collapse=", ")}].')
cli_alert_info('Subprocessing modules executed: SWAT+={fifelse(run_swat_active, "YES", "NO")}, Biodiversity Link={fifelse(run_bio_active, "YES", "NO")}')
cli_alert_info('Output artifacts written to: {out_path}')

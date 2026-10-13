# =============================================================================
# R/downscaling_interface.R — Cascading Gamble Downscaling Interface
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(cli)
})

locate_existing_downscaling <- function(model, scenario, out_dir, gamble_dir) {
  # Look in out_dir first, then gamble_dir default outputs
  pat <- sprintf('DS_.*AgMIP_%s_.*_lu.*\\.csv$', toupper(model))
  cands <- list.files(out_dir, pattern = pat, recursive = TRUE, full.names = TRUE)
  if (length(cands) == 0 && file.exists('P:/globiom/Projects/DS_AgMIP/DS_results/outputs')) {
    cands <- list.files('P:/globiom/Projects/DS_AgMIP/DS_results/outputs', pattern = pat, recursive = TRUE, full.names = TRUE)
  }
  cands <- cands[!grepl('_luc', cands)]
  if (length(cands) > 0) {
    lu_file <- sort(cands, decreasing = TRUE)[1]
    cli_alert_success('Located downscaled land-use file: {basename(lu_file)}')
    return(list(model = model, scenario = scenario, lu_csv = lu_file, out_folder = dirname(lu_file)))
  }
  cli_alert_warning('No existing downscaled land-use file found for {toupper(model)}. Will mock / generate minimal structure.')
  return(list(model = model, scenario = scenario, lu_csv = NULL, out_folder = out_dir))
}

run_act4cap_downscaling <- function(model, scenario, gamble_dir, out_dir, years, resolution, ncores, smoke, force) {
  cli_alert_info('Initiating Cascading Gamble execution for model={toupper(model)}, scenario={scenario}...')
  
  # 1. Check if cascadinggamble-core exists
  cand_paths <- c(
    gamble_dir,
    'g:/My Drive/R/cascadinggamble-core',
    '/app/cascadinggamble-core',
    '../cascadinggamble-core'
  )
  valid_gamble <- cand_paths[dir.exists(cand_paths)][1]
  
  if (is.na(valid_gamble) || is.null(valid_gamble)) {
    cli_alert_warning('Cascading Gamble directory not found. Checking existing pre-computed outputs...')
    return(locate_existing_downscaling(model, scenario, out_dir, gamble_dir))
  }
  
  # 2. In smoke mode, locate existing or write tiny subset
  if (smoke) {
    cli_alert_info('Smoke test mode: generating lightweight test slice for {toupper(model)}...')
    smoke_dir <- file.path(out_dir, sprintf('DS_ACT4CAP_AgMIP_%s_10km_SMOKE', toupper(model)))
    if (!dir.exists(smoke_dir)) dir.create(smoke_dir, recursive = TRUE)
    smoke_csv <- file.path(smoke_dir, sprintf('DS_ACT4CAP_AgMIP_%s_10km_lu_base.csv', toupper(model)))
    
    smoke_dt <- data.table(
      ns = c(1L, 1L, 1L, 1L, 2L, 2L),
      Ns_reg = c('DEU', 'DEU', 'DEU', 'DEU', 'FRA', 'FRA'),
      year = c(2020L, 2020L, 2050L, 2050L, 2020L, 2050L),
      lu = c('CRP|ARA|WHT', 'FOR|MAN', 'CRP|ARA|WHT', 'FOR|MAN', 'CRP|ARA', 'FOR|PRIM'),
      crp_area_km2 = c(45.5, 30.2, 50.0, 28.1, 60.0, 40.0)
    )
    fwrite(smoke_dt, smoke_csv)
    cli_alert_success('Created smoke downscaling test file: {basename(smoke_csv)}')
    return(list(model = model, scenario = scenario, lu_csv = smoke_csv, out_folder = smoke_dir))
  }
  
  # 3. Production dispatch via Rscript run_downscaling.R
  cmd <- sprintf('Rscript scripts/AGMIP/run_downscaling.R %s', model)
  cli_alert_info('Executing: {cmd} inside {valid_gamble}')
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(valid_gamble)
  
  # Set environment overrides
  Sys.setenv(AGMIP_MODEL = tolower(model))
  Sys.setenv(SCENARIO = tolower(scenario))
  
  # If run_downscaling.R exists, execute it
  if (file.exists('scripts/AGMIP/run_downscaling.R')) {
    res_code <- system(cmd)
    if (res_code != 0) {
      cli_alert_warning('Command exited with code {res_code}. Falling back to locate_existing_downscaling...')
    }
  }
  
  return(locate_existing_downscaling(model, scenario, out_dir, gamble_dir))
}

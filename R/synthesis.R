# =============================================================================
# R/synthesis.R — Multi-Pillar Integration & Indicator Synthesis
# =============================================================================
# Merges downscaling, SWAT+ water quality, and Biodiversity Link metrics into
# unified indicator tables and summary dashboards.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(cli)
})

synthesize_act4cap_results <- function(model, scenario, downscale_res, swat_res, bio_res, out_dir) {
  cli_alert_info('Compiling cross-pillar synthesis for {toupper(model)}...')
  
  synth_dir <- file.path(out_dir, 'act4cap_synthesis')
  if (!dir.exists(synth_dir)) dir.create(synth_dir, recursive = TRUE)
  
  swat_dt <- if (!is.null(swat_res$summary_csv) && file.exists(swat_res$summary_csv)) fread(swat_res$summary_csv) else data.table()
  bio_dt  <- if (!is.null(bio_res$summary_csv) && file.exists(bio_res$summary_csv))   fread(bio_res$summary_csv)  else data.table()
  
  if (nrow(swat_dt) > 0 && nrow(bio_dt) > 0) {
    synth_dt <- merge(swat_dt, bio_dt, by = c('Ns_reg', 'year'), suffixes = c('_swat', '_bio'), all = TRUE)
  } else if (nrow(swat_dt) > 0) {
    synth_dt <- swat_dt
  } else if (nrow(bio_dt) > 0) {
    synth_dt <- bio_dt
  } else {
    synth_dt <- data.table(model = model, scenario = scenario, status = 'no_post_models_run')
  }
  
  synth_dt[, model := toupper(model)]
  synth_dt[, scenario := scenario]
  
  synth_csv <- file.path(synth_dir, sprintf('act4cap_integrated_synthesis_%s_%s.csv', tolower(model), tolower(scenario)))
  fwrite(synth_dt, synth_csv)
  cli_alert_success('Saved integrated multi-pillar synthesis CSV: {basename(synth_csv)}')
  
  return(synth_csv)
}

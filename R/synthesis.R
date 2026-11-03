# =============================================================================
# R/synthesis.R — Multi-Pillar Integration & Indicator Synthesis
# =============================================================================
# Merges downscaling, SWAT+ water quality, and Biodiversity Link (BII, cSAR, MSA)
# metrics into unified indicator tables and summary dashboards.
#
# Generates standardized versioning labels:
#   act4cap_integrated_synthesis_<model>_<scenario>_<macro_output>_<res>_<date>[_<custom_tag>][_TRIAL].csv
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(cli)
})

if (!exists('construct_act4cap_output_filenames')) {
  if (file.exists('R/version_labels.R')) source('R/version_labels.R')
}

synthesize_act4cap_results <- function(model,
                                       scenario,
                                       downscale_res,
                                       swat_res,
                                       bio_res,
                                       out_dir,
                                       force = FALSE,
                                       resolution = '10km',
                                       run_date = NULL,
                                       version_tag = NULL,
                                       macro_output = NULL,
                                       trial = FALSE) {
  # Resolve macro model output name / vintage
  macro_out_name <- if (!is.null(macro_output) && nzchar(macro_output)) {
    macro_output
  } else if (!is.null(downscale_res$input_vintage$name)) {
    downscale_res$input_vintage$name
  } else if (!is.null(downscale_res$source_name)) {
    downscale_res$source_name
  } else {
    basename(downscale_res$lu_csv)
  }

  fn <- construct_act4cap_output_filenames(
    model = model,
    scenario = scenario,
    macro_output = macro_out_name,
    downscale_csv = downscale_res$lu_csv,
    resolution = resolution,
    date = run_date,
    trial = trial,
    custom_tag = version_tag,
    out_dir = out_dir
  )

  synth_dir <- fn$synthesis$dir_path
  synth_csv <- fn$synthesis$csv

  # Check if synthesis already exists and upstream inputs were cached
  upstream_newly_computed <- (identical(swat_res$cached, FALSE)) || (identical(bio_res$cached, FALSE))
  if (!force && file.exists(synth_csv) && !upstream_newly_computed) {
    cli_alert_success('Reusing existing integrated multi-pillar synthesis CSV: {basename(synth_csv)} [FORCE=0]')
    return(synth_csv)
  }

  cli_alert_info('Compiling cross-pillar synthesis for {toupper(model)} [{fn$tag}]...')
  if (!dir.exists(synth_dir)) dir.create(synth_dir, recursive = TRUE)

  # 1. SWAT+ water quality summary
  swat_dt <- if (!is.null(swat_res$summary_csv) && file.exists(swat_res$summary_csv)) {
    fread(swat_res$summary_csv)
  } else data.table()

  # 2. Biodiversity Link summaries (BII, cSAR, MSA)
  bii_dt <- if (!is.null(bio_res$bii_csv) && file.exists(bio_res$bii_csv)) {
    fread(bio_res$bii_csv)
  } else data.table()

  csar_dt <- if (!is.null(bio_res$csar_csv) && file.exists(bio_res$csar_csv)) {
    fread(bio_res$csar_csv)
  } else data.table()

  msa_dt <- if (!is.null(bio_res$summary_csv) && file.exists(bio_res$summary_csv)) {
    fread(bio_res$summary_csv)
  } else data.table()

  # 3. Combine multi-pillar tables
  synth_dt <- data.table()

  if (nrow(swat_dt) > 0) {
    synth_dt <- copy(swat_dt)
  }

  if (nrow(bii_dt) > 0) {
    keep_cols <- c('Ns_reg', 'year', 'bii_overall_index', 'primary_forest_km2', 'managed_forest_km2', 'arable_cropland_km2', 'grassland_km2')
    keep_cols <- intersect(keep_cols, names(bii_dt))
    bii_sub <- bii_dt[, ..keep_cols]
    if (nrow(synth_dt) > 0) {
      synth_dt <- merge(synth_dt, bii_sub, by = c('Ns_reg', 'year'), all = TRUE)
    } else {
      synth_dt <- bii_sub
    }
  }

  if (nrow(csar_dt) > 0) {
    keep_cols <- c('Ns_reg', 'year', 'csar_intactness_score', 'extinction_risk_index')
    keep_cols <- intersect(keep_cols, names(csar_dt))
    csar_sub <- csar_dt[, ..keep_cols]
    if (nrow(synth_dt) > 0) {
      synth_dt <- merge(synth_dt, csar_sub, by = c('Ns_reg', 'year'), all = TRUE)
    } else {
      synth_dt <- csar_sub
    }
  }

  if (nrow(msa_dt) > 0 && !('msa_overall_index' %in% names(synth_dt))) {
    keep_cols <- c('Ns_reg', 'year', 'msa_overall_index')
    keep_cols <- intersect(keep_cols, names(msa_dt))
    msa_sub <- msa_dt[, ..keep_cols]
    if (nrow(synth_dt) > 0) {
      synth_dt <- merge(synth_dt, msa_sub, by = c('Ns_reg', 'year'), all = TRUE)
    } else {
      synth_dt <- msa_sub
    }
  }

  if (nrow(synth_dt) == 0) {
    synth_dt <- data.table(model = toupper(model), scenario = scenario, status = 'no_post_models_run')
  } else {
    synth_dt[, model := toupper(model)]
    synth_dt[, scenario := scenario]
    synth_dt[, tag := fn$tag]
    setcolorder(synth_dt, c('model', 'scenario', 'tag', 'Ns_reg', 'year'))
  }

  fwrite(synth_dt, synth_csv)
  cli_alert_success('Saved integrated multi-pillar synthesis CSV: {basename(synth_csv)} ({nrow(synth_dt)} records)')

  return(synth_csv)
}

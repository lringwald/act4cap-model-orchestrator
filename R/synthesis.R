# =============================================================================
# R/synthesis.R — Multi-Pillar Integration & Indicator Synthesis
# =============================================================================
# Merges downscaling, SWAT+ water quality, and Biodiversity Link (BII, cSAR, MSA)
# metrics into unified indicator tables and summary dashboards.
#
# Unified Structure:
#   outputs/<tag>/synthesis/act4cap_integrated_synthesis_<tag>.csv
#   outputs/<tag>/manifest.json
#   outputs/synthesis/act4cap_integrated_synthesis_<tag>.csv (central index)
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

  synth_dir    <- fn$synthesis$dir_path
  synth_csv    <- fn$synthesis$csv
  central_dir  <- fn$synthesis$central_dir_path
  central_csv  <- fn$synthesis$central_csv
  manifest_file <- fn$synthesis$manifest_json

  # Check if synthesis already exists and upstream inputs were cached
  upstream_newly_computed <- (identical(swat_res$cached, FALSE)) || (identical(bio_res$cached, FALSE))
  if (!force && file.exists(synth_csv) && !upstream_newly_computed) {
    cli_alert_success('Reusing existing integrated multi-pillar synthesis CSV: {basename(synth_csv)} [FORCE=0]')
    return(synth_csv)
  }

  cli_alert_info('Compiling cross-pillar synthesis for {toupper(model)} [{fn$tag}]...')
  if (!dir.exists(synth_dir)) dir.create(synth_dir, recursive = TRUE)
  if (!dir.exists(central_dir)) dir.create(central_dir, recursive = TRUE)

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
    keep_cols <- c('Ns_reg', 'year', 'csar_intactness_score', 'committed_extinctions')
    keep_cols <- intersect(keep_cols, names(csar_dt))
    csar_sub <- csar_dt[, ..keep_cols]
    if (nrow(synth_dt) > 0) {
      synth_dt <- merge(synth_dt, csar_sub, by = c('Ns_reg', 'year'), all = TRUE)
    } else {
      synth_dt <- csar_sub
    }
  }

  downscale_path <- if (!is.null(downscale_res$lu_parquet)) downscale_res$lu_parquet else downscale_res$lu_file
  downscale_name <- if (!is.null(downscale_path)) basename(downscale_path) else "none"

  if (nrow(synth_dt) == 0) {
    synth_dt <- data.table(model = toupper(model), scenario = scenario, status = 'no_post_models_run')
  } else {
    synth_dt[, model := toupper(model)]
    synth_dt[, scenario := scenario]
    synth_dt[, tag := fn$tag]
    synth_dt[, macro_input_file := macro_out_name]
    synth_dt[, downscale_file := downscale_name]
    setcolorder(synth_dt, c('model', 'scenario', 'tag', 'macro_input_file', 'downscale_file', 'Ns_reg', 'year'))
  }

  # Write to unified job directory
  fwrite(synth_dt, synth_csv)
  # Also write to central synthesis directory for instant multi-model access
  tryCatch(fwrite(synth_dt, central_csv), error = function(e) NULL)

  cli_alert_success('Saved integrated multi-pillar synthesis CSV: {basename(synth_csv)} ({nrow(synth_dt)} records)')

  # 4. Generate structured run manifest
  manifest <- list(
    tag                  = fn$tag,
    model                = toupper(model),
    scenario             = scenario,
    macro_input_file     = macro_out_name,
    downscale_file       = downscale_name,
    resolution           = resolution,
    execution_mode       = if (trial) "TRIAL" else "PRODUCTION",
    timestamp_utc        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    job_directory        = fn$job_dir,
    artifacts = list(
      synthesis = list(
        unified_csv = synth_csv,
        central_csv = central_csv
      ),
      water_quality = list(
        summary_csv   = swat_res$summary_csv,
        cells_parquet = if (!is.null(swat_res$cell_parquet)) swat_res$cell_parquet else swat_res$cell_csv,
        composite_rds = swat_res$composite_rds,
        rasters_count = length(swat_res$rasters)
      ),
      biodiversity = list(
        bii_csv       = bio_res$bii_csv,
        csar_csv      = bio_res$csar_csv,
        cells_parquet = if (!is.null(bio_res$cell_parquet)) bio_res$cell_parquet else bio_res$cell_csv,
        composite_rds = bio_res$composite_rds,
        rasters_count = length(bio_res$rasters)
      ),
      downscaling = list(
        lu_file       = downscale_path
      )
    )
  )

  tryCatch({
    if (requireNamespace('jsonlite', quietly = TRUE)) {
      jsonlite::write_json(manifest, manifest_file, pretty = TRUE, auto_unbox = TRUE)
      cli_alert_success('Saved run manifest: {basename(manifest_file)}')
    }
  }, error = function(e) NULL)

  # 5. Generate human-readable RUN_PROVENANCE.md in job directory
  prov_md <- file.path(fn$job_dir, "RUN_PROVENANCE.md")
  prov_lines <- c(
    sprintf("# Act4Cap Pipeline Run Provenance: %s", fn$tag),
    "",
    sprintf("- **Macro Model:** %s", toupper(model)),
    sprintf("- **Scenario:** %s", scenario),
    sprintf("- **Macro Input File:** `%s`", macro_out_name),
    sprintf("- **Downscaled Land Use:** `%s`", downscale_name),
    sprintf("- **Resolution:** %s", resolution),
    sprintf("- **Execution Mode:** %s", if (trial) "TRIAL" else "PRODUCTION"),
    sprintf("- **Run Date:** %s", if (!is.null(run_date) && nzchar(run_date)) run_date else format(Sys.Date(), "%Y%m%d")),
    sprintf("- **Timestamp (UTC):** %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    "",
    "## Key Output Artifacts",
    sprintf("- **Integrated Synthesis:** `synthesis/%s`", basename(synth_csv)),
    if (!is.null(swat_res$summary_csv)) sprintf("- **SWAT+ Water Quality Summary:** `water_quality/%s`", basename(swat_res$summary_csv)) else NULL,
    if (!is.null(swat_res$cell_parquet)) sprintf("- **SWAT+ Cell Data (Parquet):** `water_quality/%s`", basename(swat_res$cell_parquet)) else NULL,
    if (!is.null(bio_res$bii_csv)) sprintf("- **Biodiversity Link BII:** `biodiversity/%s`", basename(bio_res$bii_csv)) else NULL,
    if (!is.null(bio_res$csar_csv)) sprintf("- **Biodiversity Link cSAR:** `biodiversity/%s`", basename(bio_res$csar_csv)) else NULL,
    if (!is.null(bio_res$cell_parquet)) sprintf("- **Biodiversity Link Cell Data (Parquet):** `biodiversity/%s`", basename(bio_res$cell_parquet)) else NULL
  )
  prov_lines <- prov_lines[!vapply(prov_lines, is.null, logical(1))]
  tryCatch(writeLines(unlist(prov_lines), prov_md), error = function(e) NULL)

  # 6. Maintain cumulative central registry index (outputs/synthesis/runs_index.csv)
  index_file <- file.path(central_dir, "runs_index.csv")
  index_row <- data.table(
    tag              = fn$tag,
    model            = toupper(model),
    scenario         = scenario,
    macro_input_file = macro_out_name,
    downscale_file   = downscale_name,
    resolution       = resolution,
    run_date         = if (!is.null(run_date) && nzchar(run_date)) run_date else format(Sys.Date(), "%Y%m%d"),
    job_directory    = fn$tag,
    synthesis_csv    = file.path("synthesis", basename(central_csv)),
    timestamp_utc    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  if (file.exists(index_file)) {
    existing_idx <- tryCatch(fread(index_file), error = function(e) data.table())
    if (nrow(existing_idx) > 0 && "tag" %in% names(existing_idx)) {
      existing_idx <- existing_idx[tag != fn$tag]
      combined_idx <- rbind(existing_idx, index_row, fill = TRUE)
      fwrite(combined_idx, index_file)
    } else {
      fwrite(index_row, index_file)
    }
  } else {
    fwrite(index_row, index_file)
  }
  cli_alert_success('Updated central runs index: {basename(index_file)}')

  return(synth_csv)
}

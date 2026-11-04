# =============================================================================
# R/version_labels.R — Standardized Output Versioning & File Naming Engine
# =============================================================================
# Generates uniform, reproducible version labels across all Act4Cap outputs:
#   <model>_<scenario>_<macro_output_name>_<resolution>_<date>[_<custom_tag>][_TRIAL]
#
# Storage Standard:
#   - Regional / Aggregated Summaries: .csv (human-readable, spreadsheet-accessible)
#   - Pixel / Cell-Level Spatial Tables: .parquet (high-throughput, snappy-compressed)
#   - Spatial Grids: .tif (EPSG:3035 GeoTIFF rasters)
#   - In-memory State / Metadata Bundles: .rds
#
# Unified Hierarchical Output Architecture:
#   outputs/<tag>/
#     ├── water_quality/   (regional summary .csv, 10km cells .parquet, composite .rds, rasters/)
#     ├── biodiversity/    (BII/cSAR/MSA summaries .csv, 10km cells .parquet, composite .rds, rasters/)
#     ├── synthesis/       (unified multi-pillar summary .csv, manifest.json)
#     └── downscaling/     (mediated 10km land use .parquet)
#
#   outputs/synthesis/     (central index containing all scenario syntheses .csv)
# =============================================================================

suppressPackageStartupMessages({
  library(tools)
  if (requireNamespace('jsonlite', quietly = TRUE)) library(jsonlite)
})

clean_token <- function(x) {
  if (is.null(x) || !nzchar(x)) return("")
  # Strip extensions
  s <- tools::file_path_sans_ext(basename(x))
  # Replace spaces and punctuation with underscores
  s <- gsub("[^A-Za-z0-9_.-]", "_", s)
  # Collapse multiple underscores
  s <- gsub("_+", "_", s)
  # Trim leading/trailing underscores
  s <- gsub("^_|_$", "", s)
  s
}

extract_macro_output_stem <- function(macro_output = NULL, downscale_csv = NULL, model = "") {
  # 1. Check macro_output string / file name
  if (!is.null(macro_output) && nzchar(macro_output)) {
    tok <- clean_token(macro_output)
    if (nzchar(tok)) return(tok)
  }
  
  # 2. Check downscale file name if available
  if (!is.null(downscale_csv) && nzchar(downscale_csv)) {
    base <- basename(downscale_csv)
    # If standard DS_ACT4CAP_AgMIP_<MODEL>_<RES>_<DATE> format, extract informative core
    m <- regexec("DS_.*AgMIP_([A-Za-z0-9]+)_([0-9]+km)_([0-9]{8}|TRIAL)", base, ignore.case = TRUE)
    matches <- regmatches(base, m)[[1]]
    if (length(matches) >= 4) {
      return(sprintf("AgMIP_%s_%s", toupper(matches[2]), matches[4]))
    }
    tok <- clean_token(base)
    if (nzchar(tok)) return(tok)
  }
  
  # 3. Fallback to model identifier
  if (nzchar(model)) return(tolower(model))
  "macro"
}

construct_version_tag <- function(model,
                                  scenario,
                                  macro_output = NULL,
                                  downscale_csv = NULL,
                                  resolution = "10km",
                                  date = NULL,
                                  trial = FALSE,
                                  custom_tag = NULL) {
  is_trial <- isTRUE(trial) || identical(trial, "1") || identical(trial, 1L)
  
  m_str  <- tolower(clean_token(model))
  sc_str <- tolower(clean_token(scenario))
  
  # Macro model output name
  macro_stem <- extract_macro_output_stem(macro_output, downscale_csv, model)
  
  # Resolution (ensure 'km' suffix)
  res_clean <- tolower(clean_token(resolution))
  if (!grepl("km$", res_clean)) res_clean <- paste0(res_clean, "km")
  
  # Date (YYYYMMDD)
  date_str <- if (!is.null(date) && nzchar(date) && date != "auto" && date != "today") {
    clean_token(date)
  } else {
    format(Sys.Date(), "%Y%m%d")
  }
  
  # Custom tag (optional; 'auto' or 'none' inherits macro output stem without extra suffix)
  ctag_str <- if (!is.null(custom_tag) && nzchar(custom_tag) && custom_tag != "auto" && custom_tag != "none") {
    clean_token(custom_tag)
  } else {
    NULL
  }
  
  # Trial mode tag
  trial_str <- if (is_trial) "TRIAL" else NULL
  
  # Assemble non-empty components
  # Format: <model>_<scenario>_<macro_output_stem>_<res>_<date>[_<custom_tag>][_TRIAL]
  parts <- c(m_str, sc_str, macro_stem, res_clean, date_str, ctag_str, trial_str)
  parts <- parts[nzchar(parts)]
  
  # De-duplicate adjacent identical tokens if macro_stem already contained model/scenario
  tag <- paste(parts, collapse = "_")
  tag <- gsub(sprintf("^%s_%s_%s_%s_", m_str, sc_str, m_str, sc_str), sprintf("%s_%s_", m_str, sc_str), tag)
  tag
}

# --- IO helpers enforcing standard file formats -------------------------------
write_cell_tabular_data <- function(dt, parquet_path) {
  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(as.data.frame(dt), parquet_path, compression = "snappy")
    return(parquet_path)
  }
  # Fallback to CSV if arrow is unavailable
  csv_path <- sub("\\.parquet$", ".csv", parquet_path)
  data.table::fwrite(dt, csv_path)
  csv_path
}

read_cell_tabular_data <- function(file_path, nrows = Inf) {
  if (grepl("\\.parquet$", file_path, ignore.case = TRUE)) {
    if (requireNamespace("arrow", quietly = TRUE)) {
      dt <- data.table::as.data.table(arrow::read_parquet(file_path))
      if (is.finite(nrows) && nrows < nrow(dt)) dt <- dt[seq_len(nrows)]
      return(dt)
    }
  }
  data.table::fread(file_path, nrows = nrows)
}

construct_act4cap_output_filenames <- function(model,
                                               scenario,
                                               macro_output = NULL,
                                               downscale_csv = NULL,
                                               resolution = "10km",
                                               date = NULL,
                                               trial = FALSE,
                                               custom_tag = NULL,
                                               out_dir = "outputs") {
  tag <- construct_version_tag(
    model = model,
    scenario = scenario,
    macro_output = macro_output,
    downscale_csv = downscale_csv,
    resolution = resolution,
    date = date,
    trial = trial,
    custom_tag = custom_tag
  )
  
  # Unified Job Directory: outputs/<tag>/
  job_dir <- file.path(out_dir, tag)
  
  # Subdirectories for each analytical pillar
  downscale_dir <- file.path(job_dir, "downscaling")
  swat_out_dir  <- file.path(job_dir, "water_quality")
  bio_out_dir   <- file.path(job_dir, "biodiversity")
  synth_out_dir <- file.path(job_dir, "synthesis")
  
  # Central cross-model synthesis directory
  central_synth_dir <- file.path(out_dir, "synthesis")
  
  # Pixel-level tabular files enforce .parquet standard
  swat_cell_parquet <- file.path(swat_out_dir, sprintf("swat_plus_cells_%s.parquet", tag))
  bio_cell_parquet  <- file.path(bio_out_dir, sprintf("biodiversity_link_cells_%s.parquet", tag))
  downscale_parquet <- file.path(downscale_dir, sprintf("DS_ACT4CAP_AgMIP_%s_lu_%s.parquet", tag, tolower(scenario)))
  
  list(
    tag     = tag,
    job_dir = job_dir,
    
    # Downscaling (Spatial Mediation)
    downscaling = list(
      dir_name    = "downscaling",
      dir_path    = downscale_dir,
      lu_parquet  = downscale_parquet,
      lu_file     = downscale_parquet,
      lu_csv      = file.path(downscale_dir, sprintf("DS_ACT4CAP_AgMIP_%s_lu_%s.csv", tag, tolower(scenario)))
    ),
    
    # SWAT+ Water Quality
    # Regional summary: .csv
    # Pixel / cell-level table: .parquet
    swat = list(
      dir_name      = "water_quality",
      dir_path      = swat_out_dir,
      summary_csv   = file.path(swat_out_dir, sprintf("swat_plus_water_quality_summary_%s.csv", tag)),
      cell_parquet  = swat_cell_parquet,
      cell_file     = swat_cell_parquet,
      cell_csv      = swat_cell_parquet, # backwards compatible alias
      composite_rds = file.path(swat_out_dir, sprintf("swat_plus_composite_%s.rds", tag)),
      raster_prefix = sprintf("swat_%s", tag)
    ),
    
    # Biodiversity Link
    # Regional summaries: .csv
    # Pixel / cell-level table: .parquet
    biodiversity = list(
      dir_name      = "biodiversity",
      dir_path      = bio_out_dir,
      bii_csv       = file.path(bio_out_dir, sprintf("biodiversity_link_bii_summary_%s.csv", tag)),
      csar_csv      = file.path(bio_out_dir, sprintf("biodiversity_link_csar_summary_%s.csv", tag)),
      cell_parquet  = bio_cell_parquet,
      cell_file     = bio_cell_parquet,
      cell_csv      = bio_cell_parquet, # backwards compatible alias
      composite_rds = file.path(bio_out_dir, sprintf("biodiversity_link_composite_%s.rds", tag)),
      raster_prefix = sprintf("biodiversity_%s", tag)
    ),
    
    # Multi-Pillar Synthesis (Regional executive index: .csv)
    synthesis = list(
      dir_name         = "synthesis",
      dir_path         = synth_out_dir,
      csv              = file.path(synth_out_dir, sprintf("act4cap_integrated_synthesis_%s.csv", tag)),
      central_csv      = file.path(central_synth_dir, sprintf("act4cap_integrated_synthesis_%s.csv", tag)),
      central_dir_path = central_synth_dir,
      manifest_json    = file.path(job_dir, "manifest.json")
    )
  )
}

# =============================================================================
# R/spatial_raster_export.R — Spatially Explicit GeoTIFF Raster Exporter
# =============================================================================
# Converts cell-level post-processing indicators (Biodiversity Link BII/cSAR/MSA,
# SWAT+ water quality) into standard European EPSG:3035 GeoTIFF rasters (10km)
# anchored on the master EEA grid lookup (one_kmID_master_mapping).
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(cli)
  if (requireNamespace('terra', quietly = TRUE)) library(terra)
})

load_master_grid_10km_lookup <- function(gamble_dir = NULL) {
  local_cache <- 'config/master_grid_10km_lookup.rds'
  if (file.exists(local_cache)) {
    return(readRDS(local_cache))
  }
  
  # Search for master mapping parquet
  cands <- c(
    if (!is.null(gamble_dir)) file.path(gamble_dir, 'data', 'aux_files'),
    'data/aux_files',
    '../cascadinggamble-core/data/aux_files',
    'g:/My Drive/R/cascadinggamble-core/data/aux_files',
    'inputs/aux_files'
  )
  cands <- cands[!is.null(cands) & dir.exists(cands)]
  
  pq_files <- character(0)
  for (cd in cands) {
    found <- list.files(cd, pattern = 'one_kmID_master_mapping_.*\\.parquet$', full.names = TRUE)
    pq_files <- c(pq_files, found)
  }
  
  if (length(pq_files) == 0) {
    cli::cli_alert_warning('Master grid mapping parquet not found. Spatial GeoTIFF export unavailable.')
    return(NULL)
  }
  
  latest_pq <- sort(pq_files, decreasing = TRUE)[1]
  cli::cli_alert_info('Building 10km grid lookup from {basename(latest_pq)}...')
  
  if (!requireNamespace('arrow', quietly = TRUE)) {
    cli::cli_alert_warning('arrow package required to read master grid mapping.')
    return(NULL)
  }
  
  mg <- as.data.table(arrow::read_parquet(latest_pq, col_select = c('EEA_10kmID', 'x_10kmID', 'y_10kmID'), memory_map = FALSE))
  mg_u <- unique(mg, by = 'EEA_10kmID')
  setorder(mg_u, x_10kmID, y_10kmID)
  
  # Cache for instant subsequent runs
  dir.create('config', showWarnings = FALSE, recursive = TRUE)
  tryCatch(saveRDS(mg_u, local_cache), error = function(e) NULL)
  return(mg_u)
}

safe_writeRaster <- function(r, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) tryCatch(unlink(path, force = TRUE), error = function(e) NULL)

  ok <- tryCatch({
    terra::writeRaster(r, path, overwrite = TRUE, ...)
    tryCatch(Sys.chmod(path, mode = "0666", use_umask = FALSE), error = function(e) NULL)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) {
    tmp_path <- file.path(dirname(path), sprintf(".tmp_%s_%s", Sys.getpid(), basename(path)))
    tryCatch({
      terra::writeRaster(r, tmp_path, overwrite = TRUE, ...)
      tryCatch(Sys.chmod(tmp_path, mode = "0666", use_umask = FALSE), error = function(e) NULL)
      renamed <- file.rename(tmp_path, path)
      if (!renamed) {
        alt_path <- file.path(dirname(path), sprintf("%s_%s.tif", tools::file_path_sans_ext(basename(path)), Sys.getpid()))
        file.rename(tmp_path, alt_path)
        tryCatch(Sys.chmod(alt_path, mode = "0666", use_umask = FALSE), error = function(e) NULL)
        path <- alt_path
      } else {
        tryCatch(Sys.chmod(path, mode = "0666", use_umask = FALSE), error = function(e) NULL)
      }
      TRUE
    }, error = function(e2) {
      FALSE
    })
  }
}

export_indicators_to_geotiff <- function(cell_dt,
                                         indicator_cols,
                                         out_dir,
                                         prefix,
                                         years = NULL,
                                         gamble_dir = NULL) {
  if (!requireNamespace('terra', quietly = TRUE)) {
    cli::cli_alert_warning('Package terra not available; skipping GeoTIFF export.')
    return(character(0))
  }
  
  if (is.null(cell_dt) || nrow(cell_dt) == 0) return(character(0))
  
  lookup <- load_master_grid_10km_lookup(gamble_dir)
  if (is.null(lookup)) return(character(0))
  
  raster_dir <- file.path(out_dir, 'rasters')
  if (!dir.exists(raster_dir)) dir.create(raster_dir, recursive = TRUE)
  
  # Ensure column names
  dt <- copy(cell_dt)
  if (!('ns' %in% names(dt)) && ('EEA_10kmID' %in% names(dt))) {
    setnames(dt, 'EEA_10kmID', 'ns')
  }
  if (!('ns' %in% names(dt))) {
    cli::cli_alert_warning('Cell table lacks spatial identifier (ns / EEA_10kmID); skipping GeoTIFF export.')
    return(character(0))
  }
  
  target_years <- if (is.null(years)) sort(unique(dt$year)) else intersect(years, unique(dt$year))
  if (length(target_years) == 0) target_years <- sort(unique(dt$year))
  
  created_tifs <- character(0)
  
  for (yr in target_years) {
    dt_yr <- dt[year == yr]
    if (nrow(dt_yr) == 0) next
    
    # Check if aggregation per cell is needed (e.g. if multiple land use classes per cell)
    has_dups <- anyDuplicated(dt_yr$ns) > 0
    if (has_dups) {
      agg_expr <- parse(text = sprintf(
        "list(%s)",
        paste(sprintf("%s = mean(%s, na.rm = TRUE)", indicator_cols, indicator_cols), collapse = ", ")
      ))
      dt_yr <- dt_yr[, eval(agg_expr), by = ns]
    }
    
    # Merge with master 10km grid
    m <- merge(lookup, dt_yr, by.x = 'EEA_10kmID', by.y = 'ns', all.x = TRUE)
    setorder(m, x_10kmID, y_10kmID)
    
    for (ind in indicator_cols) {
      if (!(ind %in% names(m))) next
      
      has_res <- grepl('[0-9]+km', prefix, ignore.case = TRUE)
      tif_name <- if (has_res) {
        sprintf('%s_%s_%s.tif', prefix, ind, yr)
      } else {
        sprintf('%s_%s_%s_10km.tif', prefix, ind, yr)
      }
      tif_path <- file.path(raster_dir, tif_name)
      
      xyz_df <- as.data.frame(m[, .(x = x_10kmID, y = y_10kmID, z = get(ind))])
      r <- terra::rast(xyz_df, type = 'xyz', crs = 'EPSG:3035')
      names(r) <- sprintf('%s_%s', ind, yr)
      
      safe_writeRaster(r, tif_path, gdal = c('COMPRESS=DEFLATE'))
      created_tifs <- c(created_tifs, tif_path)
    }
  }
  
  if (length(created_tifs) > 0) {
    cli::cli_alert_success('Exported {length(created_tifs)} spatially explicit GeoTIFF rasters (EPSG:3035) to {basename(raster_dir)}/')
  }
  
  return(created_tifs)
}

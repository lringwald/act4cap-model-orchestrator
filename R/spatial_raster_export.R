# =============================================================================
# R/spatial_raster_export.R — Spatially Explicit GeoTIFF Raster Exporter
# =============================================================================
# Converts cell-level post-processing indicators (Biodiversity Link BII/cSAR/MSA,
# SWAT+ water quality) into standard European EPSG:3035 GeoTIFF rasters at any
# supported EEA grid resolution (1km, 2km, 3km, 4km, 5km, 10km, 20km, 30km,
# 40km, 50km, 100km, 200km) anchored on one_kmID_master_mapping_*.parquet.
# =============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(cli)
  if (requireNamespace('terra', quietly = TRUE)) library(terra)
})

normalize_grid_resolution <- function(resolution = '10km') {
  if (is.null(resolution) || !nzchar(as.character(resolution))) return('10km')
  res_num <- suppressWarnings(as.integer(gsub('[^0-9]', '', as.character(resolution))))
  supported <- c(1L, 2L, 3L, 4L, 5L, 10L, 20L, 30L, 40L, 50L, 100L, 200L)
  if (is.na(res_num) || !(res_num %in% supported)) res_num <- 10L
  paste0(res_num, 'km')
}

load_master_grid_lookup <- function(resolution = '10km', gamble_dir = NULL) {
  res_label <- normalize_grid_resolution(resolution)
  id_col <- paste0('EEA_', res_label, 'ID')
  x_col  <- paste0('x_', res_label, 'ID')
  y_col  <- paste0('y_', res_label, 'ID')

  tmp_cache <- file.path(tempdir(), sprintf('master_grid_%s_lookup.rds', res_label))
  if (file.exists(tmp_cache)) {
    return(readRDS(tmp_cache))
  }

  # Search for master mapping parquet across container, cluster FUSE mount, and local paths
  cands <- c(
    if (!is.null(gamble_dir)) file.path(gamble_dir, 'data', 'aux_files'),
    if (nzchar(Sys.getenv('CASCADING_GAMBLE_DIR', ''))) file.path(Sys.getenv('CASCADING_GAMBLE_DIR'), 'data', 'aux_files'),
    '/tmp/gamble-cascade-live/data/aux_files',
    '/mnt/wdrv/ACT4CAP-model-pipeline/01_interimmodel/gamble-cascade/data/aux_files',
    '/mnt/wdrv/ACT4CAP-model-pipeline/01_inputs/gamble-cascade-data/data/downscaling/auxiliary',
    'T:/ACT4CAP-model-pipeline/01_interimmodel/gamble-cascade/data/aux_files',
    'T:/ACT4CAP-model-pipeline/01_inputs/gamble-cascade-data/data/downscaling/auxiliary',
    'W:/ACT4CAP-model-pipeline/01_interimmodel/gamble-cascade/data/aux_files',
    'W:/ACT4CAP-model-pipeline/01_inputs/gamble-cascade-data/data/downscaling/auxiliary',
    'data/aux_files',
    '../cascadinggamble-core/data/aux_files',
    'g:/My Drive/R/cascadinggamble-core/data/aux_files',
    'inputs/aux_files'
  )
  cands <- unique(cands[!is.null(cands) & nzchar(cands) & dir.exists(cands)])

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
  cli::cli_alert_info('Building {res_label} grid lookup from {basename(latest_pq)}...')

  if (!requireNamespace('arrow', quietly = TRUE)) {
    cli::cli_alert_warning('arrow package required to read master grid mapping.')
    return(NULL)
  }

  mg <- as.data.table(arrow::read_parquet(
    latest_pq,
    col_select = c(id_col, x_col, y_col),
    memory_map = FALSE
  ))
  setnames(mg, c(id_col, x_col, y_col), c('grid_id', 'x_coord', 'y_coord'))
  mg_u <- unique(mg[!is.na(grid_id)], by = 'grid_id')
  setorder(mg_u, x_coord, y_coord)

  # Keep backward-compatible column aliases alongside generic grid_id / x_coord / y_coord
  mg_u[, (id_col) := grid_id]
  mg_u[, (x_col)  := x_coord]
  mg_u[, (y_col)  := y_coord]

  # Cache in /tmp for subsequent calls within the same run
  tryCatch(saveRDS(mg_u, tmp_cache), error = function(e) NULL)
  return(mg_u)
}

# Backward-compatible wrapper
load_master_grid_10km_lookup <- function(gamble_dir = NULL, resolution = '10km') {
  load_master_grid_lookup(resolution = resolution, gamble_dir = gamble_dir)
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
                                         gamble_dir = NULL,
                                         resolution = '10km') {
  if (!requireNamespace('terra', quietly = TRUE)) {
    cli::cli_alert_warning('Package terra not available; skipping GeoTIFF export.')
    return(character(0))
  }

  if (is.null(cell_dt) || nrow(cell_dt) == 0) return(character(0))

  res_label <- normalize_grid_resolution(resolution)
  id_col <- paste0('EEA_', res_label, 'ID')

  lookup <- load_master_grid_lookup(resolution = res_label, gamble_dir = gamble_dir)
  if (is.null(lookup)) return(character(0))

  raster_dir <- file.path(out_dir, 'rasters')
  if (!dir.exists(raster_dir)) dir.create(raster_dir, recursive = TRUE)

  # Ensure spatial identifier column is normalized to 'ns'
  dt <- copy(cell_dt)
  if (!('ns' %in% names(dt))) {
    cand_id_cols <- unique(c(id_col, grep('^EEA_[0-9]+kmID$', names(dt), value = TRUE)))
    if (length(cand_id_cols) > 0 && cand_id_cols[1] %in% names(dt)) {
      setnames(dt, cand_id_cols[1], 'ns')
    }
  }
  if (!('ns' %in% names(dt))) {
    cli::cli_alert_warning('Cell table lacks spatial identifier (ns / {id_col}); skipping GeoTIFF export.')
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

    # Merge with master grid for the requested resolution
    m <- merge(lookup[, .(grid_id, x_coord, y_coord)], dt_yr, by.x = 'grid_id', by.y = 'ns', all.x = TRUE)
    setorder(m, x_coord, y_coord)

    for (ind in indicator_cols) {
      if (!(ind %in% names(m))) next

      has_res <- grepl('[0-9]+km', prefix, ignore.case = TRUE)
      tif_name <- if (has_res) {
        sprintf('%s_%s_%s.tif', prefix, ind, yr)
      } else {
        sprintf('%s_%s_%s_%s.tif', prefix, ind, yr, res_label)
      }
      tif_path <- file.path(raster_dir, tif_name)

      xyz_df <- as.data.frame(m[, .(x = x_coord, y = y_coord, z = get(ind))])
      r <- terra::rast(xyz_df, type = 'xyz', crs = 'EPSG:3035')
      names(r) <- sprintf('%s_%s', ind, yr)

      safe_writeRaster(r, tif_path, gdal = c('COMPRESS=DEFLATE'))
      created_tifs <- c(created_tifs, tif_path)
    }
  }

  if (length(created_tifs) > 0) {
    cli::cli_alert_success('Exported {length(created_tifs)} spatially explicit {res_label} GeoTIFF rasters (EPSG:3035) to {basename(raster_dir)}/')
  }

  return(created_tifs)
}

#' Convert Tabular Downscaling Results to RasterStack (SpatRaster)
#'
#' @param results data.table containing 'ns', 'value', and a class column (lu.to or pop_type)
#' @param template_path path to the template .tif file (e.g., EEA_5kmID_TOT_AREA.tif)
#' @param class_col name of the column containing classes (default "lu.to")
#' @param value_col name of the value column (default "value")
#' @param output_path path to save the resulting .tif file
#' @param value_type what each band holds: "area" (per pixel), "share", "count",
#'   "category" or "other" (default). Sets the overview resampling and is recorded in the sidecar.
#' @param units units of the band values (e.g. "km2", "LSU"). Default NULL: the sidecar records
#'   null rather than a guessed unit; callers that know the unit pass it.
#' @param datatype GDAL data type written (default "FLT4S", float32).
#' @param nodata NoData value written to the file (default -9999).
#' @param sidecar write a JSON sidecar `<output_path>.json` next to the raster.
#' @param metadata named list with sidecar fields the function cannot derive itself:
#'   product, version, scenario, period, description, class_table (path to a CSV with a
#'   `class` column), run_id, run_manifest, config_hash, uncertainty (list), validation_report,
#'   licence, citation, doi. Missing entries are written as null, never guessed.
#'
#' Rasters are written as Cloud-Optimized GeoTIFF (spatial rulebook, Principle 8):
#' DEFLATE compression, predictor, 512 x 512 tiles, and overviews resampled by value type
#' (AVERAGE for continuous values, NEAREST for categories). Overviews are for display only.
#'
#' @return SpatRaster object
results_to_raster <- function(results, 
                              template_path, 
                              class_col = "to_class", 
                              value_col = "value",
                              output_path = NULL,
                              check_area = FALSE,
                              max_pixel_area = 100,
                              value_type = "other",
                              units = NULL,
                              datatype = "FLT4S",
                              nodata = -9999,
                              sidecar = TRUE,
                              metadata = list()) {
  require(terra)
  require(data.table)
  require(cli)
  if (is.character(template_path)) {
    cli_alert_info("Creating raster from results using template: {basename(template_path)}")
    template <- rast(template_path)
  } else {
    cli_alert_info("Creating raster from results using SpatRaster object as template")
    template <- template_path
  }
  
  # Precompute ns_template to avoid doing it inside the layer loop
  if (!"ns_template" %in% names(results) && "ns" %in% names(results)) {
    results[, ns_template := as.integer(gsub("_.*", "", ns))]
  }
  
  # Detect years if 'times' column exists
  has_years <- "times" %in% names(results)
  years <- if (has_years) sort(unique(results$times)) else NULL
  
  # Area Conservation Check right before mapping
  if (check_area) {
    cli_alert_info("Running pre-rasterization area conservation checks...")
    if (has_years) {
      pixel_totals <- results[, .(total = sum(get(value_col), na.rm = TRUE)), by = .(ns, times)]
      region_totals <- pixel_totals[, .(region_total = sum(total, na.rm = TRUE)), by = times]
      if (nrow(region_totals) > 1) {
         area_variance <- max(region_totals$region_total) - min(region_totals$region_total)
         if (area_variance > 0.01) {
             cli_alert_danger(sprintf("CRITICAL (Rasterization): Total area fluctuates by %.2f km² across projection timesteps!", area_variance))
         } else {
             cli_alert_success("Area conservation: Total area is stable across all timesteps prior to rasterization.")
         }
      }
    } else {
      pixel_totals <- results[, .(total = sum(get(value_col), na.rm = TRUE)), by = .(ns)]
      total_map_area <- sum(pixel_totals$total, na.rm = TRUE)
      cli_alert_info("Area conservation: Total raster map area is {round(total_map_area, 2)} km².")
    }
    
    if (!is.null(max_pixel_area)) {
      exceeded_dt <- pixel_totals[total > (max_pixel_area + 0.01)]
      n_exceeded <- nrow(exceeded_dt)
      n_lower <- sum(pixel_totals$total < (max_pixel_area - 0.01), na.rm = TRUE)
      if (n_exceeded > 0) {
        exceed_diff <- exceeded_dt$total - max_pixel_area
        max_ex <- max(exceed_diff, na.rm = TRUE)
        mean_ex <- mean(exceed_diff, na.rm = TRUE)
        severity <- if (max_ex < 0.1) "minor numerical inaccuracy" else "substantial excess"
        cli_alert_danger(sprintf("CRITICAL (Rasterization): %d pixels exceed theoretical max pixel area of %.1f km²! Exceedance: Max +%.4f km², Mean +%.4f km² (%s).", n_exceeded, max_pixel_area, max_ex, mean_ex, severity))
      }
      if (n_lower > 0) {
        cli_alert_info(sprintf("Info (Rasterization): %d pixels have area lower than %.1f km² (likely coastal/border pixels).", n_lower, max_pixel_area))
      }
    }
  }
  
  # Get unique classes
  classes <- sort(unique(results[[class_col]]))
  
  if (length(classes) == 0) {
    cli_alert_warning("No results found in column '{class_col}'. Skipping raster creation.")
    return(NULL)
  }
  
  # Define all year-class combinations for layers
  if (has_years) {
    lyr_names <- as.vector(outer(years, classes, function(y, c) paste0(y, "_", c)))
  } else {
    lyr_names <- classes
  }
  
  # Prepare for adaptive in-memory layer assembly
  cli_progress_step("Mapping {length(lyr_names)} layers (Memory-Safe Adaptive Batching)...")
  
  # Extract cell values and mask ONCE outside the loop
  cell_values <- values(template[[1]], mat = FALSE)
  n_cells <- length(cell_values)
  mask_target <- !is.na(cell_values)
  
  # --- Adaptive in-memory accumulation ---
  bytes_per_col <- n_cells * 8  # 8 bytes per numeric cell
  max_batch_bytes <- 4e9        # ~4 GB threshold per batch
  max_cols_per_batch <- max(10L, as.integer(max_batch_bytes / bytes_per_col))
  
  all_col_names <- character()
  all_col_vals <- list()
  batch_temp_files <- character()
  
  # Helper: flush accumulated columns to a temp TIF batch file
  flush_batch <- function() {
    if (length(all_col_names) == 0) return()
    batch_idx <- length(batch_temp_files) + 1
    r2r_tmp_dir <- if (exists("paths") && !is.null(paths$temp) && dir.exists(paths$temp)) paths$temp else tempdir()
    tmp_batch <- file.path(r2r_tmp_dir, paste0("r2r_batch_", format(Sys.time(), "%H%M%S"), "_", sample.int(1e6, 1), "_", batch_idx, ".tif"))
    r_batch <- rast(template[[1]], nlyrs = length(all_col_names))
    names(r_batch) <- all_col_names
    values(r_batch) <- do.call(cbind, all_col_vals)
    writeRaster(r_batch, tmp_batch, overwrite = TRUE, gdal = "COMPRESS=LZW")
    rm(r_batch)
    batch_temp_files <<- c(batch_temp_files, tmp_batch)
    all_col_names <<- character()
    all_col_vals <<- list()
    gc(verbose = FALSE)
  }
  
  # Iterate through years and classes
  idx <- 0
  for (y in if (has_years) years else list(NULL)) {
    # Filter by year if applicable
    res_yr <- if (has_years) results[times == y] else results
    
    for (cls in classes) {
      idx <- idx + 1
      lyr_name <- if (has_years) paste0(y, "_", cls) else cls
      
      # Subset results for this class and aggregate by the base EEA ID (ns_template) 
      res_sub <- res_yr[get(class_col) == cls, .(value = sum(get(value_col), na.rm = TRUE)), by = ns_template]
      
      if (nrow(res_sub) == 0) {
        # Empty layer: 0 where template is valid, NA elsewhere
        col_vals <- rep(NA_real_, n_cells)
        col_vals[mask_target] <- 0
      } else {
        # Fast integer matching
        match_idx <- match(cell_values, res_sub$ns_template)
        col_vals <- res_sub$value[match_idx]
        
        # Only fill missing values with 0 if they are within the template's valid area
        col_vals[mask_target & is.na(col_vals)] <- 0
      }
      
      all_col_names <- c(all_col_names, lyr_name)
      all_col_vals[[length(all_col_vals) + 1]] <- col_vals
      
      # Adaptive flush
      if (length(all_col_names) >= max_cols_per_batch) {
        cli_alert_info("Flushing batch of {length(all_col_names)} layers to disk (memory budget reached)...")
        flush_batch()
      }
      
      # Clear memory periodically
      if (idx %% 50 == 0) gc(verbose = FALSE)
    }
  }
  
  # Assemble final multi-layer raster
  cli_alert_info("Assembling final {length(lyr_names)}-layer raster...")
  if (length(all_col_names) > 0 && length(batch_temp_files) == 0) {
    # Everything fit in memory — single matrix write
    out_raster <- rast(template[[1]], nlyrs = length(all_col_names))
    names(out_raster) <- all_col_names
    values(out_raster) <- do.call(cbind, all_col_vals)
  } else {
    # Flush remaining columns, then stack all batch files
    if (length(all_col_names) > 0) flush_batch()
    out_raster <- rast(batch_temp_files)
  }
  rm(all_col_names, all_col_vals)
  gc(verbose = FALSE)
  
  # Ensure CRS is set
  if (crs(out_raster) == "") {
    crs(out_raster) <- "EPSG:3035"
  }

  # Save final multi-layer raster
  if (!is.null(output_path)) {
    if (!dir.exists(dirname(output_path))) dir.create(dirname(output_path), recursive = TRUE)
    cli_alert_success("Saving multi-layer raster to: {output_path}")
    
    # Fail before writing if a class is missing from the declared class table
    .r2r_check_class_table(classes, metadata$class_table)

    # Direct GDAL temp directory to paths$temp if available, avoiding local C: drive space exhaustion
    r2r_tmp_dir <- if (exists("paths") && !is.null(paths$temp) && dir.exists(paths$temp)) paths$temp else tempdir()
    tryCatch({
      terra::setGDALconfig("CPL_TMPDIR", r2r_tmp_dir)
      Sys.setenv(CPL_TMPDIR = r2r_tmp_dir)
    }, error = function(e) NULL)

    # Use local SSD only for compact rasters (<= 30 layers) to prevent disk space exhaustion on local C: drive
    use_local_tmp <- length(classes) <= 30
    written_to_dest <- FALSE
    
    if (use_local_tmp) {
      local_tmp <- file.path(tempdir(), basename(output_path))
      tryCatch({
        .r2r_write_cog(out_raster, local_tmp, value_type = value_type, datatype = datatype, nodata = nodata)
        if (file.copy(local_tmp, output_path, overwrite = TRUE)) {
          written_to_dest <- TRUE
        }
      }, error = function(e) {
        cli_alert_warning(sprintf("Local SSD COG write failed (%s). Writing directly to output destination...", conditionMessage(e)))
      }, finally = {
        if (file.exists(local_tmp)) tryCatch(file.remove(local_tmp), error = function(e) NULL)
      })
    }

    if (!written_to_dest) {
      .r2r_write_cog(out_raster, output_path, value_type = value_type, datatype = datatype, nodata = nodata)
    }
    .r2r_assert_cog(output_path)
    rm(out_raster)
    
    # Clean up any batch temp files
    if (exists("batch_temp_files") && length(batch_temp_files) > 0) file.remove(batch_temp_files)
    
    gc(verbose = FALSE)
    
    out_raster <- rast(output_path)

    if (isTRUE(sidecar)) {
      .r2r_write_sidecar(out_raster, output_path, classes = classes, has_years = has_years,
                         value_type = value_type, units = units, datatype = datatype,
                         nodata = nodata, metadata = metadata)
    }
  }
  
  return(out_raster)
}

# ===============================================================
# Cloud-Optimized GeoTIFF writing and JSON sidecars
# ===============================================================

# Overview resampling follows the variable type: categories must not be averaged.
.r2r_overview_resampling <- function(value_type) {
  if (identical(value_type, "category")) "NEAREST" else "AVERAGE"
}

.r2r_write_cog <- function(r, path, value_type, datatype, nodata) {
  r2r_tmp_dir <- if (exists("paths") && !is.null(paths$temp) && dir.exists(paths$temp)) paths$temp else tempdir()
  tryCatch({
    terra::setGDALconfig("CPL_TMPDIR", r2r_tmp_dir)
    Sys.setenv(CPL_TMPDIR = r2r_tmp_dir)
  }, error = function(e) NULL)

  terra::writeRaster(r, path, overwrite = TRUE, filetype = "COG", datatype = datatype, NAflag = nodata,
                     gdal = c("COMPRESS=DEFLATE", "PREDICTOR=YES", "BLOCKSIZE=512", "BIGTIFF=IF_SAFER",
                              paste0("OVERVIEW_RESAMPLING=", .r2r_overview_resampling(value_type))))
}

.r2r_assert_cog <- function(path) {
  info <- terra::describe(path)
  if (!any(grepl("LAYOUT=COG", info, fixed = TRUE))) {
    stop("results_to_raster: ", basename(path), " was not written with a COG layout.", call. = FALSE)
  }
  invisible(TRUE)
}

# Every class in the raster must be listed in the declared class table (spatial
# rulebook, Principle 8, item 5). Without a class table, the sidecar records null.
.r2r_check_class_table <- function(classes, class_table) {
  if (is.null(class_table)) return(invisible(NULL))
  if (!file.exists(class_table)) stop("results_to_raster: class table not found: ", class_table, call. = FALSE)
  tab <- data.table::fread(class_table)
  col <- intersect(c("class", "name", "code"), names(tab))[1]
  if (is.na(col)) stop("results_to_raster: class table ", class_table, " needs a `class`, `name` or `code` column.", call. = FALSE)
  missing <- setdiff(as.character(classes), as.character(tab[[col]]))
  if (length(missing)) {
    stop(sprintf("results_to_raster: %d class(es) missing from the class table %s: %s",
                 length(missing), basename(class_table), paste(utils::head(missing, 10), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

.r2r_file_checksum <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    list(algorithm = "sha256", value = digest::digest(file = path, algo = "sha256"))
  } else {
    list(algorithm = "md5", value = unname(tools::md5sum(path)))
  }
}

.r2r_git_info <- function(dir = ".") {
  run_git <- function(args) tryCatch(suppressWarnings(system2("git", c("-C", dir, args), stdout = TRUE, stderr = FALSE)),
                                     error = function(e) character())
  sha <- run_git(c("rev-parse", "HEAD"))
  if (!length(sha) || !nzchar(sha[1]) || !is.null(attr(sha, "status"))) return(list(commit = NULL, dirty = NULL))
  list(commit = sha[1], dirty = length(run_git(c("status", "--porcelain", "--untracked-files=no"))) > 0)
}

# Required top-level fields (schemas/raster_sidecar.schema.json).
.r2r_sidecar_required <- c("schema_version", "identity", "content", "spatial", "legend",
                           "lineage", "uncertainty", "validation", "software", "terms")

.r2r_write_sidecar <- function(r, path, classes, has_years, value_type, units, datatype, nodata, metadata) {
  md <- function(key) if (!is.null(metadata[[key]])) metadata[[key]] else NULL
  band_names <- names(r)
  bands <- lapply(band_names, function(b) {
    period <- if (has_years) sub("_.*$", "", b) else NULL
    cls <- if (has_years) sub("^[^_]*_", "", b) else b
    list(name = b, class = cls, period = period, value_type = value_type, units = units, data_type = datatype)
  })
  crs_info <- tryCatch(terra::crs(r, describe = TRUE), error = function(e) NULL)
  epsg <- if (!is.null(crs_info) && !is.na(crs_info$code)) paste0(crs_info$authority, ":", crs_info$code) else NULL
  e <- terra::ext(r)
  versions <- tryCatch(as.list(terra::gdal(lib = "all")), error = function(e) list())
  class_table <- md("class_table")
  engine_dir <- if (dir.exists("external/cascade-core")) "external/cascade-core" else NULL

  sc <- list(
    schema_version = "1.0",
    identity = list(
      product = md("product"), version = md("version"), file = basename(path),
      checksum = .r2r_file_checksum(path),
      created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ),
    content = list(
      description = md("description"), scenario = md("scenario"),
      period = if (!is.null(md("period"))) md("period") else if (has_years) unique(vapply(bands, function(b) b$period, "")) else NULL,
      classes = as.character(classes), bands = bands
    ),
    spatial = list(
      crs_code = epsg, crs_wkt = terra::crs(r),
      grid = list(origin = as.numeric(terra::origin(r)), resolution = as.numeric(terra::res(r)),
                  extent = list(xmin = e$xmin, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax),
                  nrow = terra::nrow(r), ncol = terra::ncol(r)),
      nodata = list(value = nodata, meaning = "outside the template's valid area (grid, land mask or territory)"),
      format = "COG", overview_resampling = .r2r_overview_resampling(value_type)
    ),
    legend = list(
      class_table = if (!is.null(class_table)) basename(class_table) else NULL,
      class_table_checksum = if (!is.null(class_table) && file.exists(class_table)) .r2r_file_checksum(class_table) else NULL
    ),
    lineage = list(
      run_id = md("run_id"), run_manifest = md("run_manifest"), config_hash = md("config_hash"),
      code = list(pipeline = .r2r_git_info("."), engine = if (!is.null(engine_dir)) .r2r_git_info(engine_dir) else NULL),
      inputs = md("inputs")
    ),
    uncertainty = if (!is.null(md("uncertainty"))) md("uncertainty") else list(
      mode = "declared_point_estimate",
      statement = "Single deterministic product; no per-pixel uncertainty is quantified in this file.",
      not_quantified = NULL
    ),
    validation = list(report = md("validation_report"), measures = md("validation_measures")),
    software = list(
      R = paste(R.version$major, R.version$minor, sep = "."),
      terra = as.character(utils::packageVersion("terra")),
      gdal = versions$gdal, proj = versions$proj, geos = versions$geos
    ),
    terms = list(licence = md("licence"), citation = md("citation"), doi = md("doi"))
  )

  missing <- setdiff(.r2r_sidecar_required, names(sc))
  if (length(missing)) stop("results_to_raster: sidecar is missing required fields: ", paste(missing, collapse = ", "), call. = FALSE)

  json_path <- paste0(path, ".json")
  writeLines(jsonlite::toJSON(sc, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA), json_path)
  cli_alert_success("Sidecar written: {basename(json_path)}")
  invisible(json_path)
}

# ==============================================================================
# aggregate_lum_bioclima_to_1km.R
# ==============================================================================
# Aggregates the LUM_BioClima v2 100m categorical rasters (years 2000, 2010,
# 2018) to the European 1km INSPIRE buffer grid
# (data/aux_files/INSPIRE_Europe_buffer_1km.tif).
#
# These rasters contain integer class codes representing combined Land Use /
# Bioclimate class assignments from the BIOCLIMA3_CAPRI project.
#
# Aggregation strategy: for each 1km cell, count the number of 100m pixels
# per class code (10x10 = 100 pixels per cell max). Each pixel = 1 ha
# at 100m resolution, so pixel_count directly equals area_ha.
#
# Outputs (in data/BIOCLIMA3_CAPRI/LUM_BioClima_1km/):
# 1. Parquet table:
#    - lum_bioclima_1km.parquet: Long-format table with columns
#        INSPIRE_Europe_buffer_1kmID | year | lum_class | area_ha | share
# 2. CSV summary:
#    - lum_bioclima_class_universe.csv: Class x year pixel coverage stats
# 3. GeoTIFFs:
#    - lum_bioclima_dominant_<year>.tif: Dominant class per year (INT2S)
#    - lum_bioclima_shares_<year>.tif: Multi-band raster, one band per class
#                                       (INT1U, values = ha 0..100)
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(data.table)
  library(arrow)
  library(cli)
})

cli_h1("LUM BioClima 1km Aggregation Pipeline (BIOCLIMA3_CAPRI)")

# --- 1. Configuration & Paths ------------------------------------------------
input_dir        <- "H:/R/LARGEDATAPROCESSING/LUM_BioClima"
output_dir       <- "data/BIOCLIMA3_CAPRI/LUM_BioClima_1km"
buffer_grid_path <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"

years <- c(2000L, 2010L, 2018L)

# v2 input files (single-band categorical, 100m EPSG:3035)
lum_files <- c(
  `2000` = file.path(input_dir, "LUM_BioClima_v2_2000.tif"),
  `2010` = file.path(input_dir, "LUM_BioClima_v2_2010.tif"),
  `2018` = file.path(input_dir, "LUM_BioClima_v2_2018.tif")
)

# --- 2. Validate Inputs -------------------------------------------------------
cli_alert_info("Validating input files...")
for (f in c(lum_files, buffer_grid_path)) {
  if (!file.exists(f)) stop("Missing required file: ", f)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cli_alert_success("Created output directory: {output_dir}")
}

# --- 3. Load INSPIRE 1km Buffer Grid -----------------------------------------
cli_alert_info("Loading template 1km buffer grid: {buffer_grid_path}")
r_buf       <- rast(buffer_grid_path)
buf_vals    <- values(r_buf, mat = FALSE)
mask_buf    <- !is.na(buf_vals)
inspire_ids <- as.integer(buf_vals[mask_buf])
n_buf_cells <- length(inspire_ids)

nr_1km      <- nrow(r_buf)
nc_1km      <- ncol(r_buf)
n_cells_1km <- ncell(r_buf)

cli_alert_success("Buffer grid loaded: {n_buf_cells} valid cells ({nr_1km} rows x {nc_1km} cols)")

# Lookup: INSPIRE ID -> raster cell index (for GeoTIFF back-mapping)
cell_map_dt <- data.table(
  cell_idx                    = which(mask_buf),
  INSPIRE_Europe_buffer_1kmID = inspire_ids
)

# --- 4. Discover All LUM Class Codes Across All Years -----------------------
# Lightweight scan (evenly-sampled rows) to build the full class universe
# before the main loop so band names and matrix dimensions are consistent.
cli_h2("Discovering LUM class universe across all years")

all_classes_found <- integer(0)
n_scan_rows <- 500L  # number of rows to sample per file

for (yr in years) {
  r_src   <- rast(lum_files[as.character(yr)])
  row_seq <- as.integer(seq(1L, nrow(r_src), length.out = n_scan_rows))
  for (ri in row_seq) {
    v       <- values(r_src, row = ri, nrows = 1L)
    v_valid <- v[!is.na(v)]
    if (length(v_valid) > 0L) {
      all_classes_found <- union(all_classes_found, as.integer(v_valid))
    }
  }
  cli_alert_info("Year {yr}: {length(all_classes_found)} distinct classes found so far (scan)")
}

lum_classes <- sort(all_classes_found)
n_classes   <- length(lum_classes)
cli_alert_success("LUM class universe: {n_classes} distinct codes ({min(lum_classes)}..{max(lum_classes)})")

# Fast O(1) class code -> column index lookup
max_class <- max(lum_classes)
idx_map   <- integer(max_class + 1L)
idx_map[lum_classes + 1L] <- seq_along(lum_classes)

# --- 5. Main Processing Loop: One Year at a Time ----------------------------
# Strip-based chunked processing: 100 km strips (100 rows of 1km raster
# = 1000 rows of 100m raster per strip). Avoids loading the full raster.

strip_size_1km <- 100L
n_strips       <- ceiling(nr_1km / strip_size_1km)
cli_alert_info("Strip processing: {n_strips} strips of {strip_size_1km} km each")

annual_results <- vector("list", length(years))
names(annual_results) <- as.character(years)

for (yr in years) {
  yr_chr        <- as.character(yr)
  r_src         <- rast(lum_files[yr_chr])
  strip_results <- vector("list", n_strips)
  t_yr          <- Sys.time()
  cli_h2("Processing year {yr}")

  for (s in seq_len(n_strips)) {
    r1_1km         <- (s - 1L) * strip_size_1km + 1L
    r2_1km         <- min(s * strip_size_1km, nr_1km)
    nrows_1km_curr <- r2_1km - r1_1km + 1L

    # Strip extent in EPSG:3035 (metre units)
    y_top   <- ymax(r_buf) - (r1_1km - 1L) * 1000
    y_bot   <- ymax(r_buf) - r2_1km * 1000
    e_strip <- ext(xmin(r_buf), xmax(r_buf), y_bot, y_top)

    # 1km buffer values for this strip
    buf_crop  <- crop(r_buf, e_strip)
    buf_strip <- values(buf_crop, mat = FALSE)

    # Skip empty strips (no valid buffer cells)
    if (!any(!is.na(buf_strip))) next

    # Crop 100m source raster
    r_crop <- crop(r_src, e_strip)
    m_100m <- as.integer(values(r_crop, mat = FALSE))

    # Map 100m pixels to parent 1km cell (local strip indices)
    nr_100m_curr   <- nrows_1km_curr * 10L
    nc_100m_curr   <- nc_1km * 10L
    row_100m       <- rep(seq_len(nr_100m_curr), each  = nc_100m_curr)
    col_100m       <- rep(seq_len(nc_100m_curr), times = nr_100m_curr)
    row_1km_local  <- (row_100m - 1L) %/% 10L + 1L
    col_1km_local  <- (col_100m - 1L) %/% 10L + 1L
    cell_local_idx <- (row_1km_local - 1L) * nc_1km + col_1km_local

    inspire_100m <- buf_strip[cell_local_idx]

    # Filter: valid buffer cell AND valid class value
    keep <- !is.na(inspire_100m) & !is.na(m_100m)
    if (!any(keep)) next

    dt_strip <- data.table(
      INSPIRE_Europe_buffer_1kmID = as.integer(inspire_100m[keep]),
      lum_class                   = m_100m[keep]
    )
    agg_strip         <- dt_strip[, .(pixel_count = .N),
                                   by = .(INSPIRE_Europe_buffer_1kmID, lum_class)]
    strip_results[[s]] <- agg_strip

    elapsed <- round(difftime(Sys.time(), t_yr, units = "mins"), 2)
    cli_alert_info("  Strip {s}/{n_strips} (rows {r1_1km}..{r2_1km}). Elapsed: {elapsed} min")
    gc(verbose = FALSE)
  }

  # Consolidate strips
  yr_dt <- rbindlist(strip_results[!sapply(strip_results, is.null)])
  rm(strip_results); gc()

  # Safety re-aggregation (handles strip boundary duplicates)
  yr_dt <- yr_dt[!is.na(INSPIRE_Europe_buffer_1kmID),
                 .(pixel_count = sum(pixel_count)),
                 by = .(INSPIRE_Europe_buffer_1kmID, lum_class)]

  # pixel_count = area_ha (100m pixel = 0.01 km^2 = 1 ha at this resolution;
  # but terra reports pixel dimensions as 100x100 m = 10000 m^2 = 1 ha)
  yr_dt[, area_ha := as.numeric(pixel_count)]
  yr_dt[, share   := round(pixel_count / 100.0, 4)]
  yr_dt[, pixel_count := NULL]
  yr_dt[, year := yr]

  annual_results[[yr_chr]] <- yr_dt
  cli_alert_success("Year {yr} done: {nrow(yr_dt)} class x cell records. Elapsed: {round(difftime(Sys.time(), t_yr, units='mins'), 2)} min")
  gc()
}

# --- 6. Master Parquet Table -------------------------------------------------
cli_h2("Saving master Parquet table")
dt_all <- rbindlist(annual_results)
setcolorder(dt_all, c("INSPIRE_Europe_buffer_1kmID", "year", "lum_class", "area_ha", "share"))
setorder(dt_all, INSPIRE_Europe_buffer_1kmID, year, lum_class)

parquet_path <- file.path(output_dir, "lum_bioclima_1km.parquet")
write_parquet(dt_all, parquet_path, compression = "zstd", compression_level = 7)
cli_alert_success("Saved: {parquet_path} ({round(file.size(parquet_path)/1024^2, 1)} MB)")

# Class universe CSV
class_csv_path <- file.path(output_dir, "lum_bioclima_class_universe.csv")
class_universe <- dt_all[, .(
  n_cells  = uniqueN(INSPIRE_Europe_buffer_1kmID),
  total_ha = sum(area_ha)
), by = .(lum_class, year)]
setorder(class_universe, year, lum_class)
fwrite(class_universe, class_csv_path)
cli_alert_success("Saved class universe: {class_csv_path}")

# --- 7. GeoTIFF Outputs per Year -------------------------------------------
cli_h2("Generating GeoTIFF outputs")

for (yr in years) {
  yr_chr  <- as.character(yr)
  yr_data <- annual_results[[yr_chr]]

  # ── 7a. Dominant class raster (modal class per 1km cell) ──────────────────
  cli_alert_info("  Dominant class raster for {yr}...")
  setorder(yr_data, INSPIRE_Europe_buffer_1kmID, -area_ha)
  dom_dt  <- unique(yr_data, by = "INSPIRE_Europe_buffer_1kmID")

  dom_vec <- rep(NA_integer_, n_cells_1km)
  m_idx   <- match(dom_dt$INSPIRE_Europe_buffer_1kmID, cell_map_dt$INSPIRE_Europe_buffer_1kmID)
  valid_m <- !is.na(m_idx)
  dom_vec[cell_map_dt$cell_idx[m_idx[valid_m]]] <- dom_dt$lum_class[valid_m]

  r_dom         <- rast(r_buf)
  values(r_dom) <- dom_vec
  names(r_dom)  <- paste0("LUM_BioClima_dominant_", yr)

  dom_path <- file.path(output_dir, paste0("lum_bioclima_dominant_", yr, ".tif"))
  writeRaster(r_dom, dom_path,
              datatype  = "INT2S",
              NAflag    = -32768L,
              gdal      = c("COMPRESS=DEFLATE", "PREDICTOR=2"),
              overwrite = TRUE)
  cli_alert_success("  Saved: {basename(dom_path)}")
  rm(r_dom, dom_vec); gc()

  # ── 7b. Multi-band class share raster (one band per class) ────────────────
  cli_alert_info("  {n_classes}-band class share raster for {yr}...")

  mat_yr <- matrix(NA_integer_, nrow = n_cells_1km, ncol = n_classes)
  mat_yr[cell_map_dt$cell_idx, ] <- 0L   # valid cells default to 0 ha

  for (k in seq_along(lum_classes)) {
    cls    <- lum_classes[k]
    sub_dt <- yr_data[lum_class == cls]
    if (nrow(sub_dt) == 0L) next
    m_k     <- match(sub_dt$INSPIRE_Europe_buffer_1kmID, cell_map_dt$INSPIRE_Europe_buffer_1kmID)
    valid_k <- !is.na(m_k)
    mat_yr[cell_map_dt$cell_idx[m_k[valid_k]], k] <- as.integer(round(sub_dt$area_ha[valid_k]))
  }

  r_shares         <- rast(r_buf, nlyrs = n_classes)
  names(r_shares)  <- paste0("LUM_", lum_classes)
  values(r_shares) <- mat_yr
  rm(mat_yr); gc()

  shares_path <- file.path(output_dir, paste0("lum_bioclima_shares_", yr, ".tif"))
  writeRaster(r_shares, shares_path,
              datatype  = "INT1U",
              NAflag    = 255L,
              gdal      = c("COMPRESS=DEFLATE", "PREDICTOR=2"),
              overwrite = TRUE)
  cli_alert_success("  Saved: {basename(shares_path)} ({n_classes} bands)")
  rm(r_shares); gc()
}

# --- 8. Final Summary --------------------------------------------------------
cli_h1("Pipeline Complete")
cli_alert_success("All outputs written to: {output_dir}")
cat("\nFiles generated:\n")
for (f in list.files(output_dir, full.names = TRUE)) {
  cat(sprintf("  %-60s  %.2f MB\n", basename(f), file.size(f) / 1024^2))
}
cat("\nClass universe (v2) :", n_classes, "distinct LUM class codes\n")
cat("Years processed     :", paste(years, collapse = ", "), "\n")
cat("Spatial key         : INSPIRE_Europe_buffer_1kmID\n")

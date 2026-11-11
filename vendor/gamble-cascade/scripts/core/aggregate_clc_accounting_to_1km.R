# ==============================================================================
# aggregate_clc_accounting_to_1km.R
# ==============================================================================
# Aggregates 100m CLC accounting layers (2000, 2006, 2012, 2018, 2024) to the 
# European 1km INSPIRE buffer grid using deterministic Radix-45 integer encoding.
#
# Outputs:
# 1. Parquet Tables:
#    - clc_trajectory_catalog.parquet (and .csv): Full codebook of unique 5-step trajectories
#    - clc_transitions_1km.parquet: Sparse 1km transition history (cell_id, trajectory_id, area_ha, share)
#    - clc_shares_1km_annual.parquet: Annual Level 3 class shares (cell_id, year, clc_code, area_ha, share)
# 2. GeoTIFFs (for GIS Visual Confirmation):
#    - clc_dominant_class_2000_2024.tif: 5-band raster of dominant CLC class per year (111..523)
#    - clc_transition_intensity_2000_2024.tif: Single band raster showing % of cell changed (0..100%)
#    - clc_shares_<year>.tif: Multi-band rasters of Level 3 class shares (0..100% in Byte)
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(data.table)
  library(arrow)
  library(foreign)
  library(cli)
})

cli_h1("Starting CLC Accounting 1km Aggregation Pipeline")

# --- 1. Configuration & Paths ---
input_dir <- "H:/R/LARGEDATAPROCESSING/CLCACC_2000_to_2024_v2026"
output_dir <- "H:/R/LARGEDATAPROCESSING/CLCACC_1km_aggregated"
buffer_grid_path <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

years <- c(2000, 2006, 2012, 2018, 2024)
clc_files <- c(
  y2000 = file.path(input_dir, "eea_r_3035_100_m_clc-2024-acc_p_2000_v21_r01/CLC2000ACC_V2024_21.tif"),
  y2006 = file.path(input_dir, "eea_r_3035_100_m_clc-2024-acc_p_2006_v21_r01/CLC2006ACC_V2024_21.tif"),
  y2012 = file.path(input_dir, "eea_r_3035_100_m_clc-2024-acc_p_2012_v21_r01/CLC2012ACC_V2024_21.tif"),
  y2018 = file.path(input_dir, "eea_r_3035_100_m_clc-2024-acc_p_2018_v21_r01/CLC2018ACC_V2024_21.tif"),
  y2024 = file.path(input_dir, "eea_r_3035_100_m_clc-2024-acc_p_2024_v21_r01/CLC2024ACC_V2024_21.tif")
)

# Verify input files exist
for (yf in clc_files) {
  if (!file.exists(yf)) stop("Missing CLC input raster: ", yf)
}
if (!file.exists(buffer_grid_path)) stop("Missing buffer grid: ", buffer_grid_path)

# --- 2. Load Legend / VAT & Setup Radix-45 Mapping ---
vat_path <- file.path(input_dir, "eea_r_3035_100_m_clc-2024-acc_p_2000_v21_r01/CLC2000ACC_V2024_21.tif.vat.dbf")
vat <- foreign::read.dbf(vat_path, as.is = TRUE)
clc_classes <- sort(as.integer(vat$VALUE))
n_classes <- length(clc_classes) # exactly 44
cli_alert_info("Found {n_classes} valid Level 3 CLC classes in legend.")

# Lookup table: 3-digit code -> 1..44 (0 for nodata/unclassified)
idx_map <- integer(600)
idx_map[clc_classes] <- seq_along(clc_classes)
idx_to_code <- c(0L, clc_classes)
rad <- 45L # Base 45 (44 classes + 0 nodata)

# Radix multipliers: rad^0, rad^1, rad^2, rad^3, rad^4
m0 <- 1L
m1 <- rad
m2 <- rad * rad
m3 <- rad * rad * rad
m4 <- rad * rad * rad * rad

# --- 3. Grid Geometry Setup ---
cli_alert_info("Reading template 1km buffer grid...")
r_buf <- rast(buffer_grid_path)
buf_extent <- ext(r_buf)
nr_1km <- nrow(r_buf)
nc_1km <- ncol(r_buf)
n_cells_1km <- ncell(r_buf)

r_stack <- rast(clc_files)

# Strip configuration: 100 rows of 1km (= 1000 rows of 100m)
strip_size_1km <- 100L
n_strips <- ceiling(nr_1km / strip_size_1km)
cli_alert_info("Processing grid in {n_strips} horizontal strips ({strip_size_1km} km per strip)...")

# --- 4. Main Chunked Processing Loop ---
strip_results <- vector("list", n_strips)
t_start <- Sys.time()

for (s in seq_len(n_strips)) {
  r1_1km <- (s - 1L) * strip_size_1km + 1L
  r2_1km <- min(s * strip_size_1km, nr_1km)
  nrows_1km_curr <- r2_1km - r1_1km + 1L
  
  # Strip extent in EPSG:3035
  y_top <- ymax(buf_extent) - (r1_1km - 1L) * 1000
  y_bot <- ymax(buf_extent) - r2_1km * 1000
  e_strip <- ext(xmin(buf_extent), xmax(buf_extent), y_bot, y_top)
  
  # 1km buffer values for this strip
  buf_crop <- crop(r_buf, e_strip)
  buf_vals <- values(buf_crop)
  
  # Skip strip if completely empty (e.g. open ocean north/south)
  valid_buf_mask <- !is.na(buf_vals)
  if (!any(valid_buf_mask)) {
    next
  }
  
  # Crop 100m raster stack for this strip
  r_crop <- crop(r_stack, e_strip)
  m_100m <- as.matrix(r_crop)
  
  # 100m to 1km cell index mapping within strip
  # Strip has (nrows_1km_curr * 10) rows and (nc_1km * 10) cols at 100m
  nr_100m_curr <- nrows_1km_curr * 10L
  nc_100m_curr <- nc_1km * 10L
  
  row_100m <- rep(seq_len(nr_100m_curr), each = nc_100m_curr)
  col_100m <- rep(seq_len(nc_100m_curr), times = nr_100m_curr)
  
  row_1km_local <- (row_100m - 1L) %/% 10L + 1L
  col_1km_local <- (col_100m - 1L) %/% 10L + 1L
  cell_1km_idx_local <- (row_1km_local - 1L) * nc_1km + col_1km_local
  
  # Get INSPIRE ID
  inspire_ids <- buf_vals[cell_1km_idx_local]
  
  # Map 100m values to indices 1..44
  p0 <- idx_map[as.integer(m_100m[, 1])]
  p1 <- idx_map[as.integer(m_100m[, 2])]
  p2 <- idx_map[as.integer(m_100m[, 3])]
  p3 <- idx_map[as.integer(m_100m[, 4])]
  p4 <- idx_map[as.integer(m_100m[, 5])]
  
  # Radix-45 packing (32-bit integer arithmetic)
  packed_vals <- p0 * m0 + p1 * m1 + p2 * m2 + p3 * m3 + p4 * m4
  
  # Fast filtering: only keep valid 1km buffer cells with valid land cover
  keep <- !is.na(inspire_ids) & (packed_vals > 0L)
  
  if (any(keep)) {
    dt_strip <- data.table(
      INSPIRE_Europe_buffer_1kmID = as.integer(inspire_ids[keep]),
      packed = packed_vals[keep]
    )
    # Count pixels per 1km cell and trajectory
    agg_strip <- dt_strip[, .(pixel_count = .N), by = .(INSPIRE_Europe_buffer_1kmID, packed)]
    strip_results[[s]] <- agg_strip
  }
  
  cli_alert_info("Strip {s}/{n_strips} processed (Rows {r1_1km}..{r2_1km}). Elapsed: {round(difftime(Sys.time(), t_start, units='mins'), 2)} min")
  gc(verbose = FALSE)
}

# --- 5. Consolidate & Build Master Trajectory Catalog ---
cli_alert_info("Consolidating strip results...")
all_trans <- rbindlist(strip_results)
rm(strip_results)
gc()

# Safety aggregation in case of any duplicate keys (and drop any unmapped buffer NA)
all_trans <- all_trans[!is.na(INSPIRE_Europe_buffer_1kmID), .(pixel_count = sum(pixel_count)), by = .(INSPIRE_Europe_buffer_1kmID, packed)]
setorder(all_trans, INSPIRE_Europe_buffer_1kmID, -pixel_count)

unique_packed <- sort(unique(all_trans$packed))
n_unique_traj <- length(unique_packed)
cli_alert_success("Total unique 5-step land cover trajectories across Europe: {n_unique_traj}")

# Decode all unique trajectories
cli_alert_info("Building trajectory codebook...")
dec_c0 <- idx_to_code[(unique_packed %% rad) + 1L]
dec_c1 <- idx_to_code[((unique_packed %/% rad) %% rad) + 1L]
dec_c2 <- idx_to_code[((unique_packed %/% (rad^2)) %% rad) + 1L]
dec_c3 <- idx_to_code[((unique_packed %/% (rad^3)) %% rad) + 1L]
dec_c4 <- idx_to_code[((unique_packed %/% (rad^4)) %% rad) + 1L]

traj_catalog <- data.table(
  trajectory_id = seq_along(unique_packed),
  packed = unique_packed,
  clc_2000 = dec_c0,
  clc_2006 = dec_c1,
  clc_2012 = dec_c2,
  clc_2018 = dec_c3,
  clc_2024 = dec_c4,
  trajectory_code = paste(dec_c0, dec_c1, dec_c2, dec_c3, dec_c4, sep = "_"),
  is_static = (dec_c0 == dec_c1 & dec_c1 == dec_c2 & dec_c2 == dec_c3 & dec_c3 == dec_c4)
)

# Number of transitions and first transition year
traj_catalog[, num_transitions := (clc_2000 != clc_2006) + (clc_2006 != clc_2012) + (clc_2012 != clc_2018) + (clc_2018 != clc_2024)]
traj_catalog[, first_change_year := fcase(
  clc_2000 != clc_2006, 2006L,
  clc_2006 != clc_2012, 2012L,
  clc_2012 != clc_2018, 2018L,
  clc_2018 != clc_2024, 2024L,
  default = NA_integer_
)]

# Add Level 1 macro-classes (1=Artificial, 2=Agricultural, 3=Forest/Semi-natural, 4=Wetlands, 5=Water)
l1_map <- integer(600)
for (cls in clc_classes) {
  l1_map[cls] <- as.integer(substr(as.character(cls), 1, 1))
}
traj_catalog[, trajectory_l1 := paste(
  l1_map[clc_2000], l1_map[clc_2006], l1_map[clc_2012], l1_map[clc_2018], l1_map[clc_2024], sep = "_"
)]

# Save Trajectory Catalog
catalog_pq_path <- file.path(output_dir, "clc_trajectory_catalog.parquet")
catalog_csv_path <- file.path(output_dir, "clc_trajectory_catalog.csv")
write_parquet(traj_catalog, catalog_pq_path)
fwrite(traj_catalog, catalog_csv_path)
cli_alert_success("Saved trajectory catalog to {catalog_pq_path}")

# --- 6. Save Product 1: 1km Transition History Parquet ---
cli_alert_info("Building sparse 1km transition history table...")
all_trans[traj_catalog, on = .(packed), trajectory_id := i.trajectory_id]
all_trans[, share := round(pixel_count / 100.0, 4)]
setnames(all_trans, "pixel_count", "area_ha")
trans_out <- all_trans[, .(INSPIRE_Europe_buffer_1kmID, trajectory_id, area_ha, share)]

trans_pq_path <- file.path(output_dir, "clc_transitions_1km.parquet")
write_parquet(trans_out, trans_pq_path)
cli_alert_success("Saved 1km transition history ({nrow(trans_out)} rows) to {trans_pq_path}")

# --- 7. Save Product 2: Annual Level 3 Class Shares Parquet ---
cli_alert_info("Computing annual Level 3 class shares...")
# Join trajectory catalog classes to transitions
all_trans_full <- merge(all_trans, traj_catalog[, .(trajectory_id, clc_2000, clc_2006, clc_2012, clc_2018, clc_2024)], by = "trajectory_id")

annual_shares_list <- list()
for (yr in years) {
  col_name <- paste0("clc_", yr)
  dt_yr <- all_trans_full[, .(area_ha = sum(area_ha)), by = .(INSPIRE_Europe_buffer_1kmID, clc_code = get(col_name))]
  dt_yr[, year := as.integer(yr)]
  dt_yr[, share := round(area_ha / 100.0, 4)]
  annual_shares_list[[as.character(yr)]] <- dt_yr
}
clc_shares_annual <- rbindlist(annual_shares_list)
setcolorder(clc_shares_annual, c("INSPIRE_Europe_buffer_1kmID", "year", "clc_code", "area_ha", "share"))

annual_pq_path <- file.path(output_dir, "clc_shares_1km_annual.parquet")
write_parquet(clc_shares_annual, annual_pq_path)
cli_alert_success("Saved annual class shares ({nrow(clc_shares_annual)} rows) to {annual_pq_path}")

# Free intermediate full join
rm(all_trans_full)
gc()

# --- 8. Build & Save GeoTIFFs for Visual Confirmation ---
cli_h2("Generating GeoTIFF Rasters for Visual Inspection")

# Precompute cell mapping to buffer template
cli_alert_info("Preparing 1km raster canvas...")
buf_cells <- values(r_buf) # 16M cells
valid_idx <- which(!is.na(buf_cells))
inspire_ids_valid <- buf_cells[valid_idx]

# Lookup table: INSPIRE_Europe_buffer_1kmID -> raster cell index
cell_map_dt <- data.table(
  cell_idx = valid_idx,
  INSPIRE_Europe_buffer_1kmID = as.integer(inspire_ids_valid)
)

# 8.1. Dominant CLC Class per Year (5-band GeoTIFF)
cli_alert_info("Creating dominant CLC class raster (5 bands)...")
dom_mat <- matrix(NA_integer_, nrow = n_cells_1km, ncol = length(years))
colnames(dom_mat) <- paste0("Dominant_CLC_", years)

for (idx_yr in seq_along(years)) {
  yr <- years[idx_yr]
  yr_shares <- clc_shares_annual[year == yr]
  
  # Fast dominant class per cell via setorder + unique
  setorder(yr_shares, INSPIRE_Europe_buffer_1kmID, -area_ha)
  dom_dt <- unique(yr_shares, by = "INSPIRE_Europe_buffer_1kmID")
  
  m_idx <- match(dom_dt$INSPIRE_Europe_buffer_1kmID, cell_map_dt$INSPIRE_Europe_buffer_1kmID)
  valid_m <- !is.na(m_idx)
  dom_mat[cell_map_dt$cell_idx[m_idx[valid_m]], idx_yr] <- dom_dt$clc_code[valid_m]
}

r_dom_stack <- rast(r_buf, nlyrs = length(years))
names(r_dom_stack) <- paste0("Dominant_CLC_", years)
values(r_dom_stack) <- dom_mat
rm(dom_mat)
gc()

dom_tif_path <- file.path(output_dir, "clc_dominant_class_2000_2024.tif")
writeRaster(r_dom_stack, dom_tif_path, datatype = "INT2U", NAflag = 0, gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2"), overwrite = TRUE)
cli_alert_success("Saved dominant CLC class GeoTIFF: {dom_tif_path}")

# 8.2. Transition Intensity (2000 - 2024 Land Change %)
cli_alert_info("Creating transition intensity raster (2000-2024)...")
# Dynamic trajectories: sum area_ha where is_static is FALSE
static_traj_ids <- traj_catalog[is_static == TRUE, trajectory_id]
change_dt <- all_trans[!trajectory_id %in% static_traj_ids, .(change_ha = sum(area_ha)), by = INSPIRE_Europe_buffer_1kmID]

change_vec <- rep(NA_integer_, n_cells_1km)
change_vec[cell_map_dt$cell_idx] <- 0L # default valid cells to 0% change

m_idx <- match(change_dt$INSPIRE_Europe_buffer_1kmID, cell_map_dt$INSPIRE_Europe_buffer_1kmID)
valid_m <- !is.na(m_idx)
change_vec[cell_map_dt$cell_idx[m_idx[valid_m]]] <- as.integer(change_dt$change_ha[valid_m])

r_change <- rast(r_buf)
values(r_change) <- change_vec
names(r_change) <- "Change_Intensity_pct"
rm(change_vec)
gc()

change_tif_path <- file.path(output_dir, "clc_transition_intensity_2000_2024.tif")
writeRaster(r_change, change_tif_path, datatype = "INT1U", NAflag = 255, gdal = c("COMPRESS=DEFLATE"), overwrite = TRUE)
cli_alert_success("Saved transition intensity GeoTIFF: {change_tif_path}")

# 8.3. Multi-band Class Share GeoTIFFs (per Year)
cli_alert_info("Creating multi-band class share GeoTIFFs for each year...")
for (yr in years) {
  cli_alert_info("Writing 44-band share GeoTIFF for {yr}...")
  yr_shares <- clc_shares_annual[year == yr]
  
  # Allocate matrix: n_cells_1km x 44 classes
  mat_yr <- matrix(NA_integer_, nrow = n_cells_1km, ncol = n_classes)
  mat_yr[cell_map_dt$cell_idx, ] <- 0L # default valid cells to 0% share
  
  for (k in seq_along(clc_classes)) {
    cls <- clc_classes[k]
    sub_dt <- yr_shares[clc_code == cls]
    if (nrow(sub_dt) > 0) {
      m_idx <- match(sub_dt$INSPIRE_Europe_buffer_1kmID, cell_map_dt$INSPIRE_Europe_buffer_1kmID)
      valid_m <- !is.na(m_idx)
      mat_yr[cell_map_dt$cell_idx[m_idx[valid_m]], k] <- as.integer(sub_dt$area_ha[valid_m])
    }
  }
  
  r_yr_stack <- rast(r_buf, nlyrs = n_classes)
  names(r_yr_stack) <- paste0("CLC_", clc_classes)
  values(r_yr_stack) <- mat_yr
  rm(mat_yr)
  gc()
  
  yr_tif_path <- file.path(output_dir, paste0("clc_shares_", yr, ".tif"))
  writeRaster(r_yr_stack, yr_tif_path, datatype = "INT1U", NAflag = 255, gdal = c("COMPRESS=DEFLATE"), overwrite = TRUE)
  cli_alert_success("Saved {yr} shares GeoTIFF: {yr_tif_path}")
}

cli_h1("Pipeline Finished Successfully!")
cli_alert_success("All Parquet and GeoTIFF outputs saved in: {output_dir}")

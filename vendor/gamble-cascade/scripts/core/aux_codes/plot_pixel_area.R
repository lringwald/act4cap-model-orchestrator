library(data.table)
library(terra)
library(cli)

# Source the raster utility
source("scripts/results_to_raster.R")

# 1. Config
rds_path <- "temp/processed_ds_start_map_5km_20260426.rds"
template_1km_path <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"
output_path <- "temp/pixel_area_5km.tif"

# 2. Logic
cli_h1("Processing Pixel Area Map")

if (!file.exists(rds_path)) {
  stop("Input RDS file not found: ", rds_path)
}

if (!file.exists(template_1km_path)) {
  # Try alternative locations or subdirectories
  stop("1km Template GIS file not found: ", template_1km_path)
}

message(">>> Creating 5km template from 1km buffer...")
r1km <- rast(template_1km_path)
template_5km <- aggregate(r1km, fact = 5, fun = "mean", na.rm = TRUE)
values(template_5km) <- 1:ncell(template_5km)

message(">>> Loading RDS data...")
dt <- readRDS(rds_path)

# Calculate total area per pixel (ns) for the "Total" layer
message(">>> Summarizing areas...")
total_dt <- dt[, .(LUM_area_km2 = sum(LUM_area_km2)), by = .(ns)]
total_dt[, GLOB_lu_class := "TOTAL_AREA"]

# Combine with individual classes if we want a multi-layer TIFF
# but the user specifically asked for "total pixel area of this file".
# I'll provide both for completeness.
plot_dt <- rbind(
  dt[, .(ns, GLOB_lu_class, LUM_area_km2)],
  total_dt[, .(ns, GLOB_lu_class, LUM_area_km2)]
)

# 3. Raster Generation
cli_alert_info("Generating Multi-Layer GeoTIFF (Classes + Total)...")
results_to_raster(
  results     = plot_dt,
  template_path = template_5km,
  class_col   = "GLOB_lu_class",
  value_col   = "LUM_area_km2",
  output_path = output_path
)

# Also save a dedicated single-layer total area check
total_output_path <- "temp/total_area_check_5km.tif"
cli_alert_info("Generating Single-Layer Total Area Check...")
results_to_raster(
  results     = total_dt,
  template_path = template_5km,
  class_col   = "GLOB_lu_class",
  value_col   = "LUM_area_km2",
  output_path = total_output_path
)

cli_alert_success("GeoTIFFs successfully saved to:")
cli_li("{output_path}")
cli_li("{total_output_path}")


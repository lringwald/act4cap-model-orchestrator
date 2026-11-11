# Process SPEI48 netCDF layer to the LAMASUS 1km buffer grid for 2000, 2010 and 2018
# Author: Antigravity / Leopold

library(terra)
library(data.table)

# --- 1. Configurations & Settings ---
spei_path <- "H:/R/LARGEDATAPROCESSING/spei48.nc"
lamasus_buffer_path <- "aux_files/INSPIRE_Europe_buffer_1km.tif"
output_dir <- "output/SPEI_aggregated"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# --- 2. Load Reference Template Raster ---
cat("Loading reference template raster:", lamasus_buffer_path, "...\n")
if (!file.exists(lamasus_buffer_path)) stop("Template raster not found!")
r_template <- terra::rast(lamasus_buffer_path)
template_crs <- terra::crs(r_template)
template_ext <- terra::ext(r_template)

# Get buffer IDs from the template raster as a vector
cat("Extracting template cell values...\n")
template_vals <- terra::values(r_template, mat = FALSE)

# --- 3. Process SPEI file ---
cat("Loading SPEI dataset...\n")
r_spei <- terra::rast(spei_path)
spei_crs <- terra::crs(r_spei)

# Identify time indices for 2000, 2010 and 2018
spei_time <- terra::time(r_spei)
idx_2000 <- which(format(spei_time, "%Y-%m") == "2000-12")
idx_2010 <- which(format(spei_time, "%Y-%m") == "2010-12")
idx_2018 <- which(format(spei_time, "%Y-%m") == "2018-12")

cat("Extracting specific years...\n")
r_spei_2000 <- r_spei[[idx_2000]]
r_spei_2010 <- r_spei[[idx_2010]]
r_spei_2018 <- r_spei[[idx_2018]]
names(r_spei_2000) <- paste0("spei48_", format(spei_time[idx_2000], "%Y_%m"))
names(r_spei_2010) <- paste0("spei48_", format(spei_time[idx_2010], "%Y_%m"))
names(r_spei_2018) <- paste0("spei48_", format(spei_time[idx_2018], "%Y_%m"))

# Combine years into a single raster stack
r_spei_subset <- c(r_spei_2000, r_spei_2010, r_spei_2018)

# Reproject the extent of the template to the SPEI CRS for fast cropping
cat("Cropping global raster to template extent...\n")
ext_spei_crs <- terra::project(r_template, spei_crs)
r_cropped <- terra::crop(r_spei_subset, ext_spei_crs)

# Project and resample the cropped raster to match the template exactly
cat("Projecting and resampling to template grid (bilinear)...\n")
r_projected <- terra::project(r_cropped, r_template, method = "bilinear")

cat("Masking to LAMASUS buffer pixels...\n")
r_projected <- terra::mask(r_projected, r_template)

# Extract values as data.table
cat("Extracting and matching values...\n")
projected_vals_df <- as.data.frame(r_projected, na.rm = FALSE)
dt_aligned <- as.data.table(projected_vals_df)

# Attach buffer ID
dt_aligned$INSPIRE_Europe_buffer_1kmID <- as.integer(template_vals)

# Put ID as first column
setcolorder(dt_aligned, "INSPIRE_Europe_buffer_1kmID")

# Filter out cells outside the study area (where template ID is NA)
dt_aligned <- dt_aligned[!is.na(INSPIRE_Europe_buffer_1kmID)]

# Save RDS
out_rds <- file.path(output_dir, "spei48_2000_2010_2018_masked_aggregated.rds")
saveRDS(dt_aligned, out_rds)
cat("Saved RDS to:", out_rds, " (", nrow(dt_aligned), " records)\n")

# Save projected 1km GeoTIFF
out_tif <- file.path(output_dir, "spei48_2000_2010_2018_masked_1km.tif")
terra::writeRaster(r_projected, out_tif, overwrite = TRUE)
cat("Saved aligned 1km TIFF to:", out_tif, "\n")

cat("\n==================================================\n")
cat("SPEI processing complete!\n")

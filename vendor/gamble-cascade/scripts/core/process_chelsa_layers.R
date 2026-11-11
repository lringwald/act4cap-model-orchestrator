# Process CHELSA climatology layers to the LAMASUS 1km buffer grid
# Author: Antigravity / Leopold
# Date: 2026-05-19

library(terra)
library(data.table)

# --- 1. Configurations & Settings ---
chelsa_dir <- "H:/R/LARGEDATAPROCESSING/CHELSA"
lamasus_buffer_path <- "aux_files/INSPIRE_Europe_buffer_1km.tif"
output_dir <- "output/CHELSA_aggregated"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# --- 2. Load Reference Template Raster ---
cat("Loading reference template raster:", lamasus_buffer_path, "...\n")
if (!file.exists(lamasus_buffer_path)) stop("Template raster not found!")
r_template <- terra::rast(lamasus_buffer_path)

# Extract reference template properties
template_crs <- terra::crs(r_template)
template_ext <- terra::ext(r_template)

# Get buffer IDs from the template raster as a vector
cat("Extracting template cell values...\n")
template_vals <- terra::values(r_template, mat = FALSE)

# --- 3. Process CHELSA TIFF files ---
tif_files <- list.files(chelsa_dir, pattern = "\\.tif$", full.names = TRUE)
if (length(tif_files) == 0) {
  stop("No CHELSA TIFF files found in: ", chelsa_dir)
}

cat("Found", length(tif_files), "CHELSA files to process.\n")

for (tif_path in tif_files) {
  var_name <- tools::file_path_sans_ext(basename(tif_path))
  cat("\n==================================================\n")
  cat("Processing:", var_name, "\n")

  # Load global CHELSA raster
  r_chelsa <- terra::rast(tif_path)
  chelsa_crs <- terra::crs(r_chelsa)

  # Reproject the extent of the template to the CHELSA CRS for fast cropping
  cat("  Cropping global raster to template extent...\n")
  ext_chelsa_crs <- terra::project(r_template, chelsa_crs)
  r_cropped <- terra::crop(r_chelsa, ext_chelsa_crs)

  # Project and resample the cropped raster to match the template exactly
  cat("  Projecting and resampling to template grid (bilinear)...\n")
  r_projected <- terra::project(r_cropped, r_template, method = "bilinear")
  
  cat("  Masking to LAMASUS buffer pixels...\n")
  r_projected <- terra::mask(r_projected, r_template)

  # Extract values
  cat("  Extracting and matching values...\n")
  projected_vals <- terra::values(r_projected, mat = FALSE)

  # Build aligned data.table
  indicator_code <- strsplit(var_name, "_")[[1]][2]
  
  # Map to descriptive label
  indicator_labels <- c(
    "bio12" = "Annual_Precipitation_bio12",
    "bio15" = "Precipitation_Seasonality_bio15",
    "gdd5"  = "Growing_Degree_Days_gdd5"
  )
  
  if (indicator_code %in% names(indicator_labels)) {
    indicator <- indicator_labels[[indicator_code]]
  } else {
    indicator <- indicator_code
  }

  dt_aligned <- data.table(
    INSPIRE_Europe_buffer_1kmID = as.integer(template_vals)
  )
  dt_aligned[[indicator]] <- projected_vals

  # Filter out cells outside the study area (where template ID is NA)
  dt_aligned <- dt_aligned[!is.na(INSPIRE_Europe_buffer_1kmID)]

  # Save RDS
  out_rds <- file.path(output_dir, paste0(var_name, "_masked_aggregated.rds"))
  saveRDS(dt_aligned, out_rds)
  cat("  Saved RDS to:", out_rds, " (", nrow(dt_aligned), " records)\n")

  # Save projected 1km GeoTIFF for validation / GIS use
  out_tif <- file.path(output_dir, paste0(var_name, "_masked_1km.tif"))
  r_out <- r_template
  terra::values(r_out) <- projected_vals # Re-assign mapped values to the template geometry
  names(r_out) <- indicator
  terra::writeRaster(r_out, out_tif, overwrite = TRUE)
  cat("  Saved aligned 1km TIFF to:", out_tif, "\n")

  # Print summary statistics for verification
  cat("\n  Summary Statistics for", var_name, ":\n")
  print(summary(dt_aligned[[indicator]]))

  # Clean up memory
  rm(r_chelsa, r_cropped, r_projected, r_out, dt_aligned); gc()
}

cat("\n==================================================\n")
cat("CHELSA processing complete!\n")

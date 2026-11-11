library(terra)
library(data.table)

# Configuration
esdb_root <- "input/ESDB-Raster-Library-1k-GeoTIFF-20240507"
lamasus_buffer_path <- "aux_files/INSPIRE_Europe_buffer_1km.tif"
output_dir <- "output/ESDAC"

# Create output directory
if(!dir.exists("output")) dir.create("output")
if(!dir.exists(output_dir)) dir.create(output_dir)

# Load LAMASUS Buffer IDs Raster
# This raster contains the buffer IDs we want to aggregate to.
# Since it's exactly the same resolution and aligned with ESDB, we can map directly.
cat("Loading LAMASUS 1km buffer raster from:", lamasus_buffer_path, "...\n")
if(!file.exists(lamasus_buffer_path)) stop("LAMASUS buffer raster not found!")
r_lamasus <- rast(lamasus_buffer_path)
ids_lamasus <- values(r_lamasus)

# Variables to process
esdac_variables <- c(
  "WRBLV1", "VS", "IL", "DIMP", "DR", "ROO", "AWC_TOP", "AWC_SUB", 
  "WR", "WM1", "WM2", "OC_TOP", "BS_TOP", "CEC_TOP", "ERODI"
)

# Processing Loop
for(var_name in esdac_variables){
  
  out_path <- file.path(output_dir, paste0(var_name, "_aggregated.rds"))
  if(file.exists(out_path)){
    cat("Skipping", var_name, "- Output already exists.\n")
    next
  }
  
  tif_path <- file.path(esdb_root, var_name, paste0(var_name, ".tif"))
  if(!file.exists(tif_path)){
    cat("Warning:", tif_path, "not found. Skipping.\n")
    next
  }
  
  cat("\nProcessing variable:", var_name, "\n")
  
  # Load ESDB Raster
  r_esdb <- rast(tif_path)
  
  # Optimization: Direct cell-to-cell lookup
  # Since both rasters are aligned 1km grids in EPSG:3035,
  # we simply crop ESDB to the LAMASUS extent and extract values.
  cat("  Cropping and extracting values...\n")
  r_esdb_cropped <- crop(r_esdb, r_lamasus)
  
  # Ensure dimensions match (they should if grids are aligned)
  if(any(dim(r_esdb_cropped) != dim(r_lamasus))){
    cat("  Warning: Dimensions do not match exactly. Falling back to resampling...\n")
    r_esdb_cropped <- resample(r_esdb, r_lamasus, method = "near")
  }
  
  val_esdb <- values(r_esdb_cropped)
  
  # Build and Aggregate
  cat("  Aggregating results...\n")
  dt_agg <- data.table(
    INSPIRE_Europe_buffer_1kmID = as.vector(ids_lamasus),
    value = as.vector(val_esdb)
  )
  
  # Remove NAs and aggregate areas
  # Since each pixel is 1km x 1km, each observation is 1 km2.
  dt_agg <- dt_agg[!is.na(INSPIRE_Europe_buffer_1kmID) & !is.na(value), 
                   .(area_km2 = .N), 
                   by = .(INSPIRE_Europe_buffer_1kmID, value)]
  
  # Save
  saveRDS(dt_agg, out_path)
  cat("  Saved to:", out_path, "\n")
  cat("  Processed", length(unique(dt_agg$INSPIRE_Europe_buffer_1kmID)), "buffer IDs.\n")
  
  # Clean up per variable
  rm(r_esdb, r_esdb_cropped, val_esdb, dt_agg); gc()
}

cat("\nAggregation complete.\n")

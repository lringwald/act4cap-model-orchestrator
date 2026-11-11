# Add library path if needed
library(terra)
library(data.table)

# Set directories
yield_data_file <- "data/aux_files/EQ_yield_index_summary.rds"
simuid_raster_file <- "C:/Users/ringwald/IIASA/LAMASUS - IIASA documents/8. Work Packages/WP 5/EPIC-IIASA/Cropland/Auxiliary/EUEPIC_raster_1k_updt_agg.tif"
bufferid_raster_file <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"
output_file <- "data/aux_files/EQ_yield_index_1km_mapped.rds"

message("Loading yield summary data...")
yield_dt <- readRDS(yield_data_file)
setDT(yield_dt)

message("Loading rasters...")
r_simuid <- rast(simuid_raster_file)
r_bufferid <- rast(bufferid_raster_file)
names(r_bufferid) <- "INSPIRE_Europe_buffer_1kmID"
# Ensure they align
message("Checking raster alignment...")
if (!compareGeom(r_simuid, r_bufferid, stopOnError = FALSE)) {
  message("Rasters do not align perfectly. Projecting SimUID to match bufferID...")
  r_simuid <- project(r_simuid, r_bufferid, method = "near")
}

message("Extracting mapping values (memory efficient)...")
# Extract values as a matrix/vector to avoid xy overhead
v_sim <- values(r_simuid)
v_buff <- values(r_bufferid)

dt_mapping <- data.table(
  SimUID = as.vector(v_sim),
  INSPIRE_Europe_buffer_1kmID = as.vector(v_buff)
)

# Remove NAs
message("Filtering valid mappings...")
dt_mapping <- dt_mapping[!is.na(SimUID) & !is.na(INSPIRE_Europe_buffer_1kmID)]

# Ensure unique mapping entries
dt_mapping <- unique(dt_mapping)

message(paste("Found", nrow(dt_mapping), "unique SimUID-bufferID mappings."))

# Join yield data to mapping
message("Joining yield index to 1km bufferIDs...")
yield_1km <- merge(dt_mapping, yield_dt, by = "SimUID", allow.cartesian = TRUE)
yield_1km[, SimUID := NULL]
yield_1km[, num_crops := NULL]

message(paste("Saving final mapped results to", output_file))
saveRDS(yield_1km, output_file)

message("Process complete. Sample of mapped data:")
print(head(yield_1km))
message(paste("Total rows in mapped data:", nrow(yield_1km)))

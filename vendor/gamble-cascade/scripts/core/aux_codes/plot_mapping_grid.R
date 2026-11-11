library(data.table)
library(terra)
library(cli)

# Source the raster utility
source("scripts/results_to_raster.R")

# 1. Config
mapping_path <- "data/aux_files/one_kmID_master_mapping_2026-05-28.rds"
template_path <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"
output_path  <- "temp/germany_mapping_grid_1km_v2.tif"

# 2. Logic
cli_h1("Processing Germany Mapping Grid Plot (1km)")

if (!file.exists(mapping_path)) {
  stop("Mapping RDS file not found: ", mapping_path)
}

if (!file.exists(template_path)) {
  stop("Template GIS file not found: ", template_path)
}

message(">>> Loading mapping grid data...")
mapping_grid <- readRDS(mapping_path)
setDT(mapping_grid)

if (!"TERRA_GLOBIOM" %in% names(mapping_grid)) {
  mapping_grid$TERRA_GLOBIOM <- as.numeric(mapping_grid$GLOB_SimUID != 0 & mapping_grid$GLOB_SimUID!="GLOB_NA")
}

message(">>> Filtering for Germany...")
de_grid <- mapping_grid[GLOB_country == "Germany"]

if (nrow(de_grid) == 0) {
  stop("No data found for Germany in mapping grid.")
}

# Identify GLOB columns
glob_cols <- names(de_grid)[grepl("^GLOB_", names(de_grid))]
glob_cols <- setdiff(glob_cols, "GLOB_country")

# Handle categorical columns
message(">>> Handling categorical columns (mapping to integers)...")
char_cols <- names(de_grid)[sapply(de_grid, function(x) is.character(x) | is.factor(x))]
char_cols <- intersect(char_cols, c(glob_cols, "TERRA_GLOBIOM"))

if (length(char_cols) > 0) {
  for (col in char_cols) {
    message("  - Mapping column: ", col)
    # Convert to factor and then to integer
    f <- as.factor(de_grid[[col]])
    levels_map <- levels(f)
    de_grid[, (col) := as.numeric(f)]
    # Optional: save the mapping to a log or rds if needed later
  }
}

# Ensure all columns are numeric
num_cols <- c(glob_cols, "TERRA_GLOBIOM")
de_grid[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# Prepare for results_to_raster (which expects long format: ns, value, layer_name)
message(">>> Preparing 1km data for GeoTIFF...")

id_col <- "INSPIRE_Europe_buffer_1kmID"
if (!id_col %in% names(de_grid)) {
  id_col <- "EEA_1kmID" # fallback
}

plot_dt <- melt(de_grid[, c(id_col, num_cols), with = FALSE], 
                id.vars = id_col, 
                variable.name = "layer_name", 
                value.name = "value")

setnames(plot_dt, id_col, "ns")

# 3. Raster Generation
cli_alert_info("Generating Multi-Layer GeoTIFF (1km)...")
results_to_raster(
  results       = plot_dt,
  template_path = template_path,
  class_col     = "layer_name",
  value_col     = "value",
  output_path   = output_path
)

cli_alert_success("GeoTIFF successfully saved to: {output_path}")


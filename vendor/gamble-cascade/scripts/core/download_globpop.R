# Download GlobPOP Population Count from GEE
# Dataset: projects/sat-io/open-datasets/GLOBPOP_COUNT
# Author: Antigravity / Leopold

library(rgee)
library(sf)
library(terra)

# --- 1. Initialize ---
ee_project <- Sys.getenv("EE_PROJECT")
if (ee_project == "") stop("Error: EE_PROJECT not found in environment. Please add it to your .Renviron file.")
Sys.setenv(EARTHENGINE_PROJECT = ee_project)
ee$Initialize(project = ee_project)

# --- 2. Load Collection and Get IDs ---
pop_col_id <- "projects/sat-io/open-datasets/GLOBPOP_COUNT"
pop_collection <- ee$ImageCollection(pop_col_id)

# Get all available image IDs to ensure we don't request a "null" year
all_pop_ids <- pop_collection$aggregate_array("system:index")$getInfo()

# --- 3. Define Years and Extent ---
target_years <- c(1995, 2000, 2005, 2010, 2015, 2020)
aoi <- ee$Geometry$Rectangle(coords = c(-12.0, 34.0, 35.0, 72.0))

# --- 4. Loop through years and download ---
for (yr in target_years) {
  cat("\n--- Locating GlobPOP Year:", yr, "---\n")

  # Search the ID list for the specific year string
  # GlobPOP IDs usually look like "GlobPOP_Count_1995"
  match <- all_pop_ids[grep(as.character(yr), all_pop_ids)]

  if (length(match) == 0) {
    cat("Warning: No GlobPOP image found for year", yr, ". Skipping...\n")
    next
  }

  # Select the specific image by its full path
  pop_img <- ee$Image(paste0(pop_col_id, "/", match[1]))

  # Define filename
  out_name <- paste0("GlobPOP_Count_", yr, "_EU_UK")

  try({
    task <- ee_image_to_drive(
      image = pop_img,
      description = out_name,
      folder = "EE_Downloads",
      region = aoi,
      scale = 1000, # Resample to 1km to match GDP and GHM
      fileFormat = "GeoTIFF"
    )

    task$start()
    cat("Success: Task submitted for", yr, "\n")
  }, silent = FALSE)
}

# Check total queue status
cat("\n--- Current Queue Status ---\n")
ee_monitoring()

# Download GDP Data from Google Earth Engine
# Dataset: projects/sat-io/open-datasets/GRIDDED_HDI_GDP/total_gdp_perCapita_1990_2020_30arcsec
# Author: Antigravity / Leopold
# Date: 2026-04-02

library(rgee)
library(sf)
library(terra)
reticulate::py_require("earthengine-api")
# --- 1. Fix the "No such file" Windows Bug ---
# We manually create the path rgee is looking for to stop the "cannot open connection" error
path_1 <- "C:/Users/ringwald/.config/earthengine/"
path_2 <- "C:/Users/ringwald/.config/earthengine/leopold.ringwald@gmail.com/"

if (!dir.exists(path_1)) dir.create(path_1, recursive = TRUE)
if (!dir.exists(path_2)) dir.create(path_2, recursive = TRUE)

# Create the dummy session files in both locations
writeLines("fixed", file.path(path_1, "rgee_sessioninfo.txt"))
writeLines("fixed", file.path(path_2, "rgee_sessioninfo.txt"))

# --- 2. Initialize Earth Engine ---
ee_project <- Sys.getenv("EE_PROJECT")
if (ee_project == "") stop("Error: EE_PROJECT not found in environment. Please add it to your .Renviron file.")
Sys.setenv(EARTHENGINE_PROJECT = ee_project)
# Use the direct Python bridge to bypass the expired credential false-positive
ee$Initialize(project = ee_project)

# Verify connection
cat("Connection Status: ", ee$String("Success")$getInfo(), "\n")

# --- 3. Load Dataset and Define Parameters ---
gdp_img <- ee$Image("projects/sat-io/open-datasets/GRIDDED_HDI_GDP/total_gdp_perCapita_1990_2020_30arcsec")
target_years <- c(1995, 2000, 2005, 2010, 2015, 2020)

# Bounding box for Continental EU + UK
aoi <- ee$Geometry$Rectangle(coords = c(-12.0, 34.0, 35.0, 72.0))

# --- 4. Loop and SUBMIT TASKS (Much faster for large areas) ---
for (year in target_years) {
  cat("\n--- Submitting Year:", year, "---\n")

  band_name <- paste0("PPP_", year)
  gdp_band <- gdp_img$select(band_name)
  task_name <- paste0("GDP_perCapita_", year, "_EU_UK")

  # This sends the command to Google's cloud.
  # It does NOT wait for the download to finish.
  task <- ee_image_to_drive(
    image = gdp_band,
    description = task_name,
    folder = "EE_Downloads", # This folder will be created in your Google Drive
    region = aoi,
    scale = 1000,
    fileFormat = "GeoTIFF"
  )

  # Start the task on the Google server
  task$start()
  cat("Task started! Check progress in your browser or with ee_monitoring()\n")
}

# --- 5. Monitor progress ---
# Run this line to see the status of all your years
ee_monitoring()

# Download Rural Access Index (RAI) Data from GEE
# Dataset: projects/sat-io/open-datasets/RAI/ruralpopaccess
# Author: Antigravity / Leopold

library(rgee)
library(sf)
library(terra)

# --- 1. Initialize ---
ee_project <- Sys.getenv("EE_PROJECT")
if (ee_project == "") stop("Error: EE_PROJECT not found in environment. Please add it to your .Renviron file.")
Sys.setenv(EARTHENGINE_PROJECT = ee_project)
ee$Initialize(project = ee_project)

# --- 2. Load RAI Dataset ---
# Options: 'raimultiplier', 'ruralpop', or 'ruralpopaccess'
rai_img <- ee$Image('projects/sat-io/open-datasets/RAI/raimultiplier')

# --- 3. Define Extent (Same EU + UK Bounding Box) ---
aoi <- ee$Geometry$Rectangle(coords = c(-12.0, 34.0, 35.0, 72.0))

# --- 4. Submit Download Task ---
out_file_rai <- "RAI_Rural_Pop_Access_EU_UK.tif"

cat("Submitting RAI download task to Google Drive...\n")

try({
  task <- ee_image_to_drive(
    image = rai_img,
    description = "RAI_Rural_Access_Export",
    folder = "EE_Downloads",
    region = aoi,
    scale = 1000, # 1km resolution to match your GDP data
    fileFormat = "GeoTIFF"
  )

  task$start()
  cat("Task started! Monitor with ee_monitoring() or check Google Drive folder 'EE_Downloads'.\n")
}, silent = FALSE)

# Check status
ee_monitoring()

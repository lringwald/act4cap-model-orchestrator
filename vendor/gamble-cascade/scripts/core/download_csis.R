# Download Critical Infrastructure Spatial Index (CISI) from GEE
# Dataset: projects/sat-io/open-datasets/CISI/global_CISI
# Author: Antigravity / Leopold

library(rgee)
library(sf)
library(terra)

# --- 1. Initialize ---
ee_project <- Sys.getenv("EE_PROJECT")
if (ee_project == "") stop("Error: EE_PROJECT not found in environment. Please add it to your .Renviron file.")
Sys.setenv(EARTHENGINE_PROJECT = ee_project)
ee$Initialize(project = ee_project)

# --- 2. Load CISI Image ---
cisi_img <- ee$Image("projects/sat-io/open-datasets/CISI/global_CISI")

# --- 3. Define Extent (Same EU + UK Bounding Box) ---
aoi <- ee$Geometry$Rectangle(coords = c(-12.0, 34.0, 35.0, 72.0))

# --- 4. Submit Download Task ---
out_file_cisi <- "CISI_Infrastructure_Intensity_EU_UK.tif"

cat("Submitting CISI download task to Google Drive...\n")

try({
  task <- ee_image_to_drive(
    image = cisi_img,
    description = "CISI_Global_Index_Export",
    folder = "EE_Downloads",
    region = aoi,
    scale = 1000, # 1km resolution to match GDP and RAI
    fileFormat = "GeoTIFF"
  )

  task$start()
  cat("Task started! Monitor with ee_monitoring() or check Google Drive folder 'EE_Downloads'.\n")
}, silent = FALSE)

# Check status of all your running tasks (GDP, RAI, and now CISI)
ee_monitoring()

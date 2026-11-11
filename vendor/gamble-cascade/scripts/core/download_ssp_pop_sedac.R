#!/usr/bin/env Rscript
# =========================================================================
# download_ssp_pop_sedac.R
# Downloads Gao et al. 1km SSP Population Grids from NASA SEDAC
# Dataset DOI: 10.7927/q7z9-9r69
# =========================================================================

suppressPackageStartupMessages({
  library(httr)
  library(terra)
})

# --- 1. Settings ---
# NASA Earthdata Login Credentials
earthdata_user <- Sys.getenv("EARTHDATA_USER", unset = "lringwald")
earthdata_pass <- Sys.getenv("EARTHDATA_PASS", unset = "vWE75F_9#5m8ePU")

if (earthdata_user == "" || earthdata_pass == "") {
  stop("Earthdata credentials not found. Please set them in the script or via environment variables.")
}

dest_dir <- "data/01_raw/ssp_population_sedac"
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

# Increase timeout for large files
options(timeout = 7200)

# Bounding Box for Europe & UK
aoi_ext <- ext(-12.0, 35.0, 34.0, 72.0)

# Target SSPs to download
ssps <- c("ssp1", "ssp2", "ssp3", "ssp4", "ssp5")

# --- 2. Define Download Function ---
download_sedac <- function(ssp, dest_folder) {
  # SEDAC naming convention for GeoTIFF zip files for this dataset
  # Note: You may need to verify the exact string if SEDAC updates their URL format
  base_url <- "https://sedac.ciesin.columbia.edu/downloads/data/popdynamics/popdynamics-1-km-downscaled-pop-base-year-projection-ssp-2000-2100-rev01"
  filename <- sprintf("popdynamics-1-km-downscaled-pop-base-year-projection-ssp-2000-2100-rev01-%s-geotiff.zip", ssp)
  dl_url <- file.path(base_url, filename)
  
  zip_path <- file.path(dest_folder, filename)
  
  if (file.exists(zip_path)) {
    cat(sprintf("  Skipping %s (already downloaded)\n", filename))
    return(zip_path)
  }
  
  # Retry loop for SEDAC connection timeouts
  max_retries <- 3
  attempt <- 1
  success <- FALSE
  
  while(attempt <= max_retries && !success) {
    cat(sprintf("  Attempt %d of %d: Authenticating and downloading %s from SEDAC...\n", attempt, max_retries, filename))
    
    response <- try({
      GET(
        dl_url,
        authenticate(earthdata_user, earthdata_pass, type = "basic"),
        write_disk(zip_path, overwrite = TRUE),
        progress(),
        timeout(600) # Increased timeout to 10 minutes for slow connections
      )
    }, silent = TRUE)
    
    if (!inherits(response, "try-error") && status_code(response) == 200) {
      success <- TRUE
      cat("  Download successful.\n")
      return(zip_path)
    } else {
      cat(sprintf("  Attempt %d failed.\n", attempt))
      if (file.exists(zip_path)) file.remove(zip_path)
      attempt <- attempt + 1
      if (attempt <= max_retries) Sys.sleep(5)
    }
  }
  
  if (!success) {
    cat(sprintf("  Failed to download %s after %d attempts.\n", filename, max_retries))
    return(NULL)
  }
}

# --- 3. Execute Downloads and Crop ---
cat("\n--- Downloading SSP Population (1km) from SEDAC ---\n")

for (ssp in ssps) {
  cat(sprintf("\nProcessing %s...\n", toupper(ssp)))
  
  zip_file <- download_sedac(ssp, dest_dir)
  
  if (!is.null(zip_file) && file.exists(zip_file)) {
    cat(sprintf("  Extracting %s...\n", basename(zip_file)))
    
    contents <- unzip(zip_file, list = TRUE)
    tif_files <- contents$Name[grepl("\\.tif$", contents$Name, ignore.case = TRUE)]
    
    if (length(tif_files) > 0) {
      # Unzip
      unzip(zip_file, files = tif_files, exdir = dest_dir)
      
      # Crop
      for (f in tif_files) {
        extracted_path <- file.path(dest_dir, f)
        
        # Flatten directory structure if zip contains folders
        out_name <- paste0("EU_", basename(f)) 
        out_path <- file.path(dest_dir, out_name)
        
        if (!file.exists(out_path)) {
          cat(sprintf("    Cropping %s to Europe Extent...\n", basename(f)))
          try({
            r <- rast(extracted_path)
            r_crop <- crop(r, aoi_ext)
            writeRaster(r_crop, out_path, overwrite = TRUE, gdal=c("COMPRESS=LZW"))
          })
        }
        
        # Remove uncompressed original
        if (file.exists(extracted_path)) file.remove(extracted_path)
      }
    }
  }
}

cat("\nDone processing SEDAC SSP Population grids.\n")

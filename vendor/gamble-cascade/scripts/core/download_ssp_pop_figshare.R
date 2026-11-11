#!/usr/bin/env Rscript
# =========================================================================
# download_ssp_pop_figshare.R
# Downloads Wang et al. (2022) 1km SSP Population Grids from Figshare
# Dataset DOI: 10.6084/m9.figshare.19608594
# =========================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(terra)
})

# --- 1. Settings ---
figshare_article_id <- "19608594"

dest_dir <- "data/01_raw/ssp_population_figshare"
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

# Increase timeout for large files
options(timeout = 7200)

# Bounding Box for Europe & UK
aoi_ext <- ext(-12.0, 35.0, 34.0, 72.0)

# --- 2. Fetch Metadata from Figshare API ---
cat(sprintf("\nQuerying Figshare Article: %s\n", figshare_article_id))
url <- sprintf("https://api.figshare.com/v2/articles/%s/files", figshare_article_id)

res <- tryCatch({
  jsonlite::fromJSON(url)
}, error = function(e) {
  cat("  Error fetching Figshare metadata:", e$message, "\n")
  return(NULL)
})

if (is.null(res) || length(res) == 0) {
  stop("Failed to retrieve file list from Figshare.")
}

# We only want ZIP files (e.g., SSP1.zip, SSP2.zip)
target_files <- res[grepl("\\.zip$", res$name, ignore.case = TRUE), ]
cat(sprintf("  Found %d ZIP files.\n", nrow(target_files)))

# --- 3. Download and Crop ---
for (i in seq_len(nrow(target_files))) {
  filename <- target_files$name[i]
  dl_url <- target_files$download_url[i]
  zip_path <- file.path(dest_dir, filename)
  
  if (file.exists(zip_path)) {
    cat(sprintf("  Skipping %s (already downloaded)\n", filename))
  } else {
    cat(sprintf("  Downloading %s...\n", filename))
    try({
      download.file(dl_url, zip_path, mode = "wb", quiet = FALSE)
    })
  }
  
  # --- 4. Unzip and Process ---
  if (file.exists(zip_path)) {
    cat(sprintf("  Extracting %s...\n", filename))
    contents <- unzip(zip_path, list = TRUE)
    tif_files <- contents$Name[grepl("\\.tif$", contents$Name, ignore.case = TRUE)]
    
    if (length(tif_files) > 0) {
      unzip(zip_path, files = tif_files, exdir = dest_dir)
      
      for (f in tif_files) {
        extracted_path <- file.path(dest_dir, f)
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

cat("\nDone downloading and processing Figshare SSP Population grids.\n")

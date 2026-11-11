suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(cli)
  library(qs2)
})

source("scripts/results_to_raster.R")

models <- c("globiom", "capri", "image", "magnet", "clumondo")
date_str <- "20260924"
temp_dir <- "P:/globiom/Projects/DS_AgMIP/DS_results/temp"
out_base <- "P:/globiom/Projects/DS_AgMIP/DS_results/outputs"
template_path <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"

# If 1km buffer is high-res, aggregate to 10km template or use an existing 10km TIF as template
sample_tif <- list.files(file.path(out_base, paste0("DS_ACT4CAP_AgMIP_GLOBIOM_10km_", date_str)), pattern = "_lu_base_.*\\.tif$", full.names = TRUE)[1]
template <- if (file.exists(sample_tif)) rast(sample_tif)[[1]] else rast(template_path)

cli_h1("Exporting Year 2000 Start Map GeoTIFFs for ACT4CAP_AgMIP Submodels")

for (m in models) {
  mod_upper <- toupper(m)
  prefix <- paste0("DS_ACT4CAP_AgMIP_", mod_upper, "_10km_", date_str)
  out_dir <- file.path(out_base, prefix)
  out_tif <- file.path(out_dir, paste0(prefix, "_lu_base_2000.tif"))
  
  sm_file <- file.path(temp_dir, paste0("processed_ds_start_map_10km_AGMIP_ACT4CAP_AgMIP_", mod_upper, "_", date_str, ".qs"))
  if (!file.exists(sm_file)) {
    cli_alert_warning("Start map file not found: {sm_file}")
    next
  }
  
  cli_alert_info("Reading {mod_upper} harmonized start map...")
  sm <- qs_read(sm_file)
  setDT(sm)
  
  # Format for results_to_raster
  sm_sub <- sm[, .(value = sum(area_km2, na.rm = TRUE)), by = .(ns, to_class = lu_class)]
  
  cli_alert_info("Rasterizing year 2000 base map for {mod_upper} -> {basename(out_tif)}")
  r_out <- results_to_raster(
    results = sm_sub,
    template_path = template,
    class_col = "to_class",
    value_col = "value",
    output_path = out_tif,
    check_area = TRUE,
    max_pixel_area = 100
  )
  
  cli_alert_success("Saved Year 2000 raster: {basename(out_tif)} ({nlyr(r_out)} bands, CRS: EPSG:3035)")
}

cli_alert_success("All submodel year 2000 start maps exported successfully!")

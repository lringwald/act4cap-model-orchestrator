# ==============================================================================
# aggregate_land_degradation_to_1km.R
# ==============================================================================
# Aggregates the 13 European Land Degradation indicators (Prăvălie et al.)
# from 500m resolution to the European 1km INSPIRE buffer grid
# (INSPIRE_Europe_buffer_1km.tif).
#
# Processes both agricultural land and arable land domains across:
#   1. Aridity (Critical = 1, No critical = 0)
#   2. Groundwater decline (Critical = 1, No critical = 0)
#   3. Soil acidification (Critical = 1, No critical = 2)
#   4. Soil compaction (Critical = 2, No critical = 1)
#   5. Soil nutrient imbalances / SNI (Critical = 2, No critical = 1)
#   6. Soil organic carbon loss / SOC loss (Critical = 0, No critical = 1)
#   7. Soil pollution with heavy metals / SPHM (Critical = 1, No critical = 0)
#   8. Soil pollution with pesticides / SPP (Critical = 0, No critical = 1)
#   9. Soil salinization (Critical = 0, No critical = 1)
#  10. Vegetation degradation (Critical = 1, No critical = 0)
#  11. Water erosion (Critical = 1, No critical = 0)
#  12. Wind erosion (Critical = 1, No critical = 0)
#  13. Land Management Intensity / LMI (Multi-indicator continuous index 0..10)
#
# Outputs:
# 1. Parquet Tables:
#    - land_degradation_1km.parquet: Wide table with INSPIRE_Europe_buffer_1kmID,
#      shares (0..1), degraded area (ha, 0..100), coverage shares, and mean LMI.
#    - land_degradation_data_dictionary.csv: Complete metadata catalog.
# 2. GeoTIFFs (for GIS Visual Confirmation in QGIS/ArcGIS):
#    - Multi-band:
#      * land_degradation_agri_shares.tif (12 bands of agricultural degradation shares)
#      * land_degradation_agri_ha.tif (12 bands of degraded agricultural hectares)
#      * land_degradation_arable_shares.tif (12 bands of arable degradation shares)
#      * land_degradation_arable_ha.tif (12 bands of degraded arable hectares)
#      * land_degradation_lmi.tif (2 bands: agri & arable mean LMI)
#      * land_degradation_coverage.tif (2 bands: agri & arable coverage fractions)
#    - Individual rasters in tifs/ for single-click inspection.
# ==============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(data.table)
  library(arrow)
  library(foreign)
  library(cli)
})

cli_h1("European Land Degradation 1km Aggregation Pipeline")

# --- 1. Configuration & Paths ---
input_dir <- "H:/R/LARGEDATAPROCESSING/Land-Degradation-Europe"
output_dir <- "H:/R/LARGEDATAPROCESSING/Land_Degradation_1km_aggregated"
tifs_dir <- file.path(output_dir, "tifs")
buffer_grid_path <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(tifs_dir)) dir.create(tifs_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(buffer_grid_path)) stop("Missing master buffer grid: ", buffer_grid_path)
if (!dir.exists(input_dir)) stop("Missing input directory: ", input_dir)

# --- 2. Load Master 1km Grid Template ---
cli_alert_info("Loading template 1km buffer grid: {buffer_grid_path}")
r_buf <- rast(buffer_grid_path)
buf_vals <- values(r_buf, mat = FALSE)
mask_buf <- !is.na(buf_vals)
inspire_ids <- as.integer(buf_vals[mask_buf])
n_buf_cells <- length(inspire_ids)
cli_alert_success("Master grid loaded: {n_buf_cells} valid cells ({nrow(r_buf)} rows x {ncol(r_buf)} cols)")

# --- 3. Indicator Metadata & Critical Threshold Specifications ---
indicator_specs <- list(
  list(code = "aridity", title = "Aridity", dir = "Aridity", crit_val = 1L),
  list(code = "groundwater_decline", title = "Groundwater decline", dir = "Groundwater decline", crit_val = 1L),
  list(code = "soil_acidification", title = "Soil acidification", dir = "Soil acidification", crit_val = 1L),
  list(code = "soil_compaction", title = "Soil compaction", dir = "Soil compaction", crit_val = 2L),
  list(code = "soil_nutrient_imbalance", title = "Soil nutrient imbalances", dir = "Soil nutrient imbalances", crit_val = 2L),
  list(code = "soil_organic_carbon_loss", title = "Soil organic carbon loss", dir = "Soil organic carbon loss", crit_val = 0L),
  list(code = "soil_pollution_heavy_metals", title = "Soil pollution with heavy metals", dir = "Soil pollution with heavy metals", crit_val = 1L),
  list(code = "soil_pollution_pesticides", title = "Soil pollution with pesticides", dir = "Soil pollution with pesticides", crit_val = 0L),
  list(code = "soil_salinization", title = "Soil salinization", dir = "Soil salinization", crit_val = 0L),
  list(code = "vegetation_degradation", title = "Vegetation degradation", dir = "Vegetation degradation", crit_val = 1L),
  list(code = "water_erosion", title = "Water erosion", dir = "Water erosion", crit_val = 1L),
  list(code = "wind_erosion", title = "Wind erosion", dir = "Wind erosion", crit_val = 1L)
)

domains <- c("agri", "arable")
domain_patterns <- c(agri = "agricultural lands\\.tif$", arable = "arable lands\\.tif$")
domain_labels <- c(agri = "agricultural lands", arable = "arable lands")

# --- 4. Process Land Domain Coverage Masks (from LMI) ---
cli_h2("Processing Land Domain Coverage Fractions")
domain_coverage_rasts <- list()
domain_coverage_vals <- list()

for (dom in domains) {
  lmi_file <- list.files(file.path(input_dir, "LMI"), pattern = domain_patterns[[dom]], full.names = TRUE)
  if (length(lmi_file) != 1) stop("Could not find LMI file for domain: ", dom)
  
  cli_alert_info("Computing overall {domain_labels[[dom]]} coverage from: {basename(lmi_file)}")
  r_lmi_dom <- rast(lmi_file)
  r_valid_dom <- as.numeric(!is.na(r_lmi_dom))
  r_cov_dom <- resample(r_valid_dom, r_buf, method = "average")
  
  # Replace any NaN or fringe NA inside raster with 0, and mask to Europe buffer
  cov_vals_all <- values(r_cov_dom, mat = FALSE)
  cov_vals_all[is.nan(cov_vals_all)] <- 0
  cov_vals_all[!mask_buf] <- NA_real_
  values(r_cov_dom) <- cov_vals_all
  names(r_cov_dom) <- paste0(dom, "_coverage_share")
  
  domain_coverage_rasts[[dom]] <- r_cov_dom
  domain_coverage_vals[[dom]] <- cov_vals_all[mask_buf]
  
  # Export individual coverage raster
  cov_tif_path <- file.path(tifs_dir, paste0(dom, "_coverage_share.tif"))
  writeRaster(r_cov_dom, cov_tif_path, gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)
  cli_alert_success("Saved: {basename(cov_tif_path)}")
}

# --- 5. Process Multi-Indicator Degradation Index (LMI) ---
cli_h2("Processing Land Management Intensity (LMI) Continuous Index")
lmi_rasts <- list()
lmi_vals <- list()

for (dom in domains) {
  lmi_file <- list.files(file.path(input_dir, "LMI"), pattern = domain_patterns[[dom]], full.names = TRUE)
  cli_alert_info("Resampling LMI for {domain_labels[[dom]]}...")
  r_lmi_dom <- rast(lmi_file)
  r_lmi_res <- resample(r_lmi_dom, r_buf, method = "average")
  
  lmi_vals_all <- values(r_lmi_res, mat = FALSE)
  lmi_vals_all[is.nan(lmi_vals_all)] <- NA_real_
  lmi_vals_all[!mask_buf] <- NA_real_
  values(r_lmi_res) <- lmi_vals_all
  names(r_lmi_res) <- paste0("lmi_", dom, "_mean")
  
  lmi_rasts[[dom]] <- r_lmi_res
  lmi_vals[[dom]] <- lmi_vals_all[mask_buf]
  
  # Export individual LMI raster
  lmi_tif_path <- file.path(tifs_dir, paste0("lmi_", dom, "_mean.tif"))
  writeRaster(r_lmi_res, lmi_tif_path, gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)
  cli_alert_success("Saved: {basename(lmi_tif_path)}")
}

# Multi-band LMI raster
r_lmi_multi <- c(lmi_rasts[["agri"]], lmi_rasts[["arable"]])
writeRaster(r_lmi_multi, file.path(output_dir, "land_degradation_lmi.tif"), 
            gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)

# Multi-band coverage raster
r_cov_multi <- c(domain_coverage_rasts[["agri"]], domain_coverage_rasts[["arable"]])
writeRaster(r_cov_multi, file.path(output_dir, "land_degradation_coverage.tif"), 
            gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)

# --- 6. Process the 12 Binary Indicators ---
cli_h2("Aggregating 12 Binary Degradation Indicators to 1km")

# Store results for parquet table and multi-band GeoTIFFs
results_list <- list(
  INSPIRE_Europe_buffer_1kmID = inspire_ids,
  agri_coverage_share = as.numeric(round(domain_coverage_vals[["agri"]], 4)),
  arable_coverage_share = as.numeric(round(domain_coverage_vals[["arable"]], 4)),
  lmi_agri_mean = as.numeric(round(lmi_vals[["agri"]], 3)),
  lmi_arable_mean = as.numeric(round(lmi_vals[["arable"]], 3))
)

agri_share_rasts <- list()
agri_ha_rasts <- list()
arable_share_rasts <- list()
arable_ha_rasts <- list()

t_start_ind <- Sys.time()

for (i in seq_along(indicator_specs)) {
  spec <- indicator_specs[[i]]
  ind_code <- spec$code
  ind_title <- spec$title
  ind_dir <- file.path(input_dir, spec$dir)
  crit_val <- spec$crit_val
  
  cli_alert_info("[{i}/12] Processing indicator: {ind_title} (Critical = {crit_val})")
  
  for (dom in domains) {
    tif_file <- list.files(ind_dir, pattern = domain_patterns[[dom]], full.names = TRUE)
    tif_file <- tif_file[!grepl("\\.aux\\.xml$", tif_file)]
    if (length(tif_file) != 1) stop("File not found for: ", ind_title, " domain: ", dom)
    
    spec[[paste0(dom, "_file")]] <- paste0(spec$dir, "/", basename(tif_file))
    r_src <- rast(tif_file)
    
    # Verify level mapping
    lev <- levels(r_src)[[1]]
    if (!is.null(lev) && ncol(lev) >= 2) {
      crit_row <- which(tolower(trimws(lev[[2]])) == "critical")
      if (length(crit_row) == 1) {
        chk_val <- as.integer(lev[[1]][crit_row])
        if (chk_val != crit_val) {
          stop("Critical value mismatch in ", basename(tif_file), ": expected ", crit_val, " got ", chk_val)
        }
      }
    }
    
    # 1. Binary critical raster (1 = Critical, 0 = No critical, NA = non-domain)
    # Average resampling computes the conditional share of domain that is critical
    r_bin <- (r_src == crit_val)
    r_share <- resample(r_bin, r_buf, method = "average")
    
    v_share_all <- values(r_share, mat = FALSE)
    v_share_all[is.nan(v_share_all)] <- NA_real_
    v_share_all[!mask_buf] <- NA_real_
    values(r_share) <- v_share_all
    names(r_share) <- paste0(ind_code, "_", dom, "_share")
    
    # 2. Degraded area in hectares = share * domain_coverage * 100
    # If domain_coverage is 0, ha is 0.
    # If domain_coverage > 0 but share is NA (unassessed), ha is NA.
    cov_vec <- domain_coverage_vals[[dom]]
    share_vec <- v_share_all[mask_buf]
    
    ha_vec <- fcase(
      cov_vec == 0, 0,
      is.na(share_vec), NA_real_,
      default = share_vec * cov_vec * 100
    )
    
    # Create raster for ha
    r_ha <- rast(r_buf)
    v_ha_all <- rep(NA_real_, ncell(r_buf))
    v_ha_all[mask_buf] <- ha_vec
    values(r_ha) <- v_ha_all
    names(r_ha) <- paste0(ind_code, "_", dom, "_ha")
    
    # Save individual GeoTIFF for visual confirmation
    tif_share_out <- file.path(tifs_dir, paste0(ind_code, "_", dom, "_share.tif"))
    tif_ha_out <- file.path(tifs_dir, paste0(ind_code, "_", dom, "_ha.tif"))
    
    writeRaster(r_share, tif_share_out, gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)
    writeRaster(r_ha, tif_ha_out, gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)
    
    # Store in multi-band lists
    if (dom == "agri") {
      agri_share_rasts[[ind_code]] <- r_share
      agri_ha_rasts[[ind_code]] <- r_ha
    } else {
      arable_share_rasts[[ind_code]] <- r_share
      arable_ha_rasts[[ind_code]] <- r_ha
    }
    
    # Store in results table
    results_list[[paste0(ind_code, "_", dom, "_share")]] <- as.numeric(round(share_vec, 4))
    results_list[[paste0(ind_code, "_", dom, "_ha")]] <- as.numeric(round(ha_vec, 3))
  }
  
  indicator_specs[[i]] <- spec
  cli_alert_success("Finished {ind_title}. Elapsed: {round(difftime(Sys.time(), t_start_ind, units='mins'), 2)} min")
}

# --- 7. Save Multi-Band GeoTIFFs ---
cli_h2("Exporting Multi-Band GeoTIFFs")

cli_alert_info("Writing land_degradation_agri_shares.tif (12 bands)...")
r_agri_share_multi <- rast(agri_share_rasts)
writeRaster(r_agri_share_multi, file.path(output_dir, "land_degradation_agri_shares.tif"),
            gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)

cli_alert_info("Writing land_degradation_agri_ha.tif (12 bands)...")
r_agri_ha_multi <- rast(agri_ha_rasts)
writeRaster(r_agri_ha_multi, file.path(output_dir, "land_degradation_agri_ha.tif"),
            gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)

cli_alert_info("Writing land_degradation_arable_shares.tif (12 bands)...")
r_arable_share_multi <- rast(arable_share_rasts)
writeRaster(r_arable_share_multi, file.path(output_dir, "land_degradation_arable_shares.tif"),
            gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)

cli_alert_info("Writing land_degradation_arable_ha.tif (12 bands)...")
r_arable_ha_multi <- rast(arable_ha_rasts)
writeRaster(r_arable_ha_multi, file.path(output_dir, "land_degradation_arable_ha.tif"),
            gdal = c("COMPRESS=DEFLATE", "ZLEVEL=6"), overwrite = TRUE)

cli_alert_success("All multi-band GeoTIFFs written successfully!")

# --- 8. Consolidate & Export Master Parquet Table ---
cli_h2("Building Master 1km Parquet Dataset")
dt_degradation <- as.data.table(results_list)
setorder(dt_degradation, INSPIRE_Europe_buffer_1kmID)

parquet_path <- file.path(output_dir, "land_degradation_1km.parquet")
cli_alert_info("Writing Parquet dataset ({nrow(dt_degradation)} rows x {ncol(dt_degradation)} columns)...")
write_parquet(dt_degradation, parquet_path, compression = "zstd", compression_level = 7)
cli_alert_success("Saved: {parquet_path} ({round(file.size(parquet_path) / 1024^2, 1)} MB)")

# --- 9. Build and Export Comprehensive Data Dictionary ---
cli_h2("Generating Data Dictionary")

dict_rows <- list(
  data.table(
    column_name = "INSPIRE_Europe_buffer_1kmID",
    indicator_code = "master_grid_id",
    indicator_title = "INSPIRE 1km European Grid Cell ID",
    domain = "all",
    unit = "integer ID",
    description = "Unique European INSPIRE 1km buffer grid cell identifier matching INSPIRE_Europe_buffer_1km.tif",
    source_file = buffer_grid_path,
    source_critical_value = NA_character_
  ),
  data.table(
    column_name = "agri_coverage_share",
    indicator_code = "agri_coverage",
    indicator_title = "Agricultural Land Coverage Share",
    domain = "agricultural",
    unit = "share (0.0 to 1.0)",
    description = "Fraction of the 1km grid cell covered by agricultural lands (from LMI agricultural lands footprint)",
    source_file = "LMI/LMI_agricultural lands.tif",
    source_critical_value = NA_character_
  ),
  data.table(
    column_name = "arable_coverage_share",
    indicator_code = "arable_coverage",
    indicator_title = "Arable Land Coverage Share",
    domain = "arable",
    unit = "share (0.0 to 1.0)",
    description = "Fraction of the 1km grid cell covered by arable lands (from LMI arable lands footprint)",
    source_file = "LMI/LMI_arable lands.tif",
    source_critical_value = NA_character_
  ),
  data.table(
    column_name = "lmi_agri_mean",
    indicator_code = "lmi",
    indicator_title = "Land Management Intensity Index (Agricultural)",
    domain = "agricultural",
    unit = "score (0.0 to 10.0)",
    description = "Area-weighted mean degradation intensity index across agricultural land within the 1km cell",
    source_file = "LMI/LMI_agricultural lands.tif",
    source_critical_value = "Continuous count 0..10"
  ),
  data.table(
    column_name = "lmi_arable_mean",
    indicator_code = "lmi",
    indicator_title = "Land Management Intensity Index (Arable)",
    domain = "arable",
    unit = "score (0.0 to 10.0)",
    description = "Area-weighted mean degradation intensity index across arable land within the 1km cell",
    source_file = "LMI/LMI_arable lands.tif",
    source_critical_value = "Continuous count 0..10"
  )
)

for (spec in indicator_specs) {
  # Agri share
  dict_rows[[length(dict_rows) + 1]] <- data.table(
    column_name = paste0(spec$code, "_agri_share"),
    indicator_code = spec$code,
    indicator_title = paste0(spec$title, " (Agri Share)"),
    domain = "agricultural",
    unit = "share (0.0 to 1.0)",
    description = paste0("Fraction of agricultural land in the 1km cell critically degraded by ", spec$title),
    source_file = spec$agri_file,
    source_critical_value = as.character(spec$crit_val)
  )
  # Agri ha
  dict_rows[[length(dict_rows) + 1]] <- data.table(
    column_name = paste0(spec$code, "_agri_ha"),
    indicator_code = spec$code,
    indicator_title = paste0(spec$title, " (Agri Area)"),
    domain = "agricultural",
    unit = "hectares (0.0 to 100.0 ha)",
    description = paste0("Area in hectares of agricultural land in the 1km cell critically degraded by ", spec$title),
    source_file = spec$agri_file,
    source_critical_value = as.character(spec$crit_val)
  )
  # Arable share
  dict_rows[[length(dict_rows) + 1]] <- data.table(
    column_name = paste0(spec$code, "_arable_share"),
    indicator_code = spec$code,
    indicator_title = paste0(spec$title, " (Arable Share)"),
    domain = "arable",
    unit = "share (0.0 to 1.0)",
    description = paste0("Fraction of arable land in the 1km cell critically degraded by ", spec$title),
    source_file = spec$arable_file,
    source_critical_value = as.character(spec$crit_val)
  )
  # Arable ha
  dict_rows[[length(dict_rows) + 1]] <- data.table(
    column_name = paste0(spec$code, "_arable_ha"),
    indicator_code = spec$code,
    indicator_title = paste0(spec$title, " (Arable Area)"),
    domain = "arable",
    unit = "hectares (0.0 to 100.0 ha)",
    description = paste0("Area in hectares of arable land in the 1km cell critically degraded by ", spec$title),
    source_file = spec$arable_file,
    source_critical_value = as.character(spec$crit_val)
  )
}

data_dict <- rbindlist(dict_rows)
dict_csv_path <- file.path(output_dir, "land_degradation_data_dictionary.csv")
fwrite(data_dict, dict_csv_path)
cli_alert_success("Data dictionary saved: {dict_csv_path}")

# --- 10. Summary & Validation Report ---
cli_h1("Pipeline Complete - Verification Summary")
cat("\nSummary of Agricultural Indicators (Non-NA agricultural cells):\n")
agri_active <- dt_degradation[agri_coverage_share > 0]
cat("Total 1km cells with agricultural land:", nrow(agri_active), "\n")
cat("Mean Agricultural Land per Active Cell:", round(mean(agri_active$agri_coverage_share) * 100, 1), "ha\n")
cat("Mean Agricultural LMI Score:", round(mean(agri_active$lmi_agri_mean, na.rm = TRUE), 2), "\n\n")

cat(sprintf("%-32s | %-12s | %-12s | %-14s\n", "Indicator", "Mean Share", "Max Share", "Total Degr (ha)"))
cat(paste(rep("-", 76), collapse = ""), "\n")
for (spec in indicator_specs) {
  sh_col <- paste0(spec$code, "_agri_share")
  ha_col <- paste0(spec$code, "_agri_ha")
  sh_val <- agri_active[[sh_col]]
  ha_val <- agri_active[[ha_col]]
  cat(sprintf("%-32s | %10.3f %% | %10.3f %% | %12s ha\n", 
              spec$title,
              mean(sh_val, na.rm = TRUE) * 100,
              max(sh_val, na.rm = TRUE) * 100,
              format(round(sum(ha_val, na.rm = TRUE)), big.mark = ",")))
}

cat("\nAggregated outputs are ready in:", output_dir, "\n")

rm(list=ls())

library(data.table)
library(terra)
library(arrow)
library(FNN)

# Paths
mapping_file <- "data/aux_files/one_kmID_master_mapping_2026-04-29.parquet"
esdac_dir <- "data/02_intermediate/ESDAC"
ee_dir <- "data/02_intermediate/EE_aggregated"
output_file <- "data/02_intermediate/prior_model_1km_master_inputs"
ref_raster_path <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"

# Additional spatial variables
pa_file <- "data/02_intermediate/all_PA_shares_merged.rds"
dem_file <- "data/02_intermediate/EU_DEM_mosaic_1000K_EEA_1kmID.rds"
slope_file <- "data/02_intermediate/eudem_slop_3035_europe_EEA_1kmID_transform.rds"
aspect_file <- "data/02_intermediate/eudem_aspc_3035_europe_EEA_1kmID_transform.rds"
hillshade_file <- "data/02_intermediate/EU_DEM_mosaic_hillshade_EEA_1kmID.rds"

# Platform detection: Accelerator (DeepOrigin / Celery) jobs provide PROJECT_SLUG
is_accelerator <- function() nzchar(Sys.getenv("PROJECT_SLUG", ""))

# 1. Load Master Mapping (Index)
cat("Loading master mapping from:", mapping_file, "...\n")
if (endsWith(mapping_file, ".parquet")) {
  master_dt <- as.data.table(read_parquet(mapping_file, memory_map = if (is_accelerator()) FALSE else TRUE))
} else {
  master_dt <- readRDS(mapping_file)
}
setDT(master_dt)
# Select relevant baseline columns
cols_to_keep <- c("INSPIRE_Europe_buffer_1kmID", "EEA_1kmID", "NUTS0", "NUTS1", "NUTS2", "NUTS3", "EEA_biogeo", "x", "y")
master_dt <- unique(master_dt[, ..cols_to_keep])

# Ensure integer types for joining
master_dt[, EEA_1kmID := as.integer(EEA_1kmID)]
master_dt[, INSPIRE_Europe_buffer_1kmID := as.integer(INSPIRE_Europe_buffer_1kmID)]
setkey(master_dt, EEA_1kmID)

cat("Master table initialized with", nrow(master_dt), "rows.\n")

# 2. Add Topographical & Hillshade Variables (Join via EEA_1kmID)
cat("\nMerging Topographical & Hillshade variables (Join via EEA_1kmID)...\n")

merge_topo <- function(m_dt, file_path, var_col, new_name) {
  if(file.exists(file_path)){
    cat("  Merging:", new_name, "\n")
    dt <- readRDS(file_path)
    # Handle cases where column name in RDS might differ
    if(!var_col %in% names(dt)) {
      cat("    Warning: column", var_col, "not found in", basename(file_path), "\n")
      return(m_dt)
    }
    dt_sub <- data.table(EEA_1kmID = as.integer(dt$EEA_1kmID), val = dt[[var_col]])
    setnames(dt_sub, "val", new_name)
    setkey(dt_sub, EEA_1kmID)
    m_dt <- dt_sub[m_dt, on = .(EEA_1kmID)]
    rm(dt, dt_sub); gc()
  } else {
    cat("  Skipping:", file_path, "(not found)\n")
  }
  return(m_dt)
}

master_dt <- merge_topo(master_dt, dem_file, "EU_DEM_mosaic_1000K_area_weighted_mean", "Elevation")
master_dt <- merge_topo(master_dt, slope_file, "slope_rad_mean", "Slope_rad")
master_dt <- merge_topo(master_dt, aspect_file, "sin_mean", "Aspect_sin_mean")
master_dt <- merge_topo(master_dt, aspect_file, "cos_mean", "Aspect_cos_mean")
master_dt <- merge_topo(master_dt, hillshade_file, "EU_DEM_mosaic_hillshade_area_weighted_mean", "Hillshade")

# Switch key to INSPIRE_Europe_buffer_1kmID for remaining joins
setkey(master_dt, INSPIRE_Europe_buffer_1kmID)

# 3. Add ESDAC Soil Variables (Join via BufferID)
esdac_files <- list.files(esdac_dir, pattern = "\\.rds$", full.names = TRUE)
cat("\nMerging", length(esdac_files), "ESDAC variables (Join via BufferID)...\n")
for(f in esdac_files){
  var_name <- gsub("_aggregated", "", tools::file_path_sans_ext(basename(f)))
  cat("  Merging:", var_name, "\n")
  dt <- readRDS(f)
  dt_sub <- dt[, .(INSPIRE_Europe_buffer_1kmID = as.integer(INSPIRE_Europe_buffer_1kmID), val = value)]
  setnames(dt_sub, "val", var_name)
  setkey(dt_sub, INSPIRE_Europe_buffer_1kmID)
  master_dt <- dt_sub[master_dt, on = .(INSPIRE_Europe_buffer_1kmID)]
  rm(dt, dt_sub); gc()
}

# 4. Add Protection Areas
if(file.exists(pa_file)){
  cat("\nMerging Protection Area shares (Join via BufferID)...\n")
  dt_pa <- readRDS(pa_file)
  dt_pa[, INSPIRE_Europe_buffer_1kmID := as.integer(INSPIRE_Europe_buffer_1kmID)]
  setkey(dt_pa, INSPIRE_Europe_buffer_1kmID)
  master_dt <- dt_pa[master_dt, on = .(INSPIRE_Europe_buffer_1kmID)]
  rm(dt_pa); gc()
}

# 5. Add Earth Engine Variables
ee_files <- list.files(ee_dir, pattern = "_aggregated\\.rds$", full.names = TRUE)
cat("\nGrouping and merging", length(ee_files), "Earth Engine variable files...\n")

# Logic: Group by variable prefix and year to handle segments
get_clean_name <- function(raw_name) {
  if(grepl("GDP_perCapita_([0-9]{4})", raw_name)) return(paste0("GDP_", sub(".*GDP_perCapita_([0-9]{4}).*", "\\1", raw_name)))
  # GHM v3 THREAT GROUPS. "Overall" -> GHM_Ovr; every threat code (HI human intrusion, TI transport
  # infrastructure, AG, BU, EX, PO, NS) keeps its code -> GHM_<CODE>_<year>. Extended 2026-08-04: the
  # old pattern only matched HI|Overall, so TI/NS/AG landed in the parquet under their RAW EE export
  # names (GHM_v3_TI_2020_EU_UK_<timestamp>_aggregated) instead of GHM_TI_2020.
  if(grepl("GHM_v3_(Overall|[A-Z]{2})_([0-9]{4})", raw_name)) {
    match <- regmatches(raw_name, regexec("GHM_v3_(Overall|[A-Z]{2})_([0-9]{4})", raw_name))[[1]]
    return(paste0("GHM_", ifelse(match[2] == "Overall", "Ovr", match[2]), "_", match[3]))
  }
  if(grepl("GlobPOP_Count_([0-9]{4})", raw_name)) return(paste0("Pop_", sub(".*GlobPOP_Count_([0-9]{4}).*", "\\1", raw_name)))
  if(grepl("CISI_Global_Index", raw_name)) return("CISI")
  if(grepl("RAI_Rural_Access", raw_name)) return("RAI")
  return(raw_name)
}

ee_info <- data.table(path = ee_files)
ee_info[, raw_name := tools::file_path_sans_ext(basename(path))]
ee_info[, clean_name := sapply(raw_name, get_clean_name)]

# Iterate by clean_name
unique_vars <- unique(ee_info$clean_name)
for(v in unique_vars){
  cat("  Processing variable:", v, "\n")
  paths <- ee_info[clean_name == v, path]

  # Combine segments if multiple files
  dts <- list()
  for(p in paths){
    # Skip if recently modified
    if(difftime(Sys.time(), file.info(p)$mtime, units="mins") < 2){
      cat("    Skipping segment:", basename(p), "(recently modified)\n")
      next
    }

    res <- tryCatch({
      readRDS(p)
    }, error = function(e){
      cat("    Error reading:", basename(p), "\n")
      return(NULL)
    })

    if(!is.null(res)){
      setDT(res)
      # Ensure consistent column naming
      if("aggregated_value" %in% names(res)){
        dts[[p]] <- res[, .(INSPIRE_Europe_buffer_1kmID = as.integer(INSPIRE_Europe_buffer_1kmID), aggregated_value)]
      }
    }
  }

  if(length(dts) > 0){
    combined_v <- rbindlist(dts)
    # If segments overlap, take mean or first (should not happen if segments are unique)
    combined_v <- combined_v[, .(val = mean(aggregated_value, na.rm=TRUE)), by = INSPIRE_Europe_buffer_1kmID]
    setnames(combined_v, "val", v)
    setkey(combined_v, INSPIRE_Europe_buffer_1kmID)

    master_dt <- combined_v[master_dt, on = .(INSPIRE_Europe_buffer_1kmID)]
    rm(combined_v, dts); gc()
  }
}

# 5.4 Add Masked CHELSA & SPEI Variables
chelsa_dir <- "data/02_intermediate/CHELSA_aggregated"
spei_dir <- "data/02_intermediate/SPEI_aggregated"

chelsa_files <- list.files(chelsa_dir, pattern = "_masked_aggregated\\.rds$", full.names = TRUE)
spei_files <- list.files(spei_dir, pattern = "_masked_aggregated\\.rds$", full.names = TRUE)
climate_files <- c(chelsa_files, spei_files)

if(length(climate_files) > 0){
  cat("\nMerging Masked CHELSA & SPEI Variables (Join via BufferID)...\n")
  for(f in climate_files){
    var_name <- gsub("_masked_aggregated", "", tools::file_path_sans_ext(basename(f)))
    cat("  Merging:", var_name, "\n")
    dt <- readRDS(f)
    
    # Rename SPEI columns to drop the _12 suffix
    new_names <- gsub("^spei48_([0-9]{4})_12$", "spei48_\\1", names(dt))
    setnames(dt, new_names)
    
    setkey(dt, INSPIRE_Europe_buffer_1kmID)
    master_dt <- dt[master_dt, on = .(INSPIRE_Europe_buffer_1kmID)]
    
    # Impute NAs for SPEI using spatial nearest neighbor
    if (grepl("spei", var_name, ignore.case = TRUE)) {
      spei_cols <- setdiff(new_names, "INSPIRE_Europe_buffer_1kmID")
      cat("  Imputing NAs for", var_name, "using spatial nearest neighbor...\n")
      for (col in spei_cols) {
        idx_na <- which(is.na(master_dt[[col]]))
        if (length(idx_na) > 0 && length(idx_na) < nrow(master_dt)) {
          idx_valid <- which(!is.na(master_dt[[col]]))
          
          nn_res <- FNN::get.knnx(
            data = as.matrix(master_dt[idx_valid, .(x, y)]),
            query = as.matrix(master_dt[idx_na, .(x, y)]),
            k = 1
          )
          
          # Assign value from nearest neighbor
          set(master_dt, i = idx_na, j = col, value = master_dt[[col]][idx_valid[nn_res$nn.index]])
        }
      }
    }
    
    rm(dt); gc()
  }
}

# 5.5 Add Yield Index
yield_file <- "data/02_intermediate/EQ_yield_index_1km_mapped.rds"
# Check if it is in output or root
if(!file.exists(yield_file)) yield_file <- "EQ_yield_index_1km_mapped.rds"

if(file.exists(yield_file)){
  cat("\nMerging Yield Index (Join via BufferID)...\n")
  dt_yield <- readRDS(yield_file)
  
  dt_yield_wide <- dcast(dt_yield, INSPIRE_Europe_buffer_1kmID ~ Mngmt_Intensity, value.var = c("yield_index", "gain_low", "gain_med"))
  setkey(dt_yield_wide, INSPIRE_Europe_buffer_1kmID)
  
  master_dt <- dt_yield_wide[master_dt, on = .(INSPIRE_Europe_buffer_1kmID)]
  
  yield_cols <- setdiff(names(dt_yield_wide), "INSPIRE_Europe_buffer_1kmID")
  
  cat("  Imputing NAs for Yield Index variables based on country coverage...\n")
  for (col in yield_cols) {
    master_dt[, (col) := {
      v <- get(col)
      if (any(!is.na(v))) {
        fifelse(is.na(v), 0, v)
      } else {
        v
      }
    }, by = NUTS0]
  }
  
  rm(dt_yield, dt_yield_wide); gc()
}

# 6. Save RDS (with Factors for categories)
cat("\nConverting regional columns to factors...\n")
cat_cols <- c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "CAPRI_NUTS", "EEA_biogeo")
for(col in cat_cols) if(col %in% names(master_dt)) master_dt[[col]] <- as.factor(master_dt[[col]])
cat("Cleaning up long variable names...\n")
new_names <- names(master_dt)
# Convert GHM_v3_TI_2020_EU_UK_2026_08_04_08_06_32_aggregated to GHM_TI_2020
new_names <- gsub("GHM_v3_([A-Z]+)_(20\\d{2})_EU_UK_.*_aggregated", "GHM_\\1_\\2", new_names)
setnames(master_dt, new_names)

cat("Saving master table to:", output_file, "...\n")
# saveRDS(master_dt, paste0(output_file,".rds"))
write_parquet(master_dt, paste0(output_file,".parquet"))

tryCatch({
  fwrite(head(master_dt, 1000),  paste0(output_file,"_sample.csv"))
}, error = function(e) {
  cat("  -> Warning: Could not write sample CSV. It might be open in another program.\n")
})

cat("Exporting comprehensive variable dictionary...\n")
var_names <- names(master_dt)

# Helper functions for dictionary
get_source <- function(v) {
  if (grepl("GHM", v)) return("Global Human Modification")
  if (grepl("CHELSA", v)) return("CHELSA Climate")
  if (grepl("SPEI", v)) return("Global SPEI Database")
  if (grepl("WR", v) || grepl("ESDB", v) || grepl("NUTS", v) || grepl("CAPRI", v) || grepl("biogeo", v)) return("ESDAC / EU Reference")
  if (grepl("Natura", v, ignore.case=TRUE)) return("Natura 2000")
  if (grepl("Elevation|Slope", v, ignore.case=TRUE)) return("Topography (EU-DEM)")
  if (grepl("LAMASUS|EEA_1km", v, ignore.case=TRUE)) return("Grid Index")
  if (grepl("yield|gain", v, ignore.case=TRUE)) return("Yield Index Model")
  return("Other / Aggregate")
}

get_desc <- function(v) {
  if (grepl("TI", v)) return("Transportation Infrastructure Modification")
  if (grepl("HI", v)) return("Human Intrusion Modification")
  if (grepl("NS", v)) return("Natural System Modification")
  if (grepl("AG", v)) return("Agriculture Modification")
  if (grepl("Ovr", v)) return("Overall Human Modification Index")
  if (grepl("Elevation", v, ignore.case=TRUE)) return("Elevation (meters)")
  if (grepl("Slope", v, ignore.case=TRUE)) return("Slope (degrees)")
  if (grepl("prcp|tas", v)) return("Climate Variable (Precipitation/Temperature)")
  if (grepl("SPEI", v)) return("Standardized Precipitation-Evapotranspiration Index")
  if (grepl("yield_index", v, ignore.case=TRUE)) return("Crop yield index under specific management intensity")
  if (grepl("gain_low", v, ignore.case=TRUE)) return("Yield gain compared to low management intensity")
  if (grepl("gain_med", v, ignore.case=TRUE)) return("Yield gain compared to medium management intensity")
  return("Refer to source documentation")
}

get_version <- function(v) {
  if (grepl("GHM", v)) return("v3")
  if (grepl("CHELSA", v)) return("V.2.1")
  if (grepl("SPEI", v)) return("v2.9")
  if (grepl("WR", v) || grepl("ESDB", v)) return("20240507")
  if (grepl("Natura", v, ignore.case=TRUE)) return("End2018")
  if (grepl("yield|gain", v, ignore.case=TRUE)) return("Latest")
  return("1.0")
}

get_download_date <- function(v) {
  if (grepl("GHM", v)) return("2026-08-04")
  if (grepl("CHELSA|SPEI", v)) return("2024-04")
  if (grepl("WR|ESDB", v)) return("2024-05")
  if (grepl("yield|gain", v, ignore.case=TRUE)) return("2026-08-04")
  return("Unknown / Legacy")
}

get_year <- function(v) {
  m <- regexpr("(19|20)\\d{2}", v)
  if (m == -1) return("Static")
  return(regmatches(v, m))
}

variable_list <- data.table(
  Variable_Name = var_names,
  Long_Name = gsub("_", " ", var_names),
  Source = sapply(var_names, get_source),
  Description = sapply(var_names, get_desc),
  Year = sapply(var_names, get_year),
  Data_Type = sapply(master_dt, class),
  Resolution = "1km",
  Version = sapply(var_names, get_version),
  Date_Downloaded = sapply(var_names, get_download_date),
  Date_Created_In_Master = Sys.Date()
)

fwrite(variable_list, paste0(output_file, "_data_dictionary.csv"))

# Also export as a nicely formatted Markdown file for Git
cat("Exporting markdown data dictionary...\n")
md_file <- paste0(output_file, "_data_dictionary.md")
md_content <- c(
  "# LAMASUS Gridwork - 1km Master Inputs Data Dictionary",
  "",
  paste("*Generated on:", Sys.Date(), "*"),
  "",
  "| Variable Name | Description | Source | Year | Data Type | Version |",
  "|---|---|---|---|---|---|"
)

for(i in 1:nrow(variable_list)) {
  row_str <- sprintf("| `%s` | %s | %s | %s | %s | %s |",
                     variable_list$Variable_Name[i],
                     variable_list$Description[i],
                     variable_list$Source[i],
                     variable_list$Year[i],
                     variable_list$Data_Type[i],
                     variable_list$Version[i])
  md_content <- c(md_content, row_str)
}

writeLines(md_content, md_file)


# 7. Export Visual Verification TIFF Stack (High Efficiency Streaming)
cat("\nGenerating Multi-layer TIFF Stack for verification...\n")
r_template <- rast(ref_raster_path)
nc <- ncol(r_template)
nr <- nrow(r_template)

# Prepare numeric layers only
cols_for_tiff <- names(master_dt)[sapply(master_dt, is.numeric)]
cols_for_tiff <- setdiff(cols_for_tiff, c("INSPIRE_Europe_buffer_1kmID", "EEA_1kmID", "x_1kmID", "y_1kmID", "x", "y"))

cat("  Exporting", length(cols_for_tiff), "layers to TIFF...\n")

# Use a memory-efficient approach: process layer by layer,
# but avoid the intermediate matrix by using terra's direct assignment if possible
# or a temporary file-backed stack.

output_tiff <- paste0(output_file, "_stack.tif")
r_out <- rast(r_template, nlyrs = length(cols_for_tiff))
names(r_out) <- cols_for_tiff

# Sort master_dt by EEA_1kmID to potentially speed up mapping,
# although we'll still use cell-index based assignment.
setkey(master_dt, EEA_1kmID)
idx <- master_dt$EEA_1kmID
mask <- !is.na(idx) & idx >= 1 & idx <= ncell(r_template)
clean_idx <- idx[mask]

# To be "even more efficient", we can write directly to a file-backed raster
# using a loop and writeRaster with a temporary file if needed,
# or just do the layer-by-layer thing but more cleanly.

# Actually, the most efficient way in terra to write many layers without crashing
# is to create a temp file for each layer and then use gdalbuildvrt or similar,
# but let's try the single-layer-at-a-time write more robustly.

# Save to H drive if available, otherwise local output folder
if (dir.exists("H:/R/LARGEDATA")) {
  output_tiff <- "H:/R/LARGEDATA/prior_model_1km_master_inputs_stack.tif"
  temp_tiff_dir <- "H:/R/LARGEDATA/temp_layers"
} else {
  output_tiff <- paste0(output_file, "_stack.tif")
  temp_tiff_dir <- file.path(dirname(output_file), "temp_layers")
}

if(!dir.exists(temp_tiff_dir)) dir.create(temp_tiff_dir, recursive = TRUE)

for(i in seq_along(cols_for_tiff)){
  col <- cols_for_tiff[i]
  cat(sprintf("  [%d/%d] Processing layer: %s\n", i, length(cols_for_tiff), col))

  # Allocate just one layer vector
  v <- rep(NaN, ncell(r_template))
  v[clean_idx] <- master_dt[[col]][mask]

  # Create a single layer SpatRaster
  r_layer <- rast(r_template)
  values(r_layer) <- v
  names(r_layer) <- col

  # Write to temp file
  tmp_f <- file.path(temp_tiff_dir, paste0(i, "_", col, ".tif"))
  writeRaster(r_layer, tmp_f, overwrite=TRUE, gdal=c("COMPRESS=LZW"))

  rm(r_layer, v); gc()
}

cat("  Stitching layers into final stack...\n")
layer_files <- list.files(temp_tiff_dir, pattern = "\\.tif$", full.names = TRUE)
# Sort by index to keep original order
layer_files <- layer_files[order(as.numeric(sub("_.*", "", basename(layer_files))))]

r_stack <- rast(layer_files)
writeRaster(r_stack, output_tiff, overwrite=TRUE, gdal=c("COMPRESS=LZW"))

# Cleanup
cat("  Cleaning up...\n")
unlink(temp_tiff_dir, recursive = TRUE)

cat("\nSUCCESS: Multi-source consolidation complete.\n")
cat("RDS:", output_file, "\n")
cat("TIFF:", output_tiff, "\n")

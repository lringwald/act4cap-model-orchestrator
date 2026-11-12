# ===============================================================
# process_exogenous_priors.R
# ===============================================================
# This script processes exogenous datasets (e.g., wetlands) and 
# aggregates them to the target downscale resolution so they can 
# be ingested as priors into the downscaling function.
# ===============================================================

require(data.table)
require(cli)

cli_h2("Processing Exogenous Priors")

# 1. Configuration (inherit from master_downscaling.R if run within pipeline)
if (!exists("PIXEL_RES")) PIXEL_RES <- 10
if (!exists("PIXEL_INTERSECT_COL")) PIXEL_INTERSECT_COL <- "NUTS3"
if (!exists("paths")) {
  paths <- list(
    gridwork_dir = "data/aux_files/",
    temp = "P:/globiom/Projects/LAMASUS/DS_results/temp/"
  )
}

# 2. Load the mapping grid (Avoid reloading if already in environment)
if (exists("mappings") && !is.null(mappings$mapping_grid)) {
  message(">>> Using existing mapping_grid from pipeline...")
  mapping_grid <- copy(mappings$mapping_grid)
} else {
  # Look for the parquet file first, fallback to rds
  mapping_grid_path_parquet <- file.path(paths$gridwork_dir, "one_kmID_master_mapping_2026-05-28.parquet")
  mapping_grid_path_rds     <- file.path(paths$gridwork_dir, "one_kmID_master_mapping_2026-05-28.rds")
  
  if (file.exists(mapping_grid_path_parquet)) {
    message(">>> Loading mapping grid from parquet...")
    mapping_grid <- as.data.table(arrow::read_parquet(
      mapping_grid_path_parquet,
      memory_map = FALSE
    ))
  } else if (file.exists(mapping_grid_path_rds)) {
    message(">>> Loading mapping grid from rds...")
    mapping_grid <- readRDS(mapping_grid_path_rds)
  } else {
    stop("Mapping grid not found.")
  }
  
  # 3. Define downscale resolution ID (ns)
  # Generate ns exactly as done in master_downscaling.R
  base_id_col <- paste0("EEA_", PIXEL_RES, "kmID")
  if (!is.null(PIXEL_INTERSECT_COL) && PIXEL_INTERSECT_COL %in% names(mapping_grid)) {
    mapping_grid[, ns := paste0(get(base_id_col), "_", get(PIXEL_INTERSECT_COL))]
  } else {
    mapping_grid[, ns := as.character(get(base_id_col))]
  }
}

# Keep only necessary mapping columns to save memory
if ("INSPIRE_Europe_buffer_1kmID" %in% names(mapping_grid)) {
  mapping_subset <- unique(mapping_grid[!is.na(INSPIRE_Europe_buffer_1kmID), .(INSPIRE_Europe_buffer_1kmID, ns, x, y)])
} else {
  stop("Missing INSPIRE_Europe_buffer_1kmID column in mapping_grid.")
}
rm(mapping_grid)
gc(verbose = FALSE)

# 4. Load the Exogenous Wetland Dataset
wetland_path <- file.path(paths$gridwork_dir, "wetlands_aggregated/wetland_map_EU_1km_by_class.rds")
if (file.exists(wetland_path)) {
  message(sprintf(">>> Processing wetland data from %s...", wetland_path))
  wetland_data <- readRDS(wetland_path)
  
  if (exists("register_source_file")) {
    register_source_file("Priors", "Exogenous Wetlands Map", wetland_path)
  }
  
  if (!is.data.table(wetland_data)) setDT(wetland_data)
  
  # Merge with full mapping grid to ensure we have a complete grid for rasterization
  wetland_mapped <- merge(mapping_subset, wetland_data, by = "INSPIRE_Europe_buffer_1kmID", all.x = TRUE)
  
  # Calculate combined area base metric
  wetland_mapped[, combined_area := rowSums(.SD, na.rm = TRUE), .SDcols = c("area_peatland", "area_wetland", "area_floodplain.potential", "area_actual.riparian.zone")]
  
  message(">>> Applying 5x5 focal smoothing to wetland mask...")
  require(terra)
  # Ensure grid is sorted by y (desc) and x (asc) for proper rasterization
  setorder(wetland_mapped, -y, x)
  r_base <- rast(wetland_mapped[!is.na(x) & !is.na(y), .(x, y, combined_area)], type="xyz")
  
  # Apply a 5x5 focal mean to spread the probability into neighboring pixels
  # na.rm = TRUE ensures that edges don't disappear
  r_smooth <- focal(r_base, w=matrix(1, 5, 5), fun=mean, na.rm=TRUE)
  
  # Convert back to data.table
  smooth_dt <- as.data.table(r_smooth, xy=TRUE)
  setnames(smooth_dt, "focal_mean", "smoothed_area")
  
  # Merge smoothed values back
  wetland_mapped <- merge(wetland_mapped, smooth_dt, by=c("x", "y"), all.x=TRUE)
  # Fallback to unsmoothed combined_area if smoothed_area is NA
  wetland_mapped[is.na(smoothed_area), smoothed_area := combined_area]
  
  # Aggregate to downscale resolution
  exogenous_wetland_prior <- wetland_mapped[!is.na(ns), .(
    value = sum(smoothed_area, na.rm = TRUE)
  ), by = .(ns)]
  
  # Ensure all pixels in the mapping grid get at least a tiny baseline probability (1e-6).
  # This acts as a uniform fallback (infinite smooth) so the solver can overflow
  # if the macro target demands more area than the mask allows.
  all_ns <- unique(mapping_subset$ns)
  missing_ns <- setdiff(all_ns, exogenous_wetland_prior$ns)
  if (length(missing_ns) > 0) {
    exogenous_wetland_prior <- rbind(exogenous_wetland_prior, data.table(ns = missing_ns, value = 0))
  }
  
  # Harmonize the value to a 0-1 range
  if (max(exogenous_wetland_prior$value, na.rm = TRUE) > 0) {
    exogenous_wetland_prior[, value := value / max(value, na.rm = TRUE)]
  }
  
  # Apply baseline probability to prevent hard 0 probability masks
  exogenous_wetland_prior[value == 0, value := 1e-6]
  
  # Format for downscale ingestion
  exogenous_wetland_prior[, to_class := "Wetlands_restored"]
  
  # The downscale function typically expects 'from_class', 'to_class', 'times', 'value', 'ns'
  # If a specific 'from_class' is not provided, this represents an unconditional prior/target for 'to_class'.
  # Adding placeholder columns to match standard downscalr prior formats.
  # Adjust 'times' or 'from_class' below if you want this to apply to a specific year or transition.
  exogenous_wetland_prior[, from_class := "__NA_LU__"]
  exogenous_wetland_prior[, times := "__NA_TIME__"]
  
  # Reorder columns
  setcolorder(exogenous_wetland_prior, c("ns", "from_class", "to_class", "times", "value"))
  
  # Do NOT filter out zero values because we specifically injected 1e-6 as a fallback
  # exogenous_wetland_prior <- exogenous_wetland_prior[value > 0]
  
  # Save the output
  if (!dir.exists(paths$temp)) dir.create(paths$temp)
  out_path <- file.path(paths$temp, "exogenous_priors_wetland.rds")
  dir.create(paths$temp, showWarnings = FALSE)
  saveRDS(exogenous_wetland_prior, out_path)
  
  message(">>> Exogenous wetland priors saved to: ", out_path)
  
} else {
  message(">>> Wetland dataset not found at: ", wetland_path)
}

# 5. Load the Exogenous allPA Dataset
allPA_path <- "data/aux_files/allPA.rds"
if (file.exists(allPA_path)) {
  message(sprintf(">>> Processing allPA data from %s...", allPA_path))
  allPA_data <- readRDS(allPA_path)
  
  if (exists("register_source_file")) {
    register_source_file("Priors", "Exogenous Protected Areas (allPA)", allPA_path)
  }
  
  if (!is.data.table(allPA_data)) setDT(allPA_data)
  
  # Merge with mapping grid to get downscale ID (ns)
  allPA_mapped <- merge(allPA_data, mapping_subset, by = "INSPIRE_Europe_buffer_1kmID", all.x = TRUE)
  
  # Aggregate to downscale resolution
  exogenous_allPA_prior <- allPA_mapped[!is.na(ns), .(
    value = mean(allPA_share, na.rm = TRUE)
  ), by = .(ns)]
  
  # Harmonize the value to a 0-1 range
  if (max(exogenous_allPA_prior$value) > 0) {
    exogenous_allPA_prior[, value := value / max(value)]
  }
  
  # Create two rows per ns for the specific transitions
  prior_allPA_forest <- copy(exogenous_allPA_prior)
  prior_allPA_forest[, from_class := "Forest"]
  prior_allPA_forest[, to_class := "Forest_protected"]
  prior_allPA_forest[, times := "__NA_TIME__"]

  prior_allPA_natural <- copy(exogenous_allPA_prior)
  prior_allPA_natural[, from_class := "Natural_unmanaged"]
  prior_allPA_natural[, to_class := "Natural_protected"]
  prior_allPA_natural[, times := "__NA_TIME__"]

  exogenous_allPA_prior <- rbindlist(list(prior_allPA_forest, prior_allPA_natural))
  
  # Reorder columns
  setcolorder(exogenous_allPA_prior, c("ns", "from_class", "to_class", "times", "value"))
  
  # Filter out zero values to keep it sparse
  exogenous_allPA_prior <- exogenous_allPA_prior[value > 0]
  
  # Save the output
  if (!dir.exists(paths$temp)) dir.create(paths$temp)
  out_path <- file.path(paths$temp, "exogenous_priors_allPA.rds")
  saveRDS(exogenous_allPA_prior, out_path)
  
  message(">>> Exogenous allPA priors saved to: ", out_path)
  
} else {
  message(">>> allPA dataset not found at: ", allPA_path)
}

# 6. Load the Exogenous N2K Dataset
n2k_path <- if (file.exists(file.path(paths$gridwork_dir, "policy_masks/N2K.rds"))) {
  file.path(paths$gridwork_dir, "policy_masks/N2K.rds")
} else {
  file.path(paths$gridwork_dir, "N2K.rds")
}
if (file.exists(n2k_path)) {
  message(sprintf(">>> Processing N2K data from %s...", n2k_path))
  n2k_data <- readRDS(n2k_path)
  
  if (exists("register_source_file")) {
    register_source_file("Priors", "Exogenous Natura 2000 (N2K)", n2k_path)
  }
  
  if (!is.data.table(n2k_data)) setDT(n2k_data)
  
  # Merge with mapping grid to get downscale ID (ns)
  n2k_mapped <- merge(n2k_data, mapping_subset, by = "INSPIRE_Europe_buffer_1kmID", all.x = TRUE)
  
  # Aggregate to downscale resolution
  exogenous_n2k_prior <- n2k_mapped[!is.na(ns), .(
    value = mean(N2K_share, na.rm = TRUE)
  ), by = .(ns)]
  
  # Harmonize the value to a 0-1 range
  if (max(exogenous_n2k_prior$value) > 0) {
    exogenous_n2k_prior[, value := value / max(value)]
  }
  
  # Create two rows per ns for the specific transitions
  prior_n2k_forest <- copy(exogenous_n2k_prior)
  prior_n2k_forest[, from_class := "Forest"]
  prior_n2k_forest[, to_class := "Forest_protected"]
  prior_n2k_forest[, times := "__NA_TIME__"]

  prior_n2k_natural <- copy(exogenous_n2k_prior)
  prior_n2k_natural[, from_class := "Natural_unmanaged"]
  prior_n2k_natural[, to_class := "Natural_protected"]
  prior_n2k_natural[, times := "__NA_TIME__"]

  exogenous_n2k_prior <- rbindlist(list(prior_n2k_forest, prior_n2k_natural))
  
  # Reorder columns
  setcolorder(exogenous_n2k_prior, c("ns", "from_class", "to_class", "times", "value"))
  
  # Filter out zero values to keep it sparse
  exogenous_n2k_prior <- exogenous_n2k_prior[value > 0]
  
  # Save the output
  if (!dir.exists(paths$temp)) dir.create(paths$temp)
  out_path <- file.path(paths$temp, "exogenous_priors_n2k.rds")
  saveRDS(exogenous_n2k_prior, out_path)
  
  message(">>> Exogenous N2K priors saved to: ", out_path)
  
} else {
  message(">>> N2K dataset not found at: ", n2k_path)
}

message(">>> Exogenous priors processing complete.")


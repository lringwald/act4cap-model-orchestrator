library(data.table)

# ---------------------------------------------------------------
# 0. Setup & Scope Inheritance
# ---------------------------------------------------------------
if (!exists("regions_to_run")) {
  regions_to_run <- c(
    "AUT", "BEL", "HRV", "CYP", "CZE", "DNK",
    "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL",
    "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL",
    "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR",
    "CHE", "NOR"
  )
}
region.slct <- regions_to_run

if (!exists("tol_rel")) tol_rel <- 1e-6
if (!exists("downscale_resolution")) downscale_resolution <- "10km"

# ---------------------------------------------------------------
# 1. Load Input Data
# ---------------------------------------------------------------
aux_dir <- if (exists("paths") && !is.null(paths$aux_files) && dir.exists(paths$aux_files)) paths$aux_files else "data/aux_files"
mapping_thematic <- fread(file.path(aux_dir, "LUM_Code_to_macro_model_mapping.csv"))

cli_alert_info("Loading 1km target spatial grid map...")

base_yr <- if (exists("current_model_cfg") && !is.null(current_model_cfg$base_year)) current_model_cfg$base_year else 2000
start_map_prefix <- if (exists("current_model_cfg") && !is.null(current_model_cfg$start_map_prefix)) current_model_cfg$start_map_prefix else "LUM_fit_with_energy_levels_and_new_FM_"

avail_files <- list.files("data/02_intermediate", pattern = paste0("^", start_map_prefix, ".*\\.rds$"), full.names = TRUE)
if (length(avail_files) == 0) {
  stop(sprintf("No start map files found matching prefix '%s' in data/02_intermediate/", start_map_prefix))
}

extracted_years <- suppressWarnings(as.integer(gsub(paste0(".*", start_map_prefix, "(\\d{4}).*\\.rds$"), "\\1", avail_files)))
valid_idx <- !is.na(extracted_years)
if (!any(valid_idx)) stop("Could not parse years from start map filenames.")

closest_idx <- which.min(abs(extracted_years[valid_idx] - base_yr))
closest_year <- extracted_years[valid_idx][closest_idx]
start_map_file <- avail_files[valid_idx][closest_idx]

message(sprintf(">>> Target base year: %s. Closest available map year: %s", base_yr, closest_year))
message(">>> Using start map: ", basename(start_map_file))

temp_map_1km <- readRDS(start_map_file)

if (exists("register_source_file")) {
  register_source_file("Mapping", "Thematic Mapping", "data/aux_files/LAMASUS_LUM_thematic_mapping.csv")
  register_source_file("Input Map", "Base Start Map (1km)", start_map_file)
}
setDT(temp_map_1km)

if ("INSPIRE_Europe_buffer_1kmID" %in% names(temp_map_1km) && !"EEA_1kmID" %in% names(temp_map_1km)) {
  temp_map_1km[, INSPIRE_Europe_buffer_1kmID := as.character(INSPIRE_Europe_buffer_1kmID)]
  id_map <- unique(mappings$mapping_grid[, .(INSPIRE_Europe_buffer_1kmID = as.character(INSPIRE_Europe_buffer_1kmID), EEA_1kmID)])
  temp_map_1km <- merge(temp_map_1km, id_map, by = "INSPIRE_Europe_buffer_1kmID", all.x = TRUE)
}

temp_map_1km[, EEA_1kmID := as.integer(EEA_1kmID)]
temp_map_1km <- temp_map_1km[EEA_1kmID %in% unique(mappings$mapping_grid$EEA_1kmID)]

map_tot_area_check   <- sum(temp_map_1km$area_km2, na.rm = TRUE)
map_tot_pixels_check <- map_tot_area_check

# (Note: Legacy irrigation integration logic was commented out in original script,
# so we avoid pivoting wide/long and simply map the data directly)

# Ensure data is in standard long format mapping
class_col <- setdiff(names(temp_map_1km), c("EEA_1kmID", "area_km2", "allPA_share"))[1]
setnames(temp_map_1km,
         old = c(class_col, "area_km2"),
         new = c("LUM_Code", "LUM_area_km2"))

# ---------------------------------------------------------------
# 2. Refine Mapping Grid
# ---------------------------------------------------------------
message(">>> Refining mapping grid and calculating spatial weights...")

if (!is.data.table(mappings$mapping_grid)) setDT(mappings$mapping_grid)

setnames(mapping_thematic,
         old = c("AgMIP_abbr_1", "AgMIP_abbr_2"),
         new = c("lu_class_1", "lu_class_2"), skip_absent = TRUE)

# ---------------------------------------------------------------
# 3. Final Aggregation to Downscaling Resolution
# ---------------------------------------------------------------
message(sprintf(">>> Aggregating to %s resolution...", downscale_resolution))

# --- ALL_PA PROCESSING ---
message(">>> Integrating protected areas from ALL_PA...")
all_pa <- readRDS("data/02_intermediate/all_PA_shares_merged.rds")

# Get mapping from INSPIRE_Europe_buffer_1kmID to EEA_1kmID
if (!exists("mappings")) mappings <- list()
if (!"mapping_grid" %in% names(mappings)) {
  grid_files <- list.files("data/aux_files", pattern = "^one_kmID_master_mapping.*\\.(rds|parquet)$", full.names = TRUE)
  grid_file <- get_latest_file_aux(grid_files)

  if (is.null(grid_file)) stop("No master mapping files found in data/aux_files.")

  if (grepl("\\.parquet$", grid_file, ignore.case = TRUE)) {
    mappings$mapping_grid <- arrow::read_parquet(grid_file)
  } else {
    mappings$mapping_grid <- readRDS(grid_file)
  }

  if (exists("register_source_file") && !is.na(grid_file)) {
    register_source_file("Mapping", "Master Mapping Grid", grid_file)
  }
  setDT(mappings$mapping_grid)
}
pa_mapping <- unique(mappings$mapping_grid[!is.na(INSPIRE_Europe_buffer_1kmID), .(INSPIRE_Europe_buffer_1kmID, EEA_1kmID)])
# Perform an inner join and only keep the required columns to save memory
all_pa <- all_pa[pa_mapping, .(EEA_1kmID = i.EEA_1kmID, allPA_share = x.allPA_share), on = "INSPIRE_Europe_buffer_1kmID", nomatch = NULL]
gc()
# Try the aggregation with reduced thread count if it fails, but standard by should work now with less memory pressure
all_pa <- all_pa[, .(allPA_share = max(allPA_share, na.rm = TRUE)), by = EEA_1kmID]

# Merge into temp_map_1km using memory-efficient update join
temp_map_1km[all_pa, allPA_share := i.allPA_share, on = "EEA_1kmID"]
temp_map_1km[is.na(allPA_share), allPA_share := 0]

# Identify relevant LU codes for PAs (Natural unmanaged and forests)
# Exclude class 1 (Primary forest) from reallocation
non_pa_natural_codes <- mapping_thematic[lu_class_1 == "ONV", LUM_Code]
forest_codes <- mapping_thematic[lu_class_1 == "FOR", LUM_Code]
forest_codes <- setdiff(forest_codes, 1) # Exclude Primary forest code

# Split Natural_unmanaged
nat_dt <- temp_map_1km[LUM_Code %in% non_pa_natural_codes & allPA_share > 0]
if (nrow(nat_dt) > 0) {
  nat_pa <- copy(nat_dt)
  nat_pa[, LUM_area_km2 := LUM_area_km2 * allPA_share]
  nat_pa[, LUM_Code := as.integer(3200)]

  temp_map_1km[LUM_Code %in% non_pa_natural_codes & allPA_share > 0,
               LUM_area_km2 := LUM_area_km2 * (1 - allPA_share)]

  temp_map_1km <- rbind(temp_map_1km, nat_pa)
}

# Split Forests
for_dt <- temp_map_1km[LUM_Code %in% forest_codes & allPA_share > 0]
if (nrow(for_dt) > 0) {
  for_pa <- copy(for_dt)
  for_pa[, LUM_area_km2 := LUM_area_km2 * allPA_share]
  for_pa[, LUM_Code := as.integer(1)]

  temp_map_1km[LUM_Code %in% forest_codes & allPA_share > 0,
               LUM_area_km2 := LUM_area_km2 * (1 - allPA_share)]

  temp_map_1km <- rbind(temp_map_1km, for_pa)
}

# Clean up
temp_map_1km[, allPA_share := NULL]
temp_map_1km <- temp_map_1km[, .(LUM_area_km2 = sum(LUM_area_km2, na.rm = TRUE)), by = .(EEA_1kmID, LUM_Code)]

# Join thematic metadata
LUM_map_long <- merge(temp_map_1km, mapping_thematic[, .(LUM_Code, lu_class_1, lu_class_2)], by = "LUM_Code", all.x = TRUE)

# Group by pixel and category
LUM_map_long <- LUM_map_long[!is.na(lu_class_1) & lu_class_1 != "" & lu_class_1 != "NODATA",
                             .(LUM_area_km2 = sum(LUM_area_km2, na.rm = TRUE)),
                             by = .(EEA_1kmID, lu_class_1, lu_class_2)]

# Validate pixel consistency
stop_on_excessive_area_gap(sum(LUM_map_long$LUM_area_km2), map_tot_pixels_check, "compile_agmip_start_map (thematic mapping)")
if (abs(sum(LUM_map_long$LUM_area_km2) - map_tot_pixels_check) > tol_rel) {
  message(sprintf("Pixel count mismatch: %.4f (Expected: %.4f, Got: %.4f)",
                  abs(sum(LUM_map_long$LUM_area_km2) - map_tot_pixels_check), map_tot_pixels_check, sum(LUM_map_long$LUM_area_km2)))
}

clean_res <- gsub("km.*$", "", downscale_resolution)
x_col <- paste0("x_", clean_res, "kmID")
y_col <- paste0("y_", clean_res, "kmID")
ns_col <- paste0("EEA_", clean_res, "kmID")

# When run from the full pipeline (run_downscaling.R), mappings$mapping_grid
# already has renamed columns: ns, x_ns, y_ns, Ns_reg.
# When run standalone, we need the raw grid column names.
pipeline_cols <- c("EEA_1kmID", "ns", "x_ns", "y_ns", "Ns_reg")
raw_cols      <- c("EEA_1kmID", ns_col, x_col, y_col, "ISO3C")
available     <- names(mappings$mapping_grid)

if (all(pipeline_cols %in% available)) {
  grid_subset <- unique(mappings$mapping_grid[, .SD, .SDcols = pipeline_cols])
} else {
  # Standalone fallback: use raw column names and rename
  use_cols <- intersect(raw_cols, available)
  grid_subset <- unique(mappings$mapping_grid[, .SD, .SDcols = use_cols])
  if (ns_col %in% names(grid_subset)) setnames(grid_subset, ns_col, "ns")
  if (x_col %in% names(grid_subset)) setnames(grid_subset, x_col, "x_ns")
  if (y_col %in% names(grid_subset)) setnames(grid_subset, y_col, "y_ns")
  if ("ISO3C" %in% names(grid_subset)) setnames(grid_subset, "ISO3C", "Ns_reg")
}

# AGMIP grid-to-grid mapping doesn't split 1km pixels across borders like NUTS does, so weight is exactly 1.
grid_subset[, pixel_weight := 1]

message(">>> [DEBUG] grid_subset cols: ", paste(names(grid_subset), collapse = ", "))
message(">>> [DEBUG] grid_subset nrow: ", nrow(grid_subset))
message(">>> [DEBUG] LUM_map_long cols before merge: ", paste(names(LUM_map_long), collapse = ", "))

LUM_map_long <- merge(LUM_map_long, grid_subset, by = "EEA_1kmID", all.x = TRUE, allow.cartesian = TRUE)

message(">>> [DEBUG] LUM_map_long cols after merge: ", paste(names(LUM_map_long), collapse = ", "))

LUM_map_long <- LUM_map_long[, .(LUM_area_km2 = sum(LUM_area_km2 * pixel_weight)),
                             by = setdiff(names(LUM_map_long), c("EEA_1kmID", "LUM_area_km2", "pixel_weight"))]

message(">>> [DEBUG] LUM_map_long cols after agg: ", paste(names(LUM_map_long), collapse = ", "))

# Final area consistency check
final_area_sum <- sum(LUM_map_long$LUM_area_km2, na.rm = TRUE)
stop_on_excessive_area_gap(final_area_sum, map_tot_area_check, "compile_agmip_start_map (grid distribution)")
if (abs(final_area_sum - map_tot_area_check) > tol_rel) {
  warning(sprintf("Spatial harmonization (irrigation & country borders) area mismatch: %.4f km2 (Expected: %.2f, Got: %.2f)",
                  abs(final_area_sum - map_tot_area_check), map_tot_area_check, final_area_sum))
} else {
  message("Spatial harmonization (irrigation & country borders) area consistency verified (", round(final_area_sum, 2), " km2).")
}

# ---------------------------------------------------------------
# 4. Filter & Handover
# ---------------------------------------------------------------
# Exclude Norway and Switzerland (outside EU scope)
LUM_map_long <- LUM_map_long[is.na(Ns_reg) | !Ns_reg %in% c("NOR", "CHE")]

if (!"TERRA" %in% names(LUM_map_long)) {
  LUM_map_long[, TERRA := TRUE]
} else {
  LUM_map_long[is.na(TERRA), TERRA := FALSE]
}

# Save pre-harmonization start map for standalone diagnostics
temp_pre_harm_save <- LUM_map_long[, .(
  ns, x_ns, y_ns, Ns_reg, lu_class_1, lu_class_2, LUM_area_km2
)][, .(LUM_area_km2 = sum(LUM_area_km2, na.rm = TRUE)),
   by = .(ns, x_ns, y_ns, Ns_reg, lu_class_1, lu_class_2)]

save_intermediate(temp_pre_harm_save, "pre_harm_start_map", downscale_resolution)
rm(temp_pre_harm_save)



# ===============================================================
# start_target_map_harmonization_check.R: Validation Script
# ===============================================================
# Compares the aggregated areas of the harmonized start map
# against the thematic targets from model targets.
# ===============================================================
require(knitr)
require(kableExtra)

# Identification of shared keys
common_keys <- intersect(names(temp_LUCOVER_COMPLETE_ACR_LUID_AEZ), names(ds_start_map))
common_keys <- setdiff(common_keys, c("GLOB_area_km2", "LUM_area_km2", "GLOB_REGION59"))

# 1. Aggregate Target Dataset
target_agg <- temp_LUCOVER_COMPLETE_ACR_LUID_AEZ[
  unique(ds_start_map[, .(GLOB_country, GLOB_LUID, GLOB_AEZ)]),
  on = .(GLOB_country, GLOB_LUID, GLOB_AEZ),
  nomatch = 0
][, .(target_area_km2 = sum(GLOB_area_km2, na.rm = TRUE)), by = common_keys]

# 2. Aggregate Start Map Dataset
start_agg <- ds_start_map[, .(start_area_km2 = sum(LUM_area_km2, na.rm = TRUE)), by = common_keys]
start_agg <- start_agg[unique(target_agg[, .(GLOB_country, GLOB_LUID, GLOB_AEZ)]),
                       on = .(GLOB_country, GLOB_LUID, GLOB_AEZ),
                       nomatch = 0]

# 3. Merge & Comparison
merged <- merge(target_agg, start_agg, by = intersect(names(target_agg), names(start_agg)), all = TRUE)

merged[is.na(target_area_km2), target_area_km2 := 0]
merged[is.na(start_area_km2),  start_area_km2  := 0]

# Exclude exogenous classes: If a class has exactly 0 target area globally, it wasn't harmonized
class_keys <- setdiff(common_keys, c("GLOB_country", "GLOB_LUID", "GLOB_AEZ", "Ns_reg"))
global_class_targets <- merged[, .(global_target = sum(target_area_km2, na.rm = TRUE)), by = class_keys]
harmonized_classes <- global_class_targets[global_target > 0]

merged <- merged[harmonized_classes[, ..class_keys], on = class_keys, nomatch = 0]

# Filter out non-relevant classes for checking land-use transitions
merged <- merged[!GLOB_lu_class %in% c("Waterbodies_marine", "Natural_other", "Urban",
                                     "Waterbodies_inland", "Wetlands_natural", "Cropland_Other", "NODATA")]

# Deviation calculation
merged[, deviation_km2 := start_area_km2 - target_area_km2]
merged[, deviation_pct := fifelse(target_area_km2 > 0, 100 * deviation_km2 / target_area_km2, NA_real_)]

# Filter out regions (LUID x AEZ) where GLOBIOM has no target area for any relevant classes (spatial mismatches)
merged[, total_target_in_region := sum(target_area_km2, na.rm = TRUE), by = .(GLOB_country, GLOB_LUID, GLOB_AEZ)]
merged <- merged[total_target_in_region > 0]
merged[, total_target_in_region := NULL]

# 4. Result display & Save
stage_prefix <- if (exists("check_stage_name")) paste0(check_stage_name, " ") else ""
file_prefix <- if (exists("check_stage_name")) paste0(tolower(gsub(" ", "_", check_stage_name)), "_") else ""

cli_h1(paste0(stage_prefix, "Harmonization Coverage Check"))
# Order by biggest absolute deviations first
merged <- merged[order(-abs(deviation_km2))]

# Only use kableExtra styling in interactive sessions, with a tryCatch safeguard
if (interactive()) {
  tryCatch({
    print(merged %>% head(30) %>% kable(digits = 2, format = "html", caption = paste0(stage_prefix, "Aggregated Area Comparison (Top 30)")) %>% 
          kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")))
  }, error = function(e) {
    message(">>> HTML table rendering failed (fallback to console print): ", e$message)
    print(head(merged, 30))
  })
} else {
  print(head(merged, 30))
}

out_dir <- if (exists("paths") && !is.null(paths$temp)) paths$temp else "temp"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(merged, file.path(out_dir, paste0(file_prefix, "start_target_map_harmonization_check_", gsub("-", "", Sys.Date()), ".csv")), row.names = FALSE)
rm(merged, target_agg, start_agg, common_keys)
if (exists("check_stage_name")) rm(check_stage_name)

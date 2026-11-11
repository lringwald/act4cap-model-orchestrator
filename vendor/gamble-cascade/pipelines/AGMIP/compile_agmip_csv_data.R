# ===============================================================
# compile_agmip_csv_data.R: AgMIP CSV Multi-Model Data Compilation
# ===============================================================
# This script replaces the GDX-reading Step 1 for models delivered via
# the AgMIP template CSV. It reads the CSV, filters to the selected
# model, maps AgMIP 3-letter region codes to country, maps AgMIP
# item codes to LAMASUS LUM thematic classes, and produces the same
# intermediates expected by the downstream pipeline.
#
# Required globals (set by master_downscaling.R):
#   current_model_cfg, paths, area_conv_factor, save_intermediate,
#   region_filter, year_filter, scen_filter
# ===============================================================
library(data.table)

# ---------------------------------------------------------------
# 0. Setup & Inheritance
# ---------------------------------------------------------------
if (!exists("area_conv_factor")) area_conv_factor <- 10  # 1000 ha = 10 km2
if (!exists("tol_rel")) tol_rel <- 1e-6

agmip_model_name <- current_model_cfg$agmip_model_name
csv_path <- file.path(paths$input_dir, current_model_cfg$input_files$main)

if (is.null(agmip_model_name) || is.null(csv_path)) {
  stop("compile_agmip_csv_data.R requires `agmip_model_name` and `input_files$main` in current_model_cfg.")
}

if (!exists("save_intermediate")) {
  aux_cands <- c("scripts/core/aux_codes.R", "scripts/aux_codes.R")
  source(aux_cands[file.exists(aux_cands)][1])
}

message(">>> [AgMIP] Reading CSV: ", basename(csv_path))

if (exists("register_source_file")) {
  register_source_file("Macro Model", paste("AgMIP CSV:", basename(csv_path)), csv_path)
}

# ---------------------------------------------------------------
# 1. Load & Filter CSV
# ---------------------------------------------------------------
cache_dir <- "data/intermediate"
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
csv_tag <- tools::file_path_sans_ext(basename(csv_path))
cache_file <- file.path(cache_dir, sprintf("agmip_dt_%s_%s.rds", tolower(paste(agmip_model_name, collapse = "_")), csv_tag))
# Fallback to legacy cache filename if vintage matches default
if (!file.exists(cache_file)) {
  legacy_cache <- file.path(cache_dir, sprintf("agmip_dt_%s.rds", tolower(paste(agmip_model_name, collapse = "_"))))
  if (file.exists(legacy_cache) && grepl("20260916", csv_tag)) cache_file <- legacy_cache
}

if (file.exists(cache_file)) {
  message(">>> [AgMIP] Loading cached model data from: ", basename(cache_file))
  dt <- readRDS(cache_file)
} else {
  message(">>> [AgMIP] Reading CSV with multi-threading: ", basename(csv_path))
  old_th <- data.table::getDTthreads()
  data.table::setDTthreads(min(8, parallel::detectCores()))
  dt_raw <- fread(csv_path)
  data.table::setDTthreads(old_th)
  
  # Normalize column names and string cases
  setnames(dt_raw, tolower(names(dt_raw)))
  dt_raw[, model := tolower(model)]
  
  # Filter to selected model first to save memory and time
  if (!("all" %in% tolower(agmip_model_name))) {
    dt_raw <- dt_raw[model %in% tolower(agmip_model_name)]
  }
  
  dt_raw[, `:=`(
    region = tolower(region),
    variable = tolower(variable),
    item = tolower(item),
    scenario = tolower(scenario)
  )]
  
  # Deduplicate if duplicate rows exist
  dt_raw <- unique(dt_raw)
  dt <- dt_raw
  rm(dt_raw)
  gc(verbose = FALSE)
  
  # Cache locally for instant loading on subsequent steps
  tryCatch(saveRDS(dt, cache_file), error = function(e) NULL)
}

message(">>> [AgMIP] Filtered to model '", agmip_model_name, "': ",
        nrow(dt), " rows, ", uniqueN(dt$scenario), " scenarios, ",
        uniqueN(dt$region), " regions.")

# ---------------------------------------------------------------
# 2. Region Mapping
# ---------------------------------------------------------------
region_map <- fread("data/aux_files/agmip_region_mapping.csv")
region_map[, agmip_code := tolower(agmip_code)]

dt <- merge(dt, region_map[, .(agmip_code, country, Ns_reg, is_eu_scope)],
            by.x = "region", by.y = "agmip_code", all.x = TRUE)

# Align Ns_reg with GLOB_country names (e.g. Germany, France, Austria) used by the mapping grid
dt[, Ns_reg := country]

# Filter to EU-scope regions only
n_before <- nrow(dt)
dt <- dt[is_eu_scope == TRUE & !is.na(Ns_reg) & Ns_reg != ""]
message(">>> [AgMIP] Filtered to EU scope: ",
        n_before, " -> ", nrow(dt), " rows (",
        uniqueN(dt$Ns_reg), " regions).")

# Apply region filter if set
if (exists("region_filter") && !is.null(region_filter)) {
  dt <- dt[Ns_reg %in% region_filter]
  message(">>> [AgMIP] Applied region_filter: ", nrow(dt), " rows remaining.")
}

# Apply scenario filter if set
if (exists("scen_filter") && !is.null(scen_filter)) {
  # Always include the base scenario
  base_scen <- current_model_cfg$base_scen
  filter_scens <- unique(c(base_scen, scen_filter))
  dt <- dt[scenario %in% filter_scens]
  message(">>> [AgMIP] Applied scen_filter: ", nrow(dt), " rows remaining (",
          paste(unique(dt$scenario), collapse = ", "), ").")
}

# Apply year filter if set
if (exists("year_filter") && !is.null(year_filter)) {
  base_year <- current_model_cfg$base_year
  dt <- dt[year %in% unique(c(base_year, year_filter))]
  message(">>> [AgMIP] Applied year_filter: ", nrow(dt), " rows remaining.")
}

# Clean up merge columns
dt[, is_eu_scope := NULL]

# ---------------------------------------------------------------
# 3. Load LU Class Mapping
# ---------------------------------------------------------------
lu_map <- fread("data/aux_files/agmip_lu_class_mapping.csv")
lu_map[, `:=`(
  model_lower = tolower(model),
  agmip_item = tolower(agmip_item)
)]

if (!("all" %in% tolower(agmip_model_name))) {
  lu_map <- lu_map[model_lower %in% tolower(agmip_model_name)]
}
message(">>> [AgMIP] Loaded ", nrow(lu_map), " LU class mappings for '",
        paste(agmip_model_name, collapse = ", "), "'.")

# ---------------------------------------------------------------
# 4. Extract Land-Use Targets (variable == "land")
# ---------------------------------------------------------------
message(">>> [AgMIP] Extracting land-use targets...")
dt[, model_lower := model]

# Synthesize CAPRI Cropland if missing from 'land' (since CAPRI reports agr and grs, crp = agr - grs)
# and split into crp|ara and crp|prm using area$crp|prm
if ("capri" %in% unique(dt$model)) {
  capri_agr <- dt[model == "capri" & variable == "land" & item == "agr"]
  capri_grs <- dt[model == "capri" & variable == "land" & item == "grs"]
  if (nrow(capri_agr) > 0 && nrow(capri_grs) > 0) {
    capri_crp <- merge(capri_agr, capri_grs, by = c("model", "scenario", "region", "country", "Ns_reg", "year"), suffixes = c("_agr", "_grs"))
    capri_crp[, value := pmax(value_agr - value_grs, 0)]
    capri_crp[, item := "crp"]
    capri_crp[, variable := "land"]
    capri_crp[, unit := "1000 ha"]
    capri_crp[, model_lower := "capri"]
    
    # Check if crp|prm exists in area to disaggregate into Level 2
    capri_prm <- dt[model == "capri" & variable == "area" & item == "crp|prm"]
    if (nrow(capri_prm) > 0) {
      c_m <- merge(capri_crp, capri_prm[, .(scenario, region, country, Ns_reg, year, val_prm = value)],
                   by = c("scenario", "region", "country", "Ns_reg", "year"), all.x = TRUE)
      c_m[is.na(val_prm), val_prm := 0]
      c_m[, val_prm := pmin(val_prm, value)]
      c_m[, val_ara := pmax(value - val_prm, 0)]
      
      c_ara <- copy(c_m)[, `:=`(item = "crp|ara", value = val_ara, val_prm = NULL, val_ara = NULL)]
      c_prm <- copy(c_m)[, `:=`(item = "crp|prm", value = val_prm, val_prm = NULL, val_ara = NULL)]
      dt <- rbindlist(list(dt, c_ara[, names(dt), with = FALSE], c_prm[, names(dt), with = FALSE]), use.names = TRUE, fill = TRUE)
      message(">>> [AgMIP] Disaggregated CAPRI cropland into crp|ara and crp|prm across ", uniqueN(c_m$Ns_reg), " EU countries.")
    } else {
      dt <- rbindlist(list(dt, capri_crp[, .(model, scenario, region, variable, item, unit, year, value, country, Ns_reg, model_lower)]), use.names = TRUE, fill = TRUE)
      message(">>> [AgMIP] Synthesized CAPRI cropland (crp = agr - grs) across ", uniqueN(capri_crp$Ns_reg), " EU countries.")
    }
  }
}

# Disaggregate CLUMONDO cropland into crp|ara and crp|prm
if ("clumondo" %in% unique(dt$model)) {
  cl_crp <- dt[model == "clumondo" & variable == "land" & item == "crp"]
  cl_prm <- dt[model == "clumondo" & variable == "land" & item == "crp|prm"]
  if (nrow(cl_crp) > 0 && nrow(cl_prm) > 0) {
    cl_m <- merge(cl_crp, cl_prm[, .(scenario, region, country, Ns_reg, year, val_prm = value)],
                  by = c("scenario", "region", "country", "Ns_reg", "year"), all.x = TRUE)
    cl_m[is.na(val_prm), val_prm := 0]
    cl_m[, val_prm := pmin(val_prm, value)]
    cl_m[, val_ara := pmax(value - val_prm, 0)]
    
    cl_ara <- copy(cl_m)[, `:=`(item = "crp|ara", value = val_ara, val_prm = NULL, val_ara = NULL)]
    cl_prm_new <- copy(cl_m)[, `:=`(item = "crp|prm", value = val_prm, val_prm = NULL, val_ara = NULL)]
    
    dt <- dt[!(model == "clumondo" & variable == "land" & item %in% c("crp", "crp|prm"))]
    dt <- rbindlist(list(dt, cl_ara[, names(dt), with = FALSE], cl_prm_new[, names(dt), with = FALSE]), use.names = TRUE, fill = TRUE)
    message(">>> [AgMIP] Disaggregated CLUMONDO cropland into crp|ara and crp|prm across ", uniqueN(cl_m$Ns_reg), " EU countries.")
  }
}

# Merge GLOBIOM energy crops (ecp) into cropland (crp)
if ("globiom" %in% dt$model) {
  dt[model == "globiom" & variable == "land" & item == "ecp", item := "crp"]
}

# Standardize MAGNET pasture (lsp -> grs) under 'land' to align with common taxonomy
if ("magnet" %in% dt$model) {
  dt[model == "magnet" & variable == "land" & item == "lsp", item := "grs"]
}

# Merge IMAGE energy crops (ecp) into arable cropland (crp|ara) under 'land'
if ("image" %in% dt$model) {
  dt[model == "image" & variable == "land" & item == "ecp", item := "crp|ara"]
}

# Floor ONV at 0 in case of raw reporting boundary deviations
dt[variable == "land" & item == "onv" & value < 0, value := 0]

dt_land <- dt[variable == "land"]

# Merge with LU mapping to identify aggregates
dt_land <- merge(dt_land, lu_map[variable == "land", .(model_lower, agmip_item, lum_class, is_aggregate)],
                 by.x = c("model_lower", "item"), by.y = c("model_lower", "agmip_item"), all.x = TRUE)

# Remove known aggregates
dt_land <- dt_land[is_aggregate == FALSE | is.na(is_aggregate)]

# Convert 1000 ha -> km2
dt_land[, area_km2 := value * area_conv_factor]

# ---------------------------------------------------------------
# 4.2 Disaggregate NLD (Non-arable Land) into BUI and OLD Targets
# ---------------------------------------------------------------
nld_base_path <- "data/aux_files/agmip_start_map_nld_baseline.csv"
if (file.exists(nld_base_path)) {
  nb <- fread(nld_base_path)
} else {
  nb <- data.table(Ns_reg = character(0), bui_base_km2 = numeric(0), old_base_km2 = numeric(0),
                   share_bui = numeric(0), share_old = numeric(0))
}

# If model reports NLD (e.g. GLOBIOM, IMAGE) but lacks explicit BUI and OLD, partition NLD into BUI and OLD
raw_nld <- dt[variable == "land" & item == "nld", .(
  nld_area_km2 = sum(value * area_conv_factor, na.rm = TRUE)
), by = .(model, region, Ns_reg, scenario, year = as.integer(year))]

if (nrow(raw_nld) > 0) {
  nld_part <- merge(raw_nld, nb[, .(Ns_reg, share_bui, share_old)], by = "Ns_reg", all.x = TRUE)
  nld_part[is.na(share_bui), `:=`(share_bui = 0.8, share_old = 0.2)]
  nld_part[, bui_area_km2 := nld_area_km2 * share_bui]
  nld_part[, old_area_km2 := nld_area_km2 * share_old]
  
  bui_recs <- nld_part[bui_area_km2 > 0, .(
    model_lower = tolower(model), item = "bui", model, scenario, region, variable = "land",
    unit = "1000 ha", year, value = bui_area_km2 / area_conv_factor, country = Ns_reg, Ns_reg,
    lum_class = "Urban", is_aggregate = FALSE, area_km2 = bui_area_km2
  )]
  old_recs <- nld_part[old_area_km2 > 0, .(
    model_lower = tolower(model), item = "old", model, scenario, region, variable = "land",
    unit = "1000 ha", year, value = old_area_km2 / area_conv_factor, country = Ns_reg, Ns_reg,
    lum_class = "Natural_other", is_aggregate = FALSE, area_km2 = old_area_km2
  )]
  
  # Remove nld from dt_land and append synthesized bui and old
  dt_land <- dt_land[item != "nld"]
  dt_land <- rbindlist(list(dt_land, bui_recs, old_recs), use.names = TRUE, fill = TRUE)
  message(">>> [AgMIP] Disaggregated NLD into BUI and OLD targets across ", uniqueN(nld_part$Ns_reg), " EU countries.")
} else if (!any(dt_land$item %in% c("bui", "old")) && nrow(nb) > 0) {
  # If model reports neither NLD nor BUI/OLD (e.g. MAGNET), inject static start-map baseline BUI and OLD
  grid_scen_year <- unique(dt_land[, .(model, region, Ns_reg, scenario, year)])
  mag_bui_old <- merge(grid_scen_year, nb[, .(Ns_reg, bui_base_km2, old_base_km2)], by = "Ns_reg", all.x = TRUE)
  mag_bui_old[is.na(bui_base_km2), bui_base_km2 := 0]
  mag_bui_old[is.na(old_base_km2), old_base_km2 := 0]
  
  bui_recs <- mag_bui_old[bui_base_km2 > 0, .(
    model_lower = tolower(model), item = "bui", model, scenario, region, variable = "land",
    unit = "1000 ha", year, value = bui_base_km2 / area_conv_factor, country = Ns_reg, Ns_reg,
    lum_class = "Urban", is_aggregate = FALSE, area_km2 = bui_base_km2
  )]
  old_recs <- mag_bui_old[old_base_km2 > 0, .(
    model_lower = tolower(model), item = "old", model, scenario, region, variable = "land",
    unit = "1000 ha", year, value = old_base_km2 / area_conv_factor, country = Ns_reg, Ns_reg,
    lum_class = "Natural_other", is_aggregate = FALSE, area_km2 = old_base_km2
  )]
  dt_land <- rbindlist(list(dt_land, bui_recs, old_recs), use.names = TRUE, fill = TRUE)
  message(">>> [AgMIP] Added static baseline BUI and OLD targets across ", uniqueN(mag_bui_old$Ns_reg), " EU countries.")
}

# ---------------------------------------------------------------
# 4.3 Disaggregate Arable Cropland into Individual Crop Targets
# ---------------------------------------------------------------
ci_script_cands <- c(
  "pipelines/AGMIP/compute_agmip_multi_cropping_index.R",
  "scripts/AGMIP/compute_agmip_multi_cropping_index.R",
  "compute_agmip_multi_cropping_index.R"
)
ci_script <- ci_script_cands[file.exists(ci_script_cands)][1]
if (!is.na(ci_script)) source(ci_script)
ci_res <- calculate_agmip_cropping_intensity(dt, agmip_model_name, lu_map, area_conv_factor = area_conv_factor)
crop_targets_country <- ci_res$crop_physical_targets

# ---------------------------------------------------------------
# 4.4 Disaggregate Forest Land into FOR|MAN and FOR|PRIM Targets
# ---------------------------------------------------------------
forest_base_path <- "data/aux_files/agmip_start_map_forest_baseline.csv"
if (file.exists(forest_base_path)) {
  fb <- fread(forest_base_path)
  fb[, base_share_man := ifelse(for_tot_base_km2 > 0, for_man_base_km2 / for_tot_base_km2, 1.0)]
  fb[, base_share_prim := 1.0 - base_share_man]
} else {
  fb <- data.table(Ns_reg = character(0), base_share_man = numeric(0), base_share_prim = numeric(0))
}

tot_for_dt <- dt[(variable == "land" | variable == "area") & item %in% c("for", "for|man", "for|prim"), .(
  tot_for_km2 = max(value * area_conv_factor, na.rm = TRUE)
), by = .(model, region, Ns_reg, scenario, year = as.integer(year))]

dyn_for <- dt[(variable == "area" | variable == "land") & item %in% c("for|man", "for|prim"), .(
  area_dyn = sum(value * area_conv_factor, na.rm = TRUE)
), by = .(model, region, Ns_reg, scenario, year = as.integer(year), item)]

if (nrow(dyn_for) > 0) {
  dyn_for_wide <- dcast(dyn_for, model + region + Ns_reg + scenario + year ~ item, value.var = "area_dyn", fill = 0)
  if (!"for|man" %in% names(dyn_for_wide)) dyn_for_wide[, `for|man` := 0]
  if (!"for|prim" %in% names(dyn_for_wide)) dyn_for_wide[, `for|prim` := 0]
  dyn_for_wide[, sum_dyn := `for|man` + `for|prim`]
} else {
  dyn_for_wide <- data.table(model = character(0), region = character(0), Ns_reg = character(0),
                             scenario = character(0), year = integer(0), `for|man` = numeric(0),
                             `for|prim` = numeric(0), sum_dyn = numeric(0))
}

for_targets <- merge(tot_for_dt, fb[, .(Ns_reg, base_share_man, base_share_prim)], by = "Ns_reg", all.x = TRUE)
for_targets[is.na(base_share_man), `:=`(base_share_man = 0.9, base_share_prim = 0.1)]
for_targets <- merge(for_targets, dyn_for_wide, by = c("model", "region", "Ns_reg", "scenario", "year"), all.x = TRUE)
for_targets[is.na(sum_dyn), sum_dyn := 0]

# Compute dynamic or baseline shares
for_targets[, `:=`(
  man_area_km2 = fifelse(
    `for|man` > 0 & `for|prim` > 0,
    tot_for_km2 * (`for|man` / sum_dyn),
    fifelse(
      `for|man` > 0 & (tot_for_km2 - `for|man`) > 1e-3,
      `for|man`,
      fifelse(
        `for|man` > 0 & `for|prim` == 0,
        tot_for_km2 * base_share_man,
        tot_for_km2 * base_share_man
      )
    )
  ),
  prim_area_km2 = fifelse(
    `for|man` > 0 & `for|prim` > 0,
    tot_for_km2 * (`for|prim` / sum_dyn),
    fifelse(
      `for|man` > 0 & (tot_for_km2 - `for|man`) > 1e-3,
      pmax(tot_for_km2 - `for|man`, 0),
      fifelse(
        `for|man` > 0 & `for|prim` == 0,
        tot_for_km2 * base_share_prim,
        tot_for_km2 * base_share_prim
      )
    )
  )
)]

forest_targets_country <- rbind(
  for_targets[man_area_km2 > 0, .(model, region, Ns_reg, lu_class = "FOR|MAN", scenario, year, area_km2 = man_area_km2)],
  for_targets[prim_area_km2 > 0, .(model, region, Ns_reg, lu_class = "FOR|PRIM", scenario, year, area_km2 = prim_area_km2)]
)

if (nrow(crop_targets_country) > 0) {
  # If any permanent crops are already disaggregated or present in crop targets (e.g. CRP|PRM or CRP|PRM|FRU),
  # exclude aggregate crp|prm from dt_non_crop to prevent double counting.
  has_prm <- any(grepl("^CRP\\|PRM", crop_targets_country$lu_class))
  
  if (has_prm) {
    dt_non_crop <- dt_land[!item %in% c("crp", "crp|ara", "crp|prm", "agr", "tot", "for", "for|man", "for|prim", "nld")]
  } else {
    dt_non_crop <- dt_land[!item %in% c("crp", "crp|ara", "agr", "tot", "for", "for|man", "for|prim", "nld")]
  }
  
  land_targets_country <- dt_non_crop[, .(
    area_km2 = sum(area_km2, na.rm = TRUE)
  ), by = .(model, region, Ns_reg, lu_class = toupper(item), scenario, year = as.integer(year))]
  
  # Ensure crp|prm is standardized to CRP|PRM
  land_targets_country[lu_class %in% c("CRP|PRM", "PRM"), lu_class := "CRP|PRM"]
  
  # Combine non-cropland, forest Level 2, and crop targets
  LUC_targets_country <- rbindlist(list(land_targets_country, forest_targets_country, crop_targets_country), use.names = TRUE, fill = TRUE)
  message(">>> [AgMIP] Successfully integrated ", uniqueN(crop_targets_country$lu_class), 
          " physical crop types and Level 2 forests (FOR|MAN, FOR|PRIM) into downscaling targets.")
  
  if (nrow(ci_res$ci_summary) > 0) save_intermediate(ci_res$ci_summary, "ci_summary")
  if (nrow(ci_res$crop_physical_yields) > 0) save_intermediate(ci_res$crop_physical_yields, "crop_physical_yields")
} else {
  # Build LUC targets at country level using the raw items (and sum sub-national units e.g. CLUMONDO)
  dt_non_for <- dt_land[!item %in% c("agr", "tot", "for", "for|man", "for|prim", "nld")]
  land_targets_country <- dt_non_for[, .(
    area_km2 = sum(area_km2, na.rm = TRUE)
  ), by = .(model, region, Ns_reg, lu_class = toupper(item), scenario, year = as.integer(year))]
  land_targets_country[lu_class %in% c("CRP|PRM", "PRM"), lu_class := "CRP|PRM"]
  land_targets_country[lu_class %in% c("CRP|ARA", "ARA"), lu_class := "CRP|ARA"]
  
  LUC_targets_country <- rbindlist(list(land_targets_country, forest_targets_country), use.names = TRUE, fill = TRUE)
}

# ---------------------------------------------------------------
# 4.5 Area Integrity Check
# ---------------------------------------------------------------
message(">>> [AgMIP] Checking total land area consistency...")
area_check <- LUC_targets_country[, .(total_area = sum(area_km2, na.rm = TRUE)),
                                  by = .(model, region, Ns_reg, scenario, year)]

area_check[, mean_area := mean(total_area), by = .(model, region, Ns_reg, scenario)]
area_check[, pct_diff := abs(total_area - mean_area) / (mean_area + 1e-9) * 100]

outliers <- area_check[pct_diff > 1.0 | total_area < 0]
if (nrow(outliers) > 0) {
  warning(">>> [AgMIP] WARNING: Unreasonable total area fluctuations (>1%) or negative areas detected:")
  print(outliers[order(-pct_diff), .(model, Ns_reg, scenario, year, total_area = round(total_area, 1), pct_dev = round(pct_diff, 2))])
} else {
  message(">>> [AgMIP] Total area consistency check passed (fluctuations < 1%).")
}

message(">>> [AgMIP] Land-use targets: ", nrow(LUC_targets_country),
        " rows, ", uniqueN(LUC_targets_country$lu_class), " classes: ",
        paste(sort(unique(LUC_targets_country$lu_class)), collapse = ", "))

# Save land targets
save_intermediate(LUC_targets_country, "LUC_targets_country")
message(">>> [AgMIP] Saved LUC_targets_country.")

# ---------------------------------------------------------------
# 5. Extract Crop Area Targets (variable == "area")
# ---------------------------------------------------------------
message(">>> [AgMIP] Extracting crop area targets...")
dt_area <- dt[variable == "area"]

if (nrow(dt_area) > 0) {
  # Merge with LU mapping for area items
  dt_area <- merge(dt_area, lu_map[variable == "area", .(model_lower, agmip_item, agmip_parent, lum_class, lum_hierarchy, is_aggregate)],
                   by.x = c("model_lower", "item"), by.y = c("model_lower", "agmip_item"), all.x = TRUE)

  # Filter to mapped, non-aggregate items
  dt_area <- dt_area[is_aggregate == FALSE & !is.na(lum_class)]

  # Convert 1000 ha -> km2
  dt_area[, area_km2 := value * area_conv_factor]

  crop_parameters <- dt_area[, .(
    model = model,
    region = region,
    Ns_reg = Ns_reg,
    crop_type = item,
    lu_class = toupper(item),
    scenario = scenario,
    year = as.integer(year),
    area_km2 = area_km2
  )]

  # Aggregate to unique key
  crop_parameters <- crop_parameters[,
    .(area_km2 = sum(area_km2, na.rm = TRUE)),
    by = .(model, region, Ns_reg, crop_type, lu_class, scenario, year)
  ]

  message(">>> [AgMIP] Crop parameters: ", nrow(crop_parameters),
          " rows, ", uniqueN(crop_parameters$crop_type), " crop types.")
  save_intermediate(crop_parameters, "crop_parameters")
  message(">>> [AgMIP] Saved crop_parameters.")
} else {
  message(">>> [AgMIP] No 'area' variable data found for this model.")
}

# ---------------------------------------------------------------
# 6. Extract Livestock (variable == "annr")
# ---------------------------------------------------------------
message(">>> [AgMIP] Extracting livestock data...")
dt_lsu <- dt[variable == "annr"]

if (nrow(dt_lsu) > 0) {
  livestock_parameters <- dt_lsu[, .(
    model = model,
    region = region,
    Ns_reg = Ns_reg,
    livestock_type = item,
    scenario = scenario,
    year = as.integer(year),
    lsu_1000 = value  # already in 1000 LSU
  )]

  # Aggregate
  livestock_parameters <- livestock_parameters[,
    .(lsu_1000 = sum(lsu_1000, na.rm = TRUE)),
    by = .(model, region, Ns_reg, livestock_type, scenario, year)
  ]

  message(">>> [AgMIP] Livestock parameters: ", nrow(livestock_parameters), " rows.")
  save_intermediate(livestock_parameters, "livestock_parameters")
  message(">>> [AgMIP] Saved livestock_parameters.")
} else {
  message(">>> [AgMIP] No 'annr' variable data found for this model.")
}

# ---------------------------------------------------------------
# 7. Extract Production Data (variable == "prod")
# ---------------------------------------------------------------
message(">>> [AgMIP] Extracting production data...")
dt_prod <- dt[variable == "prod"]

if (nrow(dt_prod) > 0) {
  production_targets <- dt_prod[, .(
    model = model,
    region = region,
    Ns_reg = Ns_reg,
    crop_type = item,
    scenario = scenario,
    year = as.integer(year),
    value = value,
    unit = unit
  )]

  message(">>> [AgMIP] Production targets: ", nrow(production_targets), " rows.")
  save_intermediate(production_targets, "OUTPUT_targets")
  message(">>> [AgMIP] Saved OUTPUT_targets.")
} else {
  message(">>> [AgMIP] No 'prod' variable data found for this model.")
}

# ---------------------------------------------------------------
# 8. Extract Other Parameters (Inputs, Economics, Emissions, etc.)
# ---------------------------------------------------------------
message(">>> [AgMIP] Extracting other parameters (inputs, economics, etc.)...")
dt_other <- dt[!variable %in% c("land", "area", "annr", "prod")]

if (nrow(dt_other) > 0) {
  other_parameters <- dt_other[, .(
    model = model,
    region = region,
    Ns_reg = Ns_reg,
    variable = variable,
    item = item,
    scenario = scenario,
    year = as.integer(year),
    value = value,
    unit = unit
  )]

  # Aggregate in case of duplicates
  other_parameters <- other_parameters[,
    .(value = sum(value, na.rm = TRUE)),
    by = .(model, region, Ns_reg, variable, item, scenario, year, unit)
  ]

  message(">>> [AgMIP] Other parameters: ", nrow(other_parameters), " rows.")
  save_intermediate(other_parameters, "other_parameters")
  message(">>> [AgMIP] Saved other_parameters.")
} else {
  message(">>> [AgMIP] No other variable data found for this model.")
}

# ---------------------------------------------------------------
# 9. Summary
# ---------------------------------------------------------------
message("\n>>> [AgMIP] ===== COMPILATION SUMMARY =====")
message(">>> [AgMIP] Model: ", agmip_model_name)
message(">>> [AgMIP] Scenarios: ", paste(sort(unique(dt$scenario)), collapse = ", "))
message(">>> [AgMIP] Years: ", paste(sort(unique(as.integer(dt$year))), collapse = ", "))
message(">>> [AgMIP] Regions: ", uniqueN(dt$Ns_reg), " (",
        paste(sort(unique(dt$Ns_reg)), collapse = ", "), ")")
message(">>> [AgMIP] Land-use target classes: ",
        paste(sort(unique(LUC_targets_country$lu_class)), collapse = ", "))
message(">>> [AgMIP] ================================\n")

# Clean up
rm(dt, dt_land)
if (exists("dt_area")) rm(dt_area)
if (exists("dt_lsu")) rm(dt_lsu)
if (exists("dt_prod")) rm(dt_prod)
if (exists("dt_other")) rm(dt_other)
gc(verbose = FALSE)

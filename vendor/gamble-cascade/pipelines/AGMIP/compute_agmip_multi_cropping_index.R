# ===============================================================
# compute_agmip_multi_cropping_index.R
# ===============================================================
# Derives endogenous multi-cropping index (CI), physical crop targets,
# and explicit residual cropland (CRP|OTH) directly from macro-model
# outputs for GLOBIOM, CAPRI, IMAGE, and MAGNET.
#
# Guarantees:
# 1. Exact physical land conservation (0% drift across years)
# 2. Complete cropland accounting: Crops + CRP|PRM + CRP|OTH
# 3. Exact commodity production conservation: A_phys * Y_phys == P_macro
# ===============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(cli)
})

#' Calculate Endogenous Cropping Intensity & Physical Crop Targets
#' @param dt data.table containing normalized macro-model variables
#' @param model_name Character, e.g. "globiom", "capri", "image", "magnet", "clumondo"
#' @param lu_map data.table of LU class mapping
#' @param phys_cropland_dt Optional data.table with precomputed physical cropland per region/scenario/year
#' @param area_conv_factor Numeric conversion factor from 1000 ha to km2 (default: 10)
#' @return List with:
#'   - crop_physical_targets: data.table of physical crop targets in km2 for LUC targets
#'   - ci_summary: data.table of CI ratios and residual CRP|OTH areas
#'   - crop_physical_yields: data.table of calibrated physical yields
#' @export
calculate_agmip_cropping_intensity <- function(dt, model_name, lu_map, phys_cropland_dt = NULL, area_conv_factor = 10) {
  model_lower <- tolower(model_name)
  
  # CLUMONDO does not simulate crop activities; returns empty crop targets (stays Level 2)
  if (model_lower == "clumondo") {
    cli_alert_info("CLUMONDO does not model crop activities; keeping Level 2 cropland.")
    return(list(
      crop_physical_targets = data.table(),
      ci_summary = data.table(),
      crop_physical_yields = data.table()
    ))
  }
  
  cli_alert_info("Calculating endogenous multi-cropping index & physical crop targets for {toupper(model_lower)}...")
  
  # Dynamic column normalization
  dt <- copy(dt)
  setnames(dt, tolower(names(dt)))
  if (!"region" %in% names(dt)) {
    if ("native_region_code" %in% names(dt)) dt[, region := native_region_code]
    else if ("ns_reg" %in% names(dt)) dt[, region := ns_reg]
    else dt[, region := country]
  }
  if (!"ns_reg" %in% names(dt)) {
    dt[, ns_reg := country]
  }
  if (!"country" %in% names(dt)) {
    dt[, country := ns_reg]
  }
  
  # 1. Identify active crop items from LU mapping
  m_lu <- lu_map[tolower(model) == model_lower]
  crop_map <- m_lu[variable == "area" & lum_class %in% c("Cropland", "Cropland_other") & !agmip_item %in% c("crp|prm", "crp", "crp|ara", "agr", "tot") & (is_aggregate == FALSE | is.na(is_aggregate))]
  valid_crops <- unique(tolower(crop_map$agmip_item))
  
  if (length(valid_crops) == 0) {
    # Fallback to standard crop list
    valid_crops <- c("wht", "cgr|mai", "cgr|ocg", "osd|srs", "osd|ooc", "ric", "sgc", 
                     "pfb|cot", "pfb", "vfn|veg", "vfn|fru", "vfn|root", "vfn|puls", "vfn|nut", "ocr", "cgr", "osd", "vfn", "ecp")
  }
  
  prm_items <- c("vfn|fru", "vfn|nut", "crp|prm", "prm")
  
  # 2. Extract harvested crop areas from dt (variable == "area")
  dt_crop <- dt[variable == "area" & tolower(item) %in% valid_crops & value > 0]
  if (nrow(dt_crop) == 0) {
    cli_alert_warning("No crop activity area records found for {toupper(model_lower)}.")
    return(list(
      crop_physical_targets = data.table(),
      ci_summary = data.table(),
      crop_physical_yields = data.table()
    ))
  }
  
  # Aggregate by country / Ns_reg, scenario, year, crop
  crop_harv <- dt_crop[, .(
    harv_area_1000ha = sum(value, na.rm = TRUE)
  ), by = .(model = tolower(model), region, country, Ns_reg = ns_reg, scenario, year = as.integer(year), crop = toupper(item))]
  
  crop_harv[, is_permanent := tolower(crop) %in% prm_items]
  
  # 3. Determine physical arable and permanent cropland envelopes
  phys_ara <- data.table()
  
  if (!is.null(phys_cropland_dt) && nrow(phys_cropland_dt) > 0) {
    p_copy <- copy(phys_cropland_dt)
    setnames(p_copy, tolower(names(p_copy)))
    if (!"phys_ara_1000ha" %in% names(p_copy)) {
      cand <- grep("phys|cropland|area|val", names(p_copy), value = TRUE)
      cand <- cand[!cand %in% c("model", "region", "country", "ns_reg", "scenario", "year")]
      if (length(cand) > 0) {
        setnames(p_copy, cand[1], "phys_ara_1000ha")
      }
    }
    p_cols <- intersect(c("model", "region", "country", "ns_reg", "scenario", "year", "phys_ara_1000ha"), names(p_copy))
    phys_ara <- p_copy[, ..p_cols]
    if ("ns_reg" %in% names(phys_ara) && !"Ns_reg" %in% names(phys_ara)) setnames(phys_ara, "ns_reg", "Ns_reg")
  } else if (model_lower == "capri") {
    # In CAPRI, crp|ara is arable cropland (excluding permanent crops crp|prm)
    phys_ara <- dt[variable == "land" & item == "crp|ara", .(
      phys_ara_1000ha = sum(value, na.rm = TRUE)
    ), by = .(model = tolower(model), region, country, Ns_reg = ns_reg, scenario, year = as.integer(year))]
    
    if (nrow(phys_ara) == 0) {
      c_agr <- dt[variable == "land" & item == "agr"]
      c_grs <- dt[variable == "land" & item == "grs"]
      c_prm <- dt[variable == "area" & item == "crp|prm"]
      if (nrow(c_agr) > 0 && nrow(c_grs) > 0) {
        m_calc <- merge(c_agr, c_grs, by = c("scenario", "region", "country", "ns_reg", "year"), suffixes = c("_agr", "_grs"))
        m_calc[, crp := pmax(value_agr - value_grs, 0)]
        if (nrow(c_prm) > 0) {
          m_calc <- merge(m_calc, c_prm[, .(scenario, region, country, ns_reg, year, val_prm = value)],
                          by = c("scenario", "region", "country", "ns_reg", "year"), all.x = TRUE)
          m_calc[is.na(val_prm), val_prm := 0]
          m_calc[, phys_ara_1000ha := pmax(crp - val_prm, 0)]
        } else {
          m_calc[, phys_ara_1000ha := crp]
        }
        phys_ara <- m_calc[, .(model = "capri", region, country, Ns_reg = ns_reg, scenario, year = as.integer(year), phys_ara_1000ha)]
      }
    }
  } else if (model_lower == "image") {
    p_ara <- dt[variable == "land" & item == "crp|ara", .(phys_ara_1000ha = sum(value, na.rm = TRUE)),
                by = .(model = tolower(model), region, country, Ns_reg = ns_reg, scenario, year = as.integer(year))]
    p_ecp <- dt[variable == "land" & item == "ecp", .(ecp_1000ha = sum(value, na.rm = TRUE)),
                by = .(model = tolower(model), region, country, Ns_reg = ns_reg, scenario, year = as.integer(year))]
    phys_ara <- merge(p_ara, p_ecp, by = c("model", "region", "country", "Ns_reg", "scenario", "year"), all = TRUE)
    phys_ara[is.na(phys_ara_1000ha), phys_ara_1000ha := 0]
    phys_ara[is.na(ecp_1000ha), ecp_1000ha := 0]
    phys_ara[, phys_ara_1000ha := phys_ara_1000ha + ecp_1000ha]
    phys_ara[, ecp_1000ha := NULL]
  } else if (model_lower == "magnet") {
    # In MAGNET, land$crp is total cropland. Subtract permanent crop area to obtain arable envelope.
    p_tot <- dt[variable == "land" & item == "crp", .(crp_tot_1000ha = sum(value, na.rm = TRUE)),
                by = .(model = tolower(model), region, country, Ns_reg = ns_reg, scenario, year = as.integer(year))]
    p_prm <- crop_harv[is_permanent == TRUE, .(phys_prm_1000ha = sum(harv_area_1000ha)),
                       by = .(model = tolower(model), region, country, Ns_reg, scenario, year = as.integer(year))]
    phys_ara <- merge(p_tot, p_prm, by = c("model", "region", "country", "Ns_reg", "scenario", "year"), all.x = TRUE)
    phys_ara[is.na(phys_prm_1000ha), phys_prm_1000ha := 0]
    phys_ara[, phys_ara_1000ha := pmax(crp_tot_1000ha - phys_prm_1000ha, 0)]
    phys_ara[, crp_tot_1000ha := NULL]
    phys_ara[, phys_prm_1000ha := NULL]
  } else {
    # GLOBIOM / models without explicit permanent crops:
    # Subtract baseline permanent crops from raw start map to isolate arable envelope.
    crp_tot <- dt[variable == "land" & item == "crp", .(
      crp_tot_1000ha = sum(value, na.rm = TRUE)
    ), by = .(model = tolower(model), region, country, Ns_reg = ns_reg, scenario, year = as.integer(year))]
    
    aux_prm_file <- "data/aux_files/agmip_start_map_cropland_baseline.csv"
    if (file.exists(aux_prm_file) && nrow(crop_harv[is_permanent == TRUE]) == 0) {
      prm_baseline <- fread(aux_prm_file)
      prm_baseline[, Ns_reg_upper := toupper(Ns_reg)]
      prm_baseline[, prm_base_1000ha := prm_base_km2 / area_conv_factor]
      
      phys_ara <- merge(crp_tot, prm_baseline[, .(Ns_reg_upper, prm_base_1000ha)],
                        by.x = "Ns_reg", by.y = "Ns_reg_upper", all.x = TRUE)
      phys_ara[is.na(prm_base_1000ha), prm_base_1000ha := 0]
      phys_ara[, phys_ara_1000ha := pmax(crp_tot_1000ha - prm_base_1000ha, 0)]
      phys_ara[, crp_tot_1000ha := NULL]
      cli_alert_info("Differentiated {toupper(model_lower)} cropland into arable and permanent using baseline start map ({round(sum(prm_baseline$prm_base_km2)/area_conv_factor, 1)} kha static CRP|PRM).")
    } else {
      phys_ara <- crp_tot[, .(model, region, country, Ns_reg, scenario, year, phys_ara_1000ha = crp_tot_1000ha)]
    }
  }
  
  # 4. Standardize spatial key (Ns_reg)
  orig_ns_reg <- unique(crop_harv[, .(Ns_reg_orig = Ns_reg, Ns_reg_upper = toupper(Ns_reg))])

  crop_harv[, Ns_reg := toupper(Ns_reg)]
  crop_harv[, scenario := tolower(scenario)]
  crop_harv[, model := tolower(model)]
  crop_harv[, year := as.integer(year)]

  tot_harv_ara <- crop_harv[is_permanent == FALSE, .(
    tot_harv_1000ha = sum(harv_area_1000ha, na.rm = TRUE)
  ), by = .(model, region, country, Ns_reg, scenario, year)]
  
  if (nrow(phys_ara) == 0) {
    cli_alert_warning("Could not identify physical arable cropland target for {toupper(model_lower)}; falling back to sum of crops.")
    phys_ara <- tot_harv_ara[, .(model, region, country, Ns_reg, scenario, year, phys_ara_1000ha = tot_harv_1000ha)]
  }
  
  if (!"Ns_reg" %in% names(phys_ara)) {
    if ("ns_reg" %in% names(phys_ara)) setnames(phys_ara, "ns_reg", "Ns_reg")
    else if ("region" %in% names(phys_ara)) setnames(phys_ara, "region", "Ns_reg")
    else if ("country" %in% names(phys_ara)) setnames(phys_ara, "country", "Ns_reg")
  }
  phys_ara[, Ns_reg := toupper(Ns_reg)]
  phys_ara[, scenario := tolower(scenario)]
  phys_ara[, model := tolower(model)]
  phys_ara[, year := as.integer(year)]
  
  # Deduplicate if needed
  phys_ara_clean <- phys_ara[, .(phys_ara_1000ha = sum(phys_ara_1000ha, na.rm = TRUE)),
                             by = .(model, Ns_reg, scenario, year)]
  
  ci_dt <- merge(tot_harv_ara, phys_ara_clean, by = c("model", "Ns_reg", "scenario", "year"), all = TRUE)
  ci_dt[is.na(tot_harv_1000ha), tot_harv_1000ha := 0]
  ci_dt[is.na(phys_ara_1000ha), phys_ara_1000ha := tot_harv_1000ha]
  if (!"country" %in% names(ci_dt) || all(is.na(ci_dt$country))) ci_dt[, country := Ns_reg]
  if (!"region" %in% names(ci_dt) || all(is.na(ci_dt$region))) ci_dt[, region := Ns_reg]
  
  # 5. Core Accounting on Arable Cropland:
  # Base-year anchored CI formulation:
  # Base year: derive CI_base = max(tot_harv / phys_ara, 1.0)
  # Future years: freeze CI_base so crop dynamics translate into genuine physical footprint changes;
  # if tot_harv expands beyond phys_ara, CI scales up to preserve physical arable envelope.
  base_year_val <- min(ci_dt$year, na.rm = TRUE)
  ci_base_dt <- ci_dt[year == base_year_val, .(
    model, Ns_reg, scenario,
    ci_base = fifelse(tot_harv_1000ha > (phys_ara_1000ha + 1e-6),
                      tot_harv_1000ha / pmax(phys_ara_1000ha, 1e-6), 1.0)
  )]
  ci_dt <- merge(ci_dt, ci_base_dt, by = c("model", "Ns_reg", "scenario"), all.x = TRUE)
  ci_dt[is.na(ci_base), ci_base := 1.0]

  ci_dt[, ci := pmax(ci_base, tot_harv_1000ha / pmax(phys_ara_1000ha, 1e-6))]
  ci_dt[, is_multicropping := ci > (1.0 + 1e-6)]
  ci_dt[, crp_oth_1000ha := pmax(phys_ara_1000ha - (tot_harv_1000ha / ci), 0)]
  
  cli_alert_success("Computed endogenous arable CI: {sum(ci_dt$is_multicropping)} multi-cropping cases (max CI: {round(max(ci_dt$ci), 2)}), {sum(!ci_dt$is_multicropping)} fallow/residual cases (max CRP|ARA|OTH: {round(max(ci_dt$crp_oth_1000ha), 1)} kha)")
  
  # 6. Distribute physical area to arable crops
  crop_phys_ara <- merge(crop_harv[is_permanent == FALSE], ci_dt[, .(model, Ns_reg, scenario, year, ci, phys_ara_1000ha)],
                         by = c("model", "Ns_reg", "scenario", "year"))
  crop_phys_ara[, phys_area_1000ha := harv_area_1000ha / ci]
  crop_phys_ara[, crop_clean := paste0("CRP|ARA|", toupper(crop))]
  
  # Add CRP|ARA|OTH records where positive
  oth_records <- ci_dt[crp_oth_1000ha > 1e-6, .(
    model, region, country, Ns_reg, scenario, year,
    crop = "CRP|ARA|OTH",
    harv_area_1000ha = 0,
    ci = 1.0,
    phys_ara_1000ha,
    phys_area_1000ha = crp_oth_1000ha,
    is_permanent = FALSE,
    crop_clean = "CRP|ARA|OTH"
  )]
  
  # Permanent crops
  crop_phys_prm <- crop_harv[is_permanent == TRUE]
  if (nrow(crop_phys_prm) > 0) {
    crop_phys_prm[, ci := 1.0]
    crop_phys_prm[, phys_ara_1000ha := 0]
    crop_phys_prm[, phys_area_1000ha := harv_area_1000ha]
    crop_phys_prm[, crop_clean := fifelse(crop == "VFN|FRU", "CRP|PRM|FRU",
                                  fifelse(crop == "VFN|NUT", "CRP|PRM|NUT", "CRP|PRM"))]
  } else if (exists("prm_baseline") && nrow(prm_baseline) > 0) {
    # Generate static CRP|PRM records across all scenarios and years from baseline start map
    grid_scen_year <- unique(ci_dt[, .(model, region, country, Ns_reg, scenario, year)])
    crop_phys_prm <- merge(grid_scen_year, prm_baseline[, .(Ns_reg_upper, prm_base_1000ha)],
                           by.x = "Ns_reg", by.y = "Ns_reg_upper", all.x = TRUE)
    crop_phys_prm[is.na(prm_base_1000ha), prm_base_1000ha := 0]
    crop_phys_prm <- crop_phys_prm[prm_base_1000ha > 1e-6]
    crop_phys_prm[, `:=`(
      crop = "CRP|PRM",
      harv_area_1000ha = prm_base_1000ha,
      ci = 1.0,
      phys_ara_1000ha = 0,
      phys_area_1000ha = prm_base_1000ha,
      is_permanent = TRUE,
      crop_clean = "CRP|PRM"
    )]
  }
  
  crop_phys_all <- rbindlist(list(crop_phys_ara, oth_records, crop_phys_prm), use.names = TRUE, fill = TRUE)
  
  # Convert to km2 for LUC targets
  crop_phys_all[, area_km2 := phys_area_1000ha * area_conv_factor]

  # Build target table format: (model, region, Ns_reg, lu_class, scenario, year, area_km2)
  crop_targets_country <- crop_phys_all[, .(
    area_km2 = sum(area_km2, na.rm = TRUE)
  ), by = .(model, region, Ns_reg, lu_class = crop_clean, scenario, year = as.integer(year))]
  
  # 7. Verification: Sum of physical arable crops + CRP|ARA|OTH == Target Arable Cropland
  check_sum <- crop_phys_all[grepl("^CRP\\|ARA\\|", crop_clean), .(
    sum_phys = sum(phys_area_1000ha),
    target_ara = head(phys_ara_1000ha, 1)
  ), by = .(model, Ns_reg, scenario, year)]
  check_sum[, diff := abs(sum_phys - target_ara)]
  max_diff <- max(check_sum$diff, na.rm = TRUE)
  
  if (max_diff > 1e-4) {
    cli_alert_danger("ERROR in arable crop target derivation: maximum land discrepancy is {max_diff} kha!")
  } else {
    cli_alert_success("Verified exact physical land conservation across all arable crops + CRP|ARA|OTH (max diff: {format(max_diff, digits=3)} kha).")
  }
  
  # 8. Physical yields: Y_phys = Y_harv * CI
  dt_yild <- dt[variable == "yild" & tolower(item) %in% valid_crops & value > 0]
  crop_physical_yields <- data.table()
  if (nrow(dt_yild) > 0) {
    yild_dt <- dt_yild[, .(
      harv_yield = mean(value, na.rm = TRUE)
    ), by = .(model = tolower(model), region, country, Ns_reg = ns_reg, scenario, year = as.integer(year), crop = toupper(item))]
    yild_dt[, Ns_reg := toupper(Ns_reg)]
    yild_dt[, scenario := tolower(scenario)]
    yild_dt[, model := tolower(model)]
    yild_dt[, year := as.integer(year)]
    
    crop_physical_yields <- merge(yild_dt, ci_dt[, .(model, Ns_reg, scenario, year, ci)],
                                  by = c("model", "Ns_reg", "scenario", "year"))
    crop_physical_yields[, phys_yield := harv_yield * ci]
  }
  
  # Restore original casing of Ns_reg (e.g. Title Case matching start map)
  if (nrow(orig_ns_reg) > 0) {
    crop_targets_country <- merge(crop_targets_country, orig_ns_reg, by.x = "Ns_reg", by.y = "Ns_reg_upper", all.x = TRUE)
    crop_targets_country[!is.na(Ns_reg_orig), Ns_reg := Ns_reg_orig]
    crop_targets_country[, Ns_reg_orig := NULL]
    
    ci_dt <- merge(ci_dt, orig_ns_reg, by.x = "Ns_reg", by.y = "Ns_reg_upper", all.x = TRUE)
    ci_dt[!is.na(Ns_reg_orig), Ns_reg := Ns_reg_orig]
    ci_dt[, Ns_reg_orig := NULL]
    
    if (nrow(crop_physical_yields) > 0) {
      crop_physical_yields <- merge(crop_physical_yields, orig_ns_reg, by.x = "Ns_reg", by.y = "Ns_reg_upper", all.x = TRUE)
      crop_physical_yields[!is.na(Ns_reg_orig), Ns_reg := Ns_reg_orig]
      crop_physical_yields[, Ns_reg_orig := NULL]
    }
  }

  return(list(
    crop_physical_targets = crop_targets_country,
    ci_summary = ci_dt,
    crop_physical_yields = crop_physical_yields
  ))
}

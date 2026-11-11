# ===============================================================
# compute_agmip_inputs_downscaling.R
# ===============================================================
# Step 5a: Production and Input Indices Downscaling for AgMIP
# Allocates crop production and agricultural inputs (fertilizer N, P,
# balances, pesticides, water) to 10km pixels with downscaled cropland,
# strictly conserving national macro model targets (0% mass balance error).
#
# Supported Indicators (if available from macro model reporting):
# - FRTN: Total Fertilizer Nitrogen (1000 t -> metric tons, kg N/ha)
# - FRTIN: Inorganic Fertilizer Nitrogen (1000 t -> metric tons, kg N/ha)
# - FRTON: Organic Fertilizer Nitrogen (1000 t -> metric tons, kg N/ha)
# - NBAL: Gross Nitrogen Balance (1000 t -> metric tons, kg N/ha)
# - FRTP: Total Fertilizer Phosphorus (1000 t -> metric tons, kg P/ha)
# - FRTIP: Inorganic Fertilizer Phosphorus (1000 t -> metric tons, kg P/ha)
# - FRTOP: Organic Fertilizer Phosphorus (1000 t -> metric tons, kg P/ha)
# - PBAL: Gross Phosphorus Balance (1000 t -> metric tons, kg P/ha)
# - PEST: Use of chemical pesticides (t -> metric tons, kg/ha)
# - PESTH: Use of more hazardous pesticides (t -> metric tons, kg/ha)
# - PROD: Crop production (1000 t -> metric tons, t/ha yield)
# - WATR: Irrigation water consumption (km3 -> m3, mm depth)
#
# Allocation Principle:
# Explicit leaf crop targets (e.g. WHT, MAI, SRS) are allocated directly
# to their specific crop footprint. Unallocated/aggregate pools (including
# OCR and general CRP/ARA targets) are distributed proportionally across
# all remaining crop types and arable land lacking explicit input info.
# ===============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(cli)
  library(arrow)
  library(qs2)
})

if (!exists("read_intermediate")) {
  aux_cands <- c("scripts/core/aux_codes.R", "scripts/aux_codes.R")
  source(aux_cands[file.exists(aux_cands)][1])
}
if (!exists("current_model_cfg")) {
  cfg_cands <- c("pipelines/AGMIP/config.R", "scripts/AGMIP/config.R")
  source(cfg_cands[file.exists(cfg_cands)][1])
}
if (!exists("model_name")) model_name <- "AGMIP"
if (!exists("run_basename")) run_basename <- paste0("AgMIP_", toupper(current_model_cfg$agmip_model_name))
if (!exists("RUN_DATE")) RUN_DATE <- format(Sys.Date(), "%Y%m%d")
if (!exists("paths")) {
  paths <- list(
    temp = if (!is.null(current_model_cfg$temp_dir)) current_model_cfg$temp_dir else "P:/globiom/Projects/DS_AgMIP/DS_results/temp/",
    output_dir = if (!is.null(current_model_cfg$output_dir)) current_model_cfg$output_dir else "P:/globiom/Projects/DS_AgMIP/DS_results/outputs",
    gridwork_dir = "data/aux_files/"
  )
}

run_agmip_inputs_downscaling <- function() {
  cli_h1("AgMIP Step 5a: Production & Agricultural Input Indicators Downscaling")

  # ---------------------------------------------------------------
  # 0. Context & Paths
  # ---------------------------------------------------------------
  scen_name <- if (exists("safe_scen_chunk")) safe_scen_chunk else if (exists("scen_chunk")) scen_chunk else "base"
  prefix <- if (exists("output_prefix")) output_prefix else paste0("DS_AgMIP_", toupper(current_model_cfg$agmip_model_name), "_10km_", format(Sys.Date(), "%Y%m%d"))
  base_out <- if (exists("paths") && !is.null(paths$output_dir)) paths$output_dir else "P:/globiom/Projects/DS_AgMIP/DS_results/outputs"
  out_dir <- if (grepl(prefix, base_out)) base_out else file.path(base_out, prefix)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  model_tag <- if (exists("current_model_cfg") && !is.null(current_model_cfg$agmip_model_name)) {
    tolower(current_model_cfg$agmip_model_name)
  } else {
    "globiom"
  }

  cli_alert_info("Macro model: {toupper(model_tag)} | Scenario: {scen_name}")
  cli_alert_info("Output directory: {out_dir}")

  # ---------------------------------------------------------------
  # 1. Load Downscaled Cropland Areas (from Step 4)
  # ---------------------------------------------------------------
  lu_file <- if (exists("files") && !is.null(files$lu_output) && file.exists(files$lu_output)) {
    files$lu_output
  } else {
    pq_cand <- file.path(out_dir, paste0(prefix, "_lu_", scen_name, ".parquet"))
    if (file.exists(pq_cand)) pq_cand else file.path(out_dir, paste0(prefix, "_lu_", scen_name, ".csv"))
  }

  if (!file.exists(lu_file)) {
    fallback_pq <- file.path(out_dir, paste0(prefix, "_lu.parquet"))
    fallback_lu <- file.path(out_dir, paste0(prefix, "_lu.csv"))
    if (file.exists(fallback_pq)) {
      lu_file <- fallback_pq
    } else if (file.exists(fallback_lu)) {
      lu_file <- fallback_lu
    } else {
      cands <- list.files(out_dir, pattern = "^DS_.*_lu.*\\.(parquet|csv)$", full.names = TRUE)
      if (length(cands) == 0 && exists("paths") && !is.null(paths$output_dir)) {
        cands <- list.files(paths$output_dir, pattern = "(AgMIP|compr_scens).*_lu.*\\.(parquet|csv)$", recursive = TRUE, full.names = TRUE)
      }
      cands <- cands[!grepl("_luc|test|subset", basename(cands), ignore.case = TRUE)]
      if (length(cands) > 0) {
        pq_cands <- cands[grepl("\\.parquet$", cands, ignore.case = TRUE)]
        lu_file <- if (length(pq_cands) > 0) sort(pq_cands, decreasing = TRUE)[1] else sort(cands, decreasing = TRUE)[1]
      } else {
        stop("Downscaled Land Use output file not found: ", lu_file)
      }
    }
  }

  cli_alert_info("Loading downscaled cropland from: {basename(lu_file)}")
  lu_dt <- if (grepl("\\.parquet$", lu_file, ignore.case = TRUE)) as.data.table(arrow::read_parquet(lu_file)) else fread(lu_file)
  
  # Normalize column names
  for (col in c("times", "year", "time")) {
    if (col %in% names(lu_dt)) setnames(lu_dt, col, "year", skip_absent = TRUE)
  }
  for (col in c("value", "area_km2", "area")) {
    if (col %in% names(lu_dt)) setnames(lu_dt, col, "crp_area_km2", skip_absent = TRUE)
  }
  
  # Filter to Cropland activities (preserve individual crop classes per pixel)
  crp_dt <- lu_dt[grepl("CRP|CROP", lu, ignore.case = TRUE) & crp_area_km2 > 0]
  if (nrow(crp_dt) == 0) {
    cli_alert_warning("No cropland found with positive area. Checking all lu classes...")
    crp_dt <- lu_dt[grepl("CRP|CROP", lu, ignore.case = TRUE) & crp_area_km2 > 0]
  }
  crp_dt[, year := as.integer(year)]
  crp_dt[, ns := as.integer(gsub("_.*", "", ns))]
  
  # Keep individual crop classes per pixel
  crp_dt <- crp_dt[, .(crp_area_km2 = sum(crp_area_km2, na.rm = TRUE)), by = .(ns, Ns_reg, year, lu)]
  
  unique_crops <- sort(unique(crp_dt$lu))
  cli_alert_success("Loaded {nrow(crp_dt)} crop records across {length(unique_crops)} crop classes: {paste(unique_crops, collapse=', ')}")

  # ---------------------------------------------------------------
  # 2. Load Macro Model Targets from Step 1 Intermediates
  # ---------------------------------------------------------------
  cli_alert_info("Loading macro model targets from Step 1...")
  oth_params <- tryCatch(read_intermediate("other_parameters", apply_filters = FALSE), error = function(e) {
    cands <- list.files(paths$temp, pattern = paste0("^other_parameters.*", toupper(model_tag), ".*\\.(rds|qs)$"), full.names = TRUE)
    if (length(cands) == 0) cands <- list.files(paths$temp, pattern = "^other_parameters.*\\.(rds|qs)$", full.names = TRUE)
    cands <- cands[!grepl("manual_run", basename(cands))]
    if (length(cands) > 0) qs2::qs_read(sort(cands, decreasing = TRUE)[1]) else NULL
  })
  prod_targets <- tryCatch(read_intermediate("OUTPUT_targets", apply_filters = FALSE), error = function(e) {
    cands <- list.files(paths$temp, pattern = paste0("^OUTPUT_targets.*", toupper(model_tag), ".*\\.(rds|qs)$"), full.names = TRUE)
    if (length(cands) == 0) cands <- list.files(paths$temp, pattern = "^OUTPUT_targets.*\\.(rds|qs)$", full.names = TRUE)
    cands <- cands[!grepl("manual_run", basename(cands))]
    if (length(cands) > 0) qs2::qs_read(sort(cands, decreasing = TRUE)[1]) else NULL
  })

  # Filter to current scenario and model
  f_sub <- data.table()
  if (!is.null(oth_params) && nrow(oth_params) > 0) {
    f_sub <- oth_params[tolower(model) == model_tag]
    if ("scenario" %in% names(f_sub) && scen_name != "ALL") {
      f_sub <- f_sub[scenario == scen_name]
    }
    f_sub[, variable := tolower(variable)]
    f_sub[, item := tolower(item)]
  }

  p_model <- data.table()
  if (!is.null(prod_targets) && nrow(prod_targets) > 0) {
    p_model <- prod_targets[tolower(model) == model_tag]
    if ("scenario" %in% names(p_model) && scen_name != "ALL") {
      p_model <- p_model[scenario == scen_name]
    }
  }

  ds_crop_inputs <- copy(crp_dt)

  # Check if model models Permanent Crops (CRP|PRM)
  prm_target_items <- c("crp|prm", "prm", "fru", "vfn|fru", "nut", "vfn|nut", "olv", "cit", "vin")
  all_target_items <- tolower(unique(c(
    if (nrow(p_model) > 0 && "crop_type" %in% names(p_model)) p_model$crop_type else if (nrow(p_model) > 0 && "item" %in% names(p_model)) p_model$item,
    if (nrow(f_sub) > 0) f_sub$item
  )))
  model_models_prm <- any(all_target_items %in% prm_target_items)

  if (!model_models_prm) {
    cli_alert_info("Model {toupper(model_tag)} does not specifically model Permanent Crops (CRP|PRM).")
  }

  # ---------------------------------------------------------------
  # 3. Mass-Conserving Allocation Algorithm
  # ---------------------------------------------------------------
  # Allocates explicit leaf crops to their specific area footprints.
  # Residual / aggregate items (e.g. OCR, general CRP/ARA) are distributed
  # across all remaining crop types and land lacking explicit input targets.
  # ---------------------------------------------------------------
  allocate_variable_with_fallback <- function(ds_dt, target_table, target_unit_factor = 1.0) {
    if (is.null(target_table) || nrow(target_table) == 0) {
      return(list(total = rep(0, nrow(ds_dt)), intensity = rep(0, nrow(ds_dt))))
    }

    t_clean <- target_table[!is.na(value) & value >= 0, .(
      target_value = sum(value, na.rm = TRUE) * target_unit_factor
    ), by = .(Ns_reg, year = as.integer(year), item = tolower(item))]

    if (nrow(t_clean) == 0) {
      return(list(total = rep(0, nrow(ds_dt)), intensity = rep(0, nrow(ds_dt))))
    }

    table_has_prm <- any(tolower(t_clean$item) %in% prm_target_items)

    unique_lus <- unique(ds_dt$lu)
    get_crop_chain <- function(cr) {
      parts <- strsplit(cr, "\\|")[[1]]
      is_prm <- any(grepl("PRM", parts))

      if (is_prm) {
        if (!table_has_prm) return(character(0))
        chain <- character(0)
        if (length(parts) == 3) chain <- c(chain, paste0("vfn|", tolower(parts[3])), tolower(parts[3]))
        chain <- c(chain, "crp|prm", "prm")
        return(unique(chain))
      }

      # Arable crops branch
      chain <- character(0)
      if (length(parts) == 4) {
        chain <- c(chain, paste0(tolower(parts[3]), "|", tolower(parts[4])), tolower(parts[4]), tolower(parts[3]))
      } else if (length(parts) == 3) {
        chain <- c(chain, tolower(parts[3]))
      } else if (length(parts) == 2) {
        chain <- c(chain, tolower(parts[2]))
      }
      # For general fallback, add crp|ara, ocr (as the unallocated arable pool), and total crp/agr
      if (any(grepl("ARA", parts))) chain <- c(chain, "crp|ara", "ocr")
      chain <- c(chain, "crp", "agr", "tot", "total")
      unique(chain)
    }

    chain_dt <- rbindlist(lapply(unique_lus, function(cr) {
      ch <- get_crop_chain(cr)
      if (length(ch) > 0) data.table(lu = cr, priority = seq_along(ch), item = ch) else data.table()
    }))

    t_avail <- unique(t_clean[, .(Ns_reg, year, item, has_target = TRUE)])
    t_matched <- merge(chain_dt, t_avail, by = "item", allow.cartesian = TRUE)
    best_matches <- t_matched[order(Ns_reg, year, lu, priority), .SD[1], by = .(Ns_reg, year, lu)]
    
    # Specific leaf targets vs aggregate/unallocated buckets
    # Note: 'ocr', 'oth', 'crp|ara', 'crp|prm', 'crp', 'agr', 'tot', 'total' are aggregate pools
    best_matches[, is_leaf := !item %in% c("ocr", "oth", "crp|ara", "crp|prm", "crp", "agr", "tot", "total")]

    dt_work <- ds_dt[, .(row_id = .I, ns, Ns_reg, year, lu, crp_area_km2)]
    dt_work[best_matches, on = .(Ns_reg, year, lu), `:=`(matched_item = i.item, is_leaf = i.is_leaf)]
    dt_work[is.na(matched_item), rate := 0.0]

    # 1. Specific Leaf items: exact uniform allocation per crop target
    leaf_area <- dt_work[is_leaf == TRUE, .(grp_area = sum(crp_area_km2, na.rm = TRUE)), by = .(Ns_reg, year, matched_item)]
    leaf_area <- merge(leaf_area, t_clean, by.x = c("Ns_reg", "year", "matched_item"), by.y = c("Ns_reg", "year", "item"), all.x = TRUE)
    leaf_area[, rate := fifelse(grp_area > 1e-6, target_value / grp_area, 0.0)]
    dt_work[leaf_area, on = .(Ns_reg, year, matched_item), rate := i.rate]

    # 2. Remaining / unallocated items: distribute remaining macro target across all remaining crop types & land
    reg_yr_combos <- unique(dt_work[, .(Ns_reg, year)])
    if (nrow(reg_yr_combos) > 0) {
      rem_rates <- rbindlist(lapply(seq_len(nrow(reg_yr_combos)), function(k) {
        reg <- reg_yr_combos$Ns_reg[k]
        yr  <- reg_yr_combos$year[k]

        # Target priority: crp -> agr -> tot -> sum of items
        top_target <- t_clean[Ns_reg == reg & year == yr & item == "crp", target_value]
        if (length(top_target) == 0 || is.na(top_target[1])) {
          top_target <- t_clean[Ns_reg == reg & year == yr & item == "agr", target_value]
        }
        if (length(top_target) == 0 || is.na(top_target[1])) {
          top_target <- t_clean[Ns_reg == reg & year == yr & item %in% c("tot", "total"), target_value]
        }
        if (length(top_target) == 0 || is.na(top_target[1])) {
          top_target <- sum(t_clean[Ns_reg == reg & year == yr, target_value], na.rm = TRUE)
        } else {
          top_target <- top_target[1]
        }

        alloc_leaf   <- dt_work[Ns_reg == reg & year == yr & is_leaf == TRUE, sum(crp_area_km2 * rate, na.rm = TRUE)]
        unalloc_area <- dt_work[Ns_reg == reg & year == yr & (is_leaf == FALSE | is.na(is_leaf)) & !is.na(matched_item), sum(crp_area_km2, na.rm = TRUE)]
        
        residual <- max(top_target - alloc_leaf, 0)
        r_val <- if (residual > 1e-6 && unalloc_area > 1e-6) residual / unalloc_area else 0.0
        data.table(Ns_reg = reg, year = yr, rem_rate = r_val)
      }))
      dt_work[rem_rates, on = .(Ns_reg, year), rem_rate := i.rem_rate]
      dt_work[is_leaf == FALSE & !is.na(matched_item), rate := rem_rate]
    }

    dt_work[is.na(rate) | is.na(matched_item), rate := 0.0]
    setorder(dt_work, row_id)

    allocated_total <- dt_work$crp_area_km2 * dt_work$rate
    return(list(total = allocated_total, intensity = dt_work$rate))
  }

  # ---------------------------------------------------------------
  # 4. Crop Production & Yield Downscaling
  # ---------------------------------------------------------------
  cli_alert_info("Calculating mass-conserving Crop Production & Yield allocation...")
  prod_t_table <- if (nrow(p_model) > 0) {
    if ("crop_type" %in% names(p_model)) {
      p_model[, .(Ns_reg, year = as.integer(year), item = tolower(crop_type), value)]
    } else {
      p_model[, .(Ns_reg, year = as.integer(year), item = tolower(item), value)]
    }
  } else data.table()

  res_prod <- allocate_variable_with_fallback(ds_crop_inputs, prod_t_table, target_unit_factor = 1000)
  ds_crop_inputs[, Crop_Production_tons := res_prod$total]
  ds_crop_inputs[, yield_t_km2 := res_prod$intensity]
  ds_crop_inputs[, Crop_Yield_t_ha := yield_t_km2 / 100]

  # ---------------------------------------------------------------
  # 5. Agricultural Inputs & Balances Downscaling
  # ---------------------------------------------------------------
  # We check and downscale all available indicator variables from the table:
  # FRTN, FRTIN, FRTON, NBAL, FRTP, FRTIP, FRTOP, PBAL, PEST, PESTH, WATR

  has_var <- function(var_name) {
    nrow(f_sub) > 0 && any(f_sub$variable == tolower(var_name))
  }

  get_var_table <- function(var_names) {
    if (nrow(f_sub) == 0) return(data.table())
    f_sub[variable %in% tolower(var_names), .(value = sum(value, na.rm = TRUE)),
          by = .(Ns_reg, year = as.integer(year), item = tolower(item))]
  }

  # 5.1 Nitrogen Indicators
  # FRTN (Total Fertilizer N)
  if (has_var("frtn") || (has_var("frtin") && has_var("frton"))) {
    cli_alert_success("Allocating FRTN (Total Fertilizer Nitrogen)...")
    f_n_table <- get_var_table(c("frtn", "frtin", "frton"))
    res_n <- allocate_variable_with_fallback(ds_crop_inputs, f_n_table, target_unit_factor = 1000000) # 1000 t -> kg
    ds_crop_inputs[, Total_N_tons := res_n$total / 1000]
    ds_crop_inputs[, N_total_kg_ha := res_n$intensity / 100]
  }

  # FRTIN (Inorganic / Mineral Fertilizer N)
  if (has_var("frtin")) {
    cli_alert_success("Allocating FRTIN (Inorganic Fertilizer Nitrogen)...")
    f_min_table <- get_var_table("frtin")
    res_min <- allocate_variable_with_fallback(ds_crop_inputs, f_min_table, target_unit_factor = 1000000)
    ds_crop_inputs[, Total_N_mineral_tons := res_min$total / 1000]
    ds_crop_inputs[, N_mineral_kg_ha := res_min$intensity / 100]
  } else if ("Total_N_tons" %in% names(ds_crop_inputs) && !has_var("frton")) {
    # If only frtn is reported (no split), mineral N defaults to total N
    ds_crop_inputs[, Total_N_mineral_tons := Total_N_tons]
    ds_crop_inputs[, N_mineral_kg_ha := N_total_kg_ha]
  }

  # FRTON (Organic Fertilizer N)
  if (has_var("frton")) {
    cli_alert_success("Allocating FRTON (Organic Fertilizer Nitrogen)...")
    f_org_table <- get_var_table("frton")
    res_org <- allocate_variable_with_fallback(ds_crop_inputs, f_org_table, target_unit_factor = 1000000)
    ds_crop_inputs[, Total_N_organic_tons := res_org$total / 1000]
    ds_crop_inputs[, N_organic_kg_ha := res_org$intensity / 100]
  }

  # NBAL (Gross Nitrogen Balance)
  if (has_var("nbal")) {
    cli_alert_success("Allocating NBAL (Gross Nitrogen Balance)...")
    f_nbal_table <- get_var_table("nbal")
    res_nbal <- allocate_variable_with_fallback(ds_crop_inputs, f_nbal_table, target_unit_factor = 1000000)
    ds_crop_inputs[, Total_N_balance_tons := res_nbal$total / 1000]
    ds_crop_inputs[, N_balance_kg_ha := res_nbal$intensity / 100]
  }

  # 5.2 Phosphorus Indicators
  # FRTP (Total Fertilizer P)
  if (has_var("frtp") || (has_var("frtip") && has_var("frtop"))) {
    cli_alert_success("Allocating FRTP (Total Fertilizer Phosphorus)...")
    f_p_table <- get_var_table(c("frtp", "frtip", "frtop"))
    res_p <- allocate_variable_with_fallback(ds_crop_inputs, f_p_table, target_unit_factor = 1000000)
    ds_crop_inputs[, Total_P_tons := res_p$total / 1000]
    ds_crop_inputs[, P_total_kg_ha := res_p$intensity / 100]
  }

  # FRTIP (Inorganic / Mineral Fertilizer P)
  if (has_var("frtip")) {
    cli_alert_success("Allocating FRTIP (Inorganic Fertilizer Phosphorus)...")
    f_pmin_table <- get_var_table("frtip")
    res_pmin <- allocate_variable_with_fallback(ds_crop_inputs, f_pmin_table, target_unit_factor = 1000000)
    ds_crop_inputs[, Total_P_mineral_tons := res_pmin$total / 1000]
    ds_crop_inputs[, P_mineral_kg_ha := res_pmin$intensity / 100]
  } else if ("Total_P_tons" %in% names(ds_crop_inputs) && !has_var("frtop")) {
    ds_crop_inputs[, Total_P_mineral_tons := Total_P_tons]
    ds_crop_inputs[, P_mineral_kg_ha := P_total_kg_ha]
  }

  # FRTOP (Organic Fertilizer P)
  if (has_var("frtop")) {
    cli_alert_success("Allocating FRTOP (Organic Fertilizer Phosphorus)...")
    f_porg_table <- get_var_table("frtop")
    res_porg <- allocate_variable_with_fallback(ds_crop_inputs, f_porg_table, target_unit_factor = 1000000)
    ds_crop_inputs[, Total_P_organic_tons := res_porg$total / 1000]
    ds_crop_inputs[, P_organic_kg_ha := res_porg$intensity / 100]
  }

  # PBAL (Gross Phosphorus Balance)
  if (has_var("pbal")) {
    cli_alert_success("Allocating PBAL (Gross Phosphorus Balance)...")
    f_pbal_table <- get_var_table("pbal")
    res_pbal <- allocate_variable_with_fallback(ds_crop_inputs, f_pbal_table, target_unit_factor = 1000000)
    ds_crop_inputs[, Total_P_balance_tons := res_pbal$total / 1000]
    ds_crop_inputs[, P_balance_kg_ha := res_pbal$intensity / 100]
  }

  # 5.3 Pesticides
  # PEST (Chemical Pesticides)
  if (has_var("pest")) {
    cli_alert_success("Allocating PEST (Chemical Pesticides)...")
    f_pest_table <- get_var_table("pest")
    res_pest <- allocate_variable_with_fallback(ds_crop_inputs, f_pest_table, target_unit_factor = 1000) # t -> kg
    ds_crop_inputs[, Total_Pest_tons := res_pest$total / 1000]
    ds_crop_inputs[, Pest_kg_ha := res_pest$intensity / 100]
  }

  # PESTH (More Hazardous Pesticides)
  if (has_var("pesth")) {
    cli_alert_success("Allocating PESTH (More Hazardous Pesticides)...")
    f_pesth_table <- get_var_table("pesth")
    res_pesth <- allocate_variable_with_fallback(ds_crop_inputs, f_pesth_table, target_unit_factor = 1000) # t -> kg
    ds_crop_inputs[, Total_Pesth_tons := res_pesth$total / 1000]
    ds_crop_inputs[, Pesth_kg_ha := res_pesth$intensity / 100]
  }

  # 5.4 Irrigation Water
  if (has_var("watr")) {
    cli_alert_info("Allocating WATR (Irrigation Water)...")
    watr_table <- get_var_table("watr")
    res_w <- allocate_variable_with_fallback(ds_crop_inputs, watr_table, target_unit_factor = 1e9) # km3 -> m3
    ds_crop_inputs[, Total_Water_m3 := res_w$total]
    ds_crop_inputs[, Water_mm := fifelse(crp_area_km2 > 0, Total_Water_m3 / (crp_area_km2 * 1e6) * 1000, 0)]
  }

  # ---------------------------------------------------------------
  # 6. National Target Mass Conservation Verification
  # ---------------------------------------------------------------
  cli_h2("Validation: National Mass Conservation")
  val_report <- data.table()

  if ("Total_N_tons" %in% names(ds_crop_inputs)) {
    f_n_table <- get_var_table(c("frtn", "frtin", "frton"))
    # Priority: crp -> agr -> tot -> sum of items
    crp_n_targets <- f_n_table[item == "crp", .(target_N_1000t = sum(value, na.rm=TRUE)), by = .(Ns_reg, year)]
    if (nrow(crp_n_targets) == 0) crp_n_targets <- f_n_table[item == "agr", .(target_N_1000t = sum(value, na.rm=TRUE)), by = .(Ns_reg, year)]
    if (nrow(crp_n_targets) == 0) crp_n_targets <- f_n_table[item %in% c("tot", "total"), .(target_N_1000t = sum(value, na.rm=TRUE)), by = .(Ns_reg, year)]
    if (nrow(crp_n_targets) == 0) crp_n_targets <- f_n_table[, .(target_N_1000t = sum(value, na.rm=TRUE)), by = .(Ns_reg, year)]

    if (nrow(crp_n_targets) > 0) {
      val_n <- ds_crop_inputs[, .(downscaled_N_1000t = sum(Total_N_tons, na.rm = TRUE) / 1000), by = .(Ns_reg, year)]
      val_n <- merge(val_n, crp_n_targets, by = c("Ns_reg", "year"), all.x = TRUE)
      val_n[, error_N_pct := round((downscaled_N_1000t - target_N_1000t) / (target_N_1000t + 1e-9) * 100, 4)]
      val_report <- val_n
      max_err_n <- max(abs(val_n[!is.na(error_N_pct)]$error_N_pct), na.rm = TRUE)
      cli_alert_success("Total Nitrogen mass conservation: Max error = {max_err_n}% across all countries and years.")
    }
  }

  if (nrow(prod_t_table) > 0) {
    crp_p_targets <- prod_t_table[item == "crp", .(target_prod_1000t = sum(value, na.rm=TRUE)), by = .(Ns_reg, year)]
    if (nrow(crp_p_targets) == 0) crp_p_targets <- prod_t_table[item == "agr", .(target_prod_1000t = sum(value, na.rm=TRUE)), by = .(Ns_reg, year)]
    if (nrow(crp_p_targets) == 0) crp_p_targets <- prod_t_table[item %in% c("tot", "total"), .(target_prod_1000t = sum(value, na.rm=TRUE)), by = .(Ns_reg, year)]
    if (nrow(crp_p_targets) == 0) crp_p_targets <- prod_t_table[, .(target_prod_1000t = sum(value, na.rm=TRUE)), by = .(Ns_reg, year)]

    if (nrow(crp_p_targets) > 0) {
      val_p <- ds_crop_inputs[, .(downscaled_prod_1000t = sum(Crop_Production_tons, na.rm = TRUE) / 1000), by = .(Ns_reg, year)]
      val_p <- merge(val_p, crp_p_targets, by = c("Ns_reg", "year"), all.x = TRUE)
      val_p[, error_prod_pct := round((downscaled_prod_1000t - target_prod_1000t) / (target_prod_1000t + 1e-9) * 100, 4)]
      val_report <- if (nrow(val_report) > 0) merge(val_report, val_p, by = c("Ns_reg", "year"), all = TRUE) else val_p
      max_err_p <- max(abs(val_p[!is.na(error_prod_pct)]$error_prod_pct), na.rm = TRUE)
      cli_alert_success("Crop Production mass conservation: Max error = {max_err_p}% across all countries and years.")
    }
  }

  val_csv <- file.path(out_dir, paste0(prefix, "_inputs_national_validation_", scen_name, ".csv"))
  if (nrow(val_report) > 0) {
    fwrite(val_report, val_csv)
    cli_alert_success("Saved national mass balance validation CSV: {basename(val_csv)}")
  }

  # ---------------------------------------------------------------
  # 7. Format & Save Standard Production Intermediate for Orchestrator
  # ---------------------------------------------------------------
  cli_alert_info("Saving standard intermediate for core orchestrator with all individual crop classes...")
  
  prod_intermediate <- ds_crop_inputs[, .(
    ns,
    GLOB_country = Ns_reg,
    Ns_reg,
    GLOB_CTYP = lu,
    GLOB_CSYS = "ALL",
    scenario = scen_name,
    year,
    area_km2 = crp_area_km2,
    prod_t = Crop_Production_tons,
    yield_t_km2 = Crop_Yield_t_ha * 100,
    N_input_kg = if ("N_total_kg_ha" %in% names(ds_crop_inputs)) N_total_kg_ha * 100 else NA_real_,
    Total_N_input_kg = if ("Total_N_tons" %in% names(ds_crop_inputs)) Total_N_tons * 1000 else NA_real_,
    P_input_kg = if ("P_total_kg_ha" %in% names(ds_crop_inputs)) P_total_kg_ha * 100 else if ("P_mineral_kg_ha" %in% names(ds_crop_inputs)) P_mineral_kg_ha * 100 else NA_real_,
    Total_P_input_kg = if ("Total_P_tons" %in% names(ds_crop_inputs)) Total_P_tons * 1000 else if ("Total_P_mineral_tons" %in% names(ds_crop_inputs)) Total_P_mineral_tons * 1000 else NA_real_,
    Water_m3 = if ("Total_Water_m3" %in% names(ds_crop_inputs)) fifelse(crp_area_km2 > 0, Total_Water_m3 / crp_area_km2, 0) else NA_real_,
    Total_Water_m3 = if ("Total_Water_m3" %in% names(ds_crop_inputs)) Total_Water_m3 else NA_real_
  )]

  save_intermediate(prod_intermediate, paste0("production_downscaled_", scen_name))
  cli_alert_success("Saved intermediate: production_downscaled_{scen_name} ({nrow(prod_intermediate)} records across {uniqueN(prod_intermediate$GLOB_CTYP)} crops)")

  # ---------------------------------------------------------------
  # 8. Export Pixel-Level CSV (with all available indicators)
  # ---------------------------------------------------------------
  pixel_csv <- file.path(out_dir, paste0(prefix, "_inputs_pixel_", scen_name, ".csv"))
  cli_alert_info("Exporting pixel-level inputs table to CSV: {basename(pixel_csv)}")
  
  # Format column values
  pixel_export <- copy(ds_crop_inputs)
  pixel_export[, crp_area_km2 := round(crp_area_km2, 3)]
  pixel_export[, Crop_Production_tons := round(Crop_Production_tons, 2)]
  pixel_export[, Crop_Yield_t_ha := round(Crop_Yield_t_ha, 3)]
  if ("yield_t_km2" %in% names(pixel_export)) pixel_export[, yield_t_km2 := NULL]

  for (cn in names(pixel_export)) {
    if (grepl("tons|Total_Water", cn)) pixel_export[, (cn) := round(get(cn), 3)]
    if (grepl("kg_ha|mm", cn)) pixel_export[, (cn) := round(get(cn), 2)]
  }

  fwrite(pixel_export, pixel_csv)

  # ---------------------------------------------------------------
  # 9. Generate Multi-Band Summary GeoTIFF Rasters
  # ---------------------------------------------------------------
  cli_h2("Generating GeoTIFF Rasters for Inputs & Production Indicators")

  template_rst <- NULL
  if (exists("template_custom") && !is.null(template_custom)) {
    template_rst <- template_custom
  } else {
    mg_pq <- list.files("data/aux_files/", pattern = "one_kmID_master_mapping_.*\\.parquet", full.names = TRUE)
    if (length(mg_pq) > 0) {
      mg <- as.data.table(arrow::read_parquet(sort(mg_pq, decreasing = TRUE)[1], col_select = c("EEA_10kmID", "x_10kmID", "y_10kmID")))
      mg_u <- unique(mg, by = "EEA_10kmID")
      template_rst <- terra::rast(data.frame(x = mg_u$x_10kmID, y = mg_u$y_10kmID, z = mg_u$EEA_10kmID), type = "xyz")
      names(template_rst) <- "EEA_10kmID"
      terra::crs(template_rst) <- "EPSG:3035"
    }
  }

  if (is.null(template_rst)) {
    cli_alert_warning("No spatial template available. Skipping raster export.")
    return(invisible(ds_crop_inputs))
  }

  cell_vals <- terra::values(template_rst[[1]], mat = FALSE)
  years_avail <- sort(unique(ds_crop_inputs$year))

  # Build dictionary of all possible indicator raster bands with descriptions
  all_possible_indicators <- list(
    # Production
    Crop_Production_tons   = "Crop Production (metric tons)",
    Crop_Yield_t_ha        = "Average Crop Yield (metric tons / ha)",
    # Nitrogen
    N_total_kg_ha          = "Total Fertilizer Nitrogen Rate (kg N / ha)",
    Total_N_tons           = "Total Fertilizer Nitrogen Application (metric tons N)",
    N_mineral_kg_ha        = "Inorganic Fertilizer Nitrogen Rate (kg N / ha)",
    Total_N_mineral_tons   = "Inorganic Fertilizer Nitrogen Application (metric tons N)",
    N_organic_kg_ha        = "Organic Fertilizer Nitrogen Rate (kg N / ha)",
    Total_N_organic_tons   = "Organic Fertilizer Nitrogen Application (metric tons N)",
    N_balance_kg_ha        = "Gross Nitrogen Balance Rate (kg N / ha)",
    Total_N_balance_tons   = "Gross Nitrogen Balance (metric tons N)",
    # Phosphorus
    P_total_kg_ha          = "Total Fertilizer Phosphorus Rate (kg P / ha)",
    Total_P_tons           = "Total Fertilizer Phosphorus Application (metric tons P)",
    P_mineral_kg_ha        = "Inorganic Fertilizer Phosphorus Rate (kg P / ha)",
    Total_P_mineral_tons   = "Inorganic Fertilizer Phosphorus Application (metric tons P)",
    P_organic_kg_ha        = "Organic Fertilizer Phosphorus Rate (kg P / ha)",
    Total_P_organic_tons   = "Organic Fertilizer Phosphorus Application (metric tons P)",
    P_balance_kg_ha        = "Gross Phosphorus Balance Rate (kg P / ha)",
    Total_P_balance_tons   = "Gross Phosphorus Balance (metric tons P)",
    # Pesticides
    Pest_kg_ha             = "Chemical Pesticides Rate (kg / ha)",
    Total_Pest_tons        = "Chemical Pesticides Application (metric tons)",
    Pesth_kg_ha            = "More Hazardous Pesticides Rate (kg / ha)",
    Total_Pesth_tons       = "More Hazardous Pesticides Application (metric tons)",
    # Water
    Water_mm               = "Irrigation Water Depth (mm)",
    Total_Water_m3         = "Irrigation Water Consumption (m3)"
  )

  # Filter to active indicators present in ds_crop_inputs
  active_raster_indicators <- all_possible_indicators[names(all_possible_indicators) %in% names(ds_crop_inputs)]

  # Aggregate to pixel level (total across all crops per pixel)
  tot_pixel <- ds_crop_inputs[, {
    row_res <- list(crp_area_km2 = sum(crp_area_km2, na.rm = TRUE))
    for (nm in names(active_raster_indicators)) {
      if (grepl("tons|Total_Water", nm)) {
        row_res[[nm]] <- sum(get(nm), na.rm = TRUE)
      } else if (grepl("Crop_Yield_t_ha", nm)) {
        row_res[[nm]] <- sum(Crop_Production_tons, na.rm = TRUE) / pmax(sum(crp_area_km2[Crop_Production_tons > 0], na.rm = TRUE) * 100, 1e-9)
      } else if (grepl("Water_mm", nm)) {
        row_res[[nm]] <- sum(Total_Water_m3, na.rm = TRUE) / pmax(sum(crp_area_km2[Total_Water_m3 > 0], na.rm = TRUE) * 1e6, 1e-9) * 1000
      } else {
        # Rate indicators (kg/ha)
        ton_col <- gsub("_kg_ha$", "_tons", nm)
        if (!ton_col %in% names(ds_crop_inputs)) ton_col <- paste0("Total_", gsub("_kg_ha$", "_tons", nm))
        if (ton_col %in% names(ds_crop_inputs)) {
          row_res[[nm]] <- (sum(get(ton_col), na.rm = TRUE) * 1000) / pmax(sum(crp_area_km2[get(ton_col) > 0], na.rm = TRUE) * 100, 1e-9)
        } else {
          row_res[[nm]] <- mean(get(nm), na.rm = TRUE)
        }
      }
    }
    row_res
  }, by = .(ns, year)]

  band_meta <- data.table(
    band_index = seq_along(active_raster_indicators),
    band_name = names(active_raster_indicators),
    description = unlist(active_raster_indicators)
  )
  meta_csv <- file.path(out_dir, paste0(prefix, "_inputs_band_metadata.csv"))
  fwrite(band_meta, meta_csv)
  cli_alert_success("Saved indicator band metadata CSV: {basename(meta_csv)} ({nrow(band_meta)} bands)")

  for (yr in years_avail) {
    cli_alert_info("Generating consolidated multi-band input raster for year: {yr}...")
    sub_yr <- tot_pixel[year == yr]
    match_idx <- match(cell_vals, sub_yr$ns)

    band_layers <- list()
    for (ind_name in names(active_raster_indicators)) {
      ind_vals <- sub_yr[[ind_name]][match_idx]
      ind_vals[(is.na(ind_vals) | is.infinite(ind_vals)) & !is.na(cell_vals)] <- 0

      r_band <- terra::rast(template_rst[[1]])
      terra::values(r_band) <- ind_vals
      names(r_band) <- ind_name
      band_layers[[ind_name]] <- r_band
    }

    multi_stack <- terra::rast(band_layers)
    terra::crs(multi_stack) <- "EPSG:3035"
    multi_tif <- file.path(out_dir, paste0(prefix, "_inputs_", scen_name, "_", yr, ".tif"))
    if (file.exists(multi_tif)) {
      suppressWarnings(file.remove(multi_tif))
      aux <- paste0(multi_tif, ".aux.xml")
      if (file.exists(aux)) suppressWarnings(file.remove(aux))
    }
    terra::writeRaster(multi_stack, multi_tif, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
    cli_alert_success("Exported multi-band input GeoTIFF: {basename(multi_tif)} ({terra::nlyr(multi_stack)} layers: {paste(names(multi_stack), collapse=', ')})")
  }

  cli_alert_success("All AgMIP input and production downscaling completed successfully.")
  return(invisible(ds_crop_inputs))
}

# Execute function
run_agmip_inputs_downscaling()

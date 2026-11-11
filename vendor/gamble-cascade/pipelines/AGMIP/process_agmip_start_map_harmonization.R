require(data.table)
# Engine: loaded once per session, pinned submodule or CASCADE_CORE_PATH (scripts/core/run_context.R)
if (!exists("ensure_cascade_engine", mode = "function")) source("scripts/core/run_context.R")
ensure_cascade_engine()

# ---------------------------------------------------------------
# 0. Preparation
# ---------------------------------------------------------------
if (!exists("paths") || is.null(paths$temp)) {
  paths <- list(temp = "C:/Users/ringwald/OneDrive - IIASA/R/LAMASUS_downscaling/temp/")
}

ds_res <- if(exists("downscale_resolution")) downscale_resolution else "10km"
if (exists("read_intermediate")) {
  message(">>> Loading pre-harmonized AgMIP start map...")
  start_map <- read_intermediate("pre_harm_start_map", ds_res)
  
  message(">>> Loading AgMIP base-year targets...")
  targets <- read_intermediate("LUC_targets_country")
} else {
  message(">>> Loading pre-harmonized AgMIP start map...")
  sm_file <- list.files(paths$temp, pattern = paste0("^pre_harm_start_map_", ds_res, "_.*\\.(rds|qs)$"), full.names = TRUE)[1]
  start_map <- if (grepl("\\.qs$", sm_file)) qs2::qs_read(sm_file) else readRDS(sm_file)
  
  message(">>> Loading AgMIP targets...")
  tg_file <- list.files(paths$temp, pattern = "^LUC_targets_country_.*\\.(rds|qs)$", full.names = TRUE)[1]
  targets <- if (grepl("\\.qs$", tg_file)) qs2::qs_read(tg_file) else readRDS(tg_file)
}

base_yr <- if (exists("current_model_cfg") && !is.null(current_model_cfg$base_year)) current_model_cfg$base_year else 2000
avail_years <- sort(unique(targets$year))
if (!base_yr %in% avail_years && length(avail_years) > 0) {
  base_yr_harm <- avail_years[1]
  message(sprintf(">>> [AgMIP] Base year %s not in targets. Using model's earliest available year %s for start map harmonization.", base_yr, base_yr_harm))
  targets <- targets[year == base_yr_harm]
} else {
  targets <- targets[year == base_yr]
}

# Filter to active model and scenario
if (exists("current_model_cfg") && !is.null(current_model_cfg$agmip_model_name) && !"all" %in% tolower(current_model_cfg$agmip_model_name)) {
  if ("model" %in% names(targets)) targets <- targets[tolower(model) %in% tolower(current_model_cfg$agmip_model_name)]
}
if (exists("scen_filter") && !is.null(scen_filter)) {
  scens_to_keep <- tolower(unique(c(scen_filter, sub(".*::", "", scen_filter))))
  targets <- targets[scenario %in% scens_to_keep | sub(".*::", "", scenario) %in% scens_to_keep]
} else if (exists("current_model_cfg") && !is.null(current_model_cfg$base_scen)) {
  targets <- targets[scenario %in% tolower(current_model_cfg$base_scen) | sub(".*::", "", scenario) %in% tolower(current_model_cfg$base_scen)]
}

# Standardize names
setnames(targets, old = c("area_km2"), new = c("AgMIP_area_km2"), skip_absent = TRUE)
if (!"Ns_reg" %in% names(targets) && "country" %in% names(targets)) setnames(targets, "country", "Ns_reg")

# Harmonize Ns_reg casing defensively between targets and start_map
sm_reg_map <- unique(start_map[, .(Ns_reg_sm = Ns_reg, Ns_reg_upper = toupper(Ns_reg))])
targets <- merge(targets, sm_reg_map, by.x = "Ns_reg", by.y = "Ns_reg_upper", all.x = TRUE)
targets[!is.na(Ns_reg_sm), Ns_reg := Ns_reg_sm]
targets[, Ns_reg_sm := NULL]


# ---------------------------------------------------------------
# 1. Align Scopes
# ---------------------------------------------------------------
# Only keep regions that exist in both the start map and the targets
common_regions <- intersect(unique(start_map$Ns_reg), unique(targets$Ns_reg))

# Ensure that the start map and target area are mutually compatible
sm_agg <- start_map[Ns_reg %in% common_regions, .(sm_area = sum(LUM_area_km2, na.rm = TRUE)), by = Ns_reg]
# Targets may have multiple models/scenarios, check max area per region
tg_agg <- targets[Ns_reg %in% common_regions, .(tg_area = sum(AgMIP_area_km2, na.rm = TRUE)), by = .(Ns_reg, model, scenario)]
tg_agg <- tg_agg[, .(tg_area = max(tg_area, na.rm = TRUE)), by = Ns_reg]

area_comp <- merge(sm_agg, tg_agg, by = "Ns_reg")
area_comp[, ratio := sm_area / tg_area]

# A region is valid if:
# 1. Both start map and target area are positive (> 0)
# 2. It is in the project default countries list, OR
# 3. Its raster coverage ratio is reasonable (>= 0.50), screening out unmapped slivers
default_eu <- if (exists("current_model_cfg") && !is.null(current_model_cfg$default_countries)) {
  current_model_cfg$default_countries
} else {
  character(0)
}

min_coverage_ratio <- 0.50

valid_regions <- area_comp[
  sm_area > 0 & tg_area > 0 & (Ns_reg %in% default_eu | ratio >= min_coverage_ratio)
]$Ns_reg

dropped_regions <- setdiff(common_regions, valid_regions)
if (length(dropped_regions) > 0) {
  cli::cli_alert_warning("Dropped {length(dropped_regions)} regions with insufficient raster coverage (ratio < {min_coverage_ratio}): {paste(dropped_regions, collapse=', ')}")
}
cli::cli_alert_success("Validated {length(valid_regions)} regions for harmonization (coverage ratio: min={round(min(area_comp[Ns_reg %in% valid_regions]$ratio), 3)}, max={round(max(area_comp[Ns_reg %in% valid_regions]$ratio), 3)}).")

common_regions <- intersect(common_regions, valid_regions)
# AgMIP default: zones below the coverage screen are mostly slivers of countries at the
# grid edge or residuals of inexact border mapping, so they are kept as `_other` land.
skipped_zone_handling <- if (exists("current_model_cfg") && !is.null(current_model_cfg$skipped_zone_handling)) current_model_cfg$skipped_zone_handling else "other"
coverage <- report_harmonization_coverage(
  start_map[, .(sm_area = sum(LUM_area_km2, na.rm = TRUE)), by = Ns_reg],
  targets[, .(tg_area = sum(AgMIP_area_km2, na.rm = TRUE)), by = .(Ns_reg, model, scenario)][, .(tg_area = max(tg_area, na.rm = TRUE)), by = Ns_reg],
  harmonized = common_regions, label = "AgMIP",
  skipped_status = if (skipped_zone_handling == "keep") "passthrough" else "kept_as_other",
  no_target_status = "no_target_kept_as_other")
record_outside_model_zones(coverage, handling = skipped_zone_handling, label = "AgMIP")
start_map_skipped <- start_map[!Ns_reg %in% common_regions]
start_map <- start_map[Ns_reg %in% common_regions]
targets <- targets[Ns_reg %in% common_regions]

# ---------------------------------------------------------------
# 2. Iterate Over Models & Scenarios for Harmonization
# ---------------------------------------------------------------
# The AgMIP targets are bundled by model & scenario.
# We map the common base start map to each bundle independently.
scen_grid <- unique(targets[, .(model, scenario)])
message(">>> [AgMIP] Harmonizing start map across ", nrow(scen_grid), " model-scenario combinations.")

all_harmonized <- list()

for (i in seq_len(nrow(scen_grid))) {
  curr_mod <- scen_grid$model[i]
  curr_scn <- scen_grid$scenario[i]
  
  cli::cli_alert_info("Harmonizing: {curr_mod} - {curr_scn}")
  
  curr_targets <- targets[model == curr_mod & scenario == curr_scn]
  
  if (exists("current_model_cfg") && !is.null(current_model_cfg$classes_to_harmonize)) {
    active_classes <- current_model_cfg$classes_to_harmonize
  } else {
    aux_dir <- if (exists("paths") && !is.null(paths$aux_files) && dir.exists(paths$aux_files)) paths$aux_files else "data/aux_files"
    lu_map_file <- file.path(aux_dir, "agmip_lu_class_mapping.csv")
    if (file.exists(lu_map_file)) {
      lu_map <- data.table::fread(lu_map_file)
      active_classes <- unique(toupper(lu_map[tolower(model) == tolower(curr_mod) & is_aggregate == FALSE, agmip_item]))
      active_classes <- active_classes[!is.na(active_classes) & active_classes != ""]
    } else {
      # Fallback to target classes if mapping is not found
      active_classes <- unique(curr_targets$lu_class)
    }
    
    # Also ensure any classes actually present in the model's targets are active
    active_classes <- unique(c(active_classes, unique(curr_targets$lu_class)))
  }
  active_classes <- unique(c(active_classes, "BUI", "OLD", "ONV"))
  active_classes <- setdiff(active_classes, c("NLD", "FOR", "CRP"))

  # Aggregate targets to the Region level
  temp_target <- curr_targets[, .(AgMIP_area_km2 = sum(AgMIP_area_km2, na.rm = TRUE)), by = .(Ns_reg, lu_class)]
  
  # Aggregate start map to Region level, mapping Level 2 to Level 1 if neither Level 2 nor its sub-classes are active
  is_l2_active <- function(l2) {
    l2 %in% active_classes | any(startsWith(active_classes, paste0(l2, "|")))
  }
  start_map[, lu_class := ifelse(sapply(lu_class_2, is_l2_active), lu_class_2, lu_class_1)]
  temp_initial <- start_map[, .(LUM_area_km2 = sum(LUM_area_km2, na.rm = TRUE)), by = .(Ns_reg, lu_class)]
  
  # ---------------------------------------------------------------
  # 3. Transition Matrix Computation (Scaling base map to targets)
  # ---------------------------------------------------------------
  temp_join <- merge(temp_initial, temp_target, by = c("Ns_reg", "lu_class"), all = TRUE)

  # Check if a class in the start map is a parent class that has more specific sub-classes in active_classes
  is_parent_class <- temp_join$lu_class %in% c("CRP", "CRP|ARA", "CRP|PRM", "FOR", "NLD") &
    sapply(temp_join$lu_class, function(p) any(startsWith(active_classes, paste0(p, "|"))))
  temp_join[is.na(AgMIP_area_km2) & !is_parent_class, AgMIP_area_km2 := LUM_area_km2]
  temp_join[is.na(AgMIP_area_km2) & is_parent_class, AgMIP_area_km2 := 0]
  temp_join[is.na(LUM_area_km2), LUM_area_km2 := 0]
  
  # Synthetic lu key for transition calculation
  temp_join[, lu := lu_class]
  
  # Estimate empirical prior
  if (!requireNamespace("nnls", quietly = TRUE)) install.packages("nnls", repos = "https://cloud.r-project.org/")
  
  start_wide <- dcast(temp_join, Ns_reg ~ lu, value.var = "LUM_area_km2", fill = 0)
  target_wide <- dcast(temp_join, Ns_reg ~ lu, value.var = "AgMIP_area_km2", fill = 0)

setnames(start_wide, setdiff(names(start_wide), "Ns_reg"), paste0("SRC_", setdiff(names(start_wide), "Ns_reg")))
setnames(target_wide, setdiff(names(target_wide), "Ns_reg"), paste0("TGT_", setdiff(names(target_wide), "Ns_reg")))

model_data <- merge(start_wide, target_wide, by = "Ns_reg", all = TRUE)
model_data[is.na(model_data)] <- 0

src_cols <- grep("^SRC_", names(model_data), value = TRUE)
tgt_cols <- grep("^TGT_", names(model_data), value = TRUE)

X <- as.matrix(model_data[, ..src_cols])
valid_rows <- rowSums(X) > 0
X <- X[valid_rows, , drop = FALSE]
model_data <- model_data[valid_rows]

beta_matrix <- matrix(0, nrow = length(src_cols), ncol = length(tgt_cols))
rownames(beta_matrix) <- gsub("^SRC_", "", src_cols)
colnames(beta_matrix) <- gsub("^TGT_", "", tgt_cols)

for (j in seq_along(tgt_cols)) {
  fit <- nnls::nnls(A = X, b = model_data[[tgt_cols[j]]])
  beta_matrix[, j] <- fit$x
}

row_sums <- rowSums(beta_matrix)
beta_matrix <- sweep(beta_matrix, 1, ifelse(row_sums > 0, row_sums, 1), FUN = "/")
beta_matrix <- beta_matrix + 0.001
beta_matrix <- sweep(beta_matrix, 1, rowSums(beta_matrix), FUN = "/")

empirical_prior <- as.data.table(as.table(beta_matrix))
setnames(empirical_prior, c("V1", "V2", "N"), c("from_class", "to_class", "value"))
empirical_prior <- empirical_prior[value > 0]
empirical_prior[, from_class := as.character(from_class)]
empirical_prior[, to_class := as.character(to_class)]

# NLD self-transitions are naturally low; let the NNLS estimate stand (do not zero out).
# Zeroing NLD here causes a crash when NLD is injected as a sink below.

# Ensure that Natural_unmanaged is connected to all classes in the prior
all_classes <- unique(temp_join$lu)
missing_priors <- data.table(expand.grid(from_class = all_classes, to_class = all_classes, stringsAsFactors = FALSE))
missing_priors <- missing_priors[!paste(from_class, to_class) %in% paste(empirical_prior$from_class, empirical_prior$to_class)]
if (nrow(missing_priors) > 0) {
  missing_priors[, value := 0.001]
  empirical_prior <- rbind(empirical_prior, missing_priors)
}

message(">>> Empirical prior estimated successfully.")

# Calculate identical area (no change needed)
  temp_join[, value := pmin(LUM_area_km2, AgMIP_area_km2)]
  temp_join[, `:=`(LUM_area_km2 = LUM_area_km2 - value,
                   AgMIP_area_km2 = AgMIP_area_km2 - value)]
  
  # Compute required thematic shifts
  all_harm_classes <- unique(c(active_classes, unique(start_map$lu_class)))
  temp_LUC_targets <- temp_join[lu_class %in% all_harm_classes, {
    if (any(LUM_area_km2 != 0) & any(AgMIP_area_km2 != 0)) {
      tmp_from <- .SD[, .(lu, value = LUM_area_km2)]
      tmp_to <- .SD[, .(lu, value = AgMIP_area_km2)]
      
      sum_from <- sum(tmp_from$value)
      sum_to <- sum(tmp_to$value)
      
      unm_sink <- if ("OLD" %in% active_classes) "OLD" else if ("ONV" %in% active_classes) "ONV" else "OLD"
      # If AgMIP has less area (start map surplus), route it to Natural_unmanaged instead of ballooning agricultural classes
      if (sum_from > sum_to) {
        if (unm_sink %in% tmp_to$lu) {
          tmp_to[lu == unm_sink, value := value + (sum_from - sum_to)]
        } else {
          tmp_to <- rbindlist(list(tmp_to, data.table(lu = unm_sink, value = sum_from - sum_to)))
        }
      } else if (sum_to > sum_from) {
        # If AgMIP demands more area, draw from Natural_unmanaged
        if (unm_sink %in% tmp_from$lu) {
          tmp_from[lu == unm_sink, value := value + (sum_to - sum_from)]
        } else {
          tmp_from <- rbindlist(list(tmp_from, data.table(lu = unm_sink, value = sum_to - sum_from)))
        }
      }
      
      # Guard: ensure all classes used in transitions are covered by the prior.
      # Dynamically injected sink/source classes (NLD, ONV) are patched in if absent.
      missing_from <- setdiff(tmp_from$lu, empirical_prior$from_class)
      missing_to   <- setdiff(tmp_to$lu,   empirical_prior$to_class)
      if (length(missing_from) > 0 || length(missing_to) > 0) {
        all_prior_classes <- unique(c(empirical_prior$from_class, empirical_prior$to_class,
                                      missing_from, missing_to))
        patch_rows <- data.table::CJ(from_class = all_prior_classes, to_class = all_prior_classes)[
          !paste(from_class, to_class) %in% paste(empirical_prior$from_class, empirical_prior$to_class)
        ]
        patch_rows[, value := 0.001]
        empirical_prior <- rbind(empirical_prior, patch_rows)
        message(sprintf(">>> [AgMIP Harmonization] Patched %d missing prior entries for: from=[%s] to=[%s]",
                        nrow(patch_rows),
                        paste(missing_from, collapse = ","),
                        paste(missing_to,   collapse = ",")))
      }
      
      tmp <- solve_gross_transitions(tmp_from,
                       tmp_to,
                       prior = empirical_prior,
                       keep_areas = "from",
                       label = ipf_label("AgMIP harmonization", .BY))
      setnames(tmp, old = names(tmp), new = c("from_class", "to_class", "value"))
    } else {
      tmp <- .SD[, .(from_class = lu, to_class = lu, value = NA_real_)]
    }
    data.table(tmp)
  }, by = .(Ns_reg)]

  # ---------------------------------------------------------------
  # 3. Net Flow & Share Calculation
  # ---------------------------------------------------------------
  temp_inflows  <- temp_LUC_targets[, .(inflow = sum(value, na.rm = TRUE)), by = .(Ns_reg, from_class, to_class)]
  temp_outflows <- temp_LUC_targets[, .(outflow = sum(value, na.rm = TRUE)), by = .(Ns_reg, from_class = to_class, to_class = from_class)]
  
  temp_LUC_net_flows <- merge(temp_inflows, temp_outflows, by = c("Ns_reg", "from_class", "to_class"), all = TRUE)
  temp_LUC_net_flows[is.na(inflow),  inflow := 0]
  temp_LUC_net_flows[is.na(outflow), outflow := 0]
  
  temp_LUC_net_flows[from_class != to_class, value := pmax(inflow - outflow, 0)]
  temp_LUC_net_flows[from_class == to_class, value := inflow]
  temp_LUC_net_flows[, c("inflow", "outflow") := NULL]
  
  # Re-add no change areas
  temp_LUC_net_fill <- temp_join[, .(Ns_reg, from_class = lu, to_class = lu, value)]
  temp_LUC_net_flows <- rbindlist(list(temp_LUC_net_flows, temp_LUC_net_fill), use.names = TRUE, fill = TRUE)
  temp_LUC_net_flows <- temp_LUC_net_flows[, .(value = sum(value, na.rm = TRUE)), by = .(Ns_reg, from_class, to_class)]
  
  # Calculate probability matrix (shares)
  temp_LUC_net_shares <- copy(temp_LUC_net_flows)
  temp_LUC_net_shares[, value := value / sum(value, na.rm = TRUE), by = .(Ns_reg, from_class)]
  
  # ---------------------------------------------------------------
  # 4. Spatially Distributed Result Reconstruction (Gravity-Weighted)
  # ---------------------------------------------------------------
  
  start_map[, from_class := lu_class]
  
  # Pixel edge length in km, derived from the resolution tag (e.g. "5km")
  pixel_km <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", ds_res)))
  if (is.na(pixel_km) || pixel_km <= 0) pixel_km <- 5
  
  temp_pix_from <- start_map[, .(area_km2 = sum(LUM_area_km2, na.rm = TRUE)),
                             by = .(Ns_reg, ns, x_ns, y_ns, from_class)]
  
  temp_map <- allocate_transitions_spatial(
    pixel_dt      = temp_pix_from,
    shares_dt     = temp_LUC_net_shares,
    zone_cols     = "Ns_reg",
    pixel_id_col  = "ns",
    from_col      = "from_class",
    to_col        = "to_class",
    share_col     = "value",
    area_col      = "area_km2",
    coord_cols    = c("x_ns", "y_ns"),
    pixel_size    = pixel_km,
    block_pixels  = max(1L, round(40 / pixel_km)),  # ~40 km gravity blocks
    gravity_floor = 0.02                            # lower = stronger clustering
  )
  
  # Re-attach pixel coordinates to preserve the output schema
  temp_map <- merge(temp_map, unique(start_map[, .(ns, x_ns, y_ns)]), by = "ns", all.x = TRUE)
  temp_map <- temp_map[!is.na(to_class) & to_class != "" & to_class != "NODATA" & area_km2 > 0]
  temp_map[, lu_class := to_class]
  temp_map[, to_class := NULL]
  
  ds_start_map <- temp_map[, .(area_km2 = sum(area_km2, na.rm = TRUE)),
                            by = setdiff(names(temp_map), "area_km2")]
  
  # ---------------------------------------------------------------
  # 5. Final Numeric Hygiene (Zonal Bilateral Swapping, decimal mode)
  # ---------------------------------------------------------------
  ds_start_map <- execute_zonal_swaps(dt = ds_start_map,
                                      zone_cols = "Ns_reg",
                                      pixel_id_col = "ns",
                                      class_col = "lu_class",
                                      area_col = "area_km2",
                                      threshold = 0.01,
                                      mode = "decimal",
                                      allowed_classes = active_classes,
                                      pixel_meta_cols = c("Ns_reg", "x_ns", "y_ns"))
  
  # Zones that are not harmonized are kept (spatial rulebook, Outside-Model Area), with
  # the same class level as this bundle and endogenous classes relabelled to `_other`.
  if (nrow(start_map_skipped) > 0) {
    pt <- copy(start_map_skipped)
    pt[, lu_class := ifelse(sapply(lu_class_2, is_l2_active), lu_class_2, lu_class_1)]
    pt <- pt[!is.na(lu_class) & lu_class != "" & lu_class != "NODATA",
             .(area_km2 = sum(LUM_area_km2, na.rm = TRUE)), by = .(Ns_reg, ns, x_ns, y_ns, lu_class)]
    pt <- keep_outside_model_zones(pt, coverage, class_col = "lu_class",
                                   endogenous = unique(c(active_classes, curr_targets$lu_class)),
                                   handling = skipped_zone_handling,
                                   primary_rules = if (exists("current_model_cfg")) current_model_cfg$outside_model_primary_rules else NULL,
                                   label = paste0("AgMIP ", curr_mod, " - ", curr_scn))
    ds_start_map <- rbindlist(list(ds_start_map, pt), use.names = TRUE, fill = TRUE)
  }

  ds_start_map[, times := base_yr]
  ds_start_map[, model := curr_mod]
  ds_start_map[, scenario := curr_scn]
  
  all_harmonized[[length(all_harmonized) + 1]] <- copy(ds_start_map)
  
} # End Loop over model/scenario

final_ds_start_map <- rbindlist(all_harmonized, use.names = TRUE, fill = TRUE)

if (exists("save_intermediate")) {
  out_rds <- save_intermediate(final_ds_start_map, "processed_ds_start_map", ds_res)
} else {
  out_rds <- file.path(paths$temp, paste0("processed_ds_start_map_", ds_res, "_AgMIP_", format(Sys.Date(), "%Y%m%d"), ".rds"))
  saveRDS(final_ds_start_map, out_rds)
}
message(">>> Saved bundled harmonized AgMIP start map to: ", out_rds)

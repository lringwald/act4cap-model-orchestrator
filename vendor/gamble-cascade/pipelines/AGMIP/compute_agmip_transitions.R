# ===============================================================
# compute_agmip_transitions.R: AgMIP Land-Use Transition Computation
# ===============================================================
# Modeled after compute_CAPRI_transitions.R. Computes LU → LUC
# transition matrices from the country-level land-use targets
# produced by compile_agmip_csv_data.R.
#
# Since AgMIP CSV data is at national resolution (no sub-regional
# LUID/AEZ), this script operates purely at the country level
# and uses the Fienberg IPF algorithm (solve_gross_transitions) to estimate
# transition flows.
#
# Required globals:
#   current_model_cfg, paths, read_intermediate, save_intermediate,
#   mappings (for country availability)
# ===============================================================
library(data.table)

# ---------------------------------------------------------------
# 0. Preparation & Data Loading
# ---------------------------------------------------------------
if (!exists("paths") || is.null(paths$temp)) {
  paths <- list(temp = tempdir())
}

if (exists("ensure_cascade_engine", mode = "function")) {
  ensure_cascade_engine()
} else if (file.exists("scripts/core/run_context.R")) {
  source("scripts/core/run_context.R")
  ensure_cascade_engine()
}

if (!exists("solve_gross_transitions")) {
  for (cand in c(
    "../cascade-core/R/solve_gross_transitions.R",
    "external/cascade-core/R/solve_gross_transitions.R",
    "vendor/cascade-core/R/solve_gross_transitions.R",
    "/app/vendor/cascade-core/R/solve_gross_transitions.R"
  )) {
    if (file.exists(cand)) {
      source(cand)
      break
    }
  }
}
if (!exists("solve_gross_transitions")) {
  stop("Missing solve_gross_transitions.R — Cannot run Fienberg algorithm.")
}

message(">>> [AgMIP] Loading LUC targets...")
temp_targets <- read_intermediate("LUC_targets_country")

# Standardize column names


# Ns_reg is the national spatial key (ISO3C), set by compile_agmip_csv_data.R
if (!"Ns_reg" %in% names(temp_targets)) {
  stop("Column 'Ns_reg' not found in LUC_targets_country. Check compile_agmip_csv_data.R output.")
}

if (exists("current_model_cfg") && !is.null(current_model_cfg$agmip_model_name) && !"all" %in% tolower(current_model_cfg$agmip_model_name)) {
  if ("model" %in% names(temp_targets)) temp_targets <- temp_targets[tolower(model) %in% tolower(current_model_cfg$agmip_model_name)]
}
if (exists("scen_filter") && !is.null(scen_filter)) {
  scens_to_keep <- tolower(unique(c(scen_filter, sub(".*::", "", scen_filter))))
  temp_targets <- temp_targets[scenario %in% scens_to_keep | sub(".*::", "", scenario) %in% scens_to_keep]
} else if (exists("current_model_cfg") && !is.null(current_model_cfg$base_scen)) {
  temp_targets <- temp_targets[scenario %in% tolower(current_model_cfg$base_scen) | sub(".*::", "", scenario) %in% tolower(current_model_cfg$base_scen)]
}

temp_targets[, year := as.integer(year)]
setorder(temp_targets, model, scenario, Ns_reg, lu_class, year)

# ---------------------------------------------------------------
# 0.5 Determine timesteps and validate
# ---------------------------------------------------------------
available_years <- sort(unique(temp_targets$year))
if (length(available_years) < 2) {
  stop("Need at least 2 years of target data to compute transitions.")
}
# Periods are paired per model x scenario: AgMIP models do not necessarily report
# the same years, and one global year list would pair a model's 2030 with a 2025
# it never reported (read as zero area everywhere).
year_pairs <- build_period_pairs(temp_targets, group_cols = c("model", "scenario"), label = "AgMIP transitions",
                                 expected_step = if (exists("current_model_cfg")) current_model_cfg$expected_period_step else NULL)

# ---------------------------------------------------------------
# 1. Build Temporal Lag Structure (Consecutive Pairing)
# ---------------------------------------------------------------
temp_current <- temp_targets[, .(model, scenario, Ns_reg, lu_class, year, area_t = area_km2)]
temp_lag     <- lag_by_period(temp_targets[, .(model, scenario, Ns_reg, lu_class, year, area_km2)], year_pairs,
                              group_cols = c("model", "scenario"), value_col = "area_km2", lag_col = "area_t_lag")
temp_data    <- merge(temp_current, temp_lag,
                      by = c("model", "scenario", "Ns_reg", "lu_class", "year"), all = TRUE)
temp_data[is.na(area_t), area_t := 0]
temp_data[is.na(area_t_lag), area_t_lag := 0]

# Keep only valid transition years (drop each group's first period)
temp_data <- merge(temp_data, unique(year_pairs[, .(model, scenario, year)]), by = c("model", "scenario", "year"))

# Calculate identical area persistence (staying in same class)
temp_data[, identical_area := pmin(area_t, area_t_lag)]
temp_data[, `:=`(area_t_lag = area_t_lag - identical_area,
                 area_t     = area_t     - identical_area)]

# Calculate net inflows/outflows from/to "nld"
temp_data[, total_t_lag := sum(area_t_lag), by = .(model, scenario, Ns_reg, year)]
temp_data[, total_t     := sum(area_t),     by = .(model, scenario, Ns_reg, year)]
temp_data[, macro_inflow  := pmax(total_t - total_t_lag, 0)]
temp_data[, macro_outflow := pmax(total_t_lag - total_t, 0)]

# ---------------------------------------------------------------
# 2. NNLS Prior Estimation
# ---------------------------------------------------------------
message(">>> [AgMIP] Estimating empirical transition prior matrix (Global NNLS)...")
if (!requireNamespace("nnls", quietly = TRUE)) {
  install.packages("nnls", repos = "https://cloud.r-project.org/")
}

start_wide  <- dcast(temp_data, model + scenario + Ns_reg + year ~ lu_class, value.var = "area_t_lag", fill = 0)
target_wide <- dcast(temp_data, model + scenario + Ns_reg + year ~ lu_class, value.var = "area_t",     fill = 0)

unm_col <- if ("NLD" %in% names(start_wide)) "NLD" else if ("ONV" %in% names(start_wide)) "ONV" else "NLD"
macro_in_dt  <- unique(temp_data[, .(model, scenario, Ns_reg, year, flow = macro_inflow)])
macro_out_dt <- unique(temp_data[, .(model, scenario, Ns_reg, year, flow = macro_outflow)])

# Add unmanaged flow columns (if not already present from the data)
if (unm_col %in% names(start_wide)) {
  start_wide[, (unm_col) := get(unm_col) + macro_in_dt[match(paste(model, scenario, Ns_reg, year), paste(macro_in_dt$model, macro_in_dt$scenario, macro_in_dt$Ns_reg, macro_in_dt$year))]$flow]
} else {
  start_wide[, (unm_col) := macro_in_dt[match(paste(model, scenario, Ns_reg, year), paste(macro_in_dt$model, macro_in_dt$scenario, macro_in_dt$Ns_reg, macro_in_dt$year))]$flow]
}
if (unm_col %in% names(target_wide)) {
  target_wide[, (unm_col) := get(unm_col) + macro_out_dt[match(paste(model, scenario, Ns_reg, year), paste(macro_out_dt$model, macro_out_dt$scenario, macro_out_dt$Ns_reg, macro_out_dt$year))]$flow]
} else {
  target_wide[, (unm_col) := macro_out_dt[match(paste(model, scenario, Ns_reg, year), paste(macro_out_dt$model, macro_out_dt$scenario, macro_out_dt$Ns_reg, macro_out_dt$year))]$flow]
}

setnames(start_wide,  setdiff(names(start_wide),  c("model", "scenario", "Ns_reg", "year")),
         paste0("SRC_", setdiff(names(start_wide),  c("model", "scenario", "Ns_reg", "year"))))
setnames(target_wide, setdiff(names(target_wide), c("model", "scenario", "Ns_reg", "year")),
         paste0("TGT_", setdiff(names(target_wide), c("model", "scenario", "Ns_reg", "year"))))

model_data <- merge(start_wide, target_wide, by = c("model", "scenario", "Ns_reg", "year"), all = TRUE)
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
beta_matrix <- beta_matrix + 0.001  # Small smoothing

empirical_prior <- as.data.table(as.table(beta_matrix))
setnames(empirical_prior, c("V1", "V2", "N"), c("from_class", "to_class", "value"))
empirical_prior <- empirical_prior[value > 0]
empirical_prior[, from_class := as.character(from_class)]
empirical_prior[, to_class := as.character(to_class)]

unm_sink <- if ("NLD" %in% temp_data$lu_class) "NLD" else if ("ONV" %in% temp_data$lu_class) "ONV" else "NLD"

# Disallow unm_sink → unm_sink self-transitions
empirical_prior[toupper(from_class) == unm_sink & toupper(to_class) == unm_sink, value := 0]

message(">>> [AgMIP] Estimated ", nrow(empirical_prior), " prior transition flows.")

# ---------------------------------------------------------------
# 3. Fienberg Transition Computation (solve_gross_transitions)
# ---------------------------------------------------------------
message(">>> [AgMIP] Computing shifts using IPF...")
transitions <- temp_data[, {
  m_in  <- first(macro_inflow)
  m_out <- first(macro_outflow)
  
  tmp_from <- .SD[, .(value = sum(area_t_lag)), by = .(lu = lu_class)]
  tmp_to   <- .SD[, .(value = sum(area_t)),     by = .(lu = lu_class)]
  
  if (isTRUE(m_in > 0)) {
    if (unm_sink %in% tmp_from$lu) {
      tmp_from[lu == unm_sink, value := value + m_in]
    } else {
      tmp_from <- rbindlist(list(tmp_from, data.table(lu = unm_sink, value = m_in)))
    }
  }
  if (isTRUE(m_out > 0)) {
    if (unm_sink %in% tmp_to$lu) {
      tmp_to[lu == unm_sink, value := value + m_out]
    } else {
      tmp_to <- rbindlist(list(tmp_to, data.table(lu = unm_sink, value = m_out)))
    }
  }
  
  if (sum(tmp_from$value) > 0 | sum(tmp_to$value) > 0) {
    rel_classes <- unique(c(tmp_from$lu, tmp_to$lu))
    
    # Ensure local_prior has ALL combinations of rel_classes
    local_prior <- CJ(from_class = rel_classes, to_class = rel_classes)
    local_prior <- merge(local_prior, empirical_prior, by = c("from_class", "to_class"), all.x = TRUE)
    local_prior[is.na(value), value := 0.001] # Fill missing priors with small smoothing
    local_prior[toupper(from_class) == unm_sink & toupper(to_class) == unm_sink, value := 0] # Re-enforce self-transition ban
    
    tmp <- solve_gross_transitions(tmp_from, tmp_to, prior = local_prior, keep_areas = "both", label = ipf_label("AgMIP transitions", .BY))
    setDT(tmp)
    setnames(tmp, old = c("lu.from", "lu.to", "value"), new = c("from_class", "to_class", "value"), skip_absent = TRUE)
    tmp <- tmp[toupper(from_class) != unm_sink | toupper(to_class) != unm_sink]
  } else {
    tmp <- data.table(from_class = character(), to_class = character(), value = numeric())
  }
  data.table(tmp)
}, by = .(model, scenario, Ns_reg, year)]

transitions[is.na(value), value := 0]

# Re-add identical area (staying in same class)
temp_net_fill <- temp_data[, .(model, scenario, Ns_reg, year,
                               from_class = lu_class, to_class = lu_class,
                               value = identical_area)]
transitions <- rbindlist(list(transitions, temp_net_fill), use.names = TRUE, fill = TRUE)

transitions <- transitions[, .(value = sum(value)), by = .(model, scenario, Ns_reg, year, from_class, to_class)]
transitions <- transitions[value > 0.001]

# Inject identical base year transitions (start map defines the base year state)
base_year <- available_years[1]
temp_base_targets <- temp_targets[year == base_year]
transitions_base <- temp_base_targets[, .(model, scenario, Ns_reg, year = base_year,
                                          from_class = lu_class, to_class = lu_class,
                                          value = area_km2)]
transitions <- rbindlist(list(transitions, transitions_base), use.names = TRUE, fill = TRUE)

# Save transitions
save_intermediate(transitions, "AgMIP_transitions")
message(">>> [AgMIP] Processed ", nrow(transitions), " transition flows.")

# ---------------------------------------------------------------


save_intermediate(transitions, "LUC_targets_full")
message(">>> [AgMIP] Saved LUC_targets_full.")

# CAPRI-like models have no sub-regional ACR disaggregation, so no ACR_priors
# or ACR_restrict intermediates are produced here. process_downscaling_v2.R
# treats both as optional.

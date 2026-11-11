# ===============================================================
# test_agmip_inputs_downscaling.R
# Test script for downscaling production and fertilizer (Nitrogen & Phosphorus)
# ===============================================================
library(data.table)
library(terra)
library(cli)
library(arrow)

source("scripts/aux_codes.R")

out_dir <- "P:/globiom/Projects/DS_AgMIP/DS_results/outputs/DS_AgMIP_GLOBIOM_10km_20260918"
lu_file <- file.path(out_dir, "DS_AgMIP_GLOBIOM_10km_20260918_lu_base.csv")

if (!file.exists(lu_file)) stop("Downscaled LU file not found: ", lu_file)

cli_h1("Testing AgMIP Production & Fertilizer Downscaling")

# 1. Load Downscaled Cropland Areas
cli_alert_info("Loading downscaled cropland areas...")
lu_dt <- fread(lu_file)[lu == "CRP" & value > 0]
setnames(lu_dt, c("times", "value"), c("year", "crp_area_km2"), skip_absent = TRUE)
lu_dt[, year := as.integer(year)]
cli_alert_success("Loaded {nrow(lu_dt)} cropland pixel records across years: {paste(unique(lu_dt$year), collapse=', ')}")

# 2. Load Macro Model Targets from Step 1
paths <- list(temp = "data/02_intermediate/")
model_name <- "AGMIP"
run_basename <- "20260428_compr_scens_merge_MAGNET_CluMondo_IMAGE_GLOBIOM_CAPRI"

crop_params <- read_intermediate("crop_parameters")
prod_targets <- read_intermediate("OUTPUT_targets")
oth_params <- read_intermediate("other_parameters")

# 3. Extract Nitrogen & Fertilizer Targets
cli_alert_info("Extracting Nitrogen & fertilizer targets...")
fert_targets <- oth_params[variable %in% c("frtn", "frton", "frtin", "nbal", "frtp", "pbal") & scenario == "base"]
fert_wide <- dcast(fert_targets, model + Ns_reg + scenario + year ~ variable, value.var = "value", fun.aggregate = sum)
print(head(fert_wide, 10))

# 4. Load High-Res Spatial Prior
p_path <- "P:/globiom/Projects/LAMASUS/DS_results/temp/cached_ns_crop_yield_priors.parquet"
cli_alert_info("Loading high-resolution spatial priors from cache...")
priors_raw <- as.data.table(arrow::read_parquet(p_path, col_select = c("ns", "GLOB_country", "HR_Yield", "N_input_kg", "P_input_kg", "Water_m3")))

# Compute average baseline intensity per pixel (weighted across crops)
priors_pixel <- priors_raw[, .(
  base_yield_t_km2 = mean(HR_Yield, na.rm = TRUE),
  base_N_kg_km2    = mean(N_input_kg, na.rm = TRUE),
  base_P_kg_km2    = mean(P_input_kg, na.rm = TRUE),
  base_W_m3_km2    = mean(Water_m3, na.rm = TRUE)
), by = .(ns, GLOB_country)]

cli_alert_success("Loaded spatial priors for {nrow(priors_pixel)} pixels.")

# 5. Merge Cropland with Spatial Priors
lu_dt[, ns := as.integer(ns)]
priors_pixel[, ns := as.integer(ns)]
merged_crp <- merge(lu_dt, priors_pixel, by.x = c("ns", "Ns_reg"), by.y = c("ns", "GLOB_country"), all.x = TRUE)

# Fallbacks for missing prior pixels
merged_crp[is.na(base_N_kg_km2) | base_N_kg_km2 <= 0, base_N_kg_km2 := mean(priors_pixel$base_N_kg_km2, na.rm = TRUE)]
merged_crp[is.na(base_P_kg_km2) | base_P_kg_km2 <= 0, base_P_kg_km2 := mean(priors_pixel$base_P_kg_km2, na.rm = TRUE)]
merged_crp[is.na(base_yield_t_km2) | base_yield_t_km2 <= 0, base_yield_t_km2 := mean(priors_pixel$base_yield_t_km2, na.rm = TRUE)]

# 6. Scale Nitrogen Application to Match Macro Targets Exactly per Country and Year
# frtn is in 1000 t N = 1,000,000 kg N
merged_crp <- merge(merged_crp, fert_wide[, .(Ns_reg, year, frtn_target_1000t = frtn, frton_target_1000t = frton, frtin_target_1000t = frtin, nbal_target_1000t = nbal)],
                    by = c("Ns_reg", "year"), all.x = TRUE)

# Compute initial baseline total N per country
merged_crp[, base_total_N_kg := crp_area_km2 * base_N_kg_km2]
merged_crp[, country_base_N_kg := sum(base_total_N_kg, na.rm = TRUE), by = .(Ns_reg, year)]

# Scaling factor so sum of pixel N matches macro model target exactly
# frtn_target_kg = frtn_target_1000t * 1e6
merged_crp[, n_scale := fifelse(!is.na(frtn_target_1000t) & country_base_N_kg > 0,
                                (frtn_target_1000t * 1e6) / country_base_N_kg, 1.0)]

# Downscaled Nitrogen Application
merged_crp[, N_mineral_kg_ha := (base_N_kg_km2 / 100) * n_scale]  # 1 km2 = 100 ha
merged_crp[, Total_N_mineral_tons := (crp_area_km2 * N_mineral_kg_ha * 100) / 1000]

# Add Organic Nitrogen if reported
merged_crp[, org_n_ratio := fifelse(!is.na(frton_target_1000t) & !is.na(frtn_target_1000t) & frtn_target_1000t > 0,
                                    frton_target_1000t / frtn_target_1000t, 0.3)]
merged_crp[, Total_N_organic_tons := Total_N_mineral_tons * org_n_ratio]
merged_crp[, Total_N_tons := Total_N_mineral_tons + Total_N_organic_tons]
merged_crp[, N_total_kg_ha := N_mineral_kg_ha * (1 + org_n_ratio)]

cli_alert_success("Nitrogen downscaling completed.")
cat("\n=== Downscaled Nitrogen Summary (Year 2020) ===\n")
print(head(merged_crp[year == 2020, .(
  Ns_reg, ns, crp_area_km2 = round(crp_area_km2, 2),
  N_mineral_kg_ha = round(N_mineral_kg_ha, 1),
  N_total_kg_ha = round(N_total_kg_ha, 1),
  Total_N_mineral_tons = round(Total_N_mineral_tons, 1)
)], 10))

# 7. Verification of National Mass Balance
val_n <- merged_crp[, .(
  downscaled_1000t = sum(Total_N_mineral_tons, na.rm = TRUE) / 1000,
  target_1000t     = max(frtn_target_1000t, na.rm = TRUE)
), by = .(Ns_reg, year)]
val_n[, diff_pct := round((downscaled_1000t - target_1000t) / (target_1000t + 1e-9) * 100, 4)]
cat("\n=== National Nitrogen Target Verification (Year 2020) ===\n")
print(head(val_n[year == 2020], 10))
cat("Max relative error across all countries and years:", max(abs(val_n[!is.na(diff_pct)]$diff_pct)), "%\n")

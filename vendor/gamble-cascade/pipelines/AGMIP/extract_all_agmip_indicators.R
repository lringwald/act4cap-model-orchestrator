# ===============================================================
# extract_all_agmip_indicators.R
# ===============================================================
# Extracts all main agricultural, environmental, and economic indicators
# from the updated AgMIP macro model merge (20260916_compr_scens_merge.csv)
# at the highest non-overlapping thematic (crop/livestock activity) resolution.
#
# Generates:
#   1. Separate .qs files per macro model with standardized metadata:
#      - data/macro_models_indicators_globiom.qs
#      - data/macro_models_indicators_capri.qs
#      - data/macro_models_indicators_image.qs
#      - data/macro_models_indicators_magnet.qs
#      - data/macro_models_indicators_clumondo.qs
#   2. Pillar-specific harmonized datasets (qs, parquet, csv) in data/
#   3. Full multi-model harmonized dataset
# ===============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(qs2)
  if (requireNamespace("arrow", quietly = TRUE)) library(arrow)
})

cat("====================================================================\n")
cat(">>> [AgMIP Indicator Extraction] Initializing...\n")
cat("====================================================================\n")

# Source file configuration
source_csv_name <- "20260916_compr_scens_merge.csv"
csv_path <- file.path("P:/globiom/Projects/DS_AgMIP/AgMIP_output", source_csv_name)
if (!file.exists(csv_path)) {
  stop("Input file not found at: ", csv_path)
}

cat(">>> Source CSV:", csv_path, "\n")
cat(">>> Reading raw macro model dataset (this may take ~20-30s)...\n")
t0 <- proc.time()
dt <- fread(csv_path)
cat(sprintf(">>> Loaded %d rows in %.2f seconds.\n", nrow(dt), (proc.time() - t0)[3]))

# Standardize column names
setnames(dt, tolower(names(dt)))
dt[, model := tolower(model)]
dt[, variable := tolower(variable)]
dt[, item := tolower(item)]
dt[, scenario := tolower(scenario)]
dt[, region := tolower(region)]
dt[, year := as.integer(year)]

# Standard metadata tags
project_tag       <- "ACT4CAP"
macro_model_tag   <- "AgMIP"
project_model_tag <- "ACT4CAP_AgMIP"

# -------------------------------------------------------------------
# 1. Map Regions to EU Scope & Harmonize Country Names
# -------------------------------------------------------------------
cat(">>> Mapping regions to EU scope...\n")
reg_map <- fread("data/aux_files/agmip_region_mapping.csv")
reg_map[, agmip_code := tolower(agmip_code)]

dt <- merge(dt, reg_map[, .(agmip_code, country, Ns_reg, is_eu_scope)],
            by.x = "region", by.y = "agmip_code", all.x = TRUE)

dt <- dt[is_eu_scope == TRUE & !is.na(country) & country != ""]
cat(sprintf(">>> Filtered to EU scope: %d rows remaining across %d countries.\n", 
            nrow(dt), uniqueN(dt$country)))

# Use standardized country name as 'country' and keep original code as 'native_region_code'
setnames(dt, "region", "native_region_code")

# -------------------------------------------------------------------
# 2. Load Item and Variable Descriptions
# -------------------------------------------------------------------
cat(">>> Attaching variable and item descriptions...\n")
item_map <- fread("data/aux_files/agmip_item_mapping.csv")
item_map[, item := tolower(item)]
var_map <- fread("data/aux_files/agmip_variable_mapping.csv")
var_map[, variable := tolower(variable)]

dt <- merge(dt, item_map[, .(item, item_description)], by = "item", all.x = TRUE)
dt <- merge(dt, var_map[, .(variable, variable_description)], by = "variable", all.x = TRUE)

# Fill missing descriptions
dt[is.na(item_description), item_description := item]
dt[is.na(variable_description), variable_description := variable]

# Attach standard provenance metadata columns everywhere
dt[, `:=`(
  file_name     = source_csv_name,
  project       = project_tag,
  macro_model   = macro_model_tag,
  project_model = project_model_tag
)]

# -------------------------------------------------------------------
# 3. Categorize Indicators into Pillars
# -------------------------------------------------------------------
cat(">>> Assigning indicator pillars...\n")
categorize_indicator <- function(var) {
  fifelse(var %in% c("land", "area", "arir", "arrf", "area_wetland", "area_org", "area_agrofor", "area_lft", "area_til", "area_biochar", "area_aem"), "Land_and_Area",
  fifelse(var %in% c("annr", "anpr"), "Livestock",
  fifelse(var %in% c("frtn", "frtin", "frton", "frtp", "frtip", "frtop", "nbal", "pbal", "watr", "watw", "pest"), "Inputs_and_Nutrients",
  fifelse(grepl("^y", var) | var %in% c("fyld", "lyld", "lyxo"), "Yields",
  fifelse(var %in% c("prod", "prod_dm", "fprod"), "Production_and_Output",
  fifelse(var %in% c("food", "food_dm", "feed", "feed_dm", "cons", "cons_dm", "fdry", "fdry_dm", "fnrm", "fnrm_dm", "frum", "frum_dm", "fcons", "calo", "cali", "proti", "proto"), "Consumption_Food_Feed",
  fifelse(var %in% c("impo", "impo_dm", "expo", "expo_dm", "nett", "nett_dm", "fnett", "othu", "othu_dm"), "Trade_and_Balances",
  fifelse(grepl("^e(ch4|co2|n2o|mis|nh3)", var), "Emissions",
  "Economics_and_Macro"))))))))
}

dt[, pillar := categorize_indicator(variable)]

# -------------------------------------------------------------------
# 4. Define Selection of Highest Thematic Resolution (Non-Overlapping)
# -------------------------------------------------------------------
cat(">>> Applying finest non-overlapping thematic item selections per model...\n")

# PILLAR 1: LAND & CROP AREA
finest_land <- rbindlist(list(
  # GLOBIOM
  data.table(model='globiom', variable='area', item=c('cgr|mai', 'cgr|ocg', 'wht', 'ric', 'dry', 'ocr', 'osd|ooc', 'osd|srs', 'pfb|cot', 'sgc'), lum_class='Cropland', is_disaggregated=TRUE),
  data.table(model='globiom', variable='area', item=c('for|hic', 'for|hin', 'for|loc', 'for|lon', 'for|man', 'for|mic', 'for|min', 'for|prim', 'for|pro'), lum_class='Forests', is_disaggregated=TRUE),
  data.table(model='globiom', variable='land', item='grs', lum_class='Pasture', is_disaggregated=TRUE),
  data.table(model='globiom', variable='land', item='nld', lum_class='Natural_unmanaged', is_disaggregated=TRUE),
  data.table(model='globiom', variable='land', item='onv', lum_class='Natural_other', is_disaggregated=TRUE),

  # CAPRI
  data.table(model='capri', variable='area', item=c('cgr|mai', 'cgr|ocg', 'dry', 'ocr', 'osd|ooc', 'osd|srs', 'pfb', 'ric', 'sgc', 'vfn|fru', 'vfn|puls', 'vfn|root', 'vfn|veg', 'wht'), lum_class='Cropland', is_disaggregated=TRUE),
  data.table(model='capri', variable='area', item=c('crp|prm', 'ecp'), lum_class='Cropland_other', is_disaggregated=TRUE),
  data.table(model='capri', variable='land', item='for', lum_class='Forests', is_disaggregated=TRUE),
  data.table(model='capri', variable='land', item='grs', lum_class='Pasture', is_disaggregated=TRUE),
  data.table(model='capri', variable='land', item='bui', lum_class='Urban', is_disaggregated=TRUE),
  data.table(model='capri', variable='land', item='nld', lum_class='Natural_unmanaged', is_disaggregated=TRUE),
  data.table(model='capri', variable='land', item=c('old', 'onv'), lum_class='Natural_other', is_disaggregated=TRUE),

  # IMAGE
  data.table(model='image', variable='area', item=c('cgr', 'dry', 'ocr', 'osd', 'pfb', 'ric', 'sgc', 'vfn', 'wht'), lum_class='Cropland', is_disaggregated=TRUE),
  data.table(model='image', variable='land', item=c('crp|prm', 'ecp'), lum_class='Cropland_other', is_disaggregated=TRUE),
  data.table(model='image', variable='area', item=c('for|man', 'for|prim'), lum_class='Forests', is_disaggregated=TRUE),
  data.table(model='image', variable='land', item='grs', lum_class='Pasture', is_disaggregated=TRUE),
  data.table(model='image', variable='land', item='nld', lum_class='Natural_unmanaged', is_disaggregated=TRUE),
  data.table(model='image', variable='land', item='onv', lum_class='Natural_other', is_disaggregated=TRUE),

  # MAGNET
  data.table(model='magnet', variable='area', item=c('cgr', 'dry', 'ocr', 'osd', 'pfb', 'ric', 'sgc', 'vfn|fru', 'vfn|nut', 'vfn|puls', 'vfn|root', 'vfn|veg', 'wht'), lum_class='Cropland', is_disaggregated=TRUE),
  data.table(model='magnet', variable='land', item=c('for|man', 'for|prim'), lum_class='Forests', is_disaggregated=TRUE),
  data.table(model='magnet', variable='land', item='lsp', lum_class='Pasture', is_disaggregated=TRUE),
  data.table(model='magnet', variable='land', item='onv', lum_class='Natural_other', is_disaggregated=TRUE),

  # CLUMONDO
  data.table(model='clumondo', variable='land', item=c('crp', 'crp|prm'), lum_class='Cropland', is_disaggregated=TRUE),
  data.table(model='clumondo', variable='land', item='for', lum_class='Forests', is_disaggregated=TRUE),
  data.table(model='clumondo', variable='land', item='grs', lum_class='Pasture', is_disaggregated=TRUE),
  data.table(model='clumondo', variable='land', item='bui', lum_class='Urban', is_disaggregated=TRUE),
  data.table(model='clumondo', variable='land', item=c('nld', 'old', 'onv'), lum_class='Natural_other', is_disaggregated=TRUE)
))

# PILLAR 2: LIVESTOCK
finest_livestock <- rbindlist(list(
  # CAPRI
  data.table(model='capri', variable='annr', item=c('rum', 'nrm', 'dry', 'lsp'), livestock_group='Headcount_1000LSU'),
  data.table(model='capri', variable='area', item=c('rum|bef', 'rum|oru', 'lsp'), livestock_group='Pasture_Area_1000ha'),

  # IMAGE
  data.table(model='image', variable='annr', item=c('rum', 'nrm', 'dry', 'oap', 'lsp'), livestock_group='Headcount_1000LSU'),
  data.table(model='image', variable='area', item=c('rum', 'lsp'), livestock_group='Pasture_Area_1000ha'),

  # GLOBIOM
  data.table(model='globiom', variable='area', item=c('rum|bef', 'rum|oru', 'lsp'), livestock_group='Pasture_Area_1000ha'),

  # MAGNET
  data.table(model='magnet', variable='area', item=c('rum', 'lsp', 'dry', 'oap'), livestock_group='Pasture_Area_1000ha'),
  data.table(model='magnet', variable='anpr', item='tot', livestock_group='Animal_Productivity_Index'),

  # CLUMONDO
  data.table(model='clumondo', variable='annr', item='lsp', livestock_group='Headcount_1000LSU')
))

# Tag finest resolution flags
dt_land_finest <- merge(dt[pillar == "Land_and_Area"], finest_land, by = c("model", "variable", "item"))
dt_ls_finest   <- merge(dt[pillar == "Livestock"], finest_livestock, by = c("model", "variable", "item"))

# For Inputs, Yields, Production, Trade, Consumption, Emissions, Economics:
# Exclude known top-level broad aggregates (like 'tot', 'agr', 'crp') ONLY when detailed sub-activities exist for that variable & model.
identify_finest_items <- function(sub_dt) {
  # If a variable in a model has disaggregated crop items (wht, cgr, osd, etc.), keep the disaggregated items
  # and mark broad aggregates ('tot', 'agr', 'crp') as aggregate_level = "Macro_Aggregate".
  broad_aggs <- c("tot", "agr", "crp", "lsp")
  
  sub_dt[, has_disagg := any(!item %in% broad_aggs), by = .(model, variable)]
  sub_dt[, is_finest_activity := fifelse(has_disagg, !item %in% broad_aggs, TRUE)]
  sub_dt[, activity_level := fifelse(is_finest_activity, "Finest_Thematic_Activity", "Macro_Aggregate")]
  sub_dt[, has_disagg := NULL]
  return(sub_dt)
}

cat(">>> Identifying finest resolution activities across other pillars...\n")
dt_other_pillars <- dt[!pillar %in% c("Land_and_Area", "Livestock")]
dt_other_pillars <- identify_finest_items(dt_other_pillars)

# Combine into one master indicator table
cat(">>> Harmonizing full indicators table...\n")
dt_land_finest[, `:=`(is_finest_activity = TRUE, activity_level = "Finest_Thematic_Activity")]
dt_ls_finest[, `:=`(is_finest_activity = TRUE, activity_level = "Finest_Thematic_Activity", lum_class = NA_character_)]
if (!"lum_class" %in% names(dt_other_pillars)) dt_other_pillars[, lum_class := NA_character_]

# Combine all finest indicators
dt_finest_all <- rbindlist(list(
  dt_land_finest[, .(file_name, project, project_model, model, country, Ns_reg, native_region_code, scenario, year, pillar, variable, item, value, unit, lum_class, item_description, variable_description, activity_level, is_finest_activity)],
  dt_ls_finest[, .(file_name, project, project_model, model, country, Ns_reg, native_region_code, scenario, year, pillar, variable, item, value, unit, lum_class, item_description, variable_description, activity_level, is_finest_activity)],
  dt_other_pillars[is_finest_activity == TRUE, .(file_name, project, project_model, model, country, Ns_reg, native_region_code, scenario, year, pillar, variable, item, value, unit, lum_class, item_description, variable_description, activity_level, is_finest_activity)]
), use.names = TRUE, fill = TRUE)

setorder(dt_finest_all, model, pillar, variable, item, country, scenario, year)

# -------------------------------------------------------------------
# 5. Export Separate .qs Files Per Macro Model
# -------------------------------------------------------------------
cat("\n====================================================================\n")
cat(">>> EXPORTING SEPARATE .QS FILES PER MACRO MODEL\n")
cat("====================================================================\n")

models_found <- sort(unique(dt_finest_all$model))
data_dir <- "data"
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

outputs_dir <- "P:/globiom/Projects/DS_AgMIP/DS_results/outputs"
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)

for (m in models_found) {
  dt_m <- dt_finest_all[model == m]
  
  # Export to local data/
  qs_local <- file.path(data_dir, sprintf("ACT4CAP_AgMIP_indicators_%s.qs", m))
  qs2::qs_save(dt_m, qs_local)
  
  # Also copy / save to P: drive outputs
  qs_remote <- file.path(outputs_dir, sprintf("ACT4CAP_AgMIP_indicators_%s.qs", m))
  tryCatch(qs2::qs_save(dt_m, qs_remote), error = function(e) {
    cat("Warning: Could not write directly to remote outputs_dir:", e$message, "\n")
  })
  
  cat(sprintf("  * [%s] Saved %d rows across %d pillars, %d variables, %d items -> %s\n",
              toupper(m), nrow(dt_m), uniqueN(dt_m$pillar), uniqueN(dt_m$variable), uniqueN(dt_m$item), qs_local))
}

# -------------------------------------------------------------------
# 6. Export Pillar-Specific Tables & Master Dataset
# -------------------------------------------------------------------
cat("\n====================================================================\n")
cat(">>> EXPORTING PILLAR DATASETS AND COMBINED TABLES\n")
cat("====================================================================\n")

# Pillar 1: Land and Crop Area
land_file_base <- file.path(data_dir, "ACT4CAP_AgMIP_finest_land_crop_area")
land_remote_base <- file.path(outputs_dir, "ACT4CAP_AgMIP_finest_land_crop_area")
qs2::qs_save(dt_finest_all[pillar == "Land_and_Area"], paste0(land_file_base, ".qs"))
tryCatch(qs2::qs_save(dt_finest_all[pillar == "Land_and_Area"], paste0(land_remote_base, ".qs")), error = function(e) NULL)
fwrite(dt_finest_all[pillar == "Land_and_Area"], paste0(land_file_base, ".csv"))
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(dt_finest_all[pillar == "Land_and_Area"], paste0(land_file_base, ".parquet"))
  tryCatch(arrow::write_parquet(dt_finest_all[pillar == "Land_and_Area"], paste0(land_remote_base, ".parquet")), error = function(e) NULL)
}
cat(sprintf("  * [Land & Crop Area] Saved %d rows -> %s (.qs, .csv, .parquet)\n", 
            nrow(dt_finest_all[pillar == "Land_and_Area"]), land_file_base))

# Pillar 2: Livestock
ls_file_base <- file.path(data_dir, "ACT4CAP_AgMIP_finest_livestock")
ls_remote_base <- file.path(outputs_dir, "ACT4CAP_AgMIP_finest_livestock")
qs2::qs_save(dt_finest_all[pillar == "Livestock"], paste0(ls_file_base, ".qs"))
tryCatch(qs2::qs_save(dt_finest_all[pillar == "Livestock"], paste0(ls_remote_base, ".qs")), error = function(e) NULL)
fwrite(dt_finest_all[pillar == "Livestock"], paste0(ls_file_base, ".csv"))
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(dt_finest_all[pillar == "Livestock"], paste0(ls_file_base, ".parquet"))
  tryCatch(arrow::write_parquet(dt_finest_all[pillar == "Livestock"], paste0(ls_remote_base, ".parquet")), error = function(e) NULL)
}
cat(sprintf("  * [Livestock] Saved %d rows -> %s (.qs, .csv, .parquet)\n", 
            nrow(dt_finest_all[pillar == "Livestock"]), ls_file_base))

# Pillar 3: Inputs & Nutrients
in_file_base <- file.path(data_dir, "ACT4CAP_AgMIP_finest_inputs_nutrients")
in_remote_base <- file.path(outputs_dir, "ACT4CAP_AgMIP_finest_inputs_nutrients")
qs2::qs_save(dt_finest_all[pillar == "Inputs_and_Nutrients"], paste0(in_file_base, ".qs"))
tryCatch(qs2::qs_save(dt_finest_all[pillar == "Inputs_and_Nutrients"], paste0(in_remote_base, ".qs")), error = function(e) NULL)
fwrite(dt_finest_all[pillar == "Inputs_and_Nutrients"], paste0(in_file_base, ".csv"))
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(dt_finest_all[pillar == "Inputs_and_Nutrients"], paste0(in_file_base, ".parquet"))
  tryCatch(arrow::write_parquet(dt_finest_all[pillar == "Inputs_and_Nutrients"], paste0(in_remote_base, ".parquet")), error = function(e) NULL)
}
cat(sprintf("  * [Inputs & Nutrients] Saved %d rows -> %s (.qs, .csv, .parquet)\n", 
            nrow(dt_finest_all[pillar == "Inputs_and_Nutrients"]), in_file_base))

# Pillar 4: Yields
yd_file_base <- file.path(data_dir, "ACT4CAP_AgMIP_finest_yields")
yd_remote_base <- file.path(outputs_dir, "ACT4CAP_AgMIP_finest_yields")
qs2::qs_save(dt_finest_all[pillar == "Yields"], paste0(yd_file_base, ".qs"))
tryCatch(qs2::qs_save(dt_finest_all[pillar == "Yields"], paste0(yd_remote_base, ".qs")), error = function(e) NULL)
fwrite(dt_finest_all[pillar == "Yields"], paste0(yd_file_base, ".csv"))
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(dt_finest_all[pillar == "Yields"], paste0(yd_file_base, ".parquet"))
  tryCatch(arrow::write_parquet(dt_finest_all[pillar == "Yields"], paste0(yd_remote_base, ".parquet")), error = function(e) NULL)
}
cat(sprintf("  * [Yields] Saved %d rows -> %s (.qs, .csv, .parquet)\n", 
            nrow(dt_finest_all[pillar == "Yields"]), yd_file_base))

# Pillar 5: Production & Output
pr_file_base <- file.path(data_dir, "ACT4CAP_AgMIP_finest_production")
pr_remote_base <- file.path(outputs_dir, "ACT4CAP_AgMIP_finest_production")
qs2::qs_save(dt_finest_all[pillar == "Production_and_Output"], paste0(pr_file_base, ".qs"))
tryCatch(qs2::qs_save(dt_finest_all[pillar == "Production_and_Output"], paste0(pr_remote_base, ".qs")), error = function(e) NULL)
fwrite(dt_finest_all[pillar == "Production_and_Output"], paste0(pr_file_base, ".csv"))
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(dt_finest_all[pillar == "Production_and_Output"], paste0(pr_file_base, ".parquet"))
  tryCatch(arrow::write_parquet(dt_finest_all[pillar == "Production_and_Output"], paste0(pr_remote_base, ".parquet")), error = function(e) NULL)
}
cat(sprintf("  * [Production & Output] Saved %d rows -> %s (.qs, .csv, .parquet)\n", 
            nrow(dt_finest_all[pillar == "Production_and_Output"]), pr_file_base))

# Master Combined Table
all_file_base <- file.path(data_dir, "ACT4CAP_AgMIP_all_indicators_finest")
all_remote_base <- file.path(outputs_dir, "ACT4CAP_AgMIP_all_indicators_finest")
qs2::qs_save(dt_finest_all, paste0(all_file_base, ".qs"))
tryCatch(qs2::qs_save(dt_finest_all, paste0(all_remote_base, ".qs")), error = function(e) NULL)
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(dt_finest_all, paste0(all_file_base, ".parquet"))
  tryCatch(arrow::write_parquet(dt_finest_all, paste0(all_remote_base, ".parquet")), error = function(e) NULL)
}
cat(sprintf("  * [Master Combined] Saved %d total indicator records -> %s (.qs, .parquet)\n", 
            nrow(dt_finest_all), all_file_base))

cat("\n====================================================================\n")
cat(">>> [AgMIP Indicator Extraction] COMPLETED SUCCESSFULLY!\n")
cat("====================================================================\n")

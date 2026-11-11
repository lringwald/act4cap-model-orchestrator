library(data.table)

cat(">>> Loading raw macro model dataset...\n")
agmip_f <- "P:/globiom/Projects/DS_AgMIP/AgMIP_output/20260428_compr_scens_merge_MAGNET_CluMondo_IMAGE_GLOBIOM_CAPRI.csv"
if (!file.exists(agmip_f)) agmip_f <- "P:/globiom/Projects/LAMASUS/GLOBIOM_DS_interface/20260428_compr_scens_merge_MAGNET_CluMondo_IMAGE_GLOBIOM_CAPRI.csv"
dt <- fread(agmip_f)

cat(">>> Loading mappings...\n")
lu_map <- fread("data/aux_files/agmip_lu_class_mapping.csv")
reg_map <- fread("data/aux_files/agmip_region_mapping.csv")
item_map <- fread("data/aux_files/agmip_item_mapping.csv")
item_map[, item := tolower(item)]
var_map <- fread("data/aux_files/agmip_variable_mapping.csv")
var_map[, variable := tolower(variable)]

# --- 1. Map Regions & Clean ---
dt <- merge(dt, reg_map[, .(agmip_code, Ns_reg, is_eu_scope)], 
            by.x = "region", by.y = "agmip_code", all.x = TRUE)
setnames(dt, "region", "old_region_code")
setnames(dt, "Ns_reg", "region")

# Filter to EU Scope
dt <- dt[is_eu_scope == TRUE & !is.na(region) & region != ""]
dt[, scenario := tolower(scenario)]

# --- 2. Select Highest Thematic Resolution Non-Overlapping Land/Crop Areas ---
selection_land <- rbindlist(list(
  # --- GLOBIOM ---
  data.table(model='globiom', variable='area', item=c('cgr|mai', 'cgr|ocg', 'wht', 'ric', 'dry', 'ocr', 'osd|ooc', 'osd|srs', 'pfb|cot', 'sgc'), lum_class='Cropland'),
  data.table(model='globiom', variable='area', item=c('for|hic', 'for|hin', 'for|loc', 'for|lon', 'for|man', 'for|mic', 'for|min', 'for|prim', 'for|pro'), lum_class='Forests'),
  data.table(model='globiom', variable='land', item='grs', lum_class='Pasture'),
  data.table(model='globiom', variable='land', item='nld', lum_class='Natural_unmanaged'),
  data.table(model='globiom', variable='land', item='onv', lum_class='Natural_other'),

  # --- IMAGE ---
  data.table(model='image', variable='area', item=c('cgr', 'dry', 'ocr', 'osd', 'pfb', 'ric', 'sgc', 'vfn', 'wht'), lum_class='Cropland'),
  data.table(model='image', variable='land', item=c('crp|prm', 'ecp'), lum_class='Cropland_other'),
  data.table(model='image', variable='area', item=c('for|man', 'for|prim'), lum_class='Forests'),
  data.table(model='image', variable='land', item='grs', lum_class='Pasture'),
  data.table(model='image', variable='land', item='nld', lum_class='Natural_unmanaged'),
  data.table(model='image', variable='land', item='onv', lum_class='Natural_other'),

  # --- MAGNET ---
  data.table(model='magnet', variable='area', item=c('cgr', 'dry', 'ocr', 'osd', 'pfb', 'ric', 'sgc', 'vfn|fru', 'vfn|nut', 'vfn|puls', 'vfn|root', 'vfn|veg', 'wht'), lum_class='Cropland'),
  data.table(model='magnet', variable='land', item=c('for|man', 'for|prim'), lum_class='Forests'),
  data.table(model='magnet', variable='land', item='lsp', lum_class='Pasture'),
  data.table(model='magnet', variable='land', item='onv', lum_class='Natural_other'),

  # --- CAPRI ---
  data.table(model='capri', variable='area', item=c('cgr|mai', 'cgr|ocg', 'dry', 'ocr', 'osd|ooc', 'osd|srs', 'pfb', 'ric', 'sgc', 'vfn|fru', 'vfn|puls', 'vfn|root', 'vfn|veg', 'wht'), lum_class='Cropland'),
  data.table(model='capri', variable='area', item=c('crp|prm', 'ecp'), lum_class='Cropland_other'),
  data.table(model='capri', variable='land', item='for', lum_class='Forests'),
  data.table(model='capri', variable='land', item='grs', lum_class='Pasture'),
  data.table(model='capri', variable='land', item='bui', lum_class='Urban'),
  data.table(model='capri', variable='land', item='nld', lum_class='Natural_unmanaged'),
  data.table(model='capri', variable='land', item=c('old', 'onv'), lum_class='Natural_other'),

  # --- CLUMONDO ---
  data.table(model='clumondo', variable='area', item='crp', lum_class='Cropland'),
  data.table(model='clumondo', variable='area', item='for', lum_class='Forests'),
  data.table(model='clumondo', variable='area', item='grs', lum_class='Pasture'),
  data.table(model='clumondo', variable='area', item='bui', lum_class='Urban'),
  data.table(model='clumondo', variable='area', item='nld', lum_class='Natural_unmanaged'),
  data.table(model='clumondo', variable='area', item='onv', lum_class='Natural_other')
))

# --- 3. Filter & Aggregate Land/Crop Areas ---
dt_finest_land <- merge(dt, selection_land, by = c("model", "variable", "item"))

cat(">>> Aggregating finest thematic resolution land/crop items per model, region, scenario, year...\n")
agg_land <- dt_finest_land[, .(value_1000ha = sum(value, na.rm = TRUE)), 
                           by = .(model, region, scenario, year, variable, item, lum_class, unit)]

# Merge descriptions
agg_land <- merge(agg_land, item_map, by = "item", all.x = TRUE)
agg_land <- merge(agg_land, var_map[, .(variable, variable_description)], by = "variable", all.x = TRUE)

out_land_file <- "data/macro_models_aggregated_lum_classes.csv"
fwrite(agg_land, out_land_file)
cat(">>> Land/crop areas saved to:", out_land_file, "\n")

# --- 4. Select and Aggregate Ruminant & Livestock Data ---
# Highest thematic resolution for livestock across models:
selection_livestock <- rbindlist(list(
  # --- GLOBIOM ---
  data.table(model='globiom', variable='area', item=c('rum|bef', 'rum|oru'), livestock_cat='Ruminants', category_detail='Ruminant Pasture Area'),
  data.table(model='globiom', variable='area', item='lsp', livestock_cat='Total Livestock', category_detail='Total Livestock Pasture Area'),

  # --- CAPRI ---
  data.table(model='capri', variable='annr', item='rum', livestock_cat='Ruminants', category_detail='Ruminant Headcount (1000 LSU)'),
  data.table(model='capri', variable='annr', item='nrm', livestock_cat='Non-Ruminants', category_detail='Non-Ruminant Headcount (1000 LSU)'),
  data.table(model='capri', variable='annr', item='lsp', livestock_cat='Total Livestock', category_detail='Total Livestock Headcount (1000 LSU)'),
  data.table(model='capri', variable='area', item=c('rum|bef', 'rum|oru'), livestock_cat='Ruminants', category_detail='Ruminant Pasture Area'),
  data.table(model='capri', variable='area', item='lsp', livestock_cat='Total Livestock Pasture', category_detail='Total Livestock Pasture Area'),

  # --- IMAGE ---
  data.table(model='image', variable='annr', item='rum', livestock_cat='Ruminants', category_detail='Ruminant Headcount (1000 LSU)'),
  data.table(model='image', variable='annr', item='nrm', livestock_cat='Non-Ruminants', category_detail='Non-Ruminant Headcount (1000 LSU)'),
  data.table(model='image', variable='annr', item='lsp', livestock_cat='Total Livestock', category_detail='Total Livestock Headcount (1000 LSU)'),
  data.table(model='image', variable='area', item='rum', livestock_cat='Ruminants', category_detail='Ruminant Pasture Area'),
  data.table(model='image', variable='area', item='lsp', livestock_cat='Total Livestock', category_detail='Total Livestock Pasture Area'),

  # --- MAGNET ---
  data.table(model='magnet', variable='area', item='rum', livestock_cat='Ruminants', category_detail='Ruminant Pasture Area'),
  data.table(model='magnet', variable='area', item='lsp', livestock_cat='Total Livestock', category_detail='Total Livestock Pasture Area'),

  # --- CLUMONDO ---
  data.table(model='clumondo', variable='annr', item='lsp', livestock_cat='Total Livestock', category_detail='Total Livestock Headcount (1000 LSU)')
))

cat(">>> Extracting highest thematic resolution ruminant & livestock data...\n")
dt_livestock <- merge(dt, selection_livestock, by = c("model", "variable", "item"))

agg_livestock <- dt_livestock[, .(value = sum(value, na.rm = TRUE)), 
                              by = .(model, region, scenario, year, variable, item, unit, livestock_cat, category_detail)]

# Merge descriptions
agg_livestock <- merge(agg_livestock, item_map, by = "item", all.x = TRUE)
agg_livestock <- merge(agg_livestock, var_map[, .(variable, variable_description)], by = "variable", all.x = TRUE)

# Separate headcount (annr, 1000 LSU) and pasture area (area, 1000 ha) for density calculation where possible
head_dt <- agg_livestock[variable == "annr" & livestock_cat == "Ruminants", .(model, region, scenario, year, head_1000lsu = value)]
area_dt <- agg_livestock[variable == "area" & livestock_cat == "Ruminants", .(area_1000ha = sum(value)), by = .(model, region, scenario, year)]

ruminant_density <- merge(head_dt, area_dt, by = c("model", "region", "scenario", "year"), all = TRUE)
ruminant_density[, density_lsu_per_ha := ifelse(!is.na(area_1000ha) & area_1000ha > 0, head_1000lsu / area_1000ha, NA_real_)]

out_ls_file <- "data/macro_models_ruminant_livestock.csv"
fwrite(agg_livestock, out_ls_file)

out_density_file <- "data/macro_models_ruminant_density.csv"
fwrite(ruminant_density, out_density_file)

cat(">>> Livestock aggregated data saved to:", out_ls_file, "\n")
cat(">>> Ruminant livestock density saved to:", out_density_file, "\n")
cat(">>> Script completed successfully!\n")

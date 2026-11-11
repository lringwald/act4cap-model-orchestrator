library(data.table)

# Initialize empty list to hold rows
mapping_list <- list()

add_map <- function(model, variable, items, lum_class, is_aggregate=FALSE) {
  if (length(items) > 0) {
    mapping_list[[length(mapping_list) + 1]] <<- data.table(
      model = model,
      variable = variable,
      agmip_item = items,
      lum_class = lum_class,
      is_aggregate = is_aggregate
    )
  }
}

# =========================================================
# Official AgMIP Mapping Rules based on Specification
# =========================================================

# ---------------------------------------------------------
# 1. GLOBIOM
# ---------------------------------------------------------
# LAND: CRP, GRS, ONV, BUI, OLD, FOR|MAN, FOR|PRIM (base items)
#       AGR (= CRP+GRS), TOT, FOR, NLD, ECP (aggregates)
add_map("globiom", "land", c("crp"), "Cropland")
add_map("globiom", "land", c("ecp"), "Cropland_other", is_aggregate=TRUE) # ECP included in CRP
add_map("globiom", "land", c("for|man", "for|prim"), "Forests")
add_map("globiom", "land", c("for"), "Forests", is_aggregate=TRUE)
add_map("globiom", "land", c("grs"), "Pasture")
add_map("globiom", "land", c("bui"), "Urban")
add_map("globiom", "land", c("old"), "Natural_other")
add_map("globiom", "land", c("onv"), "Natural_other")
add_map("globiom", "land", c("nld"), "Natural_unmanaged", is_aggregate=TRUE)
add_map("globiom", "land", c("agr", "tot"), "Cropland", is_aggregate=TRUE)

# AREA:
# Base Crops (finest level): cgr|mai, cgr|ocg, wht, ric, dry, ocr, osd|ooc, osd|srs, pfb|cot, sgc
# Crop Aggregates: crp (=sum of crops), cgr (=cgr|mai+cgr|ocg), osd (=osd|ooc+osd|srs), agr
# Base Forests (finest level): for|man, for|prim
# Forest Aggregate: for, for|hic, for|hin, for|loc, for|lon, for|mic, for|min, for|pro
# Livestock: lsp, rum|bef, rum|oru (base); rum, agr (aggregates)

globiom_crop_fine <- c("cgr|mai", "cgr|ocg", "wht", "ric", "ocr", "osd|ooc", "osd|srs", "pfb|cot", "sgc")
globiom_for_fine  <- c("for|man", "for|prim")
globiom_for_sub   <- c("for|hic", "for|hin", "for|loc", "for|lon", "for|mic", "for|min", "for|pro")
globiom_past_fine <- c("lsp", "rum|bef", "rum|oru")

add_map("globiom", "area", globiom_crop_fine, "Cropland")
add_map("globiom", "area", globiom_for_fine, "Forests")
add_map("globiom", "area", globiom_for_sub, "Forests", is_aggregate=TRUE)
add_map("globiom", "area", globiom_past_fine, "Pasture")
add_map("globiom", "area", c("agr", "crp", "cgr", "osd"), "Cropland", is_aggregate=TRUE)
add_map("globiom", "area", c("for"), "Forests", is_aggregate=TRUE)
add_map("globiom", "area", c("rum", "dry"), "Pasture", is_aggregate=TRUE)

# ---------------------------------------------------------
# 2. CAPRI
# ---------------------------------------------------------
# LAND: bui (Urban), old (Natural_other), onv (Natural_other), grs (Pasture), for|man, for|prim
#       aggregates: for, nld, agr, crp
add_map("capri", "land", c("bui"), "Urban")
add_map("capri", "land", c("old", "onv"), "Natural_other")
add_map("capri", "land", c("grs"), "Pasture")
add_map("capri", "land", c("for|man", "for|prim"), "Forests")
add_map("capri", "land", c("for"), "Forests", is_aggregate=TRUE)
add_map("capri", "land", c("nld"), "Natural_unmanaged", is_aggregate=TRUE)
add_map("capri", "land", c("agr"), "Cropland", is_aggregate=TRUE)
add_map("capri", "land", c("crp"), "Cropland", is_aggregate=TRUE)

# AREA:
# Fine crops: cgr|mai, cgr|ocg, ocr, osd|ooc, osd|srs, pfb, ric, sgc, vfn|fru, vfn|puls, vfn|root, vfn|veg, wht
# Crop sub-types: crp|prm, ecp (Cropland_other)
# Aggregates: agr, crp, crp|ara, cgr, osd, vfn
capri_crop_fine <- c("cgr|mai", "cgr|ocg", "ocr", "osd|ooc", "osd|srs", "pfb", "ric", "sgc", "vfn|fru", "vfn|puls", "vfn|root", "vfn|veg", "wht")
capri_crop_other <- c("crp|prm", "ecp")
capri_past_fine  <- c("grs", "lsp", "nrm", "nrm|egg", "nrm|prk", "nrm|ptm", "rum|bef", "rum|oru")

add_map("capri", "area", capri_crop_fine, "Cropland")
add_map("capri", "area", capri_crop_other, "Cropland_other")
add_map("capri", "area", c("for|man", "for|prim"), "Forests")
add_map("capri", "area", c("for"), "Forests", is_aggregate=TRUE)
add_map("capri", "area", capri_past_fine, "Pasture")
add_map("capri", "area", c("agr", "crp", "crp|ara", "cgr", "osd", "vfn"), "Cropland", is_aggregate=TRUE)
add_map("capri", "area", c("rum", "dry"), "Pasture", is_aggregate=TRUE)

# ---------------------------------------------------------
# 3. IMAGE
# ---------------------------------------------------------
# LAND: crp|ara (Cropland), crp|prm (Cropland_other), for|man, for|prim, grs (Pasture), bui (Urban), old (Natural_other), onv (Natural_other)
#       Aggregates: for, nld, ecp, crp (=crp|ara+crp|prm), agr (=crp+grs), tot
add_map("image", "land", c("crp|ara"), "Cropland")
add_map("image", "land", c("crp|prm"), "Cropland_other")
add_map("image", "land", c("ecp"), "Cropland_other", is_aggregate=TRUE)
add_map("image", "land", c("for|man", "for|prim"), "Forests")
add_map("image", "land", c("for"), "Forests", is_aggregate=TRUE)
add_map("image", "land", c("grs"), "Pasture")
add_map("image", "land", c("bui"), "Urban")
add_map("image", "land", c("old"), "Natural_other")
add_map("image", "land", c("nld"), "Natural_unmanaged", is_aggregate=TRUE)
add_map("image", "land", c("onv"), "Natural_other")
add_map("image", "land", c("agr", "tot", "crp"), "Cropland", is_aggregate=TRUE)

# AREA:
# Fine crops: cgr, ocr, osd, pfb, ric, sgc, vfn, wht
# Fine forests: for|man, for|prim
# Aggregates: crp, agr, for
image_crop_fine <- c("cgr", "ocr", "osd", "pfb", "ric", "sgc", "vfn", "wht")
image_for_fine  <- c("for|man", "for|prim")

add_map("image", "area", image_crop_fine, "Cropland")
add_map("image", "area", image_for_fine, "Forests")
add_map("image", "area", c("lsp"), "Pasture")
add_map("image", "area", c("rum", "dry"), "Pasture", is_aggregate=TRUE)
add_map("image", "area", c("agr", "crp"), "Cropland", is_aggregate=TRUE)
add_map("image", "area", c("for"), "Forests", is_aggregate=TRUE)

# ---------------------------------------------------------
# 4. MAGNET
# ---------------------------------------------------------
# LAND: crp (Cropland), lsp (Pasture -> GRS), for|man (Forests), for|prim (Forests), bui (Urban), old (Natural_other), onv (Natural_other)
#       Aggregates: afo (=TOT land), agr (=crp+lsp), for (=for|man+for|prim), nld
add_map("magnet", "land", c("crp"), "Cropland")
add_map("magnet", "land", c("for|man", "for|prim"), "Forests")
add_map("magnet", "land", c("lsp", "grs"), "Pasture")
add_map("magnet", "land", c("bui"), "Urban")
add_map("magnet", "land", c("old"), "Natural_other")
add_map("magnet", "land", c("onv"), "Natural_other")
add_map("magnet", "land", c("nld"), "Natural_unmanaged", is_aggregate=TRUE)
add_map("magnet", "land", c("agr", "afo"), "Cropland", is_aggregate=TRUE)
add_map("magnet", "land", c("for"), "Forests", is_aggregate=TRUE)

# AREA:
# Fine crops: cgr, ocr, osd, pfb, ric, sgc, vfn|fru, vfn|nut, vfn|puls, vfn|root, vfn|veg, wht
# Fine forests: for|man, for|prim
# Aggregates: crp, vfn, afo, agr, for
magnet_crop_fine <- c("cgr", "ocr", "osd", "pfb", "ric", "sgc", "vfn|fru", "vfn|nut", "vfn|puls", "vfn|root", "vfn|veg", "wht")

add_map("magnet", "area", magnet_crop_fine, "Cropland")
add_map("magnet", "area", c("for|man", "for|prim"), "Forests")
add_map("magnet", "area", c("lsp"), "Pasture")
add_map("magnet", "area", c("rum", "oap", "dry"), "Pasture", is_aggregate=TRUE)
add_map("magnet", "area", c("agr", "crp", "vfn", "afo"), "Cropland", is_aggregate=TRUE)
add_map("magnet", "area", c("for"), "Forests", is_aggregate=TRUE)

# ---------------------------------------------------------
# 5. CLUMONDO
# ---------------------------------------------------------
# LAND: crp|ara, crp|prm, for|man, for|prim, grs, bui, old, onv
# Aggregates: agr, for, crp, nld
add_map("clumondo", "land", c("crp|ara", "crp|prm"), "Cropland")
add_map("clumondo", "land", c("for|man", "for|prim"), "Forests")
add_map("clumondo", "land", c("for"), "Forests", is_aggregate=TRUE)
add_map("clumondo", "land", c("grs"), "Pasture")
add_map("clumondo", "land", c("bui"), "Urban")
add_map("clumondo", "land", c("old"), "Natural_other")
add_map("clumondo", "land", c("onv"), "Natural_other")
add_map("clumondo", "land", c("agr"), "Cropland", is_aggregate=TRUE)
add_map("clumondo", "land", c("nld"), "Natural_unmanaged", is_aggregate=TRUE)
add_map("clumondo", "land", c("crp"), "Cropland", is_aggregate=TRUE)

# ---------------------------------------------------------
# ANNR (Livestock Headcount / LSU)
# ---------------------------------------------------------
add_map("capri", "annr", c("rum"), "Livestock_Ruminants")
add_map("capri", "annr", c("nrm"), "Livestock_NonRuminants")
add_map("capri", "annr", c("lsp"), "Livestock_Total", is_aggregate=TRUE)

add_map("image", "annr", c("rum"), "Livestock_Ruminants")
add_map("image", "annr", c("nrm"), "Livestock_NonRuminants")
add_map("image", "annr", c("lsp"), "Livestock_Total", is_aggregate=TRUE)

add_map("clumondo", "annr", c("lsp"), "Livestock_Total", is_aggregate=TRUE)


# =========================================================
# Combine and format
# =========================================================
final_map <- rbindlist(mapping_list)
final_map <- unique(final_map, by = c("model", "variable", "agmip_item"))

# Add standard AgMIP dummy columns needed by downstream scripts
final_map[, `:=`(
  agmip_parent = "",
  lum_hierarchy = ifelse(is_aggregate, "aggregate", "UNFCCC"),
  description = paste(lum_class, "mapping for", agmip_item)
)]

# Reorder columns to match the standard format
setcolorder(final_map, c("model", "variable", "agmip_item", "agmip_parent", "lum_class", "lum_hierarchy", "is_aggregate", "description"))

# Write out the CSV
fwrite(final_map, "data/aux_files/agmip_lu_class_mapping.csv")
cat("Successfully updated data/aux_files/agmip_lu_class_mapping.csv with", nrow(final_map), "rows.\n")

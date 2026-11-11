library(data.table)
th <- fread("data/aux_files/LUM_Code_to_macro_model_mapping.csv")

# 3200: Natural protected
th[LUM_Code == 3200, c("AgMIP_label", "AgMIP_abbr_1", "AgMIP_abbr_2") := list("Natural_unmanaged", "ONV", "ONV")]

# 9999: NODATA
th[LUM_Code == 9999, c("AgMIP_label", "AgMIP_abbr_1", "AgMIP_abbr_2") := list("NODATA", "NODATA", "NODATA")]

# 14999 & 2001999:2006999: Organic arable cropland
th[LUM_Code %in% c(14999, 2001999:2006999), c("AgMIP_label", "AgMIP_abbr_1", "AgMIP_abbr_2") := list("Cropland_arable", "CRP", "CRP|ARA")]

# 3001999:3005999: Organic permanent crops
th[LUM_Code %in% c(3001999:3005999), c("AgMIP_label", "AgMIP_abbr_1", "AgMIP_abbr_2") := list("Cropland_permanent", "CRP", "CRP|PRM")]

# 15999:26999 & 4012999:4013999: Organic grasslands and pastures
th[LUM_Code %in% c(15999:26999, 4012999:4013999), c("AgMIP_label", "AgMIP_abbr_1", "AgMIP_abbr_2") := list("Grassland", "GRS", "GRS")]

cat("Remaining missing AgMIP_abbr_1:", sum(is.na(th$AgMIP_abbr_1) | th$AgMIP_abbr_1 == ""), "\n")

fwrite(th, "data/aux_files/LUM_Code_to_macro_model_mapping.csv")
cat("Saved updated mapping table successfully.\n")

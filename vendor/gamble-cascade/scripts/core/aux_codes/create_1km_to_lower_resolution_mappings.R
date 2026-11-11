rm(list=ls())
library(terra)
library(sf)
library(dplyr)
library(data.table)
library(exactextractr)
library(FNN)
library(arrow)

# 1. Setup Master Raster & Mapping Table
LUM_raster <- terra::rast("data/aux_files/INSPIRE_Europe_buffer_1km.tif")
# Ensure we have a unique ID for every pixel in the buffer
names(LUM_raster) <- "INSPIRE_Europe_buffer_1kmID"

# Extract coordinates for pixels that exist (non-NA in original tif)
mapping_df <- as.data.frame(LUM_raster, xy = TRUE, na.rm = TRUE)
raster <- LUM_raster
values(raster) <- 1:ncell(raster)
# 2. Loop for EEA Aggregated Grids (1km to 200km)
lower_resolutions <- c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 100, 200)
for(reso in lower_resolutions) {
  # 1. Create coarse grid
  coarse_raster <- aggregate(raster, fact = reso, fun = "mean", na.rm = TRUE)
  values(coarse_raster) <- 1:ncell(coarse_raster)

  res_col_name <- paste0("EEA_", reso, "kmID")
  x_col_name   <- paste0("x_", reso, "kmID")
  y_col_name   <- paste0("y_", reso, "kmID")

  # 2. Extract IDs (Your current solid logic)
  extracted_ids <- terra::extract(coarse_raster, mapping_df[, c("x", "y")])
  mapping_df[[res_col_name]] <- extracted_ids[, 2]

  # 3. Get true geometric centers for those IDs
  # We get the xy for every cell in the coarse raster
  coarse_coords <- as.data.frame(terra::xyFromCell(coarse_raster, 1:ncell(coarse_raster)))
  coarse_coords[[res_col_name]] <- 1:ncell(coarse_raster)

  # 4. Join the coordinates back to the mapping table
  mapping_df <- mapping_df %>%
    left_join(coarse_coords, by = res_col_name, suffix = c("", ".new")) %>%
    rename(!!x_col_name := x.new, !!y_col_name := y.new)
}

# 3. Function to Map Polygons and Fill Gaps (Marine/Coastal)
map_and_fill <- function(shp_input, target_name, template_raster, master_df, shp_id_col, fill_gaps = TRUE) {
  cat("\nProcessing:", target_name, "...\n")

  # Load and align
  if (inherits(shp_input, "sf") || inherits(shp_input, "SpatVector")) {
    vec <- terra::vect(shp_input)
  } else {
    vec <- terra::vect(shp_input)
  }

  if(crs(vec) != crs(template_raster)) vec <- terra::project(vec, template_raster)

  # Rasterize using the specific ID column from the shapefile
  poly_rast <- terra::rasterize(vec, template_raster, field = shp_id_col)

  # Pull values into the dataframe
  vals <- terra::extract(poly_rast, master_df[, c("x", "y")])
  master_df[[target_name]] <- vals[[shp_id_col]]

  # Find pixels that didn't hit a polygon (NA) and fill via Nearest Neighbor
  has_val_idx <- which(!is.na(master_df[[target_name]]))
  needs_val_idx <- which(is.na(master_df[[target_name]]))

  if(fill_gaps && length(needs_val_idx) > 0) {
    cat("Filling", length(needs_val_idx), "pixels outside the polygon boundaries...\n")
    nn <- FNN::get.knnx(
      data  = as.matrix(master_df[has_val_idx,  c("x", "y")]),
      query = as.matrix(master_df[needs_val_idx, c("x", "y")]),
      k = 1
    )
    master_df[[target_name]][needs_val_idx] <- master_df[[target_name]][has_val_idx][nn$nn.index]
  } else if (!fill_gaps && length(needs_val_idx) > 0) {
    cat("Skipping gap filling for", length(needs_val_idx), "pixels outside the polygon boundaries...\n")
  }
  return(master_df)
}

library(FNN)

# 4. Function to Fill Column Gaps (GLOBIOM countries)
fill_column_gaps <- function(master_df, target_col, x_col = "x", y_col = "y") {
  cat("\nProcessing column:", target_col, "...\n")

  # 1. Identify which rows have a country assigned, and which are NA
  has_val_idx <- which(!is.na(master_df[[target_col]]))
  needs_val_idx <- which(is.na(master_df[[target_col]]))

  # 2. Safety checks
  if (length(needs_val_idx) == 0) {
    cat("No NAs found in", target_col, "- nothing to fill.\n")
    return(master_df)
  }
  if (length(has_val_idx) == 0) {
    stop(paste("No valid data found in", target_col, "to use as reference!"))
  }

  cat("Filling", length(needs_val_idx), "NA pixels using Nearest Neighbor...\n")

  # --- THE FIX: as.data.frame() ensures standard column selection ---
  df_standard <- as.data.frame(master_df)

  # 3. Perform Nearest Neighbor search
  nn <- FNN::get.knnx(
    data  = as.matrix(df_standard[has_val_idx, c(x_col, y_col)]),
    query = as.matrix(df_standard[needs_val_idx, c(x_col, y_col)]),
    k = 1
  )

  # 4. Map the results back
  master_df[[target_col]][needs_val_idx] <- master_df[[target_col]][has_val_idx][nn$nn.index]

  cat("Done! Gaps filled.\n")
  return(master_df)
}

library(FNN)

fill_spatial_gaps_multi <- function(master_df, target_cols, x_col = "x", y_col = "y") {
  cat("\nProcessing columns:", paste(target_cols, collapse = ", "), "...\n")

  indicator_col <- target_cols[1]
  has_val_idx <- which(!is.na(master_df[[indicator_col]]))
  needs_val_idx <- which(is.na(master_df[[indicator_col]]))

  if (length(needs_val_idx) == 0) return(master_df)
  if (length(has_val_idx) == 0) stop("No valid data found to use as a reference!")

  cat("Calculating Nearest Neighbor to fill", length(needs_val_idx), "gaps...\n")

  # --- THE FIX: as.data.frame() ensures standard column selection ---
  # This makes the function compatible whether you pass a data.table OR a data.frame
  df_standard <- as.data.frame(master_df)

  nn <- FNN::get.knnx(
    data  = as.matrix(df_standard[has_val_idx, c(x_col, y_col)]),
    query = as.matrix(df_standard[needs_val_idx, c(x_col, y_col)]),
    k = 1
  )
  # ------------------------------------------------------------------

  # Map the values back
  for (col in target_cols) {
    if (col %in% names(master_df)) {
      # This syntax works for both data.tables and data.frames
      master_df[[col]][needs_val_idx] <- master_df[[col]][has_val_idx][nn$nn.index]
    }
  }

  cat("Done! Gaps filled.\n")
  return(master_df)
}

# 4. Map NUTS3 and CAPRI
mapping_df <- map_and_fill("data/aux_files/NUTS_LAMASUS/shp_nuts.shp", "NUTS3", LUM_raster, mapping_df, "NUTS_ID")

mapping_df <- map_and_fill("data/aux_files/CAPRI_NUTS/NUTS2SUP_NONuts2_TRNuts1_V5_2023.shp", "CAPRI_NUTS", LUM_raster, mapping_df, "NUTS2SUP")

cat("\n======================================================\n")
cat("Mapping G4M_half_degree...\n")
cat("======================================================\n")
mapping_df <- map_and_fill("data/aux_files/G4M_half_degree_shp/G4M_half_degree_shp.shp", "G4MID", LUM_raster, mapping_df, "G4MID")

cat("\n======================================================\n")
cat("Mapping EEA Biogeographical Regions...\n")
cat("======================================================\n")
mapping_df <- map_and_fill(
  "data/aux_files/EEA_biogeo/BiogeoRegions2016.shp",
  "EEA_biogeo",
  LUM_raster,
  mapping_df,
  "short_name"
)

cat("\n======================================================\n")
cat("Mapping GLOBIOM LUID and Country from Shapefiles...\n")
cat("======================================================\n")
# Extract GLOB_country from LUID_CTY
mapping_df <- map_and_fill(
  "data/aux_files/GLOBIOM_shp_files/LUID_CTY.shp",
  "GLOB_country",
  LUM_raster,
  mapping_df,
  "Field2"
)

# Extract GLOB_LUID from LUID_CTY
mapping_df <- map_and_fill(
  "data/aux_files/GLOBIOM_shp_files/LUID_CTY.shp",
  "GLOB_LUID",
  LUM_raster,
  mapping_df,
  "Field1_1"
)

cat("\n======================================================\n")
cat("Mapping ISO3C Country Codes natively via giscoR...\n")
cat("======================================================\n")
library(giscoR)
# Download ISO3C country polygons
iso3c_sf <- gisco_get_countries(resolution = "01")
# Map it!
mapping_df <- map_and_fill(
  iso3c_sf,
  "ISO3C",
  LUM_raster,
  mapping_df,
  "ISO3_CODE"
)

setDT(mapping_df)

GLOB_5arcmin_sf <- read_sf("data/aux_files/GLOBIOM_shp_files/5arcmin_simu_raster_shp.shp")
GLOB_5arcmin_sf <- st_as_sf(GLOB_5arcmin_sf)
GLOB_5arcmin_sf <- st_transform(GLOB_5arcmin_sf, terra::crs(LUM_raster, proj=TRUE))

res_list <- exactextractr::exact_extract(LUM_raster,
                                         GLOB_5arcmin_sf,
                                         weights="area",
                                         coverage_area = TRUE,
                                         progress = TRUE,
                                         max_cells_in_memory = 2e8)

names(res_list) <- GLOB_5arcmin_sf$DN

res <- rbindlist(res_list,idcol="GLOB_5arcminID")
res[,INSPIRE_Europe_buffer_1kmID:=value]
res[,value:=NULL]

res <-  res[is.finite(INSPIRE_Europe_buffer_1kmID),.(GLOB_5arcminID_area_km2=sum(coverage_area/weight,na.rm=TRUE)),
            by=.(INSPIRE_Europe_buffer_1kmID, GLOB_5arcminID)]

res[,GLOB_5arcminID:=as.integer(GLOB_5arcminID)]
mapping_df <- res[mapping_df, on = "INSPIRE_Europe_buffer_1kmID"]

GLOB_SimUID_rast <- terra::rast("data/aux_files/GLOBIOM_SimUID_raster/w001001.adf")
res_list <- exact_extract(GLOB_SimUID_rast,
                          GLOB_5arcmin_sf[GLOB_5arcmin_sf$DN %in% res$GLOB_5arcminID, ],
                          weights="area",
                          progress = TRUE,
                          max_cells_in_memory = 2e8)

names(res_list) <-   GLOB_5arcmin_sf[GLOB_5arcmin_sf$DN %in% res$GLOB_5arcminID, ]$DN

res <- rbindlist(res_list,idcol="GLOB_5arcminID")
res[,GLOB_SimUID := as.integer(value)]
res[,value:=NULL]

# 1. Find the row indices for the max coverage per group
idx <- res[, .I[which.max(coverage_fraction)], by = GLOB_5arcminID]$V1

# 2. Subset the table using those indices, but only select the two columns you want
res <- res[idx, .(GLOB_5arcminID, GLOB_SimUID)]

res <-  res[!is.na(GLOB_5arcminID)]
res[,GLOB_5arcminID:=as.integer(GLOB_5arcminID)]

mapping_df <- res[mapping_df, on = "GLOB_5arcminID"]

cat("Extracting 5-arcmin coordinates...\n")

# Calculate centroids of the 5arcmin polygons to get X and Y
# We suppress warnings about planar coordinates if sf is feeling chatty
glob_centroids <- suppressWarnings(st_centroid(GLOB_5arcmin_sf))
glob_coords <- st_coordinates(glob_centroids)

# Create a lookup data.table
glob_xy_dt <- data.table(
  GLOB_5arcminID = as.integer(GLOB_5arcmin_sf$DN),
  x_5arcmin = glob_coords[, "X"],
  y_5arcmin = glob_coords[, "Y"]
)
mapping_df <- merge(mapping_df, glob_xy_dt, by = "GLOB_5arcminID", all.x = TRUE)

# 4. Merge tabular GLOBIOM attributes (Altitude, Slope, Soil, AEZ, ColRow30)
cat("\n======================================================\n")
cat("Merging Tabular GLOBIOM Attributes...\n")
cat("======================================================\n")
globiom_tabular <- readRDS("data/aux_files/GLOBIOM_full_mapping.rds")
# Drop country to avoid conflict with the shapefile extraction
if("ALLCOUNTRY" %in% names(globiom_tabular)) globiom_tabular[, ALLCOUNTRY := NULL]
if("COUNTRY" %in% names(globiom_tabular)) globiom_tabular[, COUNTRY := NULL]
if("ALLCOLROW" %in% names(globiom_tabular)) globiom_tabular[, ALLCOLROW := NULL]

# Rename to standard GLOB_ format
setnames(globiom_tabular,
         old = c("SimUID", "ColRow30", "AltiClass", "SlpClass", "SoilClass", "AezClass"),
         new = c("GLOB_SimUID", "GLOB_ColRow30", "GLOB_altitude", "GLOB_slope", "GLOB_soil", "GLOB_AEZ"),
         skip_absent = TRUE)


# Ensure both merge keys are character to avoid factor vs integer errors
globiom_tabular[, GLOB_SimUID := as.character(GLOB_SimUID)]
mapping_df[, GLOB_SimUID := as.character(GLOB_SimUID)]

mapping_df <- merge(mapping_df, globiom_tabular, by = "GLOB_SimUID", all.x = TRUE)

# 5. Fill the gaps
mapping_df[, NUTS0 := as.factor(substr(NUTS3, 1, 2)), by = .(NUTS3)]
mapping_df[, NUTS1 := as.factor(substr(NUTS3, 1, 3)), by = .(NUTS3)]
mapping_df[, NUTS2 := as.factor(substr(NUTS3, 1, 4)), by = .(NUTS3)]

# For SimUID == 0, the tabular merge produces NAs.
# We explicitly spatial-NN fill country, LUID, ColRow, and ColRow30 so they have valid groupings.
# The remaining environmental properties (Soil, Altitude, AEZ, Slope) will remain NA and eventually become 'GLOB_NA'.
cols_to_fill <- c("GLOB_country", "GLOB_LUID", "GLOB_ColRow30")


mapping_df <- fill_spatial_gaps_multi(
  master_df = mapping_df,
  target_cols = cols_to_fill,
  x_col = "x",
  y_col = "y"
)

# Check and fix NUTS3 mismatches (e.g., AT31 cell under GLOB_Germany)
# Determine dominant NUTS0 (first 2 chars of NUTS3) for each GLOB_country
country_nuts_map <- mapping_df[, .N, by = .(GLOB_country, NUTS_code = substr(NUTS3, 1, 2))]
country_nuts_map <- country_nuts_map[!is.na(NUTS_code) & NUTS_code != "NA"]
dominant_map <- country_nuts_map[order(-N), .SD[1], by = GLOB_country]

# Use update join to avoid row reordering
mapping_df[dominant_map, dom_NUTS := i.NUTS_code, on = "GLOB_country"]

# Identify Valid vs Invalid
mapping_df[, is_valid_nuts := (substr(NUTS3, 1, 2) == dom_NUTS | is.na(dom_NUTS))]

# Reassign invalid cells to the closest valid cell within the same GLOB_country using Nearest Neighbor
invalid_idx <- which(!mapping_df$is_valid_nuts)
if (length(invalid_idx) > 0) {
  cat(">>> Correcting", length(invalid_idx), "NUTS mismatches via spatial nearest-neighbor...\n")

  countries_with_invalid <- unique(mapping_df$GLOB_country[invalid_idx])

  for (cntr in countries_with_invalid) {
    idx_inv <- which(mapping_df$GLOB_country == cntr & !mapping_df$is_valid_nuts)
    idx_val <- which(mapping_df$GLOB_country == cntr & mapping_df$is_valid_nuts)

    if (length(idx_val) > 0) {
      coords_inv <- as.matrix(mapping_df[idx_inv, .(x, y)])
      coords_val <- as.matrix(mapping_df[idx_val, .(x, y)])

      nn <- FNN::get.knnx(data = coords_val, query = coords_inv, k = 1)
      nearest_valid_idx <- idx_val[nn$nn.index]

      mapping_df[idx_inv, c("NUTS0", "NUTS1", "NUTS2", "NUTS3") := mapping_df[nearest_valid_idx, .(NUTS0, NUTS1, NUTS2, NUTS3)]]
    }
  }
}

# Remove temporary columns
mapping_df[, c("dom_NUTS", "is_valid_nuts") := NULL]

# Check and fix CAPRI_NUTS mismatches against NUTS0 -------------------------------------------------
# NUTS3 and CAPRI_NUTS are rasterised from DIFFERENT shapefiles (above), so their borders disagree:
# measured on the 10km BMLEH design, 4.66% of pixels carried a CAPRI country different from the one
# their NUTS3 implies -- near-symmetric neighbour pairs (PT<->ES 100/98, DE<->AT 66/60, NO->SE 145),
# the signature of two geometries each claiming a sliver of the other.
#
# WHY IT MATTERS ENOUGH TO FIX HERE rather than downstream. The CAPRI country is the RANDOM-EFFECT
# key for the land-use and livestock priors, and on this design the country intercept carries
# essentially ALL of the held-out skill (pooled-only McFadden -0.0009 against 0.2454 with the RE).
# A pixel handed its neighbour's intercept is therefore handed most of the model. It is also the
# direct cause of duplicate `ns` keys downstream: ID is EEA_<res>kmID_<NUTS3> while the grouping is
# CAPRI, so one cell can split into two rows on the same key.
#
# The DOMINANT rule is deliberately data-driven rather than a hardcoded crosswalk: it LEARNS that
# Ireland's CAPRI code is IR and that Belgium and Luxembourg both map to BL, so genuine encoding
# differences stay valid and only the geometric bleed is corrected. A hardcoded table would have to
# be maintained and would "fix" IE->IR into a mismatch.
capri_map <- mapping_df[!is.na(CAPRI_NUTS) & !is.na(NUTS0),
                        .N, by = .(NUTS0, CAPRI_code = substr(as.character(CAPRI_NUTS), 1, 2))]
capri_map <- capri_map[!is.na(CAPRI_code) & CAPRI_code != "NA" & nzchar(CAPRI_code)]
dominant_capri <- capri_map[order(-N), .SD[1], by = NUTS0]
mapping_df[dominant_capri, dom_CAPRI := i.CAPRI_code, on = "NUTS0"]
mapping_df[, is_valid_capri := (substr(as.character(CAPRI_NUTS), 1, 2) == dom_CAPRI |
                                is.na(dom_CAPRI) | is.na(CAPRI_NUTS))]

invalid_cap <- which(!mapping_df$is_valid_capri)
if (length(invalid_cap) > 0) {
  cat(sprintf(">>> Correcting %s CAPRI_NUTS/NUTS0 mismatches (%.2f%% of cells) via spatial nearest-neighbor...\n",
              format(length(invalid_cap), big.mark = ","), 100 * length(invalid_cap) / nrow(mapping_df)))
  .before <- mapping_df[invalid_cap, .N, by = .(NUTS0, from = substr(as.character(CAPRI_NUTS), 1, 2))][order(-N)]
  print(head(.before, 10))
  # Reassign WITHIN the same NUTS0: the country the NUTS geometry says the cell is in is taken as
  # authoritative, and the nearest correctly-coded CAPRI region inside that country supplies the code.
  for (n0 in unique(mapping_df$NUTS0[invalid_cap])) {
    idx_inv <- which(mapping_df$NUTS0 == n0 & !mapping_df$is_valid_capri)
    idx_val <- which(mapping_df$NUTS0 == n0 &  mapping_df$is_valid_capri & !is.na(mapping_df$CAPRI_NUTS))
    if (length(idx_val) > 0 && length(idx_inv) > 0) {
      nn <- FNN::get.knnx(data = as.matrix(mapping_df[idx_val, .(x, y)]),
                          query = as.matrix(mapping_df[idx_inv, .(x, y)]), k = 1)
      mapping_df[idx_inv, CAPRI_NUTS := mapping_df[idx_val[nn$nn.index], CAPRI_NUTS]]
    } else {
      cat(sprintf("    NUTS0 %s: %d mismatching cell(s) but no valid CAPRI cell to borrow from -- left as is\n",
                  n0, length(idx_inv)))
    }
  }
} else cat(">>> CAPRI_NUTS agrees with NUTS0 everywhere; nothing to correct.\n")
mapping_df[, c("dom_CAPRI", "is_valid_capri") := NULL]

# Create the TERRA_GLOBIOM indicator
mapping_df[, TERRA_GLOBIOM := (!is.na(GLOB_5arcminID) & !is.na(GLOB_SimUID) & GLOB_SimUID > 0)]

# Mask GLOB_LUID for non-GLOBIOM pixels (will become GLOB_NA in the next step)
mapping_df[TERRA_GLOBIOM == FALSE, GLOB_LUID := NA]

# Identify target columns
glob_cols <- names(mapping_df)[grepl("^GLOB_", names(mapping_df))]
glob_cols <- glob_cols[-which(glob_cols== "GLOB_5arcminID_area_km2")]
# Apply conversion and NA labeling
for (col in glob_cols) {
  # 1. Convert to character (to allow the "GLOB_NA" string assignment)
  set(mapping_df, j = col, value = as.character(mapping_df[[col]]))

  # 2. Replace actual NAs with the string "GLOB_NA"
  mapping_df[is.na(get(col)), (col) := "GLOB_NA"]

  # 3. Final conversion to factor
  set(mapping_df, j = col, value = as.factor(mapping_df[[col]]))
}

mapping_df[is.na(GLOB_5arcminID_area_km2), GLOB_5arcminID_area_km2 := 0]

# Convert NUTS3 and CAPRI_NUTS back to factor
mapping_df[, NUTS3 := as.factor(NUTS3)]
cat("Adding GLOB_REGION59 and GLOB_REGION37...\n")
reg_map <- readRDS("data/aux_files/GLOB_country_REGION59_REGION37_mapping.rds")
mapping_df <- merge(mapping_df, reg_map, by = "GLOB_country", all.x = TRUE)

# Convert to factor just to match the rest
mapping_df[, GLOB_REGION59 := as.factor(GLOB_REGION59)]
mapping_df[, GLOB_REGION37 := as.factor(GLOB_REGION37)]

# 1. Define groups of columns
core_ids    <- c("INSPIRE_Europe_buffer_1kmID", "EEA_1kmID")
admin_hier  <- c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "CAPRI_NUTS", "EEA_biogeo")
glob_main   <- c("GLOB_5arcminID", "GLOB_country", "TERRA_GLOBIOM", "GLOB_REGION59", "GLOB_REGION37", "G4MID")
glob_env    <- c("GLOB_altitude", "GLOB_slope", "GLOB_soil", "GLOB_AEZ", "GLOB_5arcminID_area_km2")
glob_tech   <- c("GLOB_SimUID", "GLOB_LUID", "GLOB_ColRow", "GLOB_ColRow30")

# 2. Extract the EEA spatial hierarchy (1km, 5km, 10km, etc.)
# This identifies columns like EEA_5kmID, x_5kmID, y_5kmID
eea_pattern <- "^(EEA|x|y)_([0-9]+kmID|[0-9]+km)$"
eea_grid_cols <- names(mapping_df)[grepl(eea_pattern, names(mapping_df))]

# Sort EEA columns naturally (1km first, then 5km, etc.)
# We extract the number and sort by it
grid_nums <- as.numeric(gsub("[^0-9]", "", eea_grid_cols))
eea_grid_cols <- eea_grid_cols[order(grid_nums)]

# 3. Final Master Coordinates
master_coords <- c("x", "y")

# 4. Combine into one vector
final_order <- c(
  core_ids,
  admin_hier,
  glob_main,
  glob_env,
  glob_tech,
  eea_grid_cols,
  master_coords
)

# 5. Apply the order in-place
# intersect() ensures we only try to order columns that actually exist
setcolorder(mapping_df, intersect(final_order, names(mapping_df)))

ds <- format(Sys.Date(), "%Y-%m-%d")

arrow::write_parquet(mapping_df, paste0("data/aux_files/one_kmID_master_mapping_", ds, ".parquet"))
saveRDS(mapping_df, paste0("data/aux_files/one_kmID_master_mapping_", ds, ".rds"))

cat("\nSuccess. All columns mapped and NAs removed.\n")

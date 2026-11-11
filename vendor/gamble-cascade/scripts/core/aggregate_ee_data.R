library(terra)
library(sf)
library(exactextractr)
library(data.table)

# Configuration
ee_root <- "data/01_raw/input/EE_Downloads"
grid_polygon_path <- "data/aux_files/INSPIRE_Europe_buffer_1km_geom_POLYGON.rds"
lamasus_buffer_path <- "data/aux_files/INSPIRE_Europe_buffer_1km.tif"
output_dir <- "data/02_intermediate/EE_aggregated"

# Create output directory
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 1. Load Grid Polygons and Reference Raster
cat("Loading grid polygons from:", grid_polygon_path, "...\n")
if(!file.exists(grid_polygon_path)) stop("Grid polygon file not found!")
raster_sf <- readRDS(grid_polygon_path)

cat("Loading reference buffer raster from:", lamasus_buffer_path, "...\n")
r_lamasus_ref <- rast(lamasus_buffer_path)

# List EE files
ee_files <- list.files(ee_root, pattern = "\\.tif$", full.names = TRUE)
if(length(ee_files) == 0) stop("No TIFF files found in ", ee_root)
cat("Found", length(ee_files), "TIFF files to process.\n")

# 2. Pre-reproject Polygons (Assuming all have the same CRS)
cat("Preprocessing CRS transformation...\n")
r_ref <- terra::rast(ee_files[1])
ref_crs <- crs(r_ref)
cat("  Target CRS:", crs(r_ref, describe=TRUE)$name, "\n")
raster_sf_trans <- st_transform(raster_sf, ref_crs)

# 3. Processing Loop (Optimized with Stacking)
cat("\nAnalyzing rasters to stack them by geometry...\n")
geom_groups <- list()

for(tif_path in ee_files){
  var_name <- tools::file_path_sans_ext(basename(tif_path))
  out_path_rds <- file.path(output_dir, paste0(var_name, "_aggregated.rds"))
  out_path_tif <- file.path(output_dir, paste0(var_name, "_1km.tif"))
  
  if(file.exists(out_path_rds) && file.exists(out_path_tif)){
    cat("Skipping", var_name, "- Outputs already exist.\n")
    next
  }
  
  # Load to get geometry signature
  r <- terra::rast(tif_path)
  # Simple signature: "xmin_xmax_ymin_ymax_ncol_nrow"
  sig <- paste(ext(r)[1], ext(r)[2], ext(r)[3], ext(r)[4], ncol(r), nrow(r), sep="_")
  
  if(is.null(geom_groups[[sig]])) {
      geom_groups[[sig]] <- c()
  }
  geom_groups[[sig]] <- c(geom_groups[[sig]], tif_path)
}

cat(sprintf("Found %d distinct geometries among files to process.\n", length(geom_groups)))

for(sig in names(geom_groups)) {
  group_files <- geom_groups[[sig]]
  cat(sprintf("\nProcessing group with %d layers...\n", length(group_files)))
  
  r_stack <- terra::rast(group_files)
  
  # Check if population is mixed with others
  agg_fun <- "mean"
  if(any(grepl("GlobPOP_Count", group_files, ignore.case = TRUE))) {
      agg_fun <- "sum"
      cat("  Warning: Using SUM aggregation because GlobPOP is in this group.\n")
  }
  
  if(as.character(crs(r_stack)) != as.character(ref_crs)){
    cat("  Warning: CRS mismatch. Reprojecting on the fly...\n")
    current_sf <- st_transform(raster_sf, crs(r_stack))
  } else {
    current_sf <- raster_sf_trans
  }
  
  cat("  Extracting all", length(group_files), "layers with exact_extract...\n")
  res <- exact_extract(r_stack, current_sf, 
                        fun = agg_fun,
                        append_cols = "INSPIRE_Europe_buffer_1kmID",
                        progress = TRUE)
                        
  dt_res <- as.data.table(res)
  extracted_cols <- setdiff(names(dt_res), "INSPIRE_Europe_buffer_1kmID")
  
  for(i in seq_along(group_files)) {
      var_name <- tools::file_path_sans_ext(basename(group_files[i]))
      col_name <- extracted_cols[i]
      
      out_path_rds <- file.path(output_dir, paste0(var_name, "_aggregated.rds"))
      out_path_tif <- file.path(output_dir, paste0(var_name, "_1km.tif"))
      
      dt_agg <- dt_res[, .(INSPIRE_Europe_buffer_1kmID, aggregated_value = get(col_name))]
      
      saveRDS(dt_agg, out_path_rds)
      cat("  Saved RDS to:", out_path_rds, "\n")
      
      r_out <- r_lamasus_ref
      m <- match(values(r_out), dt_agg$INSPIRE_Europe_buffer_1kmID)
      values(r_out) <- dt_agg$aggregated_value[m]
      writeRaster(r_out, out_path_tif, overwrite=TRUE)
  }
  rm(r_stack, res, dt_res); gc()
}

cat("\nEE Aggregation complete.\n")

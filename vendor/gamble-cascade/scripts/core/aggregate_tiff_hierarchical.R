# ===============================================================
# aggregate_tiff_hierarchical.R
# ===============================================================
# Reads generated Land Use (LU) and Livestock (LSU) raster files and
# creates hierarchical aggregates:
#   - Land Use: Dual hierarchy (both system aggregates across crops
#               AND crop aggregates across systems, plus parent classes).
#   - Livestock: Dual hierarchy (both system aggregates across cohorts
#                AND cohort aggregates across systems, plus grand total).
# Directly writes out Deflate-compressed Cloud-Optimized GeoTIFFs (COGs).
# ===============================================================
suppressMessages(library(terra))
suppressMessages(library(cli))
suppressMessages(library(data.table))

cli_alert_info("Starting dual hierarchical aggregation of TIFF files...")

if (!exists("paths") || is.null(paths$output_dir)) {
  if (exists("output_dir") && !is.null(output_dir)) {
    paths <- list(output_dir = output_dir)
  } else {
    stop("paths$output_dir is not defined.")
  }
}

if (!exists("output_prefix") || is.null(output_prefix)) {
  output_prefix <- basename(paths$output_dir)
}

# Find all generated LU and LSU TIFF files (ignoring already aggregated ones and prod/luc ones)
lu_files <- list.files(paths$output_dir, pattern = paste0("^", output_prefix, "_lu_.*\\.tif$"), full.names = TRUE)
lu_files <- lu_files[!grepl("_hierarchical", lu_files)]

lsu_files <- list.files(paths$output_dir, pattern = paste0("^", output_prefix, "_lsu_.*\\.tif$"), full.names = TRUE)
lsu_files <- lsu_files[!grepl("_hierarchical|_indicators_", lsu_files)]

all_files <- c(lu_files, lsu_files)
if (exists("TARGET_SCENARIOS") && !is.null(TARGET_SCENARIOS)) {
  all_files <- all_files[sapply(all_files, function(f) {
    any(sapply(TARGET_SCENARIOS, function(sc) grepl(paste0("_", sc, "_"), basename(f))))
  })]
}

if (length(all_files) == 0) {
  cli_alert_warning("No LU or LSU raster files found for hierarchical aggregation.")
} else {
  for (tif_path in all_files) {
    is_lsu <- grepl("_lsu_", basename(tif_path))
    if (is_lsu) {
      out_name <- gsub("_lsu_", "_lsu_hierarchical_", basename(tif_path))
    } else {
      out_name <- gsub("_lu_", "_lu_hierarchical_", basename(tif_path))
    }
    
    out_path <- file.path(paths$output_dir, out_name)
    
    force_run <- (exists("FORCE_REDO_LSU") && isTRUE(FORCE_REDO_LSU)) || 
                 (exists("FORCE_REGENERATE_TIFFS") && isTRUE(FORCE_REGENERATE_TIFFS)) ||
                 (exists("FORCE_REGENERATE_ALL") && isTRUE(FORCE_REGENERATE_ALL))
    
    if (file.exists(out_path) && !force_run) {
      cli_alert_info(sprintf("Skipping existing hierarchical aggregation: %s", out_name))
      next
    }
    
    cli_alert_info(sprintf("Processing hierarchical aggregation: %s", basename(tif_path)))
    
    r <- terra::rast(tif_path)
    lyr_names <- names(r)
    n_cells <- terra::ncell(r)
    vals <- terra::values(r)
    
    agg_names <- character(0)
    agg_mat_list <- list()
    
    # Helper: sum across columns with na.rm = TRUE, but preserve NA if all elements in the row are NA
    safe_row_sums <- function(mat) {
      if (ncol(mat) == 0) return(rep(NA_real_, nrow(mat)))
      s <- rowSums(mat, na.rm = TRUE)
      all_na <- rowSums(!is.na(mat)) == 0
      s[all_na] <- NA_real_
      s
    }
    
    if (!is_lsu) {
      # -------------------------------------------------------------
      # LAND USE DUAL HIERARCHY
      # -------------------------------------------------------------
      # 1. System aggregates across crops (e.g. Cropland_HI_TOTAL, Cropland_LI_TOTAL)
      sys_codes <- c("HI", "HIO", "IR", "IRO", "LI", "LIO", "SS", "SSO")
      for (s in sys_codes) {
        pattern <- paste0("^Cropland_", s, "_")
        idx <- grep(pattern, lyr_names)
        if (length(idx) > 0) {
          agg_nm <- paste0("Cropland_", s, "_TOTAL")
          agg_mat_list[[agg_nm]] <- safe_row_sums(vals[, idx, drop = FALSE])
          agg_names <- c(agg_names, agg_nm)
        }
      }
      
      # 2. Crop aggregates across systems (e.g. Cropland_Corn, Cropland_Whea)
      # Extract unique crops from Cropland_<sys>_<crop>
      crp_matches <- regmatches(lyr_names, regexec("^Cropland_[A-Za-z]+_([A-Za-z]+)$", lyr_names))
      crops_found <- unique(sapply(crp_matches, function(m) if (length(m) == 2) m[2] else NA_character_))
      crops_found <- crops_found[!is.na(crops_found)]
      
      for (crp in crops_found) {
        pattern <- paste0("^Cropland_[A-Za-z]+_", crp, "$")
        idx <- grep(pattern, lyr_names)
        if (length(idx) > 1) {
          agg_nm <- paste0("Cropland_", crp)
          agg_mat_list[[agg_nm]] <- safe_row_sums(vals[, idx, drop = FALSE])
          agg_names <- c(agg_names, agg_nm)
        }
      }
      
      # 3. Parent class aggregates (Cropland, Pasture, Forests, Natural, Wetlands)
      parent_classes <- c("Cropland", "Pasture", "Forests", "Natural", "Wetlands")
      for (pref in parent_classes) {
        pattern <- paste0("^", pref, "(_|$)")
        idx <- grep(pattern, lyr_names)
        # Exclude residual/outside _other layers from parent totals
        idx <- setdiff(idx, grep("(_|\\|)other$", lyr_names, ignore.case = TRUE))
        sub_idx <- grep(paste0("^", pref, "_"), lyr_names)
        sub_idx <- setdiff(sub_idx, grep("(_|\\|)other$", lyr_names, ignore.case = TRUE))
        
        # If pref is not in lyr_names, OR if there are sub-classes to sum up into the parent
        if (length(idx) > 0) {
          # Special handling for Pasture: rename base generic Pasture to Pasture_any
          if (pref == "Pasture" && "Pasture" %in% lyr_names) {
            # Provide Pasture_any as the explicit unmanaged/any management layer
            agg_mat_list[["Pasture_any"]] <- vals[, "Pasture"]
            agg_names <- c(agg_names, "Pasture_any")
          }
          
          agg_nm <- pref
          if (!agg_nm %in% lyr_names || length(sub_idx) > 0) {
            agg_mat_list[[agg_nm]] <- safe_row_sums(vals[, idx, drop = FALSE])
            agg_names <- c(agg_names, agg_nm)
          }
        }
      }
      
    } else {
      # -------------------------------------------------------------
      # LIVESTOCK DUAL HIERARCHY
      # -------------------------------------------------------------
      # 1. Animal cohort aggregates across systems
      # (e.g. livestock_bovd_lsu_heads, livestock_bovf_lsu_heads, etc.)
      cohorts <- c("bovo", "bovd", "bovf", "sgto", "sgtd", "sgtf", "pig", "pou", "lay", "bro")
      for (coh in cohorts) {
        pattern <- paste0("_", coh, "_lsu_heads$")
        idx <- grep(pattern, lyr_names, ignore.case = TRUE)
        if (length(idx) > 0) {
          agg_nm <- paste0("livestock_", coh, "_lsu_heads")
          agg_mat_list[[agg_nm]] <- safe_row_sums(vals[, idx, drop = FALSE])
          agg_names <- c(agg_names, agg_nm)
        }
      }
      
      # 2. System aggregates across cohorts
      # (e.g. livestock_lga_lsu_heads, livestock_mra_lsu_heads, etc.)
      systems <- c("lga", "lgh", "lgt", "mra", "mrh", "mrt", "urban", "other")
      for (sy in systems) {
        pattern <- paste0("^livestock_", sy, "_.*_lsu_heads$")
        idx <- grep(pattern, lyr_names, ignore.case = TRUE)
        if (length(idx) > 0) {
          agg_nm <- paste0("livestock_", sy, "_lsu_heads")
          agg_mat_list[[agg_nm]] <- safe_row_sums(vals[, idx, drop = FALSE])
          agg_names <- c(agg_names, agg_nm)
        }
      }
      
      # 3. Grand total Livestock
      agg_nm <- "Livestock"
      agg_mat_list[[agg_nm]] <- safe_row_sums(vals)
      agg_names <- c(agg_names, agg_nm)
    }
    
    # Combine original raster with aggregate layers
    if (length(agg_names) > 0) {
      agg_mat <- do.call(cbind, agg_mat_list)
      r_agg <- terra::rast(r[[1]], nlyrs = length(agg_names))
      terra::values(r_agg) <- agg_mat
      names(r_agg) <- agg_names
      
      if (!is_lsu) {
        # Drop legacy empty/residual Cropland_HI and Cropland_LI (no more confusing Total_HI)
        # Also drop any layers that are being re-aggregated (e.g. residual Pasture)
        keep_base <- setdiff(names(r), c("Cropland_HI", "Cropland_LI", agg_names))
        r_base <- r[[keep_base]]
        r_all <- c(r_base, r_agg)
        
        # Sort layers from highest aggregation to lowest
        nms_all <- names(r_all)
        p1_macro <- c("Cropland", "Pasture", "Forests", "Natural", "Wetlands", "Urban", "Waterbodies_inland", "Waterbodies_marine", "NODATA")
        g1 <- intersect(p1_macro, nms_all)
        g2 <- sort(grep("^Cropland_.*_TOTAL$", nms_all, value = TRUE))
        g3 <- setdiff(sort(grep("^Cropland_[A-Za-z]+$", nms_all, value = TRUE)), p1_macro)
        g4 <- c(sort(grep("^Forests_", nms_all, value = TRUE)),
                sort(grep("^Pasture_", nms_all, value = TRUE)),
                sort(grep("^Natural_", nms_all, value = TRUE)),
                sort(grep("^Wetlands_", nms_all, value = TRUE)))
        g5 <- sort(setdiff(nms_all, c(g1, g2, g3, g4)))
        sorted_nms <- c(g1, g2, g3, g4, g5)
        r_all <- r_all[[sorted_nms]]
      } else {
        r_all <- c(r, r_agg)
        nms_all <- names(r_all)
        g1 <- "Livestock"
        cohorts_order <- c("bovd", "bovf", "bovo", "sgtd", "sgtf", "sgto", "pig", "pou", "lay", "bro")
        g2 <- intersect(paste0("livestock_", cohorts_order, "_lsu_heads"), nms_all)
        sys_order <- c("lga", "lgh", "lgt", "mra", "mrh", "mrt", "urban", "other")
        g3 <- intersect(paste0("livestock_", sys_order, "_lsu_heads"), nms_all)
        g4 <- sort(setdiff(nms_all, c(g1, g2, g3)))
        sorted_nms <- c(g1, g2, g3, g4)
        r_all <- r_all[[sorted_nms]]
      }
      
      local_tmp <- file.path(tempdir(), basename(out_path))
      terra::writeRaster(r_all, local_tmp, filetype = "COG", gdal = c("COMPRESS=ZSTD", "INTERLEAVE=BAND"), overwrite = TRUE)
      file.copy(local_tmp, out_path, overwrite = TRUE)
      file.remove(local_tmp)
      cli_alert_success(sprintf("Saved COG hierarchical aggregation: %s (+%d agg layers, total %d, BAND interleaved)", 
                                out_name, length(agg_names), terra::nlyr(r_all)))
      rm(r_all, r_agg, agg_mat, agg_mat_list)
    } else {
      cli_alert_info("No components found to aggregate. Skipping.")
    }
    rm(r, vals)
    gc(verbose = FALSE)
  }
}
cli_alert_success("Hierarchical aggregation complete.")

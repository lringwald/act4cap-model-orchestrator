#!/usr/bin/env Rscript
# ==============================================================================
# validate_indicator_schema.R
# Strict, deterministic schema and shape validation for ALL indicator JSON files
# across the CASCADE downscaling indicators directory.
#
# Rules enforced:
#   1. Top-Level Shape:
#      - For Indicator Rasters: title, titiler_colormap_name, band_map, band_labels, navigation
#      - For Category Captions: category, title, subtitle, caption
#   2. Type & Value Integrity:
#      - band_map: 1-based sequential integers matching the length of band_labels exactly
#      - band_labels: non-empty strings with matching names to band_map
#   3. Navigation Matrix Integrity:
#      - Standard (dimensions + matrix):
#          * dimensions: list of objects with 'id' and 'label'
#          * matrix: recursive list matching the declared dimensions depth and IDs
#          * all matrix leaf strings must resolve to valid keys in band_map
#          * no empty list branches or empty leaf arrays
#      - Hybrid (primary_selector + categories):
#          * primary_selector: list of objects with 'id' and 'label'
#          * categories: non-empty object matching primary_selector IDs
#          * each category must follow either dimensions+matrix or validated selector items
#   4. Deterministic Reporting & Exit Codes:
#      - Exits with 0 if all validated files pass
#      - Exits with 1 if any file fails validation, printing offending files and errors
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

validate_file <- function(json_path) {
  errors <- character(0)
  warnings <- character(0)
  
  if (!file.exists(json_path)) {
    return(list(valid = FALSE, errors = sprintf("File does not exist: %s", json_path)))
  }
  
  obj <- tryCatch({
    txt <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }, error = function(e) {
    return(paste0("JSON syntax error: ", conditionMessage(e)))
  })
  
  if (is.character(obj)) {
    return(list(valid = FALSE, errors = obj))
  }
  
  # Check if category_caption.json
  if (grepl("category_caption\\.json$", basename(json_path))) {
    req_cap <- c("category", "title", "subtitle", "caption")
    missing_cap <- setdiff(req_cap, names(obj))
    if (length(missing_cap) > 0) {
      errors <- c(errors, sprintf("category_caption missing required keys: %s", paste(missing_cap, collapse = ", ")))
    }
    for (k in intersect(req_cap, names(obj))) {
      if (!is.character(obj[[k]]) || nchar(trimws(obj[[k]])) == 0) {
        errors <- c(errors, sprintf("category_caption key '%s' must be a non-empty string", k))
      }
    }
    return(list(valid = length(errors) == 0, errors = errors))
  }
  
  # Standard Companion JSON Validation
  # 1. Root Keys
  req_roots <- c("title", "titiler_colormap_name", "band_map", "band_labels", "navigation")
  missing_roots <- setdiff(req_roots, names(obj))
  if (length(missing_roots) > 0) {
    errors <- c(errors, sprintf("Missing root keys: %s", paste(missing_roots, collapse = ", ")))
  }
  
  bm <- obj$band_map
  bl <- obj$band_labels
  
  # 2. band_map & band_labels
  if (!is.list(bm) || length(bm) == 0) {
    errors <- c(errors, "band_map must be a non-empty object")
  }
  if (!is.list(bl) || length(bl) == 0) {
    errors <- c(errors, "band_labels must be a non-empty object")
  }
  
  if (is.list(bm) && is.list(bl) && length(bm) > 0 && length(bl) > 0) {
    if (!setequal(names(bm), names(bl))) {
      diff1 <- setdiff(names(bm), names(bl))
      diff2 <- setdiff(names(bl), names(bm))
      if (length(diff1) > 0) errors <- c(errors, sprintf("Layers in band_map but not band_labels: %s", paste(head(diff1, 5), collapse = ", ")))
      if (length(diff2) > 0) errors <- c(errors, sprintf("Layers in band_labels but not band_map: %s", paste(head(diff2, 5), collapse = ", ")))
    }
    
    indices <- unlist(bm)
    if (!is.numeric(indices) || !setequal(indices, seq_len(length(bm)))) {
      errors <- c(errors, "band_map values must be strictly 1-based sequential integers matching total bands")
    }
  }
  
  # 3. Navigation
  nav <- obj$navigation
  if (!is.list(nav) || length(nav) == 0) {
    errors <- c(errors, "navigation must be a non-empty object")
  } else {
    valid_bands <- if (is.list(bm)) names(bm) else character(0)
    
    validate_matrix_recursive <- function(dims, mat, prefix) {
      errs <- character(0)
      if (!is.list(dims) || length(dims) == 0) return(sprintf("%s missing or empty dimensions", prefix))
      if (!is.list(mat) || length(mat) == 0) return(sprintf("%s missing or empty matrix", prefix))
      
      dim_names <- names(dims)
      dim_allowed <- lapply(dims, function(dim_items) {
        if (!is.list(dim_items)) return(character(0))
        sapply(dim_items, function(x) if (is.list(x) && !is.null(x$id)) x$id else NA_character_)
      })
      
      traverse <- function(cur_mat, depth) {
        if (!is.list(cur_mat)) return(character(0))
        if (depth > length(dim_names)) {
          return(sprintf("%s matrix nesting depth (%d) exceeds declared dimensions count (%d)", prefix, depth, length(dim_names)))
        }
        
        cur_dim <- dim_names[depth]
        allowed <- dim_allowed[[cur_dim]]
        cur_keys <- names(cur_mat)
        
        if (is.null(cur_keys) || length(cur_keys) == 0) {
          return(sprintf("%s at dimension '%s' contains empty keys", prefix, cur_dim))
        }
        
        diff_k <- setdiff(cur_keys, allowed)
        t_errs <- character(0)
        if (length(diff_k) > 0) {
          t_errs <- c(t_errs, sprintf("%s dimension '%s' missing selector options for matrix keys: %s", prefix, cur_dim, paste(diff_k, collapse = ", ")))
        }
        
        for (k in cur_keys) {
          sub_val <- cur_mat[[k]]
          if (is.list(sub_val)) {
            if (length(sub_val) == 0) {
              t_errs <- c(t_errs, sprintf("%s key '%s' in dimension '%s' contains an empty list", prefix, k, cur_dim))
            } else {
              t_errs <- c(t_errs, traverse(sub_val, depth + 1))
            }
          } else if (is.character(sub_val)) {
            # In multi-band rasters, leaf resolves to band_map key.
            # In single-band catalog navigation (Format B), leaf can also resolve to a sibling file stem or band name.
            is_valid_leaf <- (length(valid_bands) == 0) || 
                             (sub_val %in% valid_bands) || 
                             (length(valid_bands) == 1 && (sub_val == valid_bands[1] || sub_val == sub("\\.json$", "", basename(json_path))))
            
            # Check if leaf exists as a sibling file in the same directory if length(valid_bands) == 1
            if (!is_valid_leaf && length(valid_bands) == 1) {
              sibling_tif <- file.path(dirname(json_path), paste0(sub_val, ".tif"))
              sibling_json <- file.path(dirname(json_path), paste0(sub_val, ".json"))
              if (file.exists(sibling_tif) || file.exists(sibling_json)) {
                is_valid_leaf <- TRUE
              }
            }
            
            if (!is_valid_leaf) {
              t_errs <- c(t_errs, sprintf("%s matrix leaf '%s' is not found in band_map or directory file catalog", prefix, sub_val))
            }
          } else {
            t_errs <- c(t_errs, sprintf("%s key '%s' has unexpected value type '%s' (expected string band name)", prefix, k, typeof(sub_val)))
          }
        }
        t_errs
      }
      
      traverse(mat, 1)
    }
    
    if (!is.null(nav$primary_selector)) {
      # Hybrid Primary Selector + Categories
      pri <- nav$primary_selector
      cats <- nav$categories
      
      if (!is.list(pri) || length(pri) == 0) {
        errors <- c(errors, "primary_selector must be a non-empty list of categories")
      }
      if (!is.list(cats) || length(cats) == 0) {
        errors <- c(errors, "categories must be a non-empty object")
      }
      
      if (is.list(pri) && is.list(cats)) {
        pri_ids <- sapply(pri, function(x) if (is.list(x) && !is.null(x$id)) x$id else NA_character_)
        cat_keys <- names(cats)
        
        if (!setequal(pri_ids, cat_keys)) {
          errors <- c(errors, sprintf("Mismatch between primary_selector IDs (%s) and categories keys (%s)",
                                      paste(pri_ids, collapse = ", "), paste(cat_keys, collapse = ", ")))
        }
        
        for (ck in cat_keys) {
          cat_val <- cats[[ck]]
          if (!is.list(cat_val)) {
            errors <- c(errors, sprintf("Category '%s' must be an object", ck))
          } else if (!is.null(cat_val$dimensions) && !is.null(cat_val$matrix)) {
            errors <- c(errors, validate_matrix_recursive(cat_val$dimensions, cat_val$matrix, prefix = sprintf("category['%s']", ck)))
          } else if (!is.null(cat_val$animal)) {
            # Legacy simple list fallback
            if (length(cat_val$animal) == 0) {
              errors <- c(errors, sprintf("Category '%s' animal list is empty", ck))
            }
          } else {
            errors <- c(errors, sprintf("Category '%s' must declare both 'dimensions' and 'matrix'", ck))
          }
        }
      }
    } else {
      # Standard Dimensions + Matrix
      if (is.null(nav$dimensions) || is.null(nav$matrix)) {
        errors <- c(errors, "navigation must declare either 'primary_selector + categories' or 'dimensions + matrix'")
      } else {
        errors <- c(errors, validate_matrix_recursive(nav$dimensions, nav$matrix, prefix = "navigation"))
      }
    }
  }
  
  list(valid = length(errors) == 0, errors = errors, warnings = warnings)
}

# --- CLI Execution Support ---
if (sys.nframe() == 0L) {
  if (file.exists("scripts/aux_codes.R")) suppressMessages(tryCatch(source("scripts/aux_codes.R"), error = function(e) NULL))
  args <- commandArgs(trailingOnly = TRUE)
  target_path <- if (length(args) >= 1) args[1] else {
    d <- if (exists("get_cascade_indicators_dir", mode = "function")) get_cascade_indicators_dir() else {
      cand <- c("C:/cascade_indicators", "D:/cascade_indicators", "G:/cascade_indicators", "P:/cascade_indicators")
      cand[dir.exists(cand)][1]
    }
    if (!is.null(d) && !is.na(d)) d else "Q:/cascade_indicators"
  }

if (file.exists(target_path) && !dir.exists(target_path)) {
  # Single file validation
  res <- validate_file(target_path)
  if (res$valid) {
    cat(sprintf("[PASS] %s is strictly schema-compliant.\n", basename(target_path)))
    quit(status = 0)
  } else {
    cat(sprintf("[FAIL] %s failed schema validation:\n", basename(target_path)))
    for (err in res$errors) cat(sprintf("  - %s\n", err))
    quit(status = 1)
  }
} else if (dir.exists(target_path)) {
  # Batch directory validation (target category subfolders directly for network performance)
  cat_names <- c("land_use", "livestock", "production", "emissions", "biodiversity")
  has_cats <- any(dir.exists(file.path(target_path, cat_names)))
  if (has_cats) {
    files <- c(
      list.files(target_path, pattern = "\\.json$", full.names = TRUE),
      unlist(lapply(file.path(target_path, cat_names), function(d) {
        if (dir.exists(d)) list.files(d, pattern = "\\.json$", full.names = TRUE) else character(0)
      }))
    )
  } else {
    files <- list.files(target_path, pattern = "\\.json$", full.names = TRUE, recursive = TRUE)
  }
  cat(sprintf("=== Deterministic JSON Validation across: %s (%d files) ===\n", target_path, length(files)))
  
  pass_count <- 0
  fail_count <- 0
  failures <- list()
  
  for (f in files) {
    v <- validate_file(f)
    if (v$valid) {
      pass_count <- pass_count + 1
    } else {
      fail_count <- fail_count + 1
      failures[[f]] <- v$errors
    }
  }
  
  cat(sprintf("\nResult Summary: Total: %d | PASS: %d | FAIL: %d\n", length(files), pass_count, fail_count))
  
  if (fail_count > 0) {
    cat("\nDetailed Failure Report:\n")
    for (fn in names(failures)) {
      cat(sprintf("\n[FAIL] %s\n", fn))
      for (err in failures[[fn]]) cat(sprintf("  - %s\n", err))
    }
    quit(status = 1)
  } else {
    cat("All files strictly pass schema validation!\n")
    quit(status = 0)
  }
} else {
  cat(sprintf("Error: Target path '%s' not found.\n", target_path))
  quit(status = 1)
}
}


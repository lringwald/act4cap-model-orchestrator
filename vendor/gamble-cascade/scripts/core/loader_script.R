# =========================================================================
# Helper Script: Dynamically Load Latest MNL Model Outputs
# =========================================================================
# Selection rules (in order):
#   1. MODULE: prior-model outputs are tagged with a module token in the
#      filename (GLOBIOM, BIOCLIMA, CAPRI, AGMIP, ...). Files tagged with a
#      DIFFERENT known module are never eligible for a run. Files carrying
#      the requested module's token are preferred; untagged files are only
#      eligible when no tagged file exists (legacy naming).
#      Adding a new macro model = add its token to KNOWN_PRIOR_MODULES.
#   2. INTEGRITY: candidates are tried newest-first and unreadable files
#      (e.g. truncated by a full disk mid-write) are skipped with a warning
#      instead of killing the pipeline.
#   3. PAIRING: the beta file is not chosen independently by date — it is
#      scored by spec-token overlap with the selected xmat (resolution,
#      estimator, symmetry, intersection tags), then by recency, so xmat and
#      betas come from the same model specification whenever possible.

KNOWN_PRIOR_MODULES <- c("GLOBIOM", "BIOCLIMA", "CAPRI", "AGMIP")

#' Keep only files compatible with the requested module token
.filter_by_module <- function(files, module, known = KNOWN_PRIOR_MODULES) {
  if (is.null(module) || module == "" || length(files) == 0) return(files)
  known <- unique(c(known, module))
  other <- setdiff(known, module)
  if (length(other) > 0) {
    files <- files[!grepl(paste(other, collapse = "|"), basename(files), ignore.case = TRUE)]
  }
  tagged <- files[grepl(module, basename(files), ignore.case = TRUE)]
  if (length(tagged) > 0) tagged else files
}

#' Read an rds/csv/qs defensively; NULL on failure instead of an error
.safe_read <- function(path) {
  tryCatch({
    if (grepl("\\.qs$", path, ignore.case = TRUE)) {
      qs2::qs_read(path)
    } else if (grepl("\\.rds$", path, ignore.case = TRUE)) {
      suppressWarnings(readRDS(path))
    } else {
      read.csv(path, stringsAsFactors = FALSE)
    }
  }, error = function(e) NULL)
}

#' Try candidate paths in the given order until one reads cleanly
.read_first_valid <- function(paths, label) {
  for (p in paths) {
    obj <- .safe_read(p)
    if (!is.null(obj)) return(list(obj = obj, path = p))
    message(sprintf(">>> WARNING: %s file unreadable (corrupt/truncated?) — skipping: %s",
                    label, basename(p)))
  }
  list(obj = NULL, path = NULL)
}

#' Spec tokens of a filename: everything that identifies the model spec,
#' excluding generic words, module tags, and dates
.spec_tokens <- function(fname, known = KNOWN_PRIOR_MODULES) {
  stem <- gsub("\\.(rds|csv)$", "", basename(fname), ignore.case = TRUE)
  toks <- strsplit(stem, "_")[[1]]
  drop <- c("Xmat", "FULL", "mnl", "pixel", "beta", "rotated", "point", "pooled",
            "coefficients", "production", "test", "run", "agri", known)
  toks <- toks[!tolower(toks) %in% tolower(drop)]
  toks[!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", toks)]
}

#' Order files newest-first by modification time
.by_mtime <- function(files) {
  if (length(files) == 0) return(files)
  files[order(file.info(files, extra_cols = FALSE)$mtime, decreasing = TRUE)]
}

#' Load the latest covariate matrix and posterior beta means for a resolution
#' @param downscale_reso Resolution string (e.g., "10km_by_NUTS3", "5km")
#' @param base_path Base path to the high_res_prior_models directory
#' @param module Macro model token to select outputs for (e.g. "GLOBIOM")
#' @param model_type "lum" or "agri" beta family
#' @return list(xmat_pixel, beta_point, xmat_path, beta_path)
load_latest_model_outputs <- function(downscale_reso = "20km", base_path = NULL,
                                      module = "GLOBIOM", model_type = "lum") {

  if (is.null(base_path)) {
    user_home <- Sys.getenv("HOME")
    base_path <- file.path(user_home, "Library/CloudStorage/OneDrive-IIASA/R/LAMASUS_high_res_prior_models")
  }
  output_dir <- file.path(base_path, "output")
  if (!dir.exists(output_dir)) stop("Output directory does not exist: ", output_dir)

  native_reso    <- strsplit(downscale_reso, "_by_")[[1]][1]
  spatial_suffix <- if (grepl("_by_", downscale_reso)) strsplit(downscale_reso, "_by_")[[1]][2] else ""

  # ---- 1. Xmat candidates: intersected first, native as fallback pool ----
  xmat_pattern <- paste0("Xmat_FULL_", native_reso, ".*",
                         if (spatial_suffix != "") paste0("by_", spatial_suffix, ".*") else "",
                         "_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.rds$")
  xmat_files <- .filter_by_module(
    list.files(output_dir, pattern = xmat_pattern, full.names = TRUE, recursive = TRUE),
    module)

  xmat_files_native <- character(0)
  if (grepl("_by_", downscale_reso)) {
    xmat_pattern_native <- paste0("Xmat_FULL_", native_reso, "_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.rds$")
    xmat_files_native <- .filter_by_module(
      list.files(output_dir, pattern = xmat_pattern_native, full.names = TRUE, recursive = TRUE),
      module)
  }

  # Newest-first within each pool; intersected pool strictly preferred
  xmat_candidates <- c(.by_mtime(xmat_files), .by_mtime(xmat_files_native))
  if (length(xmat_candidates) == 0)
    stop(sprintf("Could not find any xmat_pixel file for %s / module %s in %s",
                 downscale_reso, module, output_dir))

  xmat_res <- .read_first_valid(xmat_candidates, "Xmat")
  if (is.null(xmat_res$obj))
    stop(sprintf("All %d candidate xmat files for %s / module %s failed to read:\n%s",
                 length(xmat_candidates), downscale_reso, module,
                 paste(basename(xmat_candidates), collapse = "\n")))
  xmat_path  <- xmat_res$path
  xmat_pixel <- xmat_res$obj
  if (length(xmat_files) == 0 && grepl("_by_", downscale_reso))
    cat(sprintf(">>> Intersected Xmat not found. Falling back to native: %s\n", basename(xmat_path)))
  cat(sprintf("Loading latest covariates from: %s\n", basename(xmat_path)))

  # ---- 2. Beta candidates: module-filtered, then spec-paired to the xmat ----
  beta_pattern <- ".*(beta_point|pooled_coefficients|mnl_pixel_beta).*\\.(csv|rds)$"
  beta_files <- list.files(output_dir, pattern = beta_pattern, full.names = TRUE, recursive = TRUE)

  if (model_type == "lum") {
    beta_files <- beta_files[!grepl("agri_mnl_", beta_files, ignore.case = TRUE)]
  } else if (model_type == "agri") {
    beta_files <- beta_files[grepl("agri_mnl_", beta_files, ignore.case = TRUE)]
  }
  beta_files <- .filter_by_module(beta_files, module)
  if (length(beta_files) == 0)
    stop(sprintf("Could not find beta means file for %s / module %s in %s",
                 native_reso, module, output_dir))

  # Score candidates by spec-token overlap with the chosen xmat, then recency.
  xtoks  <- .spec_tokens(xmat_path)
  scores <- vapply(beta_files, function(f) length(intersect(.spec_tokens(f), xtoks)), integer(1))
  mtimes <- file.info(beta_files, extra_cols = FALSE)$mtime
  beta_candidates <- beta_files[order(-scores, as.numeric(mtimes) * -1)]

  beta_res <- .read_first_valid(beta_candidates, "Beta")
  if (is.null(beta_res$obj))
    stop(sprintf("All %d candidate beta files for module %s failed to read:\n%s",
                 length(beta_candidates), module,
                 paste(basename(beta_candidates), collapse = "\n")))
  beta_path  <- beta_res$path
  beta_point <- beta_res$obj

  best_score <- length(intersect(.spec_tokens(beta_path), xtoks))
  cat(sprintf("Loading latest beta means from: %s (spec match: %d/%d tokens)\n",
              basename(beta_path), best_score, length(xtoks)))

  if (exists("register_source_file")) {
    register_source_file("Priors", sprintf("Covariate Matrix (%s)", model_type), xmat_path)
    register_source_file("Priors", sprintf("Transition Betas (%s)", model_type), beta_path)
  }

  return(list(
    xmat_pixel = xmat_pixel,
    beta_point = beta_point,
    xmat_path  = xmat_path,
    beta_path  = beta_path
  ))
}

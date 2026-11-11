#!/usr/bin/env Rscript
# =========================================================================
# fetch_priors.R
# Fetches the latest gamble-core prior files for a specific macro model
# and copies them to the local data/03_final/priors/ directory.
# =========================================================================

suppressPackageStartupMessages({
  library(optparse)
})

option_list <- list(
  make_option(c("-m", "--model"), type="character", default="GLOBIOM", 
              help="Macro model name (e.g. GLOBIOM, CAPRI, AGMIP)"),
  make_option(c("-r", "--reso"), type="character", default="10km_by_NUTS3", 
              help="Downscale resolution (e.g. 10km_by_NUTS3, 5km)"),
  make_option(c("-o", "--outdir"), type="character", default="data/03_final/priors", 
              help="Output directory to copy the files to"),
  make_option(c("-b", "--basepath"), type="character", default="../LAMASUS_high_res_prior_models", 
              help="Base path to LAMASUS_high_res_prior_models")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Source the loader script
source("scripts/loader_script.R")

if (!dir.exists(opt$outdir)) {
  dir.create(opt$outdir, recursive = TRUE)
}

# Fetch Land Use (LUM) Priors
cat(sprintf("Fetching LUM priors for %s (%s)...\n", opt$model, opt$reso))
lum_res <- load_latest_model_outputs(downscale_reso = opt$reso, 
                                     base_path = opt$basepath, 
                                     module = opt$model, 
                                     model_type = "lum")

# Fetch Agriculture (AGRI) Priors
cat(sprintf("Fetching AGRI priors for %s (%s)...\n", opt$model, opt$reso))
agri_res <- tryCatch({
  load_latest_model_outputs(downscale_reso = opt$reso, 
                            base_path = opt$basepath, 
                            module = opt$model, 
                            model_type = "agri")
}, error = function(e) {
  cat(sprintf("No AGRI priors found for %s (this is normal for some models).\n", opt$model))
  return(NULL)
})

copy_safe <- function(src, dst_name) {
  if (is.null(src)) return(NULL)
  dst <- file.path(opt$outdir, dst_name)
  cat(sprintf("Copying %s -> %s\n", basename(src), dst))
  file.copy(src, dst, overwrite = TRUE)
}

# Preserve extensions
ext <- function(path) {
    if (grepl("\\.csv$", path, ignore.case=TRUE)) return(".csv")
    if (grepl("\\.rds$", path, ignore.case=TRUE)) return(".rds")
    if (grepl("\\.qs$", path, ignore.case=TRUE)) return(".qs")
    return(".rds")
}

copy_safe(lum_res$xmat_path, paste0(opt$model, "_", opt$reso, "_lum_xmat", ext(lum_res$xmat_path)))
copy_safe(lum_res$beta_path, paste0(opt$model, "_", opt$reso, "_lum_beta", ext(lum_res$beta_path)))

if (!is.null(agri_res)) {
  copy_safe(agri_res$xmat_path, paste0(opt$model, "_", opt$reso, "_agri_xmat", ext(agri_res$xmat_path)))
  copy_safe(agri_res$beta_path, paste0(opt$model, "_", opt$reso, "_agri_beta", ext(agri_res$beta_path)))
}

# Create a done flag for Snakemake
file.create(file.path(opt$outdir, paste0(opt$model, "_", opt$reso, ".done")))
cat("Done.\n")

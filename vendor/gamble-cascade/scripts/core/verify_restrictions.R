commandArgs <- function(trailingOnly = FALSE) {
  if(trailingOnly) {
    return(c("-m", "BMLEH_Los1_CAPRI"))
  } else {
    return(base::commandArgs(trailingOnly))
  }
}

RUN_COMPILATION <- FALSE
RUN_HARMONIZATION <- FALSE
RUN_TRANSITIONS <- FALSE
RUN_PRIORS <- FALSE
RUN_DS_LU <- FALSE
RUN_DS_LSU <- FALSE
RUN_CHECKS <- FALSE
RUN_DS_AGGREGATION <- FALSE

source("scripts/run_downscaling.R")
source("scripts/process_lu_downscaling.R")

cli_h1("Preparing Data for LUC Optimization")
inputs <- prepare_luc_downscaling_inputs(
  cfg = current_model_cfg,
  mappings = if (exists("mappings", envir = .GlobalEnv)) get("mappings", envir = .GlobalEnv) else NULL,
  mapping_grid_path = mapping_grid_path,
  countries_to_run = NULL,
  country_filter = country_filter,
  downscale_resolution = downscale_resolution,
  files = files
)

cat("\n\n=== RESTRICTIONS ===\n")
if (is.null(inputs$restrictions)) {
  cat("restrictions is NULL\n")
} else {
  cat(sprintf("restrictions is a data.table with %d rows and %d cols\n", nrow(inputs$restrictions), ncol(inputs$restrictions)))
  print(head(inputs$restrictions))
}

cat("\n\n=== PRIORS ===\n")
if (is.null(inputs$priors)) {
  cat("priors is NULL\n")
} else {
  cat(sprintf("priors is a data.table with %d rows and %d cols\n", nrow(inputs$priors), ncol(inputs$priors)))
  zero_priors <- inputs$priors[value == 0]
  cat(sprintf("Number of priors with value == 0: %d\n", nrow(zero_priors)))
  if (nrow(zero_priors) > 0) print(head(zero_priors))
}

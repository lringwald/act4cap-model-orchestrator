# ===============================================================
# SCRIPT: run_downscaling.R (Root CLI Router)
# ===============================================================
library(optparse)

option_list <- list(
  make_option(c("-m", "--model"), type="character", default="LAMASUS_GLOBIOM",
              help="Target module name (e.g., LAMASUS_GLOBIOM, BIOCLIMA_CAPRI)"),
  make_option(c("-s", "--scenario"), type="character", default="ALL",
              help="Scenario to downscale (or ALL)"),
  make_option(c("-d", "--date"), type="character", default=NULL,
              help="Override output date")
)
opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

if (!exists("model_name") || is.null(model_name)) {
  model_name <- opt$model
}
scen_filter <- if (opt$scenario == "ALL") NULL else opt$scenario
if (!is.null(opt$date)) {
  OVERRIDE_DATE <- opt$date
}

# Supported models and their orchestrator paths
supported_models <- c("LAMASUS_GLOBIOM", "BIOCLIMA3_CAPRI", "BIOCLIMA_CAPRI", "BMLEH_Los1_CAPRI", "AGMIP")
if (model_name == "BIOCLIMA_CAPRI") {
  model_name <- "BIOCLIMA3_CAPRI"
}

if (!(model_name %in% supported_models)) {
  stop(sprintf("Model '%s' is not supported. Available models: %s", model_name, paste(supported_models, collapse=", ")))
}

# Route execution to the thin orchestrator
orchestrator_path <- file.path("scripts", model_name, "run_downscaling.R")

if (!file.exists(orchestrator_path)) {
  stop(sprintf("Thin orchestrator not found for model '%s' at path: %s", model_name, orchestrator_path))
}

cat(sprintf("Routing execution to %s...\n", orchestrator_path))
source(orchestrator_path)

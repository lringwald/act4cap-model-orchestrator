# ===============================================================
# SCRIPT: run_downscaling.R (Thin Orchestrator)
# MODEL: AGMIP
# ===============================================================

# Enable the core downscaling phases — default RUN_PRIORS to FALSE for prior-free downscaling
if (!exists("RUN_COMPILATION"))    RUN_COMPILATION    <- TRUE
if (!exists("RUN_HARMONIZATION"))  RUN_HARMONIZATION  <- TRUE
if (!exists("RUN_TRANSITIONS"))    RUN_TRANSITIONS    <- TRUE
if (!exists("RUN_PRIORS"))         RUN_PRIORS         <- FALSE  # Prior module skipped: downscale without priors
if (!exists("RUN_DS_LU"))          RUN_DS_LU          <- TRUE   # Run Land Use & Crop Area downscaling
if (!exists("RUN_DS_LSU"))         RUN_DS_LSU         <- FALSE  # Skip LSU downscaling
if (!exists("RUN_DS_ECONOMICS"))   RUN_DS_ECONOMICS   <- TRUE   # Run Production & Inputs downscaling
if (!exists("RUN_CHECKS"))         RUN_CHECKS         <- TRUE   # Post-processing & diagnostics
if (!exists("RUN_DS_AGGREGATION")) RUN_DS_AGGREGATION <- FALSE  # AgMIP generates TIFFs via run_all_models_crop_pipeline.R
if (!exists("scen_filter"))         scen_filter         <- NULL

model_name <- "AGMIP"
model_dir_name <- model_name

# Support CLI argument: Rscript run_downscaling.R <model_name>
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0 && nzchar(args[1])) {
  OVERRIDE_AGMIP_MODEL <- tolower(args[1])
  Sys.setenv(AGMIP_MODEL = OVERRIDE_AGMIP_MODEL)
}
if (length(args) > 1 && nzchar(args[2])) Sys.setenv(SCENARIO = tolower(args[2]))
if (length(args) > 2 && nzchar(args[3])) Sys.setenv(AGMIP_INPUT_FILE = args[3])
if (length(args) > 3 && nzchar(args[4])) {
  Sys.setenv(AGMIP_OUTPUT_DIR = args[4])
  if (!nzchar(Sys.getenv("AGMIP_TEMP_DIR", ""))) {
    Sys.setenv(AGMIP_TEMP_DIR = file.path(args[4], "intermediate"))
  }
}
if (length(args) > 4 && nzchar(args[5])) Sys.setenv(AGMIP_TEMP_DIR = args[5])
if (length(args) > 5 && nzchar(args[6])) {
  OVERRIDE_DATE <- args[6]
  Sys.setenv(RUN_DATE = args[6])
} else if (nzchar(Sys.getenv("RUN_DATE", "")) && grepl("^\\d{8}$", Sys.getenv("RUN_DATE"))) {
  OVERRIDE_DATE <- Sys.getenv("RUN_DATE")
}

# Load model-specific configuration
cfg_path <- c(file.path("pipelines", model_dir_name, "config.R"), file.path("scripts", model_dir_name, "config.R"))
source(cfg_path[file.exists(cfg_path)][1])
if (identical(Sys.getenv("FORCE", ""), "1")) {
  current_model_cfg$run_date_policy <- "new"
}

# Delegate to the core orchestrator
orch_path <- c("scripts/core/core_orchestrator.R", "scripts/core_orchestrator.R")
source(orch_path[file.exists(orch_path)][1])


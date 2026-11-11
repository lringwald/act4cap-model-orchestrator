# ===============================================================
# SCRIPT: run_agmip_models.R
# Thin CLI Orchestrator to run AgMIP downscaling for any or all
# macro models (globiom, capri, image, magnet, clumondo)
# ===============================================================

args <- commandArgs(trailingOnly = TRUE)
target_model <- if (length(args) > 0) tolower(args[1]) else "all"

available_models <- c("globiom", "capri", "image", "magnet", "clumondo")

if (target_model == "all") {
  models_to_run <- available_models
} else if (target_model %in% available_models) {
  models_to_run <- target_model
} else {
  stop(sprintf("Invalid model '%s'. Choose one of: %s, or 'all'.", target_model, paste(available_models, collapse = ", ")))
}

cli::cli_h1(sprintf("AgMIP Multi-Model Downscaling: %s", paste(toupper(models_to_run), collapse = ", ")))

rscript_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

run_stats <- list()

for (m in models_to_run) {
  cli::cli_h2(sprintf("Executing Downscaling for Macro Model: %s", toupper(m)))
  t0 <- proc.time()
  
  # Launch each model in a dedicated R process to guarantee clean memory and zero cross-model state pollution
  exit_code <- system2(
    rscript_bin,
    args = c("scripts/AGMIP/run_downscaling.R", m),
    stdout = "",
    stderr = ""
  )
  
  elapsed <- round((proc.time() - t0)[3], 1)
  
  if (exit_code == 0) {
    cli::cli_alert_success(sprintf("Completed downscaling for %s in %.1f s", toupper(m), elapsed))
    run_stats[[m]] <- sprintf("SUCCESS (%.1f s)", elapsed)
  } else {
    cli::cli_alert_danger(sprintf("Downscaling failed for %s with exit code %d (%.1f s)", toupper(m), exit_code, elapsed))
    run_stats[[m]] <- sprintf("FAILED [exit %d] (%.1f s)", exit_code, elapsed)
  }
}

cli::cli_h1("AgMIP Multi-Model Downscaling Execution Summary")
for (m in names(run_stats)) {
  status <- run_stats[[m]]
  if (grepl("SUCCESS", status)) {
    cli::cli_alert_success(sprintf("%s: %s", toupper(m), status))
  } else {
    cli::cli_alert_danger(sprintf("%s: %s", toupper(m), status))
  }
}

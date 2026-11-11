# ===============================================================
# aggregate_tiff_hierarchical_indicators.R
# ===============================================================
require(cli)

cli_alert_info("Starting hierarchical aggregation of Indicator TIFF files...")

if (!exists("paths") || is.null(paths$output_dir)) {
  if (exists("output_dir") && !is.null(output_dir)) {
    paths <- list(output_dir = output_dir)
  } else {
    stop("paths$output_dir is not defined.")
  }
}
if (!exists("output_prefix")) {
  output_prefix <- basename(paths$output_dir)
}

indicator_files <- list.files(paths$output_dir, pattern = paste0("^", output_prefix, "_indicators_.*\\.tif$"), full.names = TRUE)
if (exists("TARGET_SCENARIOS") && !is.null(TARGET_SCENARIOS)) {
  indicator_files <- indicator_files[sapply(indicator_files, function(f) {
    any(sapply(TARGET_SCENARIOS, function(sc) grepl(paste0("_", sc, "_"), basename(f))))
  })]
}

worker_name <- "aggregate_one_hierarchical_indicator.R"
worker_cands <- c(file.path("scripts", "core", worker_name), file.path("scripts", worker_name))
worker_script <- worker_cands[file.exists(worker_cands)][1]

if (is.na(worker_script)) {
  stop(sprintf("Could not find worker script '%s'. Please ensure it exists.", worker_name))
}

reporting_unit <- if (exists("REPORTING_INTENSITY_UNIT")) REPORTING_INTENSITY_UNIT else "km2"
rscript_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

if (length(indicator_files) == 0) {
  cli_alert_warning("No indicator raster files found for hierarchical aggregation.")
} else {
  for (tif_path in indicator_files) {
    if (grepl("_hierarchical", basename(tif_path))) next

    out_name <- gsub("_indicators_", "_indicators_hierarchical_", basename(tif_path))
    out_path <- file.path(paths$output_dir, out_name)
    if (file.exists(out_path)) {
      cli_alert_info(sprintf("Skipping existing hierarchical aggregation: %s", out_name))
      next
    }

    cli_alert_info(sprintf("Dispatching worker for: %s", basename(tif_path)))

    # FIX 2 & 3: Let system2 handle quoting, and capture the error output (stderr = TRUE)
    res <- system2(
      rscript_bin,
      args = c(shQuote(worker_script), shQuote(tif_path), shQuote(paths$output_dir), shQuote(reporting_unit)),
      stdout = "",
      stderr = TRUE # Capture actual error text
    )

    # Check if system2 returned an error attribute
    exit_status <- if (!is.null(attr(res, "status"))) attr(res, "status") else 0

    if (exit_status != 0) {
      cli_alert_warning(sprintf("Worker failed (exit %d) for %s.", exit_status, basename(tif_path)))
      # Print the actual Rscript crash reason
      cli_alert_danger(paste("Rscript Error:", paste(res, collapse = "\n")))
    }
  }
}
cli_alert_success("Indicator hierarchical aggregation complete.")

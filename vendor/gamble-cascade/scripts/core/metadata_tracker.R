# =========================================================================
# Metadata Tracker for Downscaling Pipeline
# =========================================================================
# This script manages a globally accessible registry of exogenous files 
# used throughout the pipeline to be reported in the performance diagnostic.

if (!exists("pipeline_metadata")) {
  pipeline_metadata <<- data.table::data.table(
    Category = character(),
    Description = character(),
    File_Path = character(),
    Creation_Date = character()
  )
}

#' Register a source file into the metadata tracker
#' @param category Category of the file (e.g. "Mapping", "GDX", "Priors")
#' @param description Short description of the file's purpose
#' @param filepath The absolute or relative path to the file
register_source_file <- function(category, description, filepath) {
  if (is.null(filepath) || is.na(filepath) || !file.exists(filepath)) {
    return(invisible(NULL))
  }
  
  # Normalize path
  filepath <- normalizePath(filepath, winslash = "/", mustWork = FALSE)
  
  # Avoid duplicates
  if (filepath %in% pipeline_metadata$File_Path) {
    return(invisible(NULL))
  }
  
  info <- file.info(filepath, extra_cols = FALSE)
  creation_date <- if (!is.na(info$mtime)) as.character(info$mtime) else "Unknown"
  
  # Append to global registry
  pipeline_metadata <<- rbind(
    pipeline_metadata,
    data.table::data.table(
      Category = category,
      Description = description,
      File_Path = filepath,
      Creation_Date = creation_date
    )
  )
  
  return(invisible(NULL))
}

# =============================================================================
# R/version_labels.R — Standardized Output Versioning & File Naming Engine
# =============================================================================
# Generates uniform, reproducible version labels across all Act4Cap outputs:
#   <model>_<scenario>_<macro_output_name>_<resolution>_<date>[_<custom_tag>][_TRIAL]
#
# Encodes:
#   1. Macro Economic Model (e.g. globiom, capri, image, magnet, clumondo)
#   2. Scenario (e.g. base, ndc, ssp2)
#   3. Macro Model Output Name / Vintage (e.g. 20260916_compr_scens_merge)
#   4. Downscaling Resolution (e.g. 10km, 5km, 2km)
#   5. Date Stamp (YYYYMMDD, e.g. 20261103)
#   6. Optional Custom Version Tag (e.g. v1, deliv3)
#   7. Trial Mode Tag (_TRIAL when trial = TRUE / TRIAL = 1)
# =============================================================================

suppressPackageStartupMessages({
  library(tools)
})

clean_token <- function(x) {
  if (is.null(x) || !nzchar(x)) return("")
  # Strip extensions
  s <- tools::file_path_sans_ext(basename(x))
  # Replace spaces and punctuation with underscores
  s <- gsub("[^A-Za-z0-9_.-]", "_", s)
  # Collapse multiple underscores
  s <- gsub("_+", "_", s)
  # Trim leading/trailing underscores
  s <- gsub("^_|_$", "", s)
  s
}

extract_macro_output_stem <- function(macro_output = NULL, downscale_csv = NULL, model = "") {
  # 1. Check macro_output string / file name
  if (!is.null(macro_output) && nzchar(macro_output)) {
    tok <- clean_token(macro_output)
    if (nzchar(tok)) return(tok)
  }
  
  # 2. Check downscale_csv file name if available
  if (!is.null(downscale_csv) && nzchar(downscale_csv)) {
    base <- basename(downscale_csv)
    # If standard DS_ACT4CAP_AgMIP_<MODEL>_<RES>_<DATE> format, extract informative core
    m <- regexec("DS_.*AgMIP_([A-Za-z0-9]+)_([0-9]+km)_([0-9]{8}|TRIAL)", base, ignore.case = TRUE)
    matches <- regmatches(base, m)[[1]]
    if (length(matches) >= 4) {
      # Return e.g. AgMIP_GLOBIOM_20261001
      return(sprintf("AgMIP_%s_%s", toupper(matches[2]), matches[4]))
    }
    tok <- clean_token(base)
    if (nzchar(tok)) return(tok)
  }
  
  # 3. Fallback to model identifier
  if (nzchar(model)) return(tolower(model))
  "macro"
}

construct_version_tag <- function(model,
                                  scenario,
                                  macro_output = NULL,
                                  downscale_csv = NULL,
                                  resolution = "10km",
                                  date = NULL,
                                  trial = FALSE,
                                  custom_tag = NULL,
                                  smoke = trial) {
  is_trial <- isTRUE(trial) || identical(trial, "1") || identical(trial, 1L) ||
              isTRUE(smoke) || identical(smoke, "1") || identical(smoke, 1L)
  
  m_str  <- tolower(clean_token(model))
  sc_str <- tolower(clean_token(scenario))
  
  # Macro model output name
  macro_stem <- extract_macro_output_stem(macro_output, downscale_csv, model)
  
  # Resolution (ensure 'km' suffix)
  res_clean <- tolower(clean_token(resolution))
  if (!grepl("km$", res_clean)) res_clean <- paste0(res_clean, "km")
  
  # Date (YYYYMMDD)
  date_str <- if (!is.null(date) && nzchar(date) && date != "auto") {
    clean_token(date)
  } else {
    format(Sys.Date(), "%Y%m%d")
  }
  
  # Custom tag (optional)
  ctag_str <- if (!is.null(custom_tag) && nzchar(custom_tag)) clean_token(custom_tag) else NULL
  
  # Trial mode tag
  trial_str <- if (is_trial) "TRIAL" else NULL
  
  # Assemble non-empty components
  # Format: <model>_<scenario>_<macro_output_stem>_<res>_<date>[_<custom_tag>][_TRIAL]
  parts <- c(m_str, sc_str, macro_stem, res_clean, date_str, ctag_str, trial_str)
  parts <- parts[nzchar(parts)]
  
  # De-duplicate adjacent identical tokens if macro_stem already contained model/scenario
  # E.g. globiom_base_globiom_base_... -> globiom_base_...
  tag <- paste(parts, collapse = "_")
  tag <- gsub(sprintf("^%s_%s_%s_%s_", m_str, sc_str, m_str, sc_str), sprintf("%s_%s_", m_str, sc_str), tag)
  tag
}

construct_act4cap_output_filenames <- function(model,
                                               scenario,
                                               macro_output = NULL,
                                               downscale_csv = NULL,
                                               resolution = "10km",
                                               date = NULL,
                                               trial = FALSE,
                                               custom_tag = NULL,
                                               out_dir = "outputs") {
  tag <- construct_version_tag(
    model = model,
    scenario = scenario,
    macro_output = macro_output,
    downscale_csv = downscale_csv,
    resolution = resolution,
    date = date,
    trial = trial,
    custom_tag = custom_tag
  )
  
  # SWAT+ paths & files
  swat_dir_name <- sprintf("swat_water_quality_%s", tag)
  swat_out_dir  <- file.path(out_dir, swat_dir_name)
  
  # Biodiversity Link paths & files
  bio_dir_name <- sprintf("biodiversity_link_%s", tag)
  bio_out_dir  <- file.path(out_dir, bio_dir_name)
  
  # Synthesis paths & files
  synth_dir_name <- "act4cap_synthesis"
  synth_out_dir  <- file.path(out_dir, synth_dir_name)
  
  list(
    tag = tag,
    
    # SWAT+
    swat = list(
      dir_name      = swat_dir_name,
      dir_path      = swat_out_dir,
      summary_csv   = file.path(swat_out_dir, sprintf("swat_plus_water_quality_summary_%s.csv", tag)),
      cell_csv      = file.path(swat_out_dir, sprintf("swat_plus_cells_%s.csv", tag)),
      composite_rds = file.path(swat_out_dir, sprintf("swat_plus_composite_%s.rds", tag)),
      raster_prefix = sprintf("swat_%s", tag)
    ),
    
    # Biodiversity Link
    biodiversity = list(
      dir_name      = bio_dir_name,
      dir_path      = bio_out_dir,
      bii_csv       = file.path(bio_out_dir, sprintf("biodiversity_link_bii_summary_%s.csv", tag)),
      csar_csv      = file.path(bio_out_dir, sprintf("biodiversity_link_csar_summary_%s.csv", tag)),
      msa_csv       = file.path(bio_out_dir, sprintf("biodiversity_link_msa_summary_%s.csv", tag)),
      cell_csv      = file.path(bio_out_dir, sprintf("biodiversity_link_cells_%s.csv", tag)),
      composite_rds = file.path(bio_out_dir, sprintf("biodiversity_link_composite_%s.rds", tag)),
      raster_prefix = sprintf("biodiversity_%s", tag)
    ),
    
    # Multi-Pillar Synthesis
    synthesis = list(
      dir_name = synth_dir_name,
      dir_path = synth_out_dir,
      csv      = file.path(synth_out_dir, sprintf("act4cap_integrated_synthesis_%s.csv", tag))
    )
  )
}


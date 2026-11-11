# ===============================================================
# merge_split_output_dirs.R: Consolidate Split Output Directories
# ===============================================================
# Merges output files from source_dir into target_dir, updating
# file prefixes to match target_prefix, and updates the run lockfile.
# ===============================================================

output_base <- "P:/globiom/Projects/LAMASUS/DS_results/outputs"
temp_dir    <- "P:/globiom/Projects/LAMASUS/DS_results/temp"

target_date   <- "20260817"
source_dates  <- c("20260818")
run_basename  <- "merged"
pixel_res     <- "10km"
model_name    <- "LAMASUS_GLOBIOM"

target_prefix <- paste0("DS_", run_basename, "_", pixel_res, "_", target_date)
target_dir    <- file.path(output_base, target_prefix)

if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

cat(sprintf(">>> Target output directory: %s\n", target_dir))

for (src_date in source_dates) {
  src_prefix <- paste0("DS_", run_basename, "_", pixel_res, "_", src_date)
  src_dir    <- file.path(output_base, src_prefix)
  
  if (!dir.exists(src_dir)) {
    cat(sprintf(">>> Source directory does not exist: %s. Skipping.\n", src_dir))
    next
  }
  
  cat(sprintf("\n>>> Consolidating files from %s -> %s ...\n", src_prefix, target_prefix))
  src_files <- list.files(src_dir, full.names = TRUE)
  
  for (sf in src_files) {
    fname <- basename(sf)
    # Replace date prefix
    new_fname <- sub(paste0("^", src_prefix), target_prefix, fname)
    tf <- file.path(target_dir, new_fname)
    
    if (file.exists(tf)) {
      # If target already exists, compare size and keep larger/newer file
      sf_info <- file.info(sf)
      tf_info <- file.info(tf)
      if (sf_info$mtime > tf_info$mtime && sf_info$size >= tf_info$size) {
        cat(sprintf("   - Replacing existing file with newer version from source: %s\n", new_fname))
        file.copy(sf, tf, overwrite = TRUE)
      } else {
        cat(sprintf("   - Skipping (target already has equivalent or newer version): %s\n", new_fname))
      }
    } else {
      cat(sprintf("   - Copying: %s -> %s\n", fname, new_fname))
      file.copy(sf, tf, overwrite = TRUE)
    }
  }
}

# Update or create run lockfile
lockfile_path <- file.path(temp_dir, paste0(".run_session_", model_name, "_", pixel_res, ".rds"))
session_info <- list(
  model_name    = model_name,
  pixel_res     = pixel_res,
  run_basename  = run_basename,
  run_date      = target_date,
  output_prefix = target_prefix,
  output_dir    = target_dir,
  updated_at    = Sys.time()
)
saveRDS(session_info, lockfile_path)
cat(sprintf("\n>>> Run session lockfile saved to %s (Locked RUN_DATE = %s)\n", lockfile_path, target_date))
cat(">>> Consolidation complete!\n")

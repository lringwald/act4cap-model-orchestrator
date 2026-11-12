# ==============================================================================
# Script: merge_future_projections.R
# Description: Merges all future scenario projections (climate & socioeconomic)
#              onto the master 1km grid (INSPIRE_Europe_buffer_1kmID).
#              Generates a single wide parquet file for padding onto historical data.
# ==============================================================================

library(arrow)
library(data.table)

cat("Starting Future Projections Merge...\n")
cat("--------------------------------------------------\n")

master_cands <- sort(list.files("data/aux_files", pattern = "one_kmID_master_mapping_.*\\.parquet$", full.names = TRUE), decreasing = TRUE)
master_file <- if (length(master_cands) > 0) master_cands[1] else "data/aux_files/one_kmID_master_mapping_2026-08-12.parquet"
future_dir <- "data/02_intermediate/future_projections"
output_parquet <- "data/02_intermediate/prior_model_1km_future_inputs.parquet"
output_csv <- file.path(future_dir, "future_data_dictionary.csv")
output_md <- file.path(future_dir, "future_data_dictionary.md")

# 1. Load Master Grid
cat("Loading Master Grid...\n")
master_dt <- read_parquet(master_file, as_data_frame = TRUE)
setDT(master_dt)

# We just want the base grid IDs so that when padding occurs, it joins perfectly
master_dt <- unique(master_dt[, .(EEA_1kmID, INSPIRE_Europe_buffer_1kmID)])
setkey(master_dt, INSPIRE_Europe_buffer_1kmID)

# 2. Identify Future Files
# We intentionally EXCLUDE the 'clipped' versions. 
# Why? The non-clipped (_aggregated) versions contain full coverage data.
# If we used clipped, we'd introduce NAs for out-of-mask pixels, which would then 
# require heavy FNN interpolation. Non-clipped is already gap-free.
files <- list.files(future_dir, pattern = "\\.rds$", full.names = TRUE)
files <- files[!grepl("clipped", files)]

cat(sprintf("Found %d future projection files to merge.\n", length(files)))

dict_entries <- list()

# 3. Merge Loop
for (f in files) {
  fname <- basename(f)
  cat("  -> Merging:", fname, "\n")
  
  dt <- readRDS(f)
  setDT(dt)
  
  if (!"INSPIRE_Europe_buffer_1kmID" %in% names(dt)) {
    warning("Skipping ", fname, ": No INSPIRE_Europe_buffer_1kmID column found.")
    next
  }
  
  setkey(dt, INSPIRE_Europe_buffer_1kmID)
  var_cols <- setdiff(names(dt), "INSPIRE_Europe_buffer_1kmID")
  
  # Merge onto master (all.x = TRUE keeps master rows intact)
  master_dt <- merge(master_dt, dt, by = "INSPIRE_Europe_buffer_1kmID", all.x = TRUE)
  
  # Prepare Dictionary Entry
  if (grepl("CHELSA", fname)) {
    source <- "CHELSA V2.1"
    desc <- "Future climate projection (Bio 15)"
  } else if (grepl("GDP", fname)) {
    source <- "EU GDP Projections"
    desc <- "Future socioeconomic projection (GDP)"
  } else {
    source <- "Unknown"
    desc <- "Future projection variable"
  }
  
  for (vc in var_cols) {
    dict_entries[[length(dict_entries) + 1]] <- data.table(
      Variable = vc,
      Source = source,
      Description = desc,
      File = fname,
      DateAdded = Sys.Date()
    )
  }
}

# 4. Save Outputs
cat("\nSaving Future Inputs Parquet to:", output_parquet, "\n")
write_parquet(master_dt, output_parquet)

cat("Saving Data Dictionaries...\n")
dict_df <- rbindlist(dict_entries)
fwrite(dict_df, output_csv)

# Markdown generation
md_lines <- c(
  "# Future Projections Data Dictionary",
  "Generated on: `r Sys.time()`",
  "",
  "| Variable | Source | Description | File | Date Added |",
  "|---|---|---|---|---|"
)
for (i in seq_len(nrow(dict_df))) {
  row_str <- sprintf("| `%s` | %s | %s | %s | %s |",
                     dict_df$Variable[i],
                     dict_df$Source[i],
                     dict_df$Description[i],
                     dict_df$File[i],
                     dict_df$DateAdded[i])
  md_lines <- c(md_lines, row_str)
}
writeLines(md_lines, output_md)

cat("--------------------------------------------------\n")
cat("Success! Future Projections Merge Complete.\n")

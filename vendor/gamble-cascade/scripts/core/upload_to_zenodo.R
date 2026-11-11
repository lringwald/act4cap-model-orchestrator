# ==============================================================================
# Script: upload_to_zenodo.R
# Description: Automates the uploading of large model outputs and raw data
#              to Zenodo using the zen4R API.
# ==============================================================================

# Install zen4R if not already installed
if (!requireNamespace("zen4R", quietly = TRUE)) {
  install.packages("zen4R")
}
library(zen4R)

cat("Starting Zenodo Upload Script...\n")
cat("--------------------------------------------------\n")

# 1. Configuration 
# Token is loaded securely from the local .Renviron file
ZENODO_TOKEN <- Sys.getenv("ZENODO_PAT")
if (ZENODO_TOKEN == "") stop("Error: ZENODO_PAT not found in environment. Please add it to your .Renviron file.")

# Define the files you want to upload
files_to_upload <- c(
  "data/02_intermediate/prior_model_1km_master_inputs.parquet",
  "data/02_intermediate/prior_model_1km_master_inputs_stack.tif"
  # Add more files here if needed (e.g. zipped raw data)
)

# Verify files exist before connecting to the API
missing_files <- files_to_upload[!file.exists(files_to_upload)]
if(length(missing_files) > 0) {
  stop("The following files do not exist:\n", paste(missing_files, collapse = "\n"))
}

# 2. Connect to Zenodo
cat("Connecting to Zenodo API...\n")
zenodo <- ZenodoManager$new(token = ZENODO_TOKEN)

# 3. Create a new Dataset Record
cat("Creating new empty record draft...\n")
my_dataset <- zenodo$createEmptyRecord()

# 4. Define Metadata
cat("Setting dataset metadata...\n")
my_dataset$setTitle("LAMASUS Cascading Gamble - 1km Gridwork Data")
my_dataset$setUploadType("dataset")
my_dataset$setAccessRight("open") # Change to "closed" or "embargoed" if you don't want it public yet
my_dataset$addCreator(name = "Ringwald, Leopold", affiliation = "IIASA")
my_dataset$setDescription("
  <p>Master mapping, climatic, and Earth Engine aggregated variables for the Cascading Gamble model.</p>
  <p>Contains the final 1km master inputs compiled from Topography, ESDAC soils, Protection Areas, CHELSA, SPEI, and GHM layers.</p>
")

# Optional: Add communities if you have a specific IIASA or LAMASUS Zenodo community
# my_dataset$addCommunity("lamasus")

# 5. Upload the Files
cat("Uploading files to Zenodo (this may take a while depending on internet speed)...\n")
for(file_path in files_to_upload) {
  cat(sprintf("  Uploading: %s (%.2f GB)\n", basename(file_path), file.info(file_path)$size / 1024^3))
  zenodo$uploadFile(file_path, my_dataset)
}

cat("--------------------------------------------------\n")
cat("SUCCESS! Files have been uploaded to a DRAFT state on Zenodo.\n")
cat("\nIMPORTANT: Your dataset is currently a DRAFT. It is NOT public yet.\n")
cat("You can review the uploaded files and metadata in your web browser at Zenodo.org.\n")
cat("Once you are happy with the draft, click 'Publish' on the website to permanently mint your DOI.\n")

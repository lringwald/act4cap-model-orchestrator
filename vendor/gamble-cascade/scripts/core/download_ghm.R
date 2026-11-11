# Download GHM-v3 Threat Group Data from GEE
# Dataset: projects/sat-io/open-datasets/GHM/HM_1990_2020_THREAT_GROUPS_300M

library(rgee)

# --- 1. Settings ---
selected_threat <- "HI" # Options: "AG", "BU", "TI", "EX", "PO", "NS"
target_years <- c("1995", "2000", "2005", "2010", "2015", "2020")
aoi <- ee$Geometry$Rectangle(coords = c(-12.0, 34.0, 35.0, 72.0))

# --- 2. Initialize ---
ee_project <- Sys.getenv("EE_PROJECT")
if (ee_project == "") stop("Error: EE_PROJECT not found in environment. Please add it to your .Renviron file.")
Sys.setenv(EARTHENGINE_PROJECT = ee_project)
ee$Initialize(project = ee_project)

# --- 3. Get the Collection List ---
ghm_col <- ee$ImageCollection("projects/sat-io/open-datasets/GHM/HM_1990_2020_THREAT_GROUPS_300M")

# Filter for the threat first
threat_only <- ghm_col$filter(ee$Filter$eq("threat_code", selected_threat))

# Get a list of all IDs in this threat group to see what we are dealing with
all_ids <- threat_only$aggregate_array("system:index")$getInfo()
cat("Available IDs for", selected_threat, ":\n", paste(all_ids, collapse = ", "), "\n")

# --- 4. Loop through target years and find the matching ID ---
for (yr in target_years) {

  # Find the ID in the list that contains the year string
  matching_id <- all_ids[grep(yr, all_ids)]

  if (length(matching_id) == 0) {
    cat("No image found for year:", yr, "- Skipping.\n")
    next
  }

  # Select the first matching image by its unique ID
  target_img <- ee$Image(paste0("projects/sat-io/open-datasets/GHM/HM_1990_2020_THREAT_GROUPS_300M/", matching_id[1]))

  out_name <- paste0("GHM_v3_", selected_threat, "_", yr, "_EU_UK")
  cat("Submitting Task:", out_name, "...\n")

  try({
    task <- ee_image_to_drive(
      image = target_img,
      description = out_name,
      folder = "EE_Downloads",
      region = aoi,
      scale = 1000,
      fileFormat = "GeoTIFF"
    )
    task$start()
    cat("Success!\n")
  }, silent = FALSE)
}

ee_monitoring()

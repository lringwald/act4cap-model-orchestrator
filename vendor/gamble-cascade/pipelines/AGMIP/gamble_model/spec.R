# =============================================================================
# AGMIP — GAMBLE model spec
# =============================================================================
# What this file is: the MODEL SPECIFICATION for this project. Orchestration lives in
# scripts/gamble_model_run.R; the branch's own class/lineage settings live in
# run_prior_nested_cut.R's BRANCH_ENV and are NOT repeated here.
#
# Anything already set in the calling environment WINS over this file, so a one-off override
# needs no edit:   NCUT_NITER=800 Rscript scripts/AGMIP/gamble_model/run.R
# =============================================================================
list(
  branch = "AGMIP",
  notes = paste(
    "Crop-type target: LUM cropland is split with HRL Crop Types (DRIVER_DO_CROP_SPLIT, set by",
    "the AGMIP branch). ~44% of cropland gets a crop type; the rest falls back to",
    "Cropland_arable/permanent_other. 10 km on NUTS3, matching the GLOBIOM design."),
  env = c(
    # ---- design ----
    NCUT_PIXEL_RES       = "10",
    NCUT_PIXEL_INTERSECT = "NUTS3",

    # ---- sampler ----
    NCUT_M      = "25",
    NCUT_NITER  = "6000",
    NCUT_CHAINS = "4",
    NCUT_USE_RE = "TRUE",
    NCUT_USE_IV = "TRUE",

    # ---- gamble-core model options (all OFF = the historical AGMIP design) ----
    # BART on topography + coordinates. Measured on GLOBIOM (not yet on AGMIP): +249 nats
    # held-out over a linear-topography arm, gains concentrated in abundant terrain-driven
    # classes. Turn on together -- DRIVER_BART_COLS is inert without DRIVER_USE_BART.
    DRIVER_USE_BART   = "FALSE",
    DRIVER_BART_COLS  = "topo",
    DRIVER_ADD_COORDS = "FALSE",
    DRIVER_COORD_TYPE = "lonlat",

    # Mundlak group-mean terms: an INTERPRETATION fix (mu becomes the population-averaged
    # effect given group composition), measured to be held-out neutral. Not an accuracy gain.
    DRIVER_MUNDLAK    = "FALSE"
  )
)

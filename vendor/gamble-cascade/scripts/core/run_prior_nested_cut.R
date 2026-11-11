#!/usr/bin/env Rscript
# =============================================================================
# run_prior_nested_cut.R — fit the CUT nested MNL land-use prior INSIDE cascadinggamble
# =============================================================================
# Thin orchestrator: cascadinggamble owns the DATA, gamble-core owns the MODEL CODE.
# This script resolves both, optionally rebuilds the pixel design FROM SCRATCH out of the
# 1km rasters, and fits the nested-cut model (see gamble-core docs/nested_cut_model.md).
#
#   Rscript scripts/run_prior_nested_cut.R [--branch GLOBIOM] [--from-scratch] [--smoke]
#
# ---- CODE ROOT (env GAMBLE_HOME) --------------------------------------------------------
# Default ../gamble-core = the LIVE development checkout. `../gamble-core` is the git
# submodule, which only sees what has been COMMITTED AND PUSHED to github.com/lringwald/gamble-core.
# As of 2026-08-05 the live checkout has ~99 uncommitted files including fixes the submodule
# lacks -- notably the const_sum_blocks="auto" reconstruction fix, which every nested_cut
# sub-fit depends on (without it the exported coefficients do not reproduce the fitted
# probabilities). Point GAMBLE_HOME at the submodule only once that work is pushed.
#
# ---- DATA ROOTS (env GAMBLE_DATA_ROOTS, colon-separated) --------------------------------
# Searched IN ORDER; the first hit wins. Default: this repo's data/02_intermediate, then
# data/aux_files for master mapping, 2018 LUM, and organic layers.
# Every resolution is printed so the provenance is explicit.
# =============================================================================
suppressMessages({library(data.table); library(arrow)})
`%||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1]) || !nzchar(a[1])) b else a

# Platform detection: Accelerator (DeepOrigin / Celery) jobs provide PROJECT_SLUG
is_accelerator <- function() nzchar(Sys.getenv("PROJECT_SLUG", ""))

args   <- commandArgs(trailingOnly = TRUE)
getarg <- function(flag, default = NULL) { i <- match(flag, args); if (is.na(i)) default else args[i + 1] }
has    <- function(flag) flag %in% args

BRANCH      <- toupper(getarg("--branch", Sys.getenv("NCUT_BRANCH", "GLOBIOM")))
FROM_SCRATCH<- has("--from-scratch") || isTRUE(as.logical(Sys.getenv("NCUT_FROM_SCRATCH", "FALSE")))
# Env fallbacks so this is usable INTERACTIVELY (RStudio / source()), where commandArgs() is empty:
#   Sys.setenv(NCUT_BRANCH = "GLOBIOM", NCUT_SMOKE = "TRUE"); source("scripts/run_prior_nested_cut.R")
SMOKE       <- has("--smoke")       || isTRUE(as.logical(Sys.getenv("NCUT_SMOKE", "FALSE")))
DESIGN_ONLY <- has("--design-only") || isTRUE(as.logical(Sys.getenv("NCUT_DESIGN_ONLY", "FALSE")))
GAMBLE_HOME <- normalizePath(Sys.getenv("GAMBLE_HOME", "../gamble-core"), mustWork = FALSE)
DATA_ROOTS  <- strsplit(Sys.getenv("GAMBLE_DATA_ROOTS",
                 paste("data/02_intermediate", "data/aux_files", sep = ":")), ":")[[1]]
OUT_DIR     <- Sys.getenv("NCUT_OUT_DIR", "results/prior_nested_cut")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
OUT_DIR     <- normalizePath(OUT_DIR)   # absolute: we setwd() into GAMBLE_HOME for the sub-steps

cat("=============================================================\n")
cat(sprintf(" nested-cut prior | branch=%s | from_scratch=%s%s\n", BRANCH, FROM_SCRATCH,
            if (SMOKE) " | SMOKE" else ""))
cat(sprintf(" code : %s\n", GAMBLE_HOME))
cat(sprintf(" data : %s\n", paste(DATA_ROOTS, collapse = "  ->  ")))
if (is_accelerator()) cat(sprintf(" plat : ACCELERATOR (PROJECT_SLUG=%s)\n", Sys.getenv("PROJECT_SLUG")))
cat("=============================================================\n")
if (!dir.exists(GAMBLE_HOME)) stop("GAMBLE_HOME not found: ", GAMBLE_HOME)

# ---- 1. resolve every input the pixel design needs, across the data roots ----------------
resolve <- function(pattern, regex = FALSE) {
  for (r in DATA_ROOTS) {
    hit <- if (regex) { f <- list.files(r, pattern = pattern, full.names = TRUE)
                        if (length(f)) f[order(file.mtime(f), decreasing = TRUE)][1] else character(0)
           } else { p <- file.path(r, pattern); if (file.exists(p)) p else character(0) }
    if (length(hit)) return(hit)
  }
  NA_character_
}
NEEDED <- list(
  master_parquet = list("prior_model_1km_master_inputs.parquet", FALSE, TRUE),
  mapping        = list("^one_kmID_master_mapping_.*\\.parquet$", TRUE,  TRUE),
  lum_2000       = list("LUM_fit_with_energy_levels_and_new_FM_2000.rds", FALSE, TRUE),
  lum_2010       = list("LUM_fit_with_energy_levels_and_new_FM_2010.rds", FALSE, TRUE),
  lum_2018       = list("LUM_fit_with_energy_levels_and_new_FM_2018.rds", FALSE, TRUE),
  ibiom_2018     = list("ibiom_LUM_EWM_2018.rds", FALSE, FALSE),
  organic        = list("organic_certificaties_final_calibrated_master.rds", FALSE, FALSE),
  crop_types     = list("Crop_Types/Crop_Types_Avg_2017_2019_1km.rds", FALSE, FALSE)
)
cat("\n-- input resolution --------------------------------------------------\n")
found <- list(); missing_required <- character(0)
for (nm in names(NEEDED)) {
  spec <- NEEDED[[nm]]; p <- resolve(spec[[1]], spec[[2]])
  found[[nm]] <- p
  where <- if (is.na(p)) "MISSING" else {
    root <- DATA_ROOTS[which(vapply(DATA_ROOTS, function(r) startsWith(normalizePath(p, mustWork = FALSE),
                     normalizePath(r, mustWork = FALSE)), logical(1)))[1]]
    if (identical(root, DATA_ROOTS[1])) "cascadinggamble" else "LEGACY gridwork" }
  cat(sprintf("  %-15s %-16s %s\n", nm, where, if (is.na(p)) "" else basename(p)))
  if (is.na(p) && isTRUE(spec[[3]])) missing_required <- c(missing_required, nm)
}
if (length(missing_required))
  stop("\nCannot build the design from scratch -- required input(s) missing from every data root:\n  ",
       paste(missing_required, collapse = ", "),
       "\nEither migrate them into data/02_intermediate or add their location to GAMBLE_DATA_ROOTS.")

# WHERE THE DESIGN LIVES. It is EXPENSIVE and BRANCH-specific, not run-specific: look in
# NCUT_OUT_DIR first, then fall back to the DEFAULT results/prior_nested_cut/. Without the fallback a
# quick assessment pass with its own NCUT_OUT_DIR would try to rebuild from the 1km rasters (and then
# die on the Drive placeholders) even though a perfectly good design already exists.
# NCUT_DESIGN=<path> points at one explicitly.
inputs_rds <- Sys.getenv("NCUT_DESIGN", "")
if (!nzchar(inputs_rds)) {
  .dn <- sprintf("pixel_model_inputs_%s.rds", BRANCH)
  .cand <- unique(c(file.path(OUT_DIR, .dn),
                    normalizePath(file.path("results/prior_nested_cut", .dn), mustWork = FALSE)))
  .hit <- .cand[file.exists(.cand)]
  inputs_rds <- if (length(.hit)) .hit[1] else .cand[1]   # first existing, else build into OUT_DIR
  if (length(.hit) && !identical(.hit[1], .cand[1]))
    cat(sprintf("\n   (no design in NCUT_OUT_DIR -- reusing the shared one:\n    %s)\n", .hit[1]))
}

# GOOGLE DRIVE PLACEHOLDERS. Drive reports the full size via ls but keeps the file un-materialised
# (and RE-EVICTS it later, even after a successful read). Reading one fails deep inside arrow with
# "IOError: Error reading bytes from file. Detail: [errno 60] Operation timed out", which looks like
# a code fault. Compare APPARENT size (file.size) with LOCAL blocks (du) and say so plainly instead.
.local_bytes <- function(p) { o <- suppressWarnings(system2("du", c("-k", shQuote(p)), stdout = TRUE, stderr = FALSE))
  if (!length(o)) return(NA_real_); as.numeric(strsplit(trimws(o[1]), "\\s+")[[1]][1]) * 1024 }
.ghost <- character(0)
for (nm in names(found)) { p <- found[[nm]]; if (is.na(p) || dir.exists(p)) next
  ap <- file.size(p); lo <- .local_bytes(p)
  if (is.finite(ap) && is.finite(lo) && ap > 1e6 && lo < ap * 0.5)
    .ghost <- c(.ghost, sprintf("%s (%s: %.0f MB apparent, %.0f MB local)", nm, basename(p), ap/1e6, lo/1e6)) }
if (length(.ghost)) {
  msg <- paste0("\nThese inputs are Google Drive PLACEHOLDERS -- not actually on disk:\n  ",
    paste(.ghost, collapse = "\n  "),
    "\nReading them fails with 'errno 60 Operation timed out' deep inside arrow.\n",
    "Materialise them first:   cat <file> > /dev/null   (or mark the folder 'Available offline').\n")
  if (FROM_SCRATCH || !file.exists(inputs_rds))
    stop(msg) else message(msg, "  (not fatal: the cached design is being reused, so these are not read.)")
}

# ---- 2. pixel design: rebuild from the 1km rasters, or reuse a dump ----------------------
BRANCH_ENV <- switch(BRANCH,
  # GLOBIOM_subclass is the CURATED single class column (gamble-core/aux_files mapping). It is a
  # faithful upgrade of "GLOBIOM_UNFCCC,GLOBIOM_mngmt", not a different scheme: all 54 managed rows
  # reproduce their old composed name EXACTLY (asserted in build_globiom_subclass.R); it only resolves
  # the Natural_unmanaged catch-all, and it carries the hand-curated nest column. Set
  # NCUT_CLASS_COLS="GLOBIOM_UNFCCC,GLOBIOM_mngmt" to rebuild the historical 19-class design.
  GLOBIOM = c(DRIVER_CLASS_COLS = Sys.getenv("NCUT_CLASS_COLS", "GLOBIOM_subclass"),
              DRIVER_LUM_LINEAGE = "globiom", DRIVER_INCLUDE_ORGANIC = "TRUE"),
  AGMIP   = c(DRIVER_CLASS_COLS = "AgMIP_label", DRIVER_DO_CROP_SPLIT = "TRUE",
              DRIVER_LUM_LINEAGE = "globiom", DRIVER_INCLUDE_ORGANIC = "TRUE"),
  BIOCLIMA= c(DRIVER_CLASS_COLS = "BIOCLIMA3_LU,BIOCLIMA3_mngmt", DRIVER_LUM_LINEAGE = "bioclima",
              DRIVER_INCLUDE_ORGANIC = "FALSE"),
  stop("unknown branch: ", BRANCH))

# (inputs_rds was resolved above -- OUT_DIR, then the shared results/prior_nested_cut/, then NCUT_DESIGN)
if (FROM_SCRATCH || !file.exists(inputs_rds)) {
  cat("\n-- building the pixel design from the 1km inputs (this is the slow step) ----------\n")
  # The pixel driver expects ONE gridwork dir. Stage a directory of links to the resolved
  # files so a two-root layout looks like one root to it (no copying of multi-GB rasters).
  stage <- file.path(OUT_DIR, "_gridwork_stage"); unlink(stage, recursive = TRUE); dir.create(stage, recursive = TRUE)
  for (nm in names(found)) { p <- found[[nm]]; if (is.na(p)) next
    dest <- file.path(stage, basename(p))
    if (nm == "crop_types") { dir.create(file.path(stage, "Crop_Types"), showWarnings = FALSE)
                              dest <- file.path(stage, "Crop_Types", basename(p)) }
    file.symlink(normalizePath(p), dest) }
  cat(sprintf("   staged %d input(s) -> %s\n", length(list.files(stage, recursive = TRUE)), stage))

  env <- c(BRANCH_ENV,
           GAMBLE_GRIDWORK_DIR = normalizePath(stage),
           DRIVER_DUMP_INPUTS = "TRUE", DRIVER_DUMP_EXIT = "TRUE",
           DRIVER_PIXEL_RES = Sys.getenv("NCUT_PIXEL_RES", "10"),
           DRIVER_PIXEL_INTERSECT = Sys.getenv("NCUT_PIXEL_INTERSECT", "NUTS3"))
  cat("   env:", paste(sprintf("%s=%s", names(env), env), collapse = " "), "\n")
  # Set in THIS process and let the child inherit. Passing system2(env=) would shell-split the
  # paths on the space in "My Drive" (the Google Drive root) and silently mis-resolve them.
  old_env <- Sys.getenv(names(env), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(env))
  on.exit({ set <- old_env[!is.na(old_env)]
            if (length(set)) do.call(Sys.setenv, as.list(set))
            un <- names(old_env)[is.na(old_env)]; if (length(un)) Sys.unsetenv(un) }, add = TRUE)

  t_prep <- Sys.time()
  # The pixel driver sources its own codes/ with RELATIVE paths, so it must run with GAMBLE_HOME
  # as the working directory (all resolved data paths above are already absolute).
  prep_wd <- setwd(GAMBLE_HOME)
  driver_script <- if (file.exists("drivers/run_lu_pixel_model.R")) "drivers/run_lu_pixel_model.R" else "run_prior_module_pixel_level_model.R"
  rc <- system2("Rscript", shQuote(driver_script), stdout = "", stderr = "")
  setwd(prep_wd)
  # Only accept a dump written BY THIS RUN. Otherwise a failed data-prep silently falls back to
  # whatever stale pixel_model_inputs*.rds is lying in gamble-core/output (a July file, here).
  dumped <- list.files(file.path(GAMBLE_HOME, "output"), "^pixel_model_inputs.*\\.rds$", full.names = TRUE)
  dumped <- dumped[file.mtime(dumped) >= t_prep]
  if (!length(dumped))
    stop("data-prep produced no NEW pixel_model_inputs*.rds (Rscript exit ", rc, ").\n",
         "  Stale dumps in gamble-core/output were ignored on purpose -- fix the data-prep and re-run.")
  file.copy(dumped[order(file.mtime(dumped), decreasing = TRUE)][1], inputs_rds, overwrite = TRUE)
  cat(sprintf("   design -> %s\n", inputs_rds))
} else cat(sprintf("\n-- reusing existing design: %s (pass --from-scratch to rebuild)\n", inputs_rds))

if (DESIGN_ONLY) { cat(sprintf("\n--design-only: stopping after the design (%s)\n", inputs_rds)); quit(save = "no", status = 0) }

# ---- 3. fit -----------------------------------------------------------------------------
# Locate the model code by ABSOLUTE path under GAMBLE_HOME. gamble-core keeps it in codes/; a future
# layout may use scripts/ or R/. Resolving against GAMBLE_HOME rather than the working directory
# matters because this file is also SOURCED interactively (RStudio), where setwd() inside a sourced
# script does not reliably change the directory the subsequent expressions see -- a cwd-relative
# check then fails with "model code not found" even though the files are right there.
.code_dir <- Filter(function(d) file.exists(file.path(GAMBLE_HOME, d, "mnlogit_rcpp_sym.R")),
                    c("codes", "scripts", "R"))
if (!length(.code_dir)) stop(sprintf(
  "model code not found under GAMBLE_HOME=%s -- looked for mnlogit_rcpp_sym.R in codes/, scripts/, R/", GAMBLE_HOME))
# NO on.exit() here. At TOP LEVEL (which is what source() gives us in RStudio) on.exit fires when the
# CURRENT EXPRESSION finishes, not at the end of the script -- so `setwd(X); on.exit(setwd(owd))` put
# the directory back immediately and every later relative path broke. Restore explicitly instead.
owd <- getwd()
setwd(GAMBLE_HOME)          # the model files source() each other and sourceCpp() relatively
suppressMessages({library(Rcpp); library(RcppArmadillo)})
for (.f in c("mnlogit_rcpp_sym.R", "nested_cut.R", "nest_trees.R")) source(file.path(.code_dir[1], .f))
setwd(owd)

inp <- readRDS(inputs_rds)
X <- as.matrix(inp$X_mat); if (is.null(colnames(X)) && !is.null(inp$col_names)) colnames(X) <- inp$col_names
X[!is.finite(X)] <- 0
Y <- as.matrix(inp$Y_pixel); Y <- Y / rowSums(Y); cats <- colnames(Y)
group_idx <- as.integer(inp$group_idx_vec)

# ---- NCUT_SUBSAMPLE: fewer PIXELS is the real lever for a fast assessment pass -------------
# Runtime is (iterations x nodes x n), and n = 64,173 here -> ~13 Gibbs iterations/min, so even
# 600 iterations is ~45 min PER LEAF. Cutting `niter` or the stored draws does not fix that:
# `thin`/`draws_per_impute` bound MEMORY, not time. Subsampling pixels cuts time ~linearly.
# NOTE this is a genuinely DIFFERENT fit, not a warm start: the node store is config-hash
# guarded (niter/M/thin/chains/data all in the hash), so a small pass CANNOT be extended into a
# production run -- give it its own NCUT_OUT_DIR and treat it as a sanity check on the plumbing,
# the tree, lambda's range and the identification table, then run production separately.
SUBSAMPLE <- as.integer(Sys.getenv("NCUT_SUBSAMPLE", "0"))
if (SUBSAMPLE > 0 && SUBSAMPLE < nrow(X)) {
  set.seed(as.integer(Sys.getenv("NCUT_SEED", "1")))
  keep <- sort(sample(nrow(X), SUBSAMPLE))
  X <- X[keep, , drop = FALSE]; Y <- Y[keep, , drop = FALSE]; group_idx <- group_idx[keep]
  drop_g <- setdiff(seq_len(ncol(Y)), which(colSums(Y) > 0))
  cat(sprintf("!! NCUT_SUBSAMPLE: %d of %d pixels (%.1f%%), %d groups%s -- ASSESSMENT ONLY, not a production fit\n",
      nrow(X), length(inp$group_idx_vec), 100 * nrow(X) / length(inp$group_idx_vec),
      length(unique(group_idx)), if (length(drop_g)) sprintf(", %d class(es) now empty", length(drop_g)) else ""))
}

socio <- c("log1p_GDP","log1p_Pop","GHM_HI","GHM_TI","CISI","allPA_share","Slope_rad","Elevation")
re_idx <- sort(intersect(c(which(colnames(X) == "intercept"), which(colnames(X) %in% socio)), seq_len(ncol(X))))

M     <- as.integer(Sys.getenv("NCUT_M",     if (SMOKE) "10"   else "25"))
NITER <- as.integer(Sys.getenv("NCUT_NITER", if (SMOKE) "800"  else "6000"))
NCH   <- as.integer(Sys.getenv("NCUT_CHAINS", if (SMOKE) "1"   else "4"))
USE_RE<- as.logical(Sys.getenv("NCUT_USE_RE", if (SMOKE) "FALSE" else "TRUE"))
cat(sprintf("\n-- fitting: n=%d cov=%d classes=%d groups=%d | M=%d niter=%d chains=%d use_re=%s\n",
            nrow(X), ncol(X), length(cats), length(unique(group_idx)), M, NITER, NCH, USE_RE))
# NCUT_TREE decouples the NESTING SCHEME from the DATA BRANCH: the branch selects the classification
# and data-prep config, the tree selects how those classes are nested. e.g. NCUT_TREE=GLOBIOM_ORG
# keeps GLOBIOM data but gives the organic twins their own level under Cropland/Pasture (see
# codes/nest_trees.R for why). Defaults to the branch, so existing behaviour is unchanged.
TREE_SCHEME <- Sys.getenv("NCUT_TREE", BRANCH)
# A hand-curated nest column in the mapping WINS over any naming-prefix scheme: it is the only route
# that can restructure the hierarchy without renaming classes. The driver carries it in the inputs as
# `class_nest` (leaf column + ancestor path). Set NCUT_TREE explicitly to override and force a coded
# scheme; without the column this falls through to the previous behaviour unchanged.
.curated <- !is.null(inp$class_nest) && nzchar(Sys.getenv("NCUT_TREE", "")) == FALSE
if (.curated) {
  .cn <- names(inp$class_nest)
  cat(sprintf(">>> nesting tree: CURATED from mapping (%s -> %s)\n", .cn[1], .cn[2]))
  tree <- nest_tree_from_mapping(inp$class_nest, cats, class_col = .cn[1], nest_col = .cn[2])
  nest_tree_check(tree, cats)
} else {
  cat(sprintf(">>> nesting tree (scheme=%s):\n", TREE_SCHEME)); tree <- build_nest_tree(TREE_SCHEME, cats)
}

t0 <- Sys.time()
# NCUT_USE_IV=FALSE -> FACTORIZED: keep the nesting (and its non-IIA substitution) but drop the
# inclusive-value coupling. Measured on real pixels (experiments/nested/iv_value_test.R, held-out jll):
# iv_nested -3192.1 | factorized -3204.9 | flat -3280.8, null -4400. So the IV coupling is worth only
# +12.8 = 1.07% of the model, while the NESTING itself is worth +75.9 -- six times more.
# IMPORTANT SIDE EFFECT: with use_iv=FALSE the focal routing is skipped entirely (nested_cut.R:378
# gates it on use_iv), so the root sees the FULL design -- all 21 spatial Y lag columns with FREE
# coefficients -- and there is no lambda to be non-RUM-consistent. That is the trade: ~1% predictive
# skill for spatial lags at every level plus no identification problem.
USE_IV <- as.logical(Sys.getenv("NCUT_USE_IV", "TRUE"))
cat(sprintf(">>> inclusive values: %s%s\n", USE_IV,
    if (!USE_IV) "  (FACTORIZED: full spatial Y lags at the root, no lambda)" else ""))
fit <- nested_cut_fit(X, Y, tree, use_iv = USE_IV, use_re = USE_RE, group_idx = group_idx, re_idx = re_idx,
                      M = M, draws_per_impute = 15, niter = NITER, nburn = round(NITER / 3), thin = 2L,
                      min_pixels = 200, iv_mode = Sys.getenv("NCUT_IVMODE", "auto"), moment_rank = 2L,
                      stream_disk = TRUE, n_chains = NCH,
                      n_cores = max(1L, min(NCH, parallel::detectCores() - 1L)),
                      focal_rule = Sys.getenv("NCUT_FOCAL_RULE", "macro_totals"),
                      # NCUT_KEEP_POSTERIOR=TRUE retains every streamed draw under
                      # <store>/posterior/<node>/... instead of a deleted tempdir. Costs GBs; buys the
                      # ability to re-derive anything from a finished run without re-fitting.
                      keep_posterior = as.logical(Sys.getenv("NCUT_KEEP_POSTERIOR", "FALSE")),
                      # The node store is config-hash guarded (it refuses to resume across a changed
                      # M/niter/chains/data), so a --smoke run must NOT share the production store
                      # or it aborts with "built with a DIFFERENT config".
                      store_dir = file.path(OUT_DIR, sprintf("store_%s%s", BRANCH, if (SMOKE) "_smoke" else "")))
cat(sprintf(">>> fit done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

idt <- nested_cut_identification(fit)
if (!is.null(idt)) { cat("\n>>> IV identification (lambda + IV~design R2):\n")
  print(idt[, c("node","iv_child","iv_r2","lambda","lambda_q025","lambda_q975")], row.names = FALSE) }

ts <- format(Sys.time(), "%Y-%m-%d_%H%M")
out <- file.path(OUT_DIR, sprintf("nested_cut_%s_%s.rds", BRANCH, ts))
# group_idx (and the row selection it belongs to) MUST be saved: predict_nested_cut needs it to reach
# the per-group coefficients, and these rows may be a SUBSET (NCUT_SUBSAMPLE above), so group_idx
# cannot be recovered from inp$group_idx_vec afterwards. Without it a consumer can only predict
# pooled -- which does not reproduce a group-heterogeneous MNL.
saveRDS(list(fit = fit, tree = tree, cats = cats, re_idx = re_idx, branch = BRANCH,
             group_idx = group_idx, row_keep = if (exists("keep")) keep else NULL,
             identification = idt, inputs = inputs_rds, data_roots = DATA_ROOTS,
             gamble_home = GAMBLE_HOME, resolved_inputs = found), out)
cat(sprintf(">>> saved %s\n", out))

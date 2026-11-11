# =============================================================================
# run_context.R
# =============================================================================
# Run context shared by all pipelines (data-pipelines rulebook, Principles 2, 5,
# 9 and 14; spatial rulebook, Principle 1, item 3):
#   - ensure_cascade_engine(): load the cascade-core engine once per session, from
#     the pinned submodule or an explicitly configured path, and record which copy;
#   - resolve_run_date(): the run date from a declared value, the configuration, or
#     the run's own session lockfile, never from "the latest output directory";
#   - the run manifest <output_prefix>_run_manifest.json: code versions, engine and
#     prior sources, run date and its source, environment, and package versions.
# =============================================================================

.run_git_info <- function(dir) {
  if (!nzchar(Sys.which("git")) || !dir.exists(dir)) return(list(commit = NA_character_, branch = NA_character_, dirty = NA))
  g <- function(...) {
    out <- suppressWarnings(system2("git", shQuote(c("-C", dir, ...)), stdout = TRUE, stderr = FALSE))
    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) character() else out
  }
  commit <- g("rev-parse", "HEAD")
  if (!length(commit)) return(list(commit = NA_character_, branch = NA_character_, dirty = NA))
  list(commit = commit[1], branch = paste(g("rev-parse", "--abbrev-ref", "HEAD"), collapse = ""),
       dirty = length(g("status", "--porcelain", "--untracked-files=no")) > 0)
}

.submodule_pin <- function(path, repo = ".") {
  if (!nzchar(Sys.which("git"))) return(NA_character_)
  out <- suppressWarnings(system2("git", shQuote(c("-C", repo, "ls-tree", "HEAD", path)), stdout = TRUE, stderr = FALSE))
  hit <- grep("^160000 commit", out, value = TRUE)
  if (length(hit)) sub("^160000 commit ([0-9a-f]+)\t.*$", "\\1", hit[1]) else NA_character_
}

# -----------------------------------------------------------------------------
# Engine
# -----------------------------------------------------------------------------

#' Load the cascade-core engine once per session and record which copy
#'
#' Source: `CASCADE_CORE_PATH` if set (a checkout path, or "installed" for the
#' installed package), else the pinned submodule `external/cascade-core`. There is
#' no silent fallback to a sibling checkout. Later calls are no-ops unless
#' `reload = TRUE`.
ensure_cascade_engine <- function(reload = FALSE) {
  if (!reload && exists(".CASCADE_ENGINE", envir = .GlobalEnv)) return(invisible(get(".CASCADE_ENGINE", envir = .GlobalEnv)))
  env_path <- Sys.getenv("CASCADE_CORE_PATH", "")
  if (nzchar(env_path)) {
    path <- env_path; source <- "CASCADE_CORE_PATH"
  } else if (file.exists(file.path("external", "cascade-core", "DESCRIPTION"))) {
    path <- file.path("external", "cascade-core"); source <- "pinned submodule"
  } else {
    stop("cascade-core engine not found: external/cascade-core is missing or not initialized. ",
         "Run `git submodule update --init external/cascade-core`, or set CASCADE_CORE_PATH ",
         "(a checkout such as ../cascade-core, or \"installed\").", call. = FALSE)
  }
  pinned <- .submodule_pin("external/cascade-core")
  if (identical(path, "installed")) {
    suppressMessages(library(cascadecore))
    info <- list(path = "installed", source = source, version = as.character(utils::packageVersion("cascadecore")),
                 commit = NA_character_, branch = NA_character_, dirty = NA)
  } else {
    if (!file.exists(file.path(path, "DESCRIPTION"))) stop("CASCADE_CORE_PATH '", path, "' is not an R package checkout.", call. = FALSE)
    if (!requireNamespace("devtools", quietly = TRUE)) stop("devtools is required to load the engine from ", path, call. = FALSE)
    suppressMessages(devtools::load_all(path, quiet = TRUE))
    desc <- read.dcf(file.path(path, "DESCRIPTION"), fields = "Version")
    info <- c(list(path = normalizePath(path, winslash = "/"), source = source, version = unname(desc[1, "Version"])),
              .run_git_info(path))
  }
  info$pinned_commit <- pinned
  info$matches_pin <- !is.na(info$commit) && !is.na(pinned) && identical(info$commit, pinned)
  assign(".CASCADE_ENGINE", info, envir = .GlobalEnv)
  msg <- sprintf("cascade-core engine: %s (%s), commit %s%s", info$path, source,
                 if (is.na(info$commit)) "unknown" else substr(info$commit, 1, 7), if (isTRUE(info$dirty)) ", uncommitted changes" else "")
  if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_info(msg) else message(msg)
  if (!isTRUE(info$matches_pin) && !identical(path, "installed")) {
    warning(sprintf("cascade-core engine commit %s differs from the pinned submodule commit %s.",
                    substr(info$commit, 1, 7), substr(pinned, 1, 7)), call. = FALSE)
  }
  record_run_manifest("engine_cascade_core", info)
  invisible(info)
}

# -----------------------------------------------------------------------------
# Run date
# -----------------------------------------------------------------------------

#' Resolve the run date and record where it came from
#'
#' In order: a declared `OVERRIDE_DATE` (set before the orchestrator), `cfg[["run_date"]]`,
#' the run's own session lockfile if updated within `cfg[["run_resume_days"]]` (default 5;
#' `Inf` always resumes) and `cfg[["run_date_policy"]]` is not "new", else today.
#' @return list(run_date = "YYYYMMDD", source = <text>)
resolve_run_date <- function(lockfile_path, cfg = list(), declared = NULL) {
  check <- function(d, src) {
    d <- as.character(d)
    if (!grepl("^\\d{8}$", d)) stop(sprintf("Run date '%s' from %s is not YYYYMMDD.", d, src), call. = FALSE)
    list(run_date = d, source = src)
  }
  if (!is.null(declared) && length(declared) && nzchar(declared)) return(check(declared, "OVERRIDE_DATE (declared before the orchestrator)"))
  if (!is.null(cfg[["run_date"]])) return(check(cfg[["run_date"]], "config.R run_date"))
  policy <- if (is.null(cfg[["run_date_policy"]])) "resume" else cfg[["run_date_policy"]]
  if (!policy %in% c("resume", "new")) stop("run_date_policy must be \"resume\" or \"new\".", call. = FALSE)
  max_days <- if (is.null(cfg[["run_resume_days"]])) 5 else as.numeric(cfg[["run_resume_days"]])
  if (policy == "resume" && file.exists(lockfile_path)) {
    lock <- tryCatch(readRDS(lockfile_path), error = function(e) NULL)
    if (!is.null(lock$run_date) && !is.null(lock$updated_at)) {
      age <- as.numeric(difftime(Sys.time(), lock$updated_at, units = "days"))
      if (age <= max_days) {
        return(check(lock$run_date, sprintf("session lockfile %s (updated %.1f days ago, resume window %s days)",
                                            basename(lockfile_path), age, format(max_days))))
      }
    }
  }
  check(format(Sys.Date(), "%Y%m%d"), if (policy == "new") "new run (run_date_policy = \"new\")" else "new run (no session within the resume window)")
}

# -----------------------------------------------------------------------------
# Run manifest
# -----------------------------------------------------------------------------

#' Record a field of the run manifest (written to the run directory once known)
record_run_manifest <- function(key, value) {
  m <- if (exists(".RUN_MANIFEST", envir = .GlobalEnv)) get(".RUN_MANIFEST", envir = .GlobalEnv) else list()
  m[[key]] <- value
  assign(".RUN_MANIFEST", m, envir = .GlobalEnv)
  .write_run_manifest(m)
  invisible(value)
}

.write_run_manifest <- function(m = get(".RUN_MANIFEST", envir = .GlobalEnv)) {
  if (!exists("output_prefix", envir = .GlobalEnv) || !exists("paths", envir = .GlobalEnv)) return(invisible(NULL))
  out_dir <- get("paths", envir = .GlobalEnv)$output_dir
  prefix <- get("output_prefix", envir = .GlobalEnv)
  if (is.null(out_dir) || !dir.exists(out_dir) || basename(out_dir) != prefix) return(invisible(NULL))
  f <- file.path(out_dir, paste0(prefix, "_run_manifest.json"))
  tmp <- paste0(f, ".tmp")
  ok <- tryCatch({
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", force = TRUE), tmp, useBytes = TRUE)
    } else {
      dput(m, file = tmp)
    }
    file.rename(tmp, f)
  }, error = function(e) FALSE)
  invisible(ok)
}

#' Start the run manifest: identity, run date and its source, code versions, environment
init_run_manifest <- function(run_date_source = NA_character_) {
  g <- function(n) if (exists(n, envir = .GlobalEnv)) get(n, envir = .GlobalEnv) else NULL
  cfg <- g("current_model_cfg")
  env_vars <- c("CASCADE_CORE_PATH", "GAMBLE_HOME", "BMLEH_BETAS", "PRIOR_BETAS", "GAMBLE_MASTER_PARQUET",
                "GRIDWORK_DIR", "GAMBLE_GRIDWORK_DIR", "GAMBLE_DATA_ROOTS", "PROJECT_SLUG", "SCENARIO",
                "DOWNSCALE_RES", "DOWNSCALE_INTERSECT", "DOWNSCALE_OUT", "DOWNSCALE_YEARS", "DOWNSCALE_FORCE",
                "DOWNSCALE_MIN_NS_MATCH")
  ev <- Sys.getenv(env_vars, unset = NA)
  gamble_home <- Sys.getenv("GAMBLE_HOME", "../gamble-core")
  record_run_manifest("run", list(
    run_id = g("output_prefix"), model_name = g("model_name"), run_basename = g("run_basename"),
    run_date = g("RUN_DATE"), run_date_source = run_date_source, started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    machine = unname(Sys.info()[c("sysname", "nodename")]), r_version = R.version.string,
    working_dir = normalizePath(getwd(), winslash = "/")))
  record_run_manifest("pipeline_code", c(list(repo = "gamble-cascade"), .run_git_info(".")))
  record_run_manifest("submodule_pins", list(
    `external/cascade-core` = .submodule_pin("external/cascade-core"),
    `external/gamble-core` = .submodule_pin("external/gamble-core"),
    `.rules-hub` = .submodule_pin(".rules-hub")))
  if (exists(".CASCADE_ENGINE", envir = .GlobalEnv)) record_run_manifest("engine_cascade_core", get(".CASCADE_ENGINE", envir = .GlobalEnv))
  record_run_manifest("gamble_core_home", c(list(path = gamble_home, exists = dir.exists(gamble_home)), .run_git_info(gamble_home)))
  record_run_manifest("environment", as.list(ev[!is.na(ev)]))
  record_run_manifest("paths", g("paths"))
  record_run_manifest("config_hash", if (!is.null(cfg) && requireNamespace("digest", quietly = TRUE)) digest::digest(cfg) else NA_character_)
  invisible(get(".RUN_MANIFEST", envir = .GlobalEnv))
}

#' Close the run manifest: end time and the versions of all loaded packages
finalize_run_manifest <- function(stage = "run") {
  si <- utils::sessionInfo()
  pk <- c(si$otherPkgs, si$loadedOnly)
  record_run_manifest("packages", stats::setNames(lapply(pk, function(p) p$Version), names(pk))[order(names(pk))])
  record_run_manifest("finished", list(stage = stage, at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
  invisible(get(".RUN_MANIFEST", envir = .GlobalEnv))
}

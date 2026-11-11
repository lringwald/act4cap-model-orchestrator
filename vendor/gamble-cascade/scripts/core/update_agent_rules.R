#' ==============================================================================
#' update_agent_rules.R
#' 
#' Automated synchronization of coding agent rulebooks from agent-rules-hub:
#' https://github.com/lringwald/agent-rules-hub
#' 
#' Ensures that Claude Code and Antigravity rulebooks are fully synchronized
#' with the latest upstream standards before every rerun of the pipeline.
#' ==============================================================================

update_agent_rules <- function(project_root = ".", 
                               rules_repo = "https://github.com/lringwald/agent-rules-hub.git",
                               quiet = FALSE) {
  
  if (requireNamespace("cli", quietly = TRUE)) {
    log_info <- function(...) cli::cli_alert_info(...)
    log_success <- function(...) cli::cli_alert_success(...)
    log_warning <- function(...) cli::cli_alert_warning(...)
  } else {
    log_info <- function(msg, ...) message("INFO: ", sprintf(msg, ...))
    log_success <- function(msg, ...) message("SUCCESS: ", sprintf(msg, ...))
    log_warning <- function(msg, ...) warning("WARNING: ", sprintf(msg, ...))
  }
  
  # Resolve absolute project root
  target_dir <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
  if (!dir.exists(target_dir)) {
    target_dir <- getwd()
  }
  
  rules_hub_dir <- file.path(target_dir, ".rules-hub")
  
  if (!quiet) {
    log_info(sprintf("Checking agent rulebooks against %s...", rules_repo))
  }
  
  # Step 1: Ensure .rules-hub is cloned or updated
  git_available <- nzchar(Sys.which("git"))
  network_ok <- FALSE
  
  if (git_available) {
    if (dir.exists(file.path(rules_hub_dir, ".git"))) {
      # Try pulling latest
      res <- tryCatch({
        suppressWarnings(system2("git", c("-C", shQuote(rules_hub_dir), "pull", "origin", "main"), 
                                 stdout = TRUE, stderr = TRUE))
      }, error = function(e) NULL)
      
      status <- attr(res, "status")
      if (is.null(status) || status == 0) {
        network_ok <- TRUE
      } else {
        log_warning("Could not reach remote agent-rules-hub via git pull; using local cached copy.")
      }
    } else {
      # Try cloning or submodule init
      res <- tryCatch({
        suppressWarnings(system2("git", c("submodule", "update", "--init", "--recursive", ".rules-hub"),
                                 stdout = TRUE, stderr = TRUE))
      }, error = function(e) NULL)
      status <- attr(res, "status")
      if (is.null(status) || status == 0) {
        network_ok <- TRUE
      } else {
        # Fallback to direct shallow clone
        res_clone <- tryCatch({
          suppressWarnings(system2("git", c("clone", "--depth", "1", rules_repo, shQuote(rules_hub_dir)),
                                   stdout = TRUE, stderr = TRUE))
        }, error = function(e) NULL)
        status_clone <- attr(res_clone, "status")
        if (is.null(status_clone) || status_clone == 0) network_ok <- TRUE
      }
    }
  } else {
    log_warning("Git executable not found in PATH; verifying existing cached rules.")
  }
  
  rules_src_dir <- file.path(rules_hub_dir, "rules")
  if (!dir.exists(rules_src_dir)) {
    log_warning(sprintf("Rules directory not found at: %s. Using existing project rules.", rules_src_dir))
    return(invisible(FALSE))
  }
  
  # Step 2: Sync rulebooks into .claude/rules and .agents/rules
  claude_rules_dir <- file.path(target_dir, ".claude", "rules")
  agents_rules_dir <- file.path(target_dir, ".agents", "rules")
  
  if (!dir.exists(claude_rules_dir)) dir.create(claude_rules_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(agents_rules_dir)) dir.create(agents_rules_dir, recursive = TRUE, showWarnings = FALSE)
  
  rule_files <- list.files(rules_src_dir, pattern = "\\.md$", full.names = TRUE)
  if (length(rule_files) == 0) {
    log_warning("No markdown rulebooks found in .rules-hub/rules.")
    return(invisible(FALSE))
  }
  
  updated_count <- 0
  for (rf in rule_files) {
    bn <- basename(rf)
    dst_claude <- file.path(claude_rules_dir, bn)
    dst_agents <- file.path(agents_rules_dir, bn)
    
    # Check if content changed
    src_md5 <- tools::md5sum(rf)
    
    need_copy_claude <- !file.exists(dst_claude) || (tools::md5sum(dst_claude) != src_md5)
    need_copy_agents <- !file.exists(dst_agents) || (tools::md5sum(dst_agents) != src_md5)
    
    if (need_copy_claude) {
      if (tryCatch(file.copy(rf, dst_claude, overwrite = TRUE), warning = function(w) FALSE, error = function(e) FALSE)) {
        updated_count <- updated_count + 1
      }
    }
    if (need_copy_agents) {
      if (tryCatch(file.copy(rf, dst_agents, overwrite = TRUE), warning = function(w) FALSE, error = function(e) FALSE)) {
        updated_count <- updated_count + 1
      }
    }
  }
  
  # Step 3: Verify AGENTS.md and CLAUDE.md root indexes
  extract_title <- function(path) {
    lines <- readLines(path, n = 30, warn = FALSE)
    h1 <- grep("^#\\s+", lines, value = TRUE)
    if (length(h1) > 0) {
      sub("^#\\s+", "", h1[1])
    } else {
      tools::file_path_sans_ext(basename(path))
    }
  }
  
  agents_md_file <- file.path(target_dir, "AGENTS.md")
  claude_md_file <- file.path(target_dir, "CLAUDE.md")
  
  # Generate or update AGENTS.md
  agents_lines <- if (file.exists(agents_md_file)) readLines(agents_md_file, warn = FALSE) else character(0)
  if (length(agents_lines) == 0 || !any(grepl("# Antigravity Global Context", agents_lines))) {
    agents_lines <- c("# Antigravity Global Context", "", "## Domain Rules")
  }
  
  claude_lines <- if (file.exists(claude_md_file)) readLines(claude_md_file, warn = FALSE) else character(0)
  if (length(claude_lines) == 0 || !any(grepl("# Project Overview & Toolchain", claude_lines))) {
    claude_lines <- c("# Project Overview & Toolchain", "", "## Core Rules & Invariants")
  }
  
  for (rf in rule_files) {
    bn <- basename(rf)
    title <- extract_title(rf)
    
    ag_pattern <- sprintf(".agents/rules/%s", bn)
    if (!any(grepl(ag_pattern, agents_lines, fixed = TRUE))) {
      agents_lines <- c(agents_lines, sprintf("- %s: see `.agents/rules/%s` (activated via model decision).", title, bn))
    }
    
    cl_pattern <- sprintf(".claude/rules/%s", bn)
    if (!any(grepl(cl_pattern, claude_lines, fixed = TRUE))) {
      claude_lines <- c(claude_lines, sprintf("- %s: see `.claude/rules/%s`.", title, bn))
    }
  }
  
  tryCatch({
    writeLines(agents_lines, agents_md_file)
    writeLines(claude_lines, claude_md_file)
  }, warning = function(w) NULL, error = function(e) NULL)
  
  if (!quiet) {
    if (updated_count > 0) {
      log_success(sprintf("Agent rulebooks updated (%d files synced from agent-rules-hub).", updated_count))
    } else {
      log_success("Agent rulebooks are up to date with agent-rules-hub.")
    }
  }
  
  invisible(TRUE)
}

# Auto-execute if invoked as standalone script: Rscript scripts/core/update_agent_rules.R
if (sys.nframe() == 0) {
  update_agent_rules()
}


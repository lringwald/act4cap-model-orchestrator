# =============================================================================
# R/update_rules.R — Dynamic Rulebook Synchronization
# =============================================================================
# Checks for updates to agent rulebooks from https://github.com/lringwald/agent-rules-hub
# and synchronizes .agents/rules/ and .claude/rules/ before pipeline reruns.
# =============================================================================

update_agent_rules <- function(repo_dir = ".") {
  sh_script <- file.path(repo_dir, "tools", "update_rules.sh")
  if (file.exists(sh_script)) {
    tryCatch({
      if (.Platform$OS.type == "windows") {
        bash_cmd <- Sys.which("bash")
        if (nzchar(bash_cmd)) {
          system2(bash_cmd, args = c(sh_script), stdout = FALSE, stderr = FALSE)
        }
      } else {
        system2("bash", args = c(sh_script), stdout = FALSE, stderr = FALSE)
      }
    }, error = function(e) NULL)
  }
}

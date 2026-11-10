# =============================================================================
# init.R — dependency contract for Act4Cap Orchestrator
# =============================================================================
message('>>> [init.R] checking Act4Cap Orchestrator dependencies')

.repos <- getOption('repos')
.configured <- !is.null(.repos) && 'CRAN' %in% names(.repos) &&
               nzchar(.repos[['CRAN']]) && !identical(unname(.repos[['CRAN']]), '@CRAN@')
if (!.configured) {
  .code <- tryCatch({
    if (!file.exists('/etc/os-release')) stop('not linux')
    os <- readLines('/etc/os-release', warn = FALSE)
    gsub('"', '', sub('^VERSION_CODENAME=', '', grep('^VERSION_CODENAME=', os, value = TRUE)[1]))
  }, error = function(e) NA_character_)
  options(repos = if (!is.na(.code) && nzchar(.code) && .Platform$OS.type == 'unix')
            c(CRAN = sprintf('https://packagemanager.posit.co/cran/__linux__/%s/latest', .code))
          else c(CRAN = 'https://cloud.r-project.org/'))
}

PKGS <- c('data.table', 'terra', 'sf', 'arrow', 'qs2', 'jsonlite', 'cli', 'yaml', 'optparse', 'devtools', 'Rcpp', 'nloptr', 'progress', 'withr')
miss <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  message('>>> installing: ', paste(miss, collapse = ', '))
  install.packages(miss)
}

ok <- vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)
for (p in PKGS) message(sprintf('    %-15s %s', p, if (ok[[p]]) 'ok' else 'MISSING'))
message('    ', R.version.string)
if (!all(ok)) stop('dependencies missing: ', paste(PKGS[!ok], collapse = ', '))
message('>>> [init.R] all dependencies ok')

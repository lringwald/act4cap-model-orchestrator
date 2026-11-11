#!/usr/bin/env Rscript
# Thin wrapper: run THIS project's GAMBLE model spec (see spec.R in this folder).
# All orchestration lives in scripts/gamble_model_run.R -- a project contributes a SPEC.
#   Rscript scripts/AGMIP/gamble_model/run.R [--smoke] [--from-scratch] [--design-only]
a <- commandArgs(trailingOnly = TRUE)
r <- Sys.getenv("CASCADING_HOME", ".")
q <- system2("Rscript", c(file.path(r, "scripts", "gamble_model_run.R"), "--project", "AGMIP", a))
quit(save = "no", status = if (is.null(q) || is.na(q)) 1L else as.integer(q))

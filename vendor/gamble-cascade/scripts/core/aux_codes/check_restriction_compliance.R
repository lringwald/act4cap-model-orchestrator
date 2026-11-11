# ===============================================================
# check_restriction_compliance.R
# ===============================================================
# Post-solve check: no restricted transition may exceed its cap.
#
# A restriction row (ns, from_class, to_class, value = r) caps the flow
# from_class -> to_class in pixel ns at (1 - r) of the pixel's from_class area at
# the start of the period. The start area is the row sum of the land-use change
# output for (scenario, times, ns, from_class), which includes the stay rows
# (from_class == to_class). Rows without from_class (or with the engine's
# "__NA_LU__" placeholder) restrict every source; the scenario placeholder
# "__NA_SCEN__" matches every scenario; rows without `times` apply to every period.
#
# Returns list(violations = data.table, n_checked = integer, n_violations = integer).
# ===============================================================
require(data.table)

check_restriction_compliance <- function(luc, restrictions, scen_col = "scenario",
                                         tol_rel = 1e-6, tol_abs = 1e-6) {
  na_lu   <- if (exists("PLCHOLD_LU"))   PLCHOLD_LU   else "__NA_LU__"
  na_scen <- if (exists("PLCHOLD_SCEN")) PLCHOLD_SCEN else "__NA_SCEN__"
  empty <- list(violations = data.table(), n_checked = 0L, n_violations = 0L)
  if (is.null(restrictions) || nrow(restrictions) == 0 || nrow(luc) == 0) return(empty)

  luc <- as.data.table(luc)[, c(scen_col, "times", "ns", "from_class", "to_class", "value"), with = FALSE]
  setnames(luc, scen_col, ".scen")
  luc[, `:=`(ns = as.character(ns), times = as.character(times))]

  r <- copy(as.data.table(restrictions))
  if (!scen_col %in% names(r)) stop("check_restriction_compliance: restrictions lack the scenario column '", scen_col, "'.")
  setnames(r, scen_col, ".scen")
  r[, ns := as.character(ns)]
  if (!"from_class" %in% names(r)) r[, from_class := na_lu]
  has_times <- "times" %in% names(r)
  if (has_times) r[, times := as.character(times)]
  keys <- c(".scen", "ns", "from_class", "to_class", if (has_times) "times")
  r <- r[value > 0, .(r = max(value)), by = keys]
  if (nrow(r) == 0) return(empty)

  # Start-of-period source area per pixel, and the off-diagonal flows.
  src <- luc[, .(src_area = sum(value)), by = .(.scen, times, ns, from_class)]
  flows <- luc[from_class != to_class, .(flow = sum(value)), by = .(.scen, times, ns, from_class, to_class)]
  flows <- merge(flows, src, by = c(".scen", "times", "ns", "from_class"), all.x = TRUE)

  # Join flows to restrictions, honouring the wildcards.
  join_part <- function(rr, by_scen, by_from) {
    if (!nrow(rr)) return(NULL)
    on <- c("ns", "to_class", if (by_scen) ".scen", if (by_from) "from_class", if (has_times) "times")
    drop <- c(if (!by_scen) ".scen", if (!by_from) "from_class")
    rr <- rr[, setdiff(names(rr), drop), with = FALSE]
    flows[rr, on = on, nomatch = 0L]
  }
  wild_s <- r$.scen == na_scen
  wild_f <- r$from_class == na_lu
  hits <- rbindlist(list(
    join_part(r[!wild_s & !wild_f], by_scen = TRUE,  by_from = TRUE),
    join_part(r[ wild_s & !wild_f], by_scen = FALSE, by_from = TRUE),
    join_part(r[!wild_s &  wild_f], by_scen = TRUE,  by_from = FALSE),
    join_part(r[ wild_s &  wild_f], by_scen = FALSE, by_from = FALSE)
  ), use.names = TRUE, fill = TRUE)
  if (is.null(hits) || nrow(hits) == 0) return(list(violations = data.table(), n_checked = 0L, n_violations = 0L))

  # Several rows can restrict the same flow (e.g. pair rule and wildcard rule): the strictest applies.
  hits <- hits[, .(flow = flow[1], src_area = src_area[1], r = max(r)), by = .(.scen, times, ns, from_class, to_class)]
  hits[, cap := (1 - r) * src_area]
  hits[, excess := flow - cap]
  viol <- hits[excess > pmax(tol_abs, tol_rel * src_area)]
  setnames(viol, ".scen", scen_col)
  setorder(viol, -excess)
  list(violations = viol, n_checked = nrow(hits), n_violations = nrow(viol))
}

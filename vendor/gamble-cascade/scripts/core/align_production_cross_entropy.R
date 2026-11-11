# ===============================================================
# align_production_cross_entropy.R
# ===============================================================
# Constrained Empirical Bayes Variance Shrinkage + Cross-Entropy
# Production Downscaling (SPAM Standard)
#
# Aligns spatially explicit exogenous crop yield proxies with 
# regional production targets P_R and downscaled crop areas A_i
# under strict mass balance and realistic agronomic bounds:
#
#   min_{\tilde{Y}_i} \sum_{i \in R} A_i \tilde{Y}_i ln(\tilde{Y}_i / Y_i^prior)
#   s.t. \sum_{i \in R} A_i \tilde{Y}_i = P_R,  Y_min,i <= \tilde{Y}_i <= Y_max,i
#
# where Y_i^prior = w_i * Y_i + (1 - w_i) * Y_R
# ===============================================================

require(data.table)

#' Solve Constrained Cross-Entropy Yield Downscaling for a Single Group
#'
#' @param area Numeric vector of pixel areas A_i (> 0)
#' @param yield_exo Numeric vector of exogenous/proxy yields Y_i (>= 0)
#' @param target_prod Scalar regional production target P_R (>= 0)
#' @param w Numeric vector or scalar shrinkage weight in (0, 1]. If NULL,
#'   computed according to `shrinkage_mode`.
#' @param shrinkage_mode Character: "constant", "area_weighted", or "empirical_bayes".
#' @param c_min Lower bound multiplier of regional mean yield Y_R (default: 0.2)
#' @param c_max Upper bound multiplier of regional mean yield Y_R (default: 3.5)
#' @param y_min Explicit lower yield bound (scalar or vector). Overrides c_min if provided.
#' @param y_max Explicit upper yield bound (scalar or vector). Overrides c_max if provided.
#' @param tol Numerical tolerance for root finding (default: 1e-10)
#' @param max_iter Maximum iterations for root bracket expansion (default: 60)
#' @return List containing:
#'   - yield_final: Vector of calibrated pixel yields \eqn{\tilde{Y}_i}
#'   - yield_prior: Vector of shrunk prior yields \eqn{Y_i^{prior}}
#'   - theta: Scalar dual multiplier \eqn{\theta^*}
#'   - mass_balance_error: Relative production error |sum(A*Y) - P_R| / P_R
#'   - clamped_min: Number of pixels clamped at y_min
#'   - clamped_max: Number of pixels clamped at y_max
#'   - n_pixels: Total number of pixels
#' @export
solve_cross_entropy_yield <- function(area,
                                      yield_exo,
                                      target_prod,
                                      w = NULL,
                                      shrinkage_mode = c("constant", "area_weighted", "empirical_bayes"),
                                      c_min = 0.2,
                                      c_max = 3.5,
                                      y_min = NULL,
                                      y_max = NULL,
                                      tol = 1e-10,
                                      max_iter = 60) {
  shrinkage_mode <- match.arg(shrinkage_mode)
  n <- length(area)
  
  if (n == 0) {
    return(list(
      yield_final = numeric(0),
      yield_prior = numeric(0),
      theta = 1.0,
      mass_balance_error = 0,
      clamped_min = 0,
      clamped_max = 0,
      n_pixels = 0
    ))
  }
  
  # Replace any NAs or negative values in inputs
  area <- pmax(fifelse(is.na(area), 0, area), 0)
  yield_exo <- pmax(fifelse(is.na(yield_exo), 0, yield_exo), 0)
  target_prod <- if (is.na(target_prod) || target_prod < 0) 0 else target_prod
  
  tot_area <- sum(area)
  
  # Degenerate case 1: zero area or zero production target
  if (tot_area <= 1e-12 || target_prod <= 1e-12) {
    return(list(
      yield_final = rep(0, n),
      yield_prior = rep(0, n),
      theta = 0,
      mass_balance_error = 0,
      clamped_min = 0,
      clamped_max = 0,
      n_pixels = n
    ))
  }
  
  # Implied regional target mean yield: Y_R = P_R / A_R
  y_region <- target_prod / tot_area
  
  # -------------------------------------------------------------
  # 1. Compute Shrinkage Prior Y_i^prior = w_i * Y_i + (1 - w_i) * Y_R
  # -------------------------------------------------------------
  if (is.null(w)) {
    if (shrinkage_mode == "constant") {
      w_vec <- rep(0.7, n)
    } else if (shrinkage_mode == "area_weighted") {
      # Higher confidence for larger fields: w_i = A_i / (A_i + median(A))
      med_a <- median(area[area > 0])
      if (is.na(med_a) || med_a <= 0) med_a <- 1.0
      w_vec <- pmin(pmax(area / (area + med_a), 0.1), 0.95)
    } else if (shrinkage_mode == "empirical_bayes") {
      # Classical variance ratio shrinkage: w_i = s_R^2 / (s_R^2 + s_i^2)
      var_y <- var(yield_exo)
      if (is.na(var_y) || var_y <= 1e-12) {
        w_vec <- rep(0.7, n)
      } else {
        # Pixel noise estimated inversely proportional to pixel area
        scale_a <- mean(area[area > 0], na.rm = TRUE)
        w_vec <- pmin(pmax(area / (area + scale_a * 0.5), 0.1), 0.95)
      }
    }
  } else {
    if (length(w) == 1) {
      w_vec <- rep(w, n)
    } else {
      w_vec <- w
    }
  }
  w_vec <- pmin(pmax(w_vec, 0.01), 1.0)
  
  # Compute shrunk prior
  y_prior <- w_vec * yield_exo + (1 - w_vec) * y_region
  y_prior <- pmax(y_prior, 1e-12)
  
  # -------------------------------------------------------------
  # 2. Determine and Verify Bounds [Y_min, Y_max]
  # -------------------------------------------------------------
  if (is.null(y_min)) y_min <- c_min * y_region
  if (is.null(y_max)) y_max <- c_max * y_region
  
  if (length(y_min) == 1) y_min <- rep(y_min, n)
  if (length(y_max) == 1) y_max <- rep(y_max, n)
  
  y_min <- pmax(y_min, 0)
  y_max <- pmax(y_max, y_min + 1e-12)
  
  # Feasibility Envelope Check:
  # Mass balance requires: sum(A_i * y_min,i) <= P_R <= sum(A_i * y_max,i)
  min_prod <- sum(area * y_min)
  max_prod <- sum(area * y_max)
  
  if (target_prod < min_prod) {
    # If target is lower than min physical bound, scale y_min down proportionally
    y_min <- y_min * (target_prod / min_prod) * 0.999999
  }
  if (target_prod > max_prod) {
    # If target is higher than max physical bound, scale y_max up proportionally
    y_max <- y_max * (target_prod / max_prod) * 1.000001
  }
  
  # -------------------------------------------------------------
  # 3. Dual Root-Finding: g(theta) = sum(A * clip(theta * Y_prior, y_min, y_max)) - P_R = 0
  # -------------------------------------------------------------
  g <- function(theta) {
    sum(area * pmin(pmax(theta * y_prior, y_min), y_max)) - target_prod
  }
  
  # Initial guess based on unconstrained linear ratio
  unconstrained_prod <- sum(area * y_prior)
  theta_init <- if (unconstrained_prod > 1e-12) target_prod / unconstrained_prod else 1.0
  
  val_init <- g(theta_init)
  
  if (abs(val_init) <= tol * target_prod) {
    theta_star <- theta_init
  } else {
    th_low <- theta_init
    th_high <- theta_init
    
    if (val_init > 0) {
      # Target is smaller than eval(theta_init), need lower theta
      step <- 0.5
      for (iter in 1:max_iter) {
        th_low <- th_low * step
        if (g(th_low) <= 0) break
      }
      res <- stats::uniroot(g, lower = th_low, upper = th_high, tol = tol)
      theta_star <- res$root
    } else {
      # Target is larger than eval(theta_init), need higher theta
      step <- 2.0
      for (iter in 1:max_iter) {
        th_high <- th_high * step
        if (g(th_high) >= 0) break
      }
      res <- stats::uniroot(g, lower = th_low, upper = th_high, tol = tol)
      theta_star <- res$root
    }
  }
  
  # -------------------------------------------------------------
  # 4. Compute Final Optimal Yields & Diagnostics
  # -------------------------------------------------------------
  yield_final <- pmin(pmax(theta_star * y_prior, y_min), y_max)
  achieved_prod <- sum(area * yield_final)
  err <- abs(achieved_prod - target_prod) / target_prod
  
  clamped_min <- sum(abs(yield_final - y_min) < 1e-8 & area > 0)
  clamped_max <- sum(abs(yield_final - y_max) < 1e-8 & area > 0)
  
  list(
    yield_final = yield_final,
    yield_prior = y_prior,
    theta = theta_star,
    mass_balance_error = err,
    clamped_min = clamped_min,
    clamped_max = clamped_max,
    n_pixels = n
  )
}

#' High-Level data.table Interface for Cross-Entropy Production Alignment
#'
#' @param dt data.table containing pixel-level data
#' @param area_col Name of pixel area column (default: "area_km2")
#' @param yield_exo_col Name of exogenous/proxy yield column (default: "yield_prior")
#' @param target_prod_col Name of target production column (default: "target_prod_t")
#' @param group_cols Character vector of grouping columns (e.g. c("GLOB_country", "GLOB_CTYP", "GLOB_CSYS", "scenario", "year"))
#' @param out_yield_col Name of output yield column (default: "yield_final")
#' @param out_prod_col Name of output production column (default: "prod_final_t")
#' @param w Shrinkage weight (default: 0.7)
#' @param shrinkage_mode Shrinkage mode (default: "constant")
#' @param c_min Lower bound multiplier of regional mean yield (default: 0.2)
#' @param c_max Upper bound multiplier of regional mean yield (default: 3.5)
#' @param tol Convergence tolerance for dual solver (default: 1e-10)
#' @return Modifies dt in-place with out_yield_col, out_prod_col, scale_multiplier,
#'   and returns a diagnostic data.table summarizing each group's performance.
#' @export
align_production_cross_entropy_dt <- function(dt,
                                              area_col = "area_km2",
                                              yield_exo_col = "yield_prior",
                                              target_prod_col = "target_prod_t",
                                              group_cols = c("GLOB_country", "GLOB_CTYP", "GLOB_CSYS", "scenario", "year"),
                                              out_yield_col = "yield_final",
                                              out_prod_col = "prod_final_t",
                                              w = 0.7,
                                              shrinkage_mode = "constant",
                                              c_min = 0.2,
                                              c_max = 3.5,
                                              tol = 1e-10) {
  stopifnot(is.data.table(dt))
  
  req_cols <- c(area_col, yield_exo_col, target_prod_col, group_cols)
  missing_cols <- setdiff(req_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in dt: ", paste(missing_cols, collapse = ", "))
  }
  
  # Ensure target_prod_t has no NAs
  dt[is.na(get(target_prod_col)), (target_prod_col) := 0]
  dt[is.na(get(yield_exo_col)), (yield_exo_col) := 0]
  dt[is.na(get(area_col)), (area_col) := 0]
  
  # Solve per group in a single pass
  dt[, c("yield_calc", "theta_calc") := {
    res <- solve_cross_entropy_yield(
      area = get(area_col),
      yield_exo = get(yield_exo_col),
      target_prod = get(target_prod_col)[1],
      w = w,
      shrinkage_mode = shrinkage_mode,
      c_min = c_min,
      c_max = c_max,
      tol = tol
    )
    list(res$yield_final, res$theta)
  }, by = group_cols]
  
  setnames(dt, "yield_calc", out_yield_col)
  setnames(dt, "theta_calc", "scale_multiplier")
  
  # Compute final production
  dt[, (out_prod_col) := get(area_col) * get(out_yield_col)]
  
  # Generate summary diagnostics
  diag_dt <- dt[, .(
    n_pixels = .N,
    total_area_km2 = sum(get(area_col), na.rm = TRUE),
    target_prod_t = get(target_prod_col)[1],
    achieved_prod_t = sum(get(out_prod_col), na.rm = TRUE),
    theta_multiplier = scale_multiplier[1],
    min_yield = min(get(out_yield_col), na.rm = TRUE),
    max_yield = max(get(out_yield_col), na.rm = TRUE),
    mean_yield = fifelse(sum(get(area_col), na.rm = TRUE) > 0,
                         sum(get(out_prod_col), na.rm = TRUE) / sum(get(area_col), na.rm = TRUE), 0)
  ), by = group_cols]
  
  diag_dt[, rel_error := fifelse(target_prod_t > 0, abs(achieved_prod_t - target_prod_t) / target_prod_t, 0)]
  
  return(diag_dt)
}

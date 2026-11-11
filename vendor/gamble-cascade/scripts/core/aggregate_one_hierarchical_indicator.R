# ===============================================================
# aggregate_one_hierarchical_indicator.R
# ===============================================================
# Worker: computes hierarchical aggregates for ONE indicator TIFF and writes
# the combined output. Invoked as a subprocess (one per file) by
# aggregate_tiff_hierarchical_indicators.R — see that file for the full
# aggregation-semantics documentation. Isolated into its own process because
# terra's GDAL-backed objects leave enough per-file resource buildup (open
# dataset handles / C-level allocations not promptly reclaimed by R's gc())
# that after 2-3 files in one long-lived session, otherwise-small allocations
# start failing with std::bad_alloc — a fresh process means a fresh heap.
#
# Usage: Rscript codes/aggregate_one_hierarchical_indicator.R <tif_path> <output_dir> <reporting_unit>
# ===============================================================
require(terra)
require(cli)
require(data.table)

args <- commandArgs(trailingOnly = TRUE)
tif_path         <- args[1]
out_dir          <- args[2]
reporting_unit   <- if (length(args) >= 3) args[3] else "km2"
REPORTING_INTENSITY_UNIT <- reporting_unit

IND_START_KEYWORDS <- c("area", "yield", "prod", "scale", "total", "revenue", "profit",
                        "price", "cost", "cap", "n", "p", "k", "water", "meat", "milk",
                        "eggs", "grazing", "ch4", "n2o", "n20", "manure", "multi",
                        "bvmeat", "bvmilk", "sgmeat", "sgmilk", "pgmeat", "ptmeat", "pteggs",
                        "entferm", "manmgttot", "manapltot", "manprptot", "mansoltot")
INTENSIVE_PATTERN <- "^(yield_t_km2|scale_factor|multi_cropping_index|price_.*|cost_.*_km2|revenue_.*_km2|subsidy_.*_km2|profit_.*_km2|n_input_.*_km2|p_input_.*_km2|k_input_.*_km2|water_.*_km2|.*_lsu_per_head)$"

parse_indicator_bands <- function(lyr_names) {
  rbindlist(lapply(lyr_names, function(nm) {
    toks <- strsplit(nm, "_", fixed = TRUE)[[1]]
    if (length(toks) < 2 || !toks[1] %in% c("cropland", "livestock")) return(NULL)
    ind_pos <- which(toks[-1] %in% IND_START_KEYWORDS)
    if (length(ind_pos) == 0) return(NULL)
    s <- min(ind_pos) + 1L
    cls_toks <- if (s > 2) toks[2:(s - 1)] else character(0)
    data.table(band = nm, domain = toks[1],
               cls  = paste(cls_toks, collapse = "_"),
               tok1 = if (length(cls_toks) >= 1) cls_toks[1] else NA_character_,
               tok2 = if (length(cls_toks) >= 2) cls_toks[2] else NA_character_,
               ind  = paste(toks[s:length(toks)], collapse = "_"))
  }))
}

cli_alert_info(sprintf("[worker] Processing: %s", basename(tif_path)))

r <- terra::rast(tif_path)
parsed <- parse_indicator_bands(names(r))
if (is.null(parsed) || nrow(parsed) == 0) {
  cli_alert_warning("[worker] No layers match the indicator naming convention. Skipping.")
  quit(status = 0)
}
unmatched <- setdiff(names(r), parsed$band)
if (length(unmatched) > 0)
  cli_alert_warning(sprintf("[worker] %d band(s) not parseable and left as-is: %s",
                            length(unmatched), paste(head(unmatched, 5), collapse = ", ")))

r_data <- terra::crop(r, terra::trim(r[[1]]))
na_mask <- is.na(terra::values(r_data[[1]]))[, 1]

agg_tmp_dir <- file.path(tempdir(), paste0("hier_agg_", tools::file_path_sans_ext(basename(tif_path))))
if (dir.exists(agg_tmp_dir)) unlink(agg_tmp_dir, recursive = TRUE)
dir.create(agg_tmp_dir, recursive = TRUE)
agg_paths <- character(0)
written_names <- character(0)
src_names <- names(r_data)

band_cache <- new.env(parent = emptyenv())
get_band <- function(nm) {
  if (!is.null(band_cache[[nm]])) return(band_cache[[nm]])
  v <- if (nm %in% src_names) {
    terra::values(r_data[[nm]])[, 1]
  } else if (nm %in% written_names) {
    safe_fn <- paste0(gsub("[|:?*\"<>/\\]", "_", nm), ".tif")
    terra::values(terra::rast(file.path(agg_tmp_dir, safe_fn)))[, 1]
  } else {
    return(NULL)
  }
  band_cache[[nm]] <- v
  v
}
row_sum <- function(names_vec) {
  Reduce(function(acc, nm) {
    v <- get_band(nm)
    if (is.null(v)) return(acc)
    v[is.na(v)] <- 0
    if (is.null(acc)) v else acc + v
  }, names_vec, NULL)
}

is_intensive <- function(ind_name) grepl(INTENSIVE_PATTERN, ind_name)
area_ind_of  <- function(domain) if (domain == "livestock") "lsu_heads" else "area_km2"

weight_band <- function(dm, cl, ind_name) {
  cand <- if (grepl("^price", ind_name)) c("prod_t", area_ind_of(dm)) else area_ind_of(dm)
  for (w_ind in cand) {
    wb <- parsed[domain == dm & cls == cl & ind == w_ind, band]
    if (length(wb) == 1) return(wb)
  }
  NA_character_
}

clean_hier_name <- function(domain, tok) {
  if (is.na(tok) || tok == "") return(domain)
  if (grepl("^(crp|for|grs|oth|urb|wet)", tok) || grepl("|", tok, fixed = TRUE)) {
    tok
  } else {
    paste(domain, tok, sep = "_")
  }
}

# Exclude residual/outside _other classes from top-level domain totals (e.g. cropland_other from cropland_*)
hier_groups <- list(
  parsed[!grepl("(^|_)other$", cls, ignore.case = TRUE), .(band, domain, cls, ind, gname = domain)],
  parsed[!is.na(tok1), .(band, domain, cls, ind, gname = vapply(tok1, function(t) clean_hier_name(domain[1], t), character(1)))],
  parsed[!is.na(tok2), .(band, domain, cls, ind, gname = vapply(tok2, function(t) clean_hier_name(domain[1], t), character(1)))]
)

# For any token containing pipe '|', expand all ancestor hierarchy prefix levels
pipe_rows <- parsed[(grepl("|", tok1, fixed = TRUE) | grepl("|", tok2, fixed = TRUE))]
if (nrow(pipe_rows) > 0) {
  pipe_list <- lapply(seq_len(nrow(pipe_rows)), function(i) {
    p_tok2 <- pipe_rows$tok2[i]
    p_tok1 <- pipe_rows$tok1[i]
    p_dom  <- pipe_rows$domain[i]
    pipe_tok <- if (!is.na(p_tok2) && grepl("|", p_tok2, fixed = TRUE)) p_tok2 else p_tok1
    parts <- strsplit(pipe_tok, "|", fixed = TRUE)[[1]]
    if (length(parts) > 1) {
      prefixes <- vapply(seq_along(parts), function(k) paste(parts[1:k], collapse = "|"), character(1))
      gnames <- vapply(prefixes, function(p) clean_hier_name(p_dom, p), character(1))
      data.table(
        band   = pipe_rows$band[i],
        domain = p_dom,
        cls    = pipe_rows$cls[i],
        ind    = pipe_rows$ind[i],
        gname  = gnames
      )
    } else {
      NULL
    }
  })
  hier_groups <- c(hier_groups, pipe_list)
}
groups <- unique(rbindlist(hier_groups))

add_layer <- function(vals, nm) {
  if (nm %in% src_names || nm %in% written_names) return(invisible(NULL))
  vals[na_mask] <- NA
  r1 <- r_data[[1]]
  terra::values(r1) <- vals
  names(r1) <- nm
  safe_fn <- paste0(gsub("[|:?*\"<>/\\]", "_", nm), ".tif")
  p <- file.path(agg_tmp_dir, safe_fn)
  terra::writeRaster(r1, p, overwrite = TRUE)
  agg_paths     <<- c(agg_paths, p)
  written_names <<- c(written_names, nm)
  band_cache[[nm]] <- vals
}

for (gn in unique(groups$gname)) {
  g <- groups[gname == gn]
  for (ind_name in unique(g$ind)) {
    member <- g[ind == ind_name]
    agg_name <- paste(gn, ind_name, sep = "_")
    if (is_intensive(ind_name)) {
      wb <- vapply(seq_len(nrow(member)),
                   function(i) weight_band(member$domain[i], member$cls[i], ind_name),
                   character(1))
      ok <- !is.na(wb)
      if (!any(ok)) next
      num <- Reduce(`+`, Map(function(bnd, w) {
        bv <- get_band(bnd); wv <- get_band(w)
        bv[is.na(bv)] <- 0; wv[is.na(wv)] <- 0
        bv * wv
      }, member$band[ok], wb[ok]))
      
      # Option C: Only include area in denominator where the crop actually has positive values
      # so unmodeled / zero-intensity crops do not dilute parent average yield/intensity
      den <- Reduce(`+`, Map(function(bnd, w) {
        bv <- get_band(bnd); wv <- get_band(w)
        bv[is.na(bv)] <- 0; wv[is.na(wv)] <- 0
        ifelse(bv > 1e-9, wv, 0)
      }, member$band[ok], wb[ok]))
      add_layer(ifelse(den > 1e-9, num / den, 0), agg_name)
    } else {
      add_layer(row_sum(member$band), agg_name)
    }
  }
}

for (ind_name in unique(parsed$ind)) {
  if (is_intensive(ind_name)) next
  dl <- paste(unique(parsed$domain), ind_name, sep = "_")
  pieces <- Filter(Negate(is.null), lapply(dl, get_band))
  if (length(pieces) == 0) next
  tot_name <- if (grepl("^total_", ind_name)) ind_name else paste0("total_", ind_name)
  add_layer(Reduce(function(a, b) {
    s <- a; miss <- is.na(s) & !is.na(b); s[miss] <- 0
    b2 <- b; b2[is.na(b2)] <- 0
    s + b2
  }, pieces), tot_name)
}

if (reporting_unit == "ha") {
  area_conv <- 100; int_suffix <- "_per_ha"
} else {
  area_conv <- 1;   int_suffix <- "_per_km2"
}
per_area_targets <- c(unique(groups$gname), "total")
for (gn in per_area_targets) {
  dm <- sub("_.*", "", gn)
  a_ind <- if (gn == "total") "area_km2" else area_ind_of(dm)
  v_area <- get_band(paste(gn, a_ind, sep = "_"))
  if (is.null(v_area)) next
  cand_inds <- unique(c(groups[gname == gn, ind], if (gn == "total") unique(parsed$ind)))
  for (ind_name in cand_inds) {
    if (is_intensive(ind_name)) next
    if (grepl("^area", ind_name)) next
    src_ind_name <- if (gn == "total" && grepl("^total_", ind_name)) ind_name else paste(gn, ind_name, sep = "_")
    num_v <- get_band(src_ind_name)
    if (is.null(num_v)) next
    
    # Calculate active area for the denominator:
    member <- groups[gname == gn & ind == ind_name]
    den_area <- if (nrow(member) > 1) {
      wb <- vapply(seq_len(nrow(member)),
                   function(i) weight_band(member$domain[i], member$cls[i], ind_name),
                   character(1))
      ok <- !is.na(wb)
      if (any(ok)) {
        Reduce(`+`, Map(function(bnd, w) {
          bv <- get_band(bnd); wv <- get_band(w)
          bv[is.na(bv)] <- 0; wv[is.na(wv)] <- 0
          ifelse(bv > 1e-9, wv, 0)
        }, member$band[ok], wb[ok]))
      } else {
        v_area
      }
    } else {
      v_area
    }
    
    clean_ind <- sub("^total_", "", ind_name)
    rate_layer_name <- if (gn == "total") {
      paste0("total_", clean_ind, int_suffix)
    } else {
      paste0(gn, "_", clean_ind, int_suffix)
    }
    
    add_layer(ifelse(!is.na(den_area) & den_area > 1e-6, num_v / (den_area * area_conv), 0),
              rate_layer_name)
  }
}

if (length(agg_paths) > 0) {
  r_agg <- terra::rast(agg_paths)
  names(r_agg) <- written_names
  r_agg <- terra::extend(r_agg, r[[1]])
  out_name <- gsub("_indicators_", "_indicators_hierarchical_", basename(tif_path))
  out_path <- file.path(out_dir, out_name)
  terra::writeRaster(c(r, r_agg), out_path, overwrite = TRUE)
  cli_alert_success(sprintf("[worker] Saved %d aggregate layer(s) to: %s", length(agg_paths), out_name))
}
unlink(agg_tmp_dir, recursive = TRUE)
quit(status = 0)

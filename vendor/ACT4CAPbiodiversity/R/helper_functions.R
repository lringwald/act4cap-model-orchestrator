#' convert_spatial_data
#'
#' The function transforms spatial datasets from different units
#' 
#' @param from_col character string. colname from mapping file
#' @param to_col character string. colname from mapping file
#' @param ext_cols character string. colname from dt of absolute variables
#' @param int_cols character string. colname from dt of ratio variables (wmean)
#' @param keep_cols character string. colname from dt to keep 
#' @param dt datatable. initial datatable
#' @param mg datatable. mapping
#' 
#' 
#' 
#' @return dt at the target level (to_col)
#' @import data.table
#' @import arrow
#' @import tidyverse
#' 
#' 
#' @export
#' 

convert_spatial_data <- function(from_col = "GLOB_SimUID",   # or "CAPRI_NUTS", "NUTS2", "GLOB_5arcminID", etc.
                                 to_col   = "EEA_10kmID",    # or "EEA_1kmID", "EEA_5kmID", "EEA_50kmID", etc.
                                 ext_cols = NULL,
                                 int_cols = NULL,
                                 keep_cols = NULL,
                                 dt ,
                                 mg){
  
  # int_cols <- c("forest_share","nonforest_share")
  # dt <- f_nf_share_SimU %>% rename("GLOB_SimUID" = "ns") %>% as.data.table()
  # mapping_path <- "Data-raw/one_kmID_master_mapping_2026-08-12.parquet"
  
  # 1. Read only the needed columns from the master mapping
# mg <- as.data.table(read_parquet(
#   mapping_path,
#   col_select = c(from_col, to_col, "GLOB_5arcminID_area_km2"),
#   memory_map = FALSE
# ))[!is.na(get(from_col)) & get(from_col) != "GLOB_NA" & !is.na(get(to_col))]

# 2. Sum the pre-computed 1km intersection areas (GLOB_5arcminID_area_km2) per (from_col, to_col)
weights_dt <- mg[, .(area_km2 = sum(GLOB_5arcminID_area_km2, na.rm = TRUE)), by = c(from_col, to_col)]
weights_dt[, w_disagg := area_km2 / sum(area_km2), by = c(from_col)] # sums to 1 per SimUID (extensive / mass-conserving)
weights_dt[, w_mean   := area_km2 / sum(area_km2), by = c(to_col)]   # sums to 1 per 10km cell (intensive / area-weighted mean)

# 3. Map `dt` (keyed by GLOB_SimUID) to `to_col` (e.g. EEA_10kmID)
dt[, (from_col) := as.character(get(from_col))]
weights_dt[, (from_col) := as.character(get(from_col))]
# out_dt <- merge(dt, weights_dt, by = from_col, allow.cartesian = TRUE)[
#   , .(
#     ext_val = sum(ext_val * w_disagg, na.rm = TRUE), # e.g. area, production, total N
#     int_val = sum(int_val * w_mean,   na.rm = TRUE)  # e.g. yield, fertilizer rate
#   ),
#   by = c(to_col)
# ]

m <- merge(dt, weights_dt, by = from_col, allow.cartesian = TRUE)

out <- NULL
if (length(ext_cols)) {
  out <- m[, lapply(.SD, function(x) sum(x * w_disagg, na.rm = TRUE)),
           by = c(to_col, keep_cols), .SDcols = ext_cols]
}
if (length(int_cols)) {
  int <- m[, lapply(.SD, function(x) sum(x * w_mean, na.rm = TRUE)),
           by = c(to_col, keep_cols), .SDcols = int_cols]
  out <- if (is.null(out)) int else merge(out, int, by = c(to_col, keep_cols))
}

return(out)
}


#' checks
#'
#' The function checks consistency in land use data (no increase over time)
#' 
#' @param land_data dataframe with land data (columns REGION, ns, times, LC)
#' 
#' 
#' @return warnings
#' @import tidyverse
#' 
#' 
#' @export
#' 


test_LUdata <- function(land_data){
  
  # Checks - to be checked - currently fails
  check_lu <- land_data %>% filter(value>0) %>%
    group_by(REGION, ns, times, LC) %>%
    summarise(value=sum(value, na.rm=TRUE), .groups = "drop") %>%
    ungroup() %>%
    pivot_wider(names_from = "LC") %>%
    mutate(total = rowSums(across(-c(REGION,ns, times)), na.rm=TRUE))%>%
    group_by(REGION, ns) %>%
    arrange(times, .by_group = TRUE) %>%
    mutate(change_vs_first = total - first(total),
           change_vs_prev  = total - lag(total)) %>%
    ungroup()
  
  # check 1: change vs first
  chk1 <- all.equal(max(check_lu$change_vs_first), 0, tolerance = 1e-6)
  if (!isTRUE(chk1)) {
    warning("check_lu: change_vs_first differs from 0: ", paste(chk1, collapse = "; "),
            call. = FALSE)
  }
  
  # check 2: change vs previous
  chk2 <- all.equal(max(check_lu$change_vs_prev, na.rm = TRUE), 0, tolerance = 1e-6)
  if (!isTRUE(chk2)) {
    warning("check_lu: change_vs_prev differs from 0: ", paste(chk2, collapse = "; "),
            call. = FALSE)
  }
  
}

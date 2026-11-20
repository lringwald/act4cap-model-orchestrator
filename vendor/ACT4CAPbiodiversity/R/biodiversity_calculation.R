
#' get_abdlnd
#'
#' The function calculates abandonned land use categories based on luc data.
#' 
#' @param data dataframe. Land use change data
#' @param LUmapping boolean. if TRUE, calculates BII
#' @param resolution "EEA_10kmID" # or "EEA_1kmID", "EEA_5kmID", "EEA_50kmID", etc.
#' 
#' @return Land use including abnlnd & restored categories
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
#' 

get_abdlnd <- function(data,
                       LUmapping,
                       resolution = "EEA_10kmID"){
  
  if (!exists("agmip_lc_class", envir = environment(), inherits = TRUE)) {
    utils::data("agmip_lc_class", package = "ACT4CAPbiodiversity", envir = environment())
  }
  if(LUmapping == "AGMIP"){
    lc_class <- agmip_lc_class
    natcat <- c("OLD")
  }
  
  first_year <- data %>% filter(value > 0 ) %>%
    pull(year) %>%
    min()
  # 
  # f_nf_share <- convert_spatial_data(from_col = "GLOB_SimUID",
  #                                    to_col = "EEA_10kmID",
  #                                    int_cols = c("forest_share","nonforest_share"),
  #                                    dt = f_nf_share_SimU %>% rename("GLOB_SimUID" = "ns") %>% as.data.table(),
  #                                    mapping_path = "Data-raw/one_kmID_master_mapping_2026-08-12.parquet") %>% 
  #   rename("ns" = all_of(resolution))
  
  results <- data %>% 
    rename("lu.from" ="from_class",
           "lu.to" = "to_class",
           "REGION" = "Ns_reg",
           "times" = "year") %>% 
    filter(value != 0)
  
  # Checks
  test_LUdata(results %>% rename("LC"="lu.to"))
  
  natlnd <- results %>% filter(lu.to %in% natcat) %>%
    tidyr::complete(REGION = unique(results$REGION),
                    times  = unique(results$times),
                    ns     = unique(results$ns),lu.from,lu.to) %>%
    replace_na(list(value=0)) %>%
    group_by(REGION,times,ns) %>%
    summarise(value=sum(value, na.rm=TRUE), .groups = "drop") %>% ungroup()
  
  natlnd_converted <- results %>% filter(lu.from %in% natcat, !(lu.to %in% natcat)) %>%
    tidyr::complete(times  = unique(results$times),
                    ns     = unique(results$ns),lu.from,lu.to) %>% replace_na(list(value=0)) %>%
    group_by(times,ns) %>% summarise(value=sum(value, na.rm = TRUE), .groups = "drop") %>% ungroup() %>%
    group_by(ns) %>% reframe(times=times,value=cumsum(value)) %>% ungroup()
  
  # Abandoned cropland
  abn_crplnd <- results %>% filter(str_detect("^CRP", lu.from), 
                                   lu.to == natcat) %>%
    mutate(lu.from ="CRP") %>% group_by(REGION,ns,lu.from, lu.to, times) %>% 
    summarise(value = sum(value, na.rm=TRUE)) %>% ungroup() %>% 
    tidyr::complete(REGION = unique(results$REGION),
                    times  = unique(results$times),
                    ns     = unique(results$ns),lu.from,lu.to) %>% 
    replace_na(list(value=0)) %>% 
    group_by(REGION,ns,lu.from) %>% reframe(times=times,value=cumsum(value)) %>%
    mutate(lu.to="AbnCrpLnd") %>% relocate(times, .before=ns)
  
  # Abandoned grassland
  abn_grslnd <- results %>% filter(str_detect("^GRS", lu.from), lu.to %in% natcat) %>%
    tidyr::complete(REGION = unique(results$REGION),
                    times  = unique(results$times),
                    ns     = unique(results$ns),lu.from,lu.to) %>% replace_na(list(value=0))%>%
    group_by(REGION,ns,lu.from) %>% reframe(times=times,value=cumsum(value)) %>%
    mutate(lu.to="AbnGrsLnd") %>% relocate(times, .before=ns)
  
  # Abandoned managed forests
  abn_mngfor <- results %>% filter(str_detect("^FOR", lu.from),lu.to %in% natcat)  %>%
    mutate(lu.from ="FOR") %>% group_by(REGION,ns,lu.from, lu.to, times) %>% 
    summarise(value = sum(value, na.rm=TRUE)) %>% ungroup() %>% 
    tidyr::complete(REGION = unique(results$REGION),
                    times  = unique(results$times),
                    ns     = unique(results$ns),lu.from,lu.to) %>% replace_na(list(value=0))%>%
    group_by(REGION,ns, lu.from) %>% reframe(times=times,value=cumsum(value)) %>%
    mutate(lu.to="AbnMngFor") %>% relocate(times, .before=ns) %>%
    group_by(REGION,times,ns,lu.from,lu.to) %>% summarise(value=sum(value), .groups = "drop")
  
  # Merge land covers and correct OthNatLnd
  natlnd_aux <- natlnd %>% rename(ONV=value) %>%
    left_join(abn_crplnd %>% rename(AbnCrpLnd=value) %>% ungroup %>% dplyr::select(-c(lu.to,lu.from)), by=c("REGION", "times", "ns")) %>%
    left_join(abn_grslnd %>% rename(AbnGrsLnd=value) %>% ungroup %>% dplyr::select(-c(lu.to,lu.from)), by=c("REGION", "times", "ns")) %>%
    left_join(abn_mngfor %>% rename(AbnMngFor=value) %>% ungroup %>% dplyr::select(-c(lu.to,lu.from)), by=c("REGION", "times", "ns")) %>%
    left_join(natlnd_converted %>% rename(converted=value) %>% ungroup, by=c("times", "ns")) %>%
    replace_na(list(AbnCrpLnd=0,AbnGrsLnd=0,AbnMngFor=0,converted=0)) 
  # select(REGION, times, ns, converted, contains("Abn"), OthNatLnd) %>%
  # mutate(balance=OthNatLnd-(AbnCrpLnd+AbnGrsLnd+AbnPltFor+AbnMngFor))
  
  # Rebalance cells with more abandoned land than natlnd
  ini_col <- 4
  ncols <- ncol(natlnd_aux)
  
  natlnd_aux[,ini_col:ncols] <- apply(natlnd_aux[,ini_col:ncols],1,function(x) rebalance(x)) %>% t()
  
  # Combined natlnd and abandoned areas
  natlnd_cons <- natlnd_aux %>% dplyr::select(-converted) %>%
    mutate(ONV=ONV-(AbnCrpLnd+AbnGrsLnd+AbnMngFor)) %>%
    gather(lu.to,value,-c(REGION,times,ns)) %>% filter(value != 0)
  #%>% replace(. < 0, 0)
  
# Restored is assumed to be transitions to Forest primary in forest biomes
  restored <- results %>% filter(lu.to == "FOR|PRIM" & lu.from != "FOR|PRIM") %>% 
    group_by(REGION,times,ns) %>%
    summarise(value=sum(value,na.rm=TRUE)) %>% ungroup() %>% 
    # SRP: for now exclude forest share condition, otherwise transitions to for prim 
    # get higher bii if they're not in forest biomes
    # left_join(f_nf_share)  %>%
    # mutate(value=value*forest_share) %>%
    # dplyr::select(-c(forest_share,nonforest_share)) %>% 
    arrange(ns,times) %>%
    spread(times,value) %>% drop_na()
  
  if(nrow(restored) > 0){
    restored <- restored %>% 
      pivot_longer(names_to = "times", values_to = "value", cols=-c("REGION", "ns")) %>%
      mutate(lu.to ="restored", times= as.numeric(times))%>%
      group_by(REGION, ns) %>%
      arrange(times, .by_group = TRUE) %>%
      mutate(value = cumsum(value)) %>%
      ungroup()
  }
  
  forest_new <- results %>% filter(lu.to == "FOR|PRIM") %>%
    #select(-lu.from) %>%
    #tidyr::complete(REGION,times,ns,lu.from,lu.to) %>% 
    #replace_na(list(value=0)) %>%
    group_by(REGION,times,ns,lu.to) %>% summarise(value=sum(value)) %>%
    rbind(restored) %>%
    group_by(REGION,times,ns, lu.to) %>%
    summarise(value = sum(value, na.rm=TRUE)) %>%
    ungroup() %>%
    pivot_wider(id_cols = c(1:3),names_from = "lu.to", values_from = "value") %>%
    replace(is.na(.), 0)   %>% replace(. < 0, 0) %>%
    {for (col in c("FOR|PRIM", "restored")) {
        if (!col %in% names(.)) .[[col]] <- 0}
      .} %>% 
    group_by(REGION,times,ns) %>%
    mutate(`FOR|PRIM` = `FOR|PRIM` - restored,
           restored = ifelse(`FOR|PRIM` < 0, restored + `FOR|PRIM`, restored),
           `FOR|PRIM` = ifelse(`FOR|PRIM` < 0 , 0, `FOR|PRIM`)) %>%
    ungroup() %>%
    gather(lu.to,value,-c( REGION,times,ns)) 
    # # SRP : there are some cases where restored land decreases so value < 0
    # mutate(lu.to = ifelse(value < 0, "restored", lu.to),
    #        value = ifelse(value < 0, -value, value))
  
  initial <- results %>% filter(times==first_year) %>%
    group_by(REGION,times,ns,lu.from) %>% summarise(value=sum(value)) %>%
    spread(lu.from,value) %>%
    gather(LC,value,-c(REGION,times,ns)) %>%
    mutate(times=first_year-10)
  
  results_final <- results %>% filter(! lu.to %in% c("OLD","FOR|PRIM")) %>%
    group_by(REGION,times,ns,lu.to) %>% summarise(value=sum(value)) %>% ungroup() %>%
    rbind(natlnd_cons, forest_new) %>%
    rename("LC"="lu.to") %>% 
    rbind(initial) %>%
    mutate(ns = as.integer(ns)) %>% 
    filter(value != 0)
  
  # Checks
  test_LUdata(results_final)
  
  return(results_final)
}

#' rebalance
#' Auxiliary function to rescale the amount
#' of abandoned land per period, according
#' to the total availability of OthNatLnd
#' @param x data
#' 
#' @import dplyr
#' @import tidyr
#' @export
#' 
rebalance <- function(x){
  
  n <- length(x)
  
  tot_abn <- sum(x[2:(n-1)])
  
  if(tot_abn > x[n]){
    
    shares <- x[2:(n-1)] / tot_abn
    delta <- x[n] * shares
    x[2:(n-1)] <- x[2:(n-1)] - delta
    
  } else {
    dif <- x[n] - tot_abn
    x[2:(n-1)] <- 0
    #x[1] <- x[1] - dif
  }
  
  return(x)
}

#' process_csar
#' Function to add cSAR coefficients  and derive impacts
#' @param x data
#' 
#' @import dplyr
#' @import tidyr
#' @export
process_csar <- function(x,lc_class, eco_map, csar_coefs){
  
  missing_lc <- x %>%
    filter(value > 0) %>%
    group_by(LC) %>% summarise(area = sum(value), .groups = "drop") %>%
    anti_join(lc_class, by = "LC")
  
  if (nrow(missing_lc) > 0) {
    warning("LC classes not found in lc_class: ",
            paste(missing_lc$LC, collapse = ", "),
            " | area affected: ", round(sum(missing_lc$area)), call. = FALSE)
  }
  out <- x %>%
    left_join(lc_class %>% select(LC, "LUclass"="LC_csar"), by="LC") %>% 
    filter(! LC %in% c( "forest_new_ha","forest_old_ha")) %>% 
    left_join(eco_map, by="ns") %>%
    left_join(csar_coefs, by=c("ecoregion", "LUclass")) %>%
    mutate(value=ifelse(is.na(value),0,value)) %>% filter(value > 0) %>%
    mutate(CF=CF/10^3) %>%
    mutate(extinctions=value*CF*simu_share_in_eco,
           value= value*simu_share_in_eco) 
  
}

#' process_bii
#' Function to add BII coefficients and derive impacts
#' @param x data
#' 
#' @import dplyr
#' @import stringr
#' @export
process_bii <- function(x, lc_class, bii_coefs){
  
  missing_lc <- x %>%
    filter(value > 0) %>%
    group_by(LC) %>% summarise(area = sum(value), .groups = "drop") %>%
    anti_join(lc_class, by = "LC")
  
  if (nrow(missing_lc) > 0) {
    warning("LC classes not found in lc_class: ",
            paste(missing_lc$LC, collapse = ", "),
            " | area affected: ", round(sum(missing_lc$area)), call. = FALSE)
  }
  
  out <- x %>%
    left_join(lc_class %>% select(-LU_class, -LC_csar), by="LC") %>% rename(LUclass=LC_bii) %>% 
    gather(pnv,f_nf_share,-c(REGION,times,ns,LC,LUclass,value,pot_npp)) %>%
    mutate(LUclass = case_when(
      LUclass %in% c("forest_managed", "forest_unmanaged") ~ LUclass,
      str_detect(LUclass, "restored_f") ~ LUclass,
      pnv == "forest_share" ~ paste0(LUclass,"_f"),
      pnv == "nonforest_share"  ~  paste0(LUclass, "_nf"))) %>% 
    filter(! LC %in% c( "forest_new_ha","forest_old_ha")) %>%
    left_join(bii_coefs, by=c("LUclass"))   %>%
    mutate(value=ifelse(is.na(value),0,value)) %>% filter(value > 0, f_nf_share > 0)
  
  return(out)
}

#' get_biodiversity
#'
#' The function calculates impact on biodiversity based on BII and CSAR methodology.
#' 
#' @param data dataframe. Land use per land use category for each timestep and per SimU
#' @param scen integer. scenario
#' @param get_bii boolean. if TRUE, calculates BII
#' @param get_csar boolean. if TRUE, calculates CSAR
#' @param start_restored_lnd integer. First year with restoration. Before that year restoration will be set to 0
#' @param start_abn_land integer. First year with abandoned land Before that year abandoned land will be set to 0
#' 
#' @return biodiversity impact
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import data.table
#' @import arrow
#' @import purrr
#' @import tibble
#' 
#' @export

get_biodiversity <- function(data,
                             scen,
                             get_bii,
                             get_csar,
                             LUmapping = "AGMIP",
                             resolution="EEA_10kmID",
                             path_mapping # "Data-raw/one_kmID_master_mapping_2026-08-12.parquet"
                             ){
  
  for (ds in c("f_nf_share_SimU", "pot_npp_SimU", "eco_map_SimU", "bii_coefs", "csar_coefs", "agmip_lc_class")) {
    if (!exists(ds, envir = environment(), inherits = TRUE)) {
      utils::data(list = ds, package = "ACT4CAPbiodiversity", envir = environment())
    }
  }
  
  mapping <- as.data.table(read_parquet(path_mapping,
    col_select = c("GLOB_SimUID", resolution, "GLOB_5arcminID_area_km2"),
    memory_map = FALSE
  ))[!is.na(get("GLOB_SimUID")) & !get("GLOB_SimUID") %in% c("GLOB_NA", "0") & !is.na(get(resolution))]
  
  f_nf_share <- convert_spatial_data(from_col = "GLOB_SimUID",
                                     to_col = resolution,
                                     int_cols = c("forest_share","nonforest_share"),
                                     dt = f_nf_share_SimU %>% rename("GLOB_SimUID" = "SimUID") %>% as.data.table(),
                                     mg = mapping) %>% 
    rename("ns" = all_of(resolution))
  
   pot_npp <-  convert_spatial_data(from_col = "GLOB_SimUID",
                                    to_col = resolution,
                                    int_cols = "pot_npp",
                                    dt = pot_npp_SimU %>% rename("GLOB_SimUID" = "SimUID") %>% as.data.table(),
                                    mg = mapping) %>% 
     rename("ns" = all_of(resolution))
   
   
    eco_map <- convert_spatial_data(from_col = "GLOB_SimUID",
                                    to_col = resolution,
                                    int_cols = "simu_share_in_eco",
                                    dt = eco_map_SimU %>% rename("GLOB_SimUID" = "ns") %>% as.data.table(),
                                    mg = mapping,
                                    keep_cols = "ecoregion") %>% 
      rename("ns" = all_of(resolution))
  
  if(LUmapping == "AGMIP"){
    lc_class <- agmip_lc_class
  }
  
  # # ## Data initialisation - test - type="LU"
   #one_drive_path <- "/Users/sibylle/OneDrive - IIASA/"
   #one_drive_path <- "C:/Users/rouetpollakis/OneDrive - IIASA/"
   print(scen)
   
   data <- data %>% 
     filter(value != 0) %>% 
     mutate(REGION = ifelse(is.na(REGION), "NotAccounted", REGION))
   
   years <- (data %>% arrange(times) %>% select(times) %>% unique())$times
 
  
   start_abn_land <- data %>% filter(value > 0 ) %>%
     pull(times) %>%
     min() + 10
  
  # Auxiliary function to recode BII coefficients
  # according to forest and non-forest ecosystems
  recode_if <- function(x, condition, ...) {
    ifelse(condition, recode(x, ...), x)
  }
  
    # Compute age classes of abandoned areas
    r_crplnd <- get_age(data %>% filter(LC=="AbnCrpLnd"),"AbnCrpLnd", years, start_abn_land)
    r_grslnd <- get_age(data %>% filter(LC=="AbnGrsLnd"),"AbnGrsLnd", years, start_abn_land)
    r_pltfor <- get_age(data %>% filter(LC=="AbnPltFor"),"AbnPltFor", years, start_abn_land)
    r_mngfor <- get_age(data %>% filter(LC=="AbnMngFor"),"AbnMngFor", years, start_abn_land)
    
    restored_per_age <- get_age(data %>% filter(LC=="restored"),"restored", years, start_abn_land)
    
    # Merge results
    data <- data %>% filter(!str_detect(LC,"Abn")) %>%
      filter(LC!="restored") %>% 
      rbind(r_crplnd,r_grslnd,r_pltfor,r_mngfor, restored_per_age) %>% drop_na()
  
    # Checks
    test_LUdata(data)
    
  ## Get BII indicator ----
  if (get_bii) {
    print("bii calculation")
    
    # Get BII coefficients and add restored abandoned land
    
    # Compute time dynamics of abandoned areas. Here the same assumption
    # employed in Leclere et al. (2020) for the cSAR17 is made: restored
    # areas are assume to linearly increase in quality  with the final
    # state 80% as good as the undisturbed vegetation when the age is > 70 years.
    # For land covers where the disturbed ecosystem is better than the
    # undisturbed * 0.8 (MngFor and PltFor), it
    # is assumed that they converge to undisturbed ecosystem values at
    # the end of the recovery period
    
    age <- seq(10,80,10)
    ecosystem <- c("f","nf")
    
    # Land cover classes
    lorg <- c("cropland_other","grassland","cropland_2Gbioen","forest_managed")
    lurest <- c("AbnCrpLnd","AbnGrsLnd","AbnPltFor","AbnMngFor")
    lu <- tibble(LUclass=lorg,LC_TYPE=lurest)
    
    # Mapping between land and BII land
    land_map <- tibble(LUclass=lorg,LC_TYPE=lurest)
    
    # Undisturbed BII coefficients
    org_map <- tibble(Eco=ecosystem,BII_ORG=c(bii_coefs$BII[which(bii_coefs$LUclass=="forest_unmanaged")],
                                              bii_coefs$BII[which(bii_coefs$LUclass=="other_nf")]))
    
    # Compute coefficients according to age of abandonment
    restored_coef <- expand.grid(lurest,age,ecosystem) %>% rename(LC_TYPE = Var1, Age = Var2, Eco = Var3) %>%
      mutate(class=str_c(LC_TYPE,"_restored_",Age,"_",Eco)) %>%
      #filter(! (LC_TYPE=="AbnMngFor" & Eco=="nf")) %>%
      left_join(land_map, by="LC_TYPE") %>% mutate(LUclass=str_c(LUclass,"_",Eco)) %>%
      mutate(LUclass = recode(LUclass, "forest_managed_f" = "forest_managed"),
             LUclass = recode(LUclass, "forest_managed_nf" = "forest_managed")) %>%
      left_join(bii_coefs, by="LUclass") %>% left_join(org_map, by="Eco") %>% mutate(BII_ORG=ifelse(BII<BII_ORG*.8,BII_ORG*.8,BII_ORG)) %>%
      mutate(BII_FINAL= ifelse( Age >= 20 & Age <= 70 , BII + (Age - 10) /(70 - 5) * (BII_ORG - BII),
                                ifelse( Age > 70, BII_ORG, BII))) %>%
      dplyr::select(class,BII_FINAL,weightSum) %>%
      rename(LUclass=class,BII=BII_FINAL)
    
    # Compute coefficients according to age of restoration
    aff_restored <- expand.grid("restored_f",age) %>% rename(LUclass = Var1, Age = Var2) %>%
      mutate(class=str_c(LUclass,"_",Age)) %>% left_join(bii_coefs, by="LUclass") %>%
      mutate(BII_INI=bii_coefs$BII[which(bii_coefs$LUclass=="forest_managed")]) %>%
      mutate(BII_FINAL= ifelse( Age >= 20 & Age <= 70 , BII_INI + (Age - 10) /(70 - 5) * (BII - BII_INI),
                                ifelse( Age > 70, BII, BII_INI))) %>%
      dplyr::select(class,BII_FINAL,weightSum) %>%
      rename(LUclass=class,BII=BII_FINAL)
    
    # Merge coeficients
    bii_coefs <- bii_coefs %>% rbind(restored_coef,aff_restored) %>% 
      mutate(LUclass = gsub("built.up", "built-up", LUclass))
    
    # Get forest and non-forest shares
    results_bii <- data %>% left_join(f_nf_share, by="ns") %>%
      left_join(pot_npp, by="ns") %>% replace(is.na(.),0)
    
    global_npp <- results_bii %>% filter(times==first(years)) %>%
      mutate(pot_npp=pot_npp*value) %>%
      group_by() %>% summarise(pot_npp=sum(pot_npp)) %>% pull()

    # Merge BII coefficients - for now we split the tibble to speed up processing
    results_bii <- results_bii  %>% group_by(times) %>% group_split()
    
    # Merge results
    results_bii <- results_bii %>% map_df(~process_bii(.,lc_class = lc_class,
                                                       bii_coefs= bii_coefs)) %>% rbind %>% 
      mutate(value=value*f_nf_share) %>% 
      mutate(pot_npp=10^6*pot_npp*value/global_npp) %>%
      mutate(score=value*BII,score_prod=pot_npp*BII)
    
    # Calculate BII at the Simu and aggregate level
    bii_simu <- results_bii %>% 
      group_by(REGION,times,ns) %>%
      summarise(score=sum(score, na.rm=TRUE)/sum(value, na.rm=TRUE),
                score_prod=sum(score_prod, na.rm=TRUE)/sum(pot_npp, na.rm=TRUE),
                value=sum(value, na.rm=TRUE)) %>% 
      mutate(ScenNr=scen)
    
    bii <- results_bii %>% 
      group_by(REGION, times) %>%
      summarise(score=sum(score, na.rm=TRUE)/sum(value, na.rm=TRUE),
                value=sum(value, na.rm=TRUE),
                score_prod=sum(score_prod, na.rm=TRUE)/sum(pot_npp, na.rm=TRUE)) %>%
      mutate(ScenNr=scen)
    
    # Checks
    test_LUdata(results_bii)
    
  } else {
    bii_simu <- bii <-NULL
  }
  
  # Get cSAR indicator----
  if (get_csar){
    print("csar calculation")
    
    # Get cSAR coeficients
    csar_coefs <- csar_coefs %>%
      mutate(forest_managed=0.5*(extensive_forestry+intensive_forestry)) %>% #,
      gather(LUclass,CF,-c(ecoregion))
    
    
    # Compute time dynamics of abandoned areas. Here the same assumption
    # employed in Leclere et al. (2020) for the cSAR17 is made: restored
    # areas are assume to linearly increase in quality  with the final
    # state 80% as good as the undisturbed vegetation when the age is > 70 years.
    # For land covers where the disturbed ecosystem is better than the
    # undisturbed * 0.8 (MngFor and PltFor in non-forest ecosystem), it
    # was assumed that they converge to undsturbed ecosystem values at
    # the end of the recovery period
    
    age <- seq(10,80,10)
    ecoreg <- csar_coefs$ecoregion %>% unique()
    
    # Land cover classes
    lorg <- c("annual_crops","pasture","permanent_crops","forest_managed")
    lurest <- c("AbnCrpLnd","AbnGrsLnd","AbnPltFor","AbnMngFor")
    lu <- tibble(LUclass=lorg,LC_TYPE=lurest)
    
    # Mapping between land and BII land
    land_map <- tibble(LUclass=lorg,LC_TYPE=lurest)
    
    # Compute coefficients for abandoned areas
    restored_coef <- expand.grid(lurest,age,ecoreg) %>% rename(LC_TYPE = Var1, Age = Var2, ecoregion = Var3) %>%
      mutate(class=str_c(LC_TYPE,"_restored_",Age)) %>%
      left_join(land_map, by="LC_TYPE") %>% left_join(csar_coefs, by=c("ecoregion", "LUclass")) %>%
      mutate(CF_ORG=CF*0.2) %>% mutate(CF_FINAL=
                                         ifelse( Age >= 20 & Age <= 70 , CF - ((Age - 10)/(70-5))*(CF - 0.2*CF),
                                                 ifelse(Age > 70 , CF_ORG, CF))) %>%
      dplyr::select(ecoregion,class,CF_FINAL) %>% rename(LUclass=class,CF=CF_FINAL)
    
    csar_coefs <- csar_coefs %>% rbind(restored_coef)
    
    # Get simu to ecoregion map
    # eco_map <- readRDS(paste0("Input/","ecoregions_share.RData")) %>% rename(ns=SimUID) 
    #%>%  mutate(ns=as.factor(ns))
    
    # Excluded ecoregions
    # my_excluded_ers <- c('AA0101','AA0102','AA0103','AA0108','AA0109','AA0110','AA0114','AA0118','AA0119','AA0125','AA0126','AA0202','AA0401','AA0407','AA1101','AA1401',
    #                      'AN1101','AN1102','AN1103','AN1104',
    #                      'AT0105','AT0113','AT0120','AT0127','AT0201','AT0703','AT0720','AT0802','AT0803','AT0902','AT1301','AT1308','AT1318','AT1405',
    #                      'IM0101','IM0110','IM0114','IM0125','IM0127','IM0133','IM0143','IM0148','IM0156','IM0170','IM1401','IM1403',
    #                      'NA0301','NA0525','NA0615','NA1102','NA1109','NA1112','NA1113','NA1117',
    #                      'NT0102','NT0110','NT0116','NT0123','NT0134','NT0172','NT0179','NT0216','NT0218','NT0220','NT0226','NT0301','NT0401','NT0403','NT0705','NT1301','NT1305','NT1306','NT1307','NT1311','NT1314','NT1318','NT1402','NT1404','NT1406',
    #                      'OC0101','OC0102','OC0103','OC0104','OC0105','OC0106','OC0107','OC0108','OC0109','OC0110','OC0111','OC0112','OC0113','OC0114','OC0115','OC0116','OC0117','OC0201','OC0202','OC0203','OC0204','OC0701','OC0702','OC0703',
    #                      'PA0403','PA0425','PA0438','PA0807','PA1101','PA1109','PA1113','PA1203'
    # )
    
    #lc_map <- tibble(LC_TYPE = lc_type,LC_BTC=btc_type)
    lc_map <- lc_class %>% select(LC, LC_bii)
    #csar_lc_map <- tibble(LC_BTC=btc_type,LUclass=csar_type)
    csar_lc_map <- lc_class %>% select(LC_bii, "LUclass" = "LC_csar")
    
    # Intensive/extensive split
    if(LUmapping == "default"){
    data <- data %>% 
        pivot_wider(names_from = LC, values_from = value) %>% 
        left_join(Primary_Secondary %>% select(ns, share_secondaryveg, share_primaryveg)) %>% 
        mutate(intensive_forestry = forest_managed*0.5,
               extensive_forestry = forest_unmanaged*share_secondaryveg+ forest_managed*0.5,
               forest_unmanaged = forest_unmanaged*share_primaryveg) %>% 
        select(-share_primaryveg, -share_secondaryveg, -forest_managed) %>% 
        pivot_longer(cols=-c("REGION", "ns", "times"),names_to = "LC", values_to = "value")
    }
    
    # Merge coefficients
    results_csar <- data %>% group_by(times) %>% group_split()
    
    # Add coefficients and compute impact
    results_csar <- results_csar %>% map_df(~process_csar(.,lc_class = lc_class,
                                                          eco_map = eco_map,
                                                          csar_coefs = csar_coefs)) %>% rbind 
    # %>% 
    #   filter(! ecoregion %in% my_excluded_ers) 
    
    # Calculate extinctions at ecoregion level
    csar_simu <- results_csar %>%
      group_by(REGION,times,ns) %>%
      summarise(extinctions=sum(extinctions, na.rm=TRUE),
                value = sum(value,na.rm=TRUE)) %>% ungroup() %>%
      mutate(score=1-extinctions) %>% 
      mutate(score=ifelse(score <0,0,score)) %>% mutate(ScenNr=scen)
    
    # Calculate total extinctions
    csar_REGION <- results_csar %>%
      group_by(REGION,times,ecoregion) %>%
      summarise(extinctions=sum(extinctions, na.rm=TRUE)) %>%
      mutate(score=extinctions) %>% mutate(score=ifelse(score <0,0,score))
    
    csar <- csar_REGION %>%
      group_by(REGION,times) %>% 
      summarise(score=1-sum(score, na.rm=TRUE)) %>% 
      mutate(score=ifelse(score <0,0,score)) %>% mutate(ScenNr=scen)
    
    # Checks
    test_LUdata(results_csar)
    
  } else {
    csar_simu <- csar <- NULL
  }
  
  
  return(list("results_bii" = results_bii,
              "bii_simu" = bii_simu,
              "bii" = bii,
              "results_csar" = results_csar,
              "csar_simu" = csar_simu,
              "csar" = csar))
  
}

#' get_age
#'
#' Auxiliary function to compute the age classes of abandoned or restored land
#' @param x dataframe. Land use per land use category for each timestep and per SimU
#' @param lnd character string. "AbnCrpLnd", "AbnGrsLnd" ,"AbnPltFor","AbnMngFor", or "restored"
#' @param years vector. years of the simulation
#' @param start_year integer. First year where there is restoration/abandonned land.
#' 
#' @return restoration
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @export

get_age <- function(x,lnd, years, start_year){
  
  # Auxiliary function to calculate the
  # amount of restoration/abandonment per period
  calc_lag <- function(x,n){
    k <- length(x)
    y <- rep(NA,k)
    y[1:n] <- 0
    for (i in (n+1):k){
      y[i] <- x[i] - x[i-n]
    }
    return(y)
  }
  
  timestep <- (years[length(years)]-years[1])/(length(years)-1)
  years <- c(years[1] - timestep, years) %>% sort()
  cols <- data.frame(matrix(nrow = 1, ncol = length(years)))
  colnames(cols) = years %>% as.character()
  
  land <- x
  x <- x %>% spread(times,value) %>% ungroup() %>% dplyr::select(-LC) %>% 
    add_column(!!!cols[!colnames(cols)  %in% names(.)]) %>% 
    replace(is.na(.), 0) %>% 
    select(REGION, ns, colnames(cols) )
  
  # ICI ----
  if (dim(x)[1] > 0) {
    x <- x %>% 
      mutate_at(vars(contains(as.character(years[which(years < start_year)]))), ~.*0)
  }
  
  if (dim(x)[1] > 0){
    # Get restoration from 0 to 10 years
    ini <- 3
    fin <- dim(x)[2]
    restored_10 <- x
    restored_10[,ini:fin] <- apply(x[,ini:fin],1,function(x) calc_lag(x,1)) %>% t()
    
    # SRP: Correction for decreasing restored/abn land - part1: set to zero negative values
    restored_10[,ini:fin] <- pmax(as.matrix(restored_10[, ini:fin]), 0)
    
    # Get remaining restoration classes
    restored_aux <- x
    restored_aux[,ini:fin] <- 0
    restored_20 <- restored_30 <- restored_40 <- restored_50 <-
      restored_60 <- restored_70 <- restored_80 <- restored_aux
    
    # Move afforestation along the time periods to the next age class
    if ((ini+1) <= fin) restored_20[,(ini+1):fin] <- restored_10[,ini:(fin-1)]
    if ((ini+2) <= fin) restored_30[,(ini+2):fin] <- restored_10[,ini:(fin-2)]
    if ((ini+3) <= fin) restored_40[,(ini+3):fin] <- restored_10[,ini:(fin-3)]
    if ((ini+4) <= fin) restored_50[,(ini+4):fin] <- restored_10[,ini:(fin-4)]
    if ((ini+5) <= fin) restored_60[,(ini+5):fin] <- restored_10[,ini:(fin-5)]
    if ((ini+6) <= fin) restored_70[,(ini+6):fin] <- restored_10[,ini:(fin-6)]
    if ((ini+7) <= fin) restored_80[,ini+7] <- restored_10[,ini]
    if ((ini+8) <= fin) restored_80[,ini+8] <- restored_10[,ini+1] + restored_10[,ini]
    if ((ini+9) <= fin) restored_80[,ini+9] <- restored_10[,ini+2] + restored_10[,ini+1] + restored_10[,ini]
    if ((ini+10) <= fin) restored_80[,ini+10] <-   restored_10[,ini+3] + restored_10[,ini+2] + restored_10[,ini+1] + restored_10[,ini]
    
    # Combined the different age classes
    restoration <- restored_10 %>% gather(times,value,-c(REGION,ns)) %>% rename(restored_10=value) %>%
      left_join(restored_20 %>% gather(times,value,-c(REGION,ns)) %>% rename(restored_20=value), by=c("REGION", "ns", "times")) %>%
      left_join(restored_30 %>% gather(times,value,-c(REGION,ns)) %>% rename(restored_30=value), by=c("REGION", "ns", "times")) %>%
      left_join(restored_40 %>% gather(times,value,-c(REGION,ns)) %>% rename(restored_40=value), by=c("REGION", "ns", "times")) %>%
      left_join(restored_50 %>% gather(times,value,-c(REGION,ns)) %>% rename(restored_50=value), by=c("REGION", "ns", "times")) %>%
      left_join(restored_60 %>% gather(times,value,-c(REGION,ns)) %>% rename(restored_60=value), by=c("REGION", "ns", "times")) %>%
      left_join(restored_70 %>% gather(times,value,-c(REGION,ns)) %>% rename(restored_70=value), by=c("REGION", "ns", "times")) %>%
      left_join(restored_80 %>% gather(times,value,-c(REGION,ns)) %>% rename(restored_80=value), by=c("REGION", "ns", "times")) %>%
      mutate(times=as.integer(times))%>%
      # SRP: Correction for decreasing restored/abn land part2: remove decreasing land proportionnally from the different age classes
      left_join(land %>% select(-LC) %>%
                  mutate(times=as.integer(times)), by=c("REGION", "times", "ns")) %>%
      replace(is.na(.), 0) %>%
      mutate(totabn = rowSums(across(contains("restored")), na.rm=TRUE),
             diff= totabn - value) %>% select(-value) %>%
      mutate_at(vars(contains("restored_")), ~ifelse(diff > 0, .-diff*./totabn, .)) %>%
      select(-totabn, -diff)
    
    if(lnd == "restored"){
      colnames(restoration)[4:11] <- colnames(restoration)[4:11]
      cols_to_select <- 1:min(fin, ncol(restoration))
    }else{
      colnames(restoration)[4:11] <- str_c(lnd,"_",colnames(restoration)[4:11])
      cols_to_select <- 1:ncol(restoration)
    }
    
    ## Test whether the sum of restoration still equals the restoration per age class
    comparison <- restoration %>% 
      left_join(land %>% dplyr::select(-LC), by=c("REGION", "ns", "times")) %>% replace(is.na(.), 0) %>% 
      mutate(sum_abn = rowSums(.[grep("restored", names(.))], na.rm=TRUE),
             diff=(round(value, 10)-round(sum_abn, 10))*100/round(value, 10))
    
    if(comparison %>% filter(times > start_year) %>% filter(abs(diff)>0.001) %>% nrow() >0){
      warning(paste(lnd, "distributed along age is different than total"))
    }
      
    # Aggregate and clean
    restoration <- restoration[cols_to_select] %>% gather(LC,value,-c( REGION,times,ns)) %>% 
      replace(. < 0, 0) %>% filter(value!=0)
    
  } else {
    restoration <- tibble(REGION=NA, ns=NA, times=NA, LC=NA, value = NA)
  }
  
  return(restoration)
  
}


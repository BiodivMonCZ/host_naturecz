n2k_hab_druhy <- function(hab_code, evl_site, typ_chu) {
  
  if(typ_chu == "EVL") {
    uzemi <- evl
  } else if(typ_chu == "MZCHU") {
    uzemi <- mzchu
  }
  
  # 1. Select the correct directory based on typ_chu
  if (typ_chu == "EVL") {
    target_dir <- "C:/Users/jonas.gaigr/Documents/host_naturecz/Outputs/Data/stanoviste/paseky/EVL"
  } else if (typ_chu == "MZCHU") {
    target_dir <- "C:/Users/jonas.gaigr/Documents/host_naturecz/Outputs/Data/stanoviste/paseky/MZCHU"
  } else {
    stop("typ_chu variable is not set to EVL or MZCHU")
  }
  
  # 2. List all CSV files in that directory
  # full.names = TRUE gives us the complete path, which we need for loading
  files <- list.files(path = target_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # 3. Check if files exist to avoid errors
  if (length(files) > 0) {
    
    # 4. Get file details (modification time)
    file_details <- file.info(files)
    
    # 5. Find the row with the maximum time (latest)
    latest_file <- rownames(file_details)[which.max(file_details$mtime)]
    
    # 6. Load the data
    print(paste("Loading:", latest_file)) # Optional: prints what file is being loaded
    paseky <- read.csv2(latest_file, fileEncoding = "Windows-1250")
    
  } else {
    warning("No CSV files found in the target directory.")
  }
  
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- data_akt$vmb_shp_sjtsk_akt %>%
    sf::st_intersection(dplyr::filter(uzemi, SITECODE == evl_site)) %>%
    dplyr::filter(HABITAT == hab_code | BIOTOP == hab_code) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                    sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                    sf::st_geometry_type(geometry) != "LINESTRING" & 
                    sf::st_geometry_type(geometry) != "MULTILINESTRING") %>%
    dplyr::mutate(AREA_real = units::drop_units(sf::st_area(geometry))) %>%
    dplyr::filter(AREA_real > 0) %>%
    dplyr::mutate(PLO_BIO_M2_EVL = STEJ_PR/100*AREA_real)
  
  # CELKOVÁ PLOCHA HABITATU VČETNĚ PASEK
  area_paseky_ha <- paseky %>%
    dplyr::filter(SITECODE == evl_site) %>%
    dplyr::filter(HABITAT_CODE == hab_code) %>%
    dplyr::pull(ROZLOHA_PASEKY)
  
  if (length(area_paseky_ha) == 0) {
    area_paseky_ha <- NA
  } else {
    area_paseky_ha <- area_paseky_ha
  }
  
  SUM_PLO_BIO <- sum(vmb_target_sjtsk %>%
                       dplyr::pull(PLO_BIO_M2_EVL) %>%
                       sum(),
                     area_paseky_ha*10000,
                     na.rm = TRUE)
  
  target_area_ha <- SUM_PLO_BIO/10000
  
  vmb_spat <- vmb_target_sjtsk %>%
    dplyr::filter(FSB_EVAL != "X")
  
   # RED LIST SPECIES
  redlist_list <- red_list_species %>%
    #dplyr::filter(SITECODE == evl_site) %>%
    sf::st_filter(., vmb_target_sjtsk) %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  redlist <- redlist_list %>%
    length()/log(SUM_PLO_BIO)*4
  
  if(redlist > 10) {
    redlist <- 10
  } 
  
  if(length(redlist_list) == 0) {
    redlist_list <- NA
  } 
  
  if(nrow(vmb_spat) == 0) {
    redlist_list <- NA
    redlist <- NA
  }
  
  # INVASIVE SPECIES
  if(hab_code == 6510 | hab_code == "T1.1") {
    invaders_all <- invasive_species %>%
      #dplyr::filter(SITECODE == evl_site) %>%
      dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  } else {
    invaders_all <- invasive_species %>%
      #dplyr::filter(SITECODE == evl_site) %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  }
  
  invaders_calc <- invaders_all %>%
    dplyr::group_by(OBJECTID.y) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  invaders_list <- invaders_all %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  
  invaders <- sum(invaders_calc$PLO_BIO_M2_EVL, na.rm = TRUE)/SUM_PLO_BIO*100
  
  if(length(invaders_list) == 0 |
     nrow(vmb_target_sjtsk) == 0) {
    invaders_list <- NA
  }
  
  # EXPANZNÍ DRUHY
  expanders_all <- expansive_species %>%
    dplyr::filter(POKRYVNOST %in% c("3", "4", "5")) %>%
    #dplyr::filter(SITECODE == evl_site) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(is.na(HABITAT) == FALSE) %>%
    dplyr::group_by(OBJECTID.y, DRUH) %>%
    dplyr::filter(DATUM_OD >= DATUM) %>%
    dplyr::slice(which.max(DATUM_OD)) %>%
    dplyr::filter(NEGATIVNI == 0) %>%
    dplyr::ungroup()
  
  expanders_calc <- expanders_all %>%
    dplyr::group_by(OBJECTID.y) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  expanders_list <- expanders_all %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  expanders <- sum(expanders_calc$PLO_BIO_M2_EVL, na.rm = TRUE)/SUM_PLO_BIO*100
  
  
  if(length(expanders_list) == 0 &
     nrow(vmb_target_sjtsk) == 0) {
    expanders <- NA
    expanders_list <- NA
  } else if (length(expanders_list) == 0 &
             nrow(vmb_target_sjtsk) > 0) {
    expanders <- 0
    expanders_list <- NA
  }
  
  if(nrow(vmb_target_sjtsk) == 0) {
    perc_spat <- NA
  }
  
  vmb_target_date <- vmb_target_sjtsk %>%
    pull(DATUM)
  
  min_date <- vmb_target_date %>%
    min() %>%
    unique()
  
  max_date <- vmb_target_date %>%
    max() %>%
    unique()
  
  mean_date <- mean(vmb_target_date)
  
  median_date <- median(vmb_target_date)
  
  perc_seg_0 <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(ROK_AKT.y == 0) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  perc_seg_1 <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(ROK_AKT.y > 0 & ROK_AKT.y <= 2012) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  perc_seg_2 <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(ROK_AKT.y > 2012 & ROK_AKT.y <= 2024) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  if(nrow(vmb_target_sjtsk) == 0) {
    perc_seg_0 <- NA
    perc_seg_1 <- NA
    perc_seg_2 <- NA
  }
  
  
  # VÝSLEDKY
  if(target_area_ha > 0 & is.na(target_area_ha) == FALSE) {
    result <- vmb_target_sjtsk %>%
      dplyr::reframe(
        SITECODE = unique(SITECODE)[1],
        NAZEV = unique(NAZEV)[1],
        HABITAT_CODE = ifelse(typ_chu == "EVL", unique(HABITAT)[1], unique(BIOTOP)[1]),
        ROZLOHA = target_area_ha,
        RED_LIST = redlist,
        INVASIVE = invaders,
        EXPANSIVE = expanders,
        RED_LIST_SPECIES = paste(redlist_list, collapse = ", "),
        INVASIVE_LIST = paste(invaders_list, collapse = ", "),
        EXPANSIVE_LIST = paste(expanders_list, collapse = ", "),
      ) %>%
      dplyr::distinct()
  } else {
    result <- tibble(
      SITECODE = evl_site,
      NAZEV = ifelse(
        typ_chu == "EVL",
        sites_habitats %>% 
          dplyr::filter(site_code == evl_site) %>%
          dplyr::pull(site_name) %>%
          unique(),
        sites_habitats_mzchu_test %>% 
          dplyr::filter(site_code == evl_site) %>%
          dplyr::pull(site_name) %>%
          unique()
        ),
      HABITAT_CODE = hab_code,
      ROZLOHA = target_area_ha,
      RED_LIST = NA,
      INVASIVE = NA,
      EXPANSIVE = NA,
      RED_LIST_SPECIES = NA_character_,
      INVASIVE_LIST = NA_character_,
      EXPANSIVE_LIST = NA_character_,
      )
  }
  
  return(result %>%
           dplyr::distinct())
  
}


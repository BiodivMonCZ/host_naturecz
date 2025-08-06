
# LOAD DATA ----
# VRSTVA EVL 
n2k_oop <- read.csv2("n2k_oop_24.csv", fileEncoding = "Windows-1250")
po <- sf::st_read("//bali.nature.cz/du/OchranaPrirody/Natura 2000/PtaciObl.shp", options = "ENCODING=WINDOWS-1250")
evl <- sf::st_read("//bali.nature.cz/du/OchranaPrirody/Natura 2000/EvVyzLok.shp", options = "ENCODING=WINDOWS-1250") %>%
  dplyr::left_join(., n2k_oop, by = c("KOD" = "kod_chu"))
#evl_akt <- sf::st_read("S:/Složky uživatelů/Gaigr/stanoviste/evl/Aktualizovaná vrstva/Aktualizovana_vrstva_EVL.shp", options = "ENCODING=UTF-8") %>%
#  dplyr::left_join(., n2k_oop, by = c("KOD" = "kod_chu"))
evl_sjtsk <- sf::st_transform(evl, CRS("+init=epsg:5514"))
mzchu_sjtsk <- sf::st_read("//bali.nature.cz/du/OchranaPrirody/UzemniOchrana/MaloplZCHU.shp", options = "ENCODING=WINDOWS-1250")

rp_n2k <- read.csv2("n2k_rp_24.csv", fileEncoding = "Windows-1250")

# VRSTVA HRANIC CZ 
czechia <- st_read("//bali.nature.cz/du/SpravniCleneni/CR/HraniceCR.shp") %>%
  st_transform(., CRS("+init=epsg:5514"))
czechia_line <- st_cast(czechia, "LINESTRING")

# SEZNAM EVL A PŘEDMĚTŮ OCHRANY
#rn2kcz::load_n2k_sites()
sites_subjects <- openxlsx::read.xlsx("S:/Složky uživatelů/Gaigr/stanoviste/evl/seznam_predmetolokalit_Natura2000_5_2025_fin.xlsx",
                                        sheet = 1)
colnames(sites_subjects) <- c("site_code", "site_name", "site_type", "feature_type", "EU_code", "feature_code", "nazev_cz", "nazev_lat")

sites_habitats <- sites_subjects %>%
  dplyr::filter(feature_type == "stanoviště")

# ČÍSELNÍK HABITATŮ
habitats <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/habitats.csv", encoding = "UTF-8")
# SEZNAM MINIMIAREÁLŮ
minimisize <- read.csv("S:/Složky uživatelů/Gaigr/stanoviste/minimiarealy/minimisize.csv", encoding = "UTF-8")
# rozloze stanoviště v ČR v rámci AVMB2022
habitat_areas_2022 <- read.csv("S:/Složky uživatelů/Gaigr/stanoviste/celkova_rozloha/stanoviste_rozloha_cr_a1.csv", encoding = "Windows-1250")
# MAXIMÁLNÍ VZDÁLENOST MEZI 2 BODY PRO KAŽDOU EVL - LINESTRINGY BYLY PŘEVEDENY NA MULTIPOINT 
# PRO EVL S OBVODEM < 10 KM BYLY POUŽITY VŠECHNY BODY, PRO VĚTŠÍ EVL KAŽDÝ SEDMÝ
evl_lengths <- read.csv("S:/Složky uživatelů/Gaigr/stanoviste/evl/evl_max_dist.csv", encoding = "UTF-8")
#evl_species <- read.csv("https://media.githubusercontent.com/media/jonasgaigr/N2K.CZ/main/cevnate_evl.csv", encoding = "UTF-8")
# LIMITNÍ HODNOTY PARAMETRŮ HODNOCENÍ
hablimits <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/hablimits.csv", encoding = "UTF-8") %>%
  filter(REG != "alp" | is.na(REG) == TRUE)

habitat_areas_a1 <- read.csv("S:/Složky uživatelů/Gaigr/stanoviste/celkova_rozloha/stanoviste_rozloha_cr_a1.csv", 
                             fileEncoding = "Windows-1250")

load_vmb <- function(vmb_x = 1, clean = TRUE) {
  
  if(vmb_x == 1) {
    
    # VMB - ZÁKLADNÍ MAPOVÁNÍ
    vmb_shp_sjtsk_orig_read <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/20060501_Segment.shp", options = "ENCODING=WINDOWS-1250")
    vmb_hab_dbf_orig <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/HAB20060501_BIOTOP.dbf", options = "ENCODING=WINDOWS-1250")
    vmb_pb_dbf_orig <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/PB20060501_BIOTOP.dbf", options = "ENCODING=WINDOWS-1250") %>%
      dplyr::filter(!OBJECTID %in% vmb_hab_dbf_orig$OBJECTID)
    
    vmb_hab_pb_dbf_orig <- dplyr::bind_rows(vmb_hab_dbf_orig, vmb_pb_dbf_orig) %>%
      dplyr::group_by(SEGMENT_ID) %>%
      dplyr::mutate(moz_num = n(),
                    FSB_EVAL_prep = dplyr::case_when(sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
                                                     sum(STEJ_PR, na.rm = TRUE) >= 50 &
                                                       sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
                                                     sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(SEGMENT_ID,
                    FSB_EVAL_prep) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_orig <- vmb_shp_sjtsk_orig_read %>%
      dplyr::left_join(vmb_hab_dbf_orig, by = "SEGMENT_ID") %>%
      dplyr::left_join(vmb_hab_pb_dbf_orig, by = "SEGMENT_ID") %>%
      dplyr::mutate(FSB_EVAL = dplyr::case_when(FSB_EVAL_prep == "X" ~ "X",
                                                TRUE ~ FSB),
                    HABITAT = dplyr::case_when(HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
                                               TRUE ~ HABITAT))
    
    assign("vmb_shp_sjtsk_orig", vmb_shp_sjtsk_orig, envir = .GlobalEnv)
    
  } else if(vmb_x == 2) {
    
    # VMB1 - PRVNÍ AKTUALIZACE
    vmb_shp_sjtsk_a1_read <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Aktualizace1_Segment.shp", options = "ENCODING=WINDOWS-1250")
    vmb_hab_dbf_a1 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Biotop/Aktualizace1_Hab_biotop.dbf", options = "ENCODING=WINDOWS-1250")
    vmb_pb_dbf_a1 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Biotop/Aktualizace1_Biotop.dbf", options = "ENCODING=WINDOWS-1250") %>%
      dplyr::filter(!OBJECTID_1 %in% vmb_hab_dbf_a1$OBJECTID_1)
    vmb_hab_pb_dbf_a1 <- dplyr::bind_rows(vmb_hab_dbf_a1, vmb_pb_dbf_a1) %>%
      dplyr::group_by(SEGMENT_ID) %>%
      dplyr::mutate(moz_num = n(),
                    FSB_EVAL_prep = dplyr::case_when(sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
                                                     sum(STEJ_PR, na.rm = TRUE) >= 50 &
                                                       sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
                                                     sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(SEGMENT_ID,
                    FSB_EVAL_prep) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_a1 <- vmb_shp_sjtsk_a1_read %>%
      dplyr::left_join(vmb_hab_dbf_a1, by = "SEGMENT_ID") %>%
      dplyr::left_join(vmb_hab_pb_dbf_a1, by = "SEGMENT_ID") %>%
      dplyr::mutate(FSB_EVAL = dplyr::case_when(FSB_EVAL_prep == "X" ~ "X",
                                                TRUE ~ FSB),
                    HABITAT = dplyr::case_when(HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
                                               TRUE ~ HABITAT))
    
    paseky_a1 <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_a1_results_20240814.csv")
    
    assign("vmb_shp_sjtsk_a1", vmb_shp_sjtsk_a1, envir = .GlobalEnv)
    assign("paseky_a1", paseky_a1, envir = .GlobalEnv)
    
  } else if(vmb_x == 0) {
    # VMB - MÁJOVÁ VRSTVA AKTUÁLNÍ
    vmb_shp_sjtsk_akt_read <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Aktualni_Segment.shp", options = "ENCODING=WINDOWS-1250")
    vmb_hab_dbf_akt <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/HAB_BIOTOP.dbf", options = "ENCODING=WINDOWS-1250")
    vmb_pb_dbf_akt <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/PB_BIOTOP.dbf", options = "ENCODING=WINDOWS-1250") %>%
      dplyr::filter(!OBJECTID %in% vmb_hab_dbf_akt$OBJECTID)
    vmb_hab_pb_dbf_akt <- dplyr::bind_rows(vmb_hab_dbf_akt, vmb_pb_dbf_akt) %>%
      dplyr::group_by(SEGMENT_ID) %>%
      dplyr::mutate(moz_num = n(),
                    FSB_EVAL_prep = dplyr::case_when(sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
                                                     sum(STEJ_PR, na.rm = TRUE) >= 50 &
                                                       sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
                                                     sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(SEGMENT_ID,
                    FSB_EVAL_prep) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_akt <- vmb_shp_sjtsk_akt_read %>%
      dplyr::left_join(., vmb_hab_dbf_akt, by = "SEGMENT_ID") %>%
      dplyr::left_join(., vmb_hab_pb_dbf_akt, by = "SEGMENT_ID") %>%
      dplyr::mutate(FSB_EVAL = dplyr::case_when(FSB_EVAL_prep == "X" ~ "X",
                                                TRUE ~ FSB),
                    HABITAT = dplyr::case_when(HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
                                               TRUE ~ HABITAT),
                    REGION_ID = REGION_ID.x) %>%
      dplyr::rename(DATUM = DATUM.x)
    
    paseky_23 <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv")
    
    
    assign("vmb_shp_sjtsk_akt", vmb_shp_sjtsk_akt, envir = .GlobalEnv)
    assign("paseky", paseky_23, envir = .GlobalEnv)
    
  } 
  
  if(vmb_x == 1 & clean == TRUE) {
    
    rm(vmb_shp_sjtsk_orig_read, vmb_hab_dbf_orig, vmb_pb_dbf_orig, vmb_hab_pb_dbf_orig)
    
  } else if(vmb_x == 2 & clean == TRUE) {
    
    rm(vmb_shp_sjtsk_a1_read, vmb_hab_dbf_a1, vmb_pb_dbf_a1, vmb_hab_pb_dbf_a1)
    
  } else if(vmb_x == 0 & clean == TRUE) {
    
    rm(vmb_shp_sjtsk_akt_read, vmb_hab_dbf_akt, vmb_pb_dbf_akt, vmb_hab_pb_dbf_akt)
    
  }
  
}

load_vmb(vmb_x = 0)
#load_vmb(vmb_x = 2)

# VÝPOČET HODNOCENÍ ----
n2k_hab_klic <- function(hab_code, evl_site) {
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_akt %>%
    sf::st_intersection(dplyr::filter(evl, SITECODE == evl_site)) %>%
    dplyr::filter(HABITAT == hab_code) %>%
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
  
  area_w_ha <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(DG == "W" | RB == "W") %>%
    pull(PLO_BIO_M2_EVL) %>%
    na.omit() %>%
    sum()/10000
  area_w_perc <- area_w_ha/target_area_ha*100
  
  area_paseky_perc <- area_paseky_ha/target_area_ha*100
  
  area_degrad_ha <- sum(area_w_ha, area_paseky_ha, na.rm = TRUE)
  area_degrad_perc <- area_degrad_ha/target_area_ha*100

  # KVALITATIVNÍ PARAMETRY HODNOCENÍ
  vmb_qual <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      TYP_DRUHY_SEG = dplyr::case_when(DG == "W" ~ 0,
                                       RB == "W" ~ 0,
                                       is.na(TD) == TRUE ~ 0,
                                       TD == "N" ~ 0,
                                       TD == "MP" ~ 1,
                                       TD == "P" ~ 2),
      REPREZENTAVITA_SEG = dplyr::case_when(DG == "W" ~ 0,
                                            RB == "W" ~ 0,
                                            RB == "F" ~ 0,
                                            RB == "P" ~ 1,
                                            RB == "V" ~ 2),
      REPRESENTAVITY_SEG = dplyr::case_when(DG == "W" ~ "D",
                                            RB == "V" & TD == "P" ~ "A",
                                            RB == "V" & TD == "MP" ~ "B",
                                            RB == "V" & TD == "N" ~ "C",
                                            RB == "V" & is.na(TD) == TRUE ~ "A",
                                            RB == "P" & TD == "P" ~ "B",
                                            RB == "P" & TD == "MP" ~ "C",
                                            RB == "P" & TD == "N" ~ "C",
                                            RB == "P" & is.na(TD) ~ "B",
                                            RB == "F" & TD == "P" ~ "C",
                                            RB == "F" & TD == "MP" ~ "C",
                                            RB == "F" & TD == "N" ~ "C",
                                            RB == "F" & is.na(TD) ~ "C",
                                            RB == "W" & TD == "P" ~ "D",
                                            RB == "W" & TD == "MP" ~ "D",
                                            RB == "W" & TD == "N" ~ "D",
                                            RB == "W" & is.na(TD) ~ "D",
                                            is.na(RB) == TRUE & TD == "P" ~ "B",
                                            is.na(RB) == TRUE & TD == "MP" ~ "C",
                                            is.na(RB) == TRUE & TD == "N" ~ "C",
                                            is.na(RB) == TRUE & is.na(TD) ~ "C"),
      CONSERVATION_SEG = dplyr::case_when(SF == "P" ~ "A",
                                          SF == "MP" ~ "B",
                                          SF == "N" ~ "C",
                                          RB == "W" ~ "C",
                                          is.na(SF) == TRUE & DG == 0 ~ "A",
                                          is.na(SF) == TRUE & DG == 1 ~ "A",
                                          is.na(SF) == TRUE & DG == 2 ~ "B",
                                          is.na(SF) == TRUE & DG == 3 ~ "C",
                                          is.na(SF) == TRUE & is.na(DG) == TRUE ~ "B"),
      REPRE_SDF_SEG = dplyr::case_when(REPRESENTAVITY_SEG == "D" ~ 0,
                                       REPRESENTAVITY_SEG == "C" ~ 1,
                                       REPRESENTAVITY_SEG == "B" ~ 2,
                                       REPRESENTAVITY_SEG == "A" ~ 3),
      CON_SEG = dplyr::case_when(CONSERVATION_SEG == "C" ~ 0,
                                 CONSERVATION_SEG == "B" ~ 1,
                                 CONSERVATION_SEG == "A" ~ 2),
      DEGREEOFCONS_SEG = dplyr::case_when(DG == "W" ~ 0,
                                          RB == "W" ~ 0,
                                          SF == "N" ~ 0,
                                          SF == "MP" ~ 100,
                                          SF == "P" ~ 100),
      KVALITA_SEG = dplyr::case_when(DG == "W" ~ 0,
                                     RB == "W" ~ 0,
                                     is.na(KVALITA) == TRUE ~ 0,  
                                     KVALITA == 0 ~ 0,
                                     KVALITA == 1 ~ 3,
                                     KVALITA == 2 ~ 2,
                                     KVALITA == 3 ~ 1,
                                     KVALITA == 4 ~ 0),
      MRTVE_DREVO_SEG = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                         DG == "W" ~ 0,
                                         RB == "W" ~ 0,
                                         MD == 0 ~ 0,
                                         MD == 1 ~ 1,
                                         MD == 2 ~ 2,
                                         MD == 3 ~ 0,
                                         MD == 4 ~ 0),
      KALAMITA_SEG = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                      DG == "W" ~ 0,
                                      RB == "W" ~ 0,
                                      MD == 0 ~ 0,
                                      MD == 1 ~ 0,
                                      MD == 2 ~ 0,
                                      MD == 3 ~ 2,
                                      MD == 4 ~ 2),
      TD_SEG = TYP_DRUHY_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      RB_SEG = REPREZENTAVITA_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      RB_SDF_SEG = REPRE_SDF_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      CS_SEG = CON_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      DC_SEG = DEGREEOFCONS_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      CN_SEG = CON_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      QUAL_SEG = KVALITA_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO,
      MD_SEG = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                TRUE ~ MRTVE_DREVO_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO),
      KAL_SEG = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                 TRUE ~ KALAMITA_SEG*PLO_BIO_M2_EVL/SUM_PLO_BIO)
    ) %>%
    dplyr::mutate(
      # TYPICKÉ DRUHY
      TD_FIN = 3 - sum(TD_SEG, na.rm = TRUE),
      # REPREZENTATIVITA
      RB_FIN = sum(RB_SEG, na.rm = TRUE),
      # REPREZENTATIVITA SDF
      RB_SDF_FIN = 4 - sum(RB_SDF_SEG, na.rm = TRUE),
      RB_SDF_FIN_KAT = dplyr::case_when(RB_SDF_FIN < 1.5 ~ "A",
                                        RB_SDF_FIN >= 1.5 & RB_SDF_FIN < 2.5 ~ "B",
                                        RB_SDF_FIN < 2.5 & RB_SDF_FIN < 3.5 ~ "C",
                                        RB_SDF_FIN >= 3.5 ~ "D",
                                        TRUE ~ NA_character_),
      # DEGREE OF CONSERVATION
      DC_FIN = sum(DC_SEG, na.rm = TRUE),
      # CONSERVATION
      CN_FIN = 3 - sum(CN_SEG, na.rm = TRUE),
      # MRTVÉ DŘEVO
      MD_FIN = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                TRUE ~ sum(MD_SEG, na.rm = TRUE)),
      # KALAMITA A POLOM
      KP_FIN = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                TRUE ~ sum(KAL_SEG, na.rm = TRUE)),
      # KVALITA
      QUALITY = 4 - sum(QUAL_SEG, na.rm = TRUE)) %>%
    dplyr::distinct()
  
  # !!!! NUTNO PŘEPSAT PŘI AKTUALIZACI EVL !!!!!
  area_evl_perc <- unique(target_area_ha/(unique(dplyr::filter(evl, SITECODE == evl_site)$SHAPE_AREA)/10000)*100)
  area_relative_perc <- target_area_ha/habitat_areas_2022 %>%
    dplyr::filter(., HABITAT == hab_code) %>%
    pull(TOTAL_AREA_ALL)/10000*100
  
  area_good_ha <- vmb_target_sjtsk %>%
    dplyr::filter(SF == "P" | SF == "MP") %>%
    pull(PLO_BIO_M2_EVL) %>%
    sum()/10000
  
  if(nrow(vmb_target_sjtsk) == 0 & is.na(target_area_ha)) {
    target_area_ha <- 0
    area_w_ha <- 0
    area_w_perc <- 0
    area_evl_perc <- 0
    area_good_ha <- 0
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
    result <- 
      vmb_qual %>%
      dplyr::reframe(
        SITECODE = unique(SITECODE)[1],
        NAZEV = unique(NAZEV)[1],
        HABITAT_CODE = unique(HABITAT)[1],
        ROZLOHA = target_area_ha,
        KVALITA = unique(QUALITY)[1],
        TYPICKE_DRUHY = unique(TD_FIN)[1],
        REPRE = unique(RB_FIN)[1],
        REPRE_SDF = unique(RB_SDF_FIN)[1],
        CONSERVATION = unique(CN_FIN)[1],
        DEGREE_OF_CONSERVATION = unique(DC_FIN)[1],
        MRTVE_DREVO = unique(MD_FIN)[1],
        KALAMITA_POLOM = unique(KP_FIN)[1],
        RELATIVE_AREA_PERC = area_relative_perc,
        EVL_AREA_PERC = area_evl_perc, # NUTNO PŘEPSAT PŘI AKTUALIZACI EVL
        GOOD_DOC_AREA_HA = area_good_ha,
        W_AREA_HA = area_w_ha,
        W_AREA_PERC = area_w_perc,
        PASEKY_AREA_HA = area_paseky_ha,
        PASEKY_AREA_PERC = area_paseky_perc,
        DEGRAD_AREA_HA = area_degrad_ha,
        DEGRAD_AREA_PERC = area_degrad_perc,
        PERC_0 = perc_seg_0,
        PERC_1 = perc_seg_1,
        PERC_2 = perc_seg_2,
        DATE_MIN = min_date,
        DATE_MAX = max_date,
        DATE_MEAN = mean_date,
        DATE_MEDIAN = median_date
      ) %>%
      dplyr::distinct()
  } else {
    result <- 
      dplyr::tibble(
        SITECODE = evl_site,
        NAZEV = sites_habitats %>% 
          dplyr::filter(site_code == evl_site) %>%
          dplyr::pull(site_name) %>%
          unique(),
        HABITAT_CODE = hab_code,
        ROZLOHA = target_area_ha,
        KVALITA = NA,
        TYPICKE_DRUHY = NA,
        REPRE = NA,
        REPRE_SDF = NA,
        CONSERVATION = NA,
        DEGREE_OF_CONSERVATION = NA,
        MRTVE_DREVO = NA,
        KALAMITA_POLOM = NA,
        RELATIVE_AREA_PERC = NA,
        EVL_AREA_PERC = NA,
        GOOD_DOC_AREA_HA = NA,
        W_AREA_HA = area_w_ha,
        W_AREA_PERC = area_w_perc,
        PASEKY_AREA_HA = area_paseky_ha,
        PASEKY_AREA_PERC = area_paseky_perc,
        DEGRAD_AREA_HA = area_degrad_ha,
        DEGRAD_AREA_PERC = area_degrad_perc,
        PERC_0 = NA,
        PERC_1 = NA,
        PERC_2 = NA,
        DATE_MIN = NA_Date_,
        DATE_MAX = NA_Date_,
        DATE_MEAN = NA_Date_,
        DATE_MEDIAN = NA_Date_)
  }
  
  return(result %>%
           distinct())
  
}


# RESULTS 2024 ----
hu <- n2k_hab_klic(sites_habitats[89,5], sites_habitats[89,1])
habresults_100_110 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 87:95) {
  habresults_100_110 <- dplyr::bind_rows(
    habresults_100_110, 
    as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}

habresults_x_1_500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1_500) <- colnames(hu)
habresults_x_501_1000 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_501_1000) <- colnames(hu)
habresults_x_1001_1500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1001_1500) <- colnames(hu)
habresults_x_1501_1893 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1501_1893) <- colnames(hu)

rm(vmb_shp_sjtsk_akt_read, vmb_hab_dbf_23, vmb_pb_dbf_23, vmb_hab_pb_dbf_23)

for(i in 1:500) {
  habresults_x_1_500 <- dplyr::bind_rows(habresults_x_1_500, 
                                       as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1_500, 
           "Data/Temp/habresults_x_1_500.csv", 
           row.names = FALSE)
for(i in 501:1000) {
  habresults_x_501_1000 <- dplyr::bind_rows(habresults_x_501_1000, 
                                          as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_501_1000, 
           "Data/Temp/habresults_x_501_1000.csv", 
           row.names = FALSE)
for(i in 1001:1500) {
  habresults_x_1001_1500 <- dplyr::bind_rows(habresults_x_1001_1500, 
                                           as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1001_1500, 
           "Data/Temp/habresults_x_1001_1500.csv", 
           row.names = FALSE)
for(i in 1501:nrow(sites_habitats)) {
  habresults_x_1501_1893 <- dplyr::bind_rows(habresults_x_1501_1893, 
                                           as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1501_1893, 
           "Data/Temp/habresults_x_1501_1893.csv", 
           row.names = FALSE)

results_habitats_x <- dplyr::bind_rows(
  habresults_x_1_500[c(2:nrow(habresults_x_1_500)),], 
  habresults_x_501_1000[c(2:nrow(habresults_x_501_1000)),],
  habresults_x_1001_1500[c(2:nrow(habresults_x_1001_1500)),],
  habresults_x_1501_1893[c(2:nrow(habresults_x_1501_1893)),]
  )

path <- paste0("Outputs/Data/results_habitats_24_", 
               gsub('-','',Sys.Date()), 
               ".csv")
write.csv2(
  results_habitats_x, 
  path, 
  row.names = FALSE,
  fileEncoding = "Windows-1250"
  )

## Clear temp files ----
# Define path to the Temp folder
temp_path <- "Data/Temp"   # or "C:/full/path/to/Data/Temp"

# List all files in the folder
files_to_delete <- list.files(temp_path, full.names = TRUE)

# Remove files
file.remove(files_to_delete)

cat("Removed", length(files_to_delete), "files from", temp_path, "\n")

# Results to long ----

results_habitats_l <- results_habitats_x %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  ) %>%
  mutate(
    across(where(is.numeric) | where(is.Date) & !c("HABITAT_CODE"), ~ as.character(as.numeric(.x)))
    )

results_habitats_long <- tidyr::pivot_longer(results_habitats_l,
                                             cols = c(4:(ncol(results_habitats_l)-4)),
                                             names_to = "PAR_NAZEV",
                                             values_to = "PAR_HODNOTA") %>%
  dplyr::mutate(ROK_HODNOCENI = 2024) %>%
  dplyr::select(-c(DATE_MEAN, DATE_MEDIAN)) %>%
  dplyr::select(SITECODE,
                NAZEV,
                HABITAT_CODE,
                DATE_MIN, 
                DATE_MAX,
                PAR_NAZEV,
                PAR_HODNOTA,
                ROK_HODNOCENI)
  
write.csv2(
  results_habitats_long, 
  paste0(
  temp_path,
  "results_habitats_long_24_", 
  gsub(
    '-',
    '',
    Sys.Date()
    ),
  ".csv"
  ),
  row.names = FALSE
  )

# AKTUALIZACE EVL ----
okrsky <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/AktualizacniOkrsek")
st_filter(evl_sjtsk, okrsky %>%
            dplyr::filter(ROK_AKT > 2020)) %>%
  pull(NAZEV)

sites_habitats_sitecodes <- sites_habitats %>%
  dplyr::select(site_code) %>%
  dplyr::distinct()

hu_akt <- hvezdice_update(sites_habitats_sitecodes[1,])
habupdate <- matrix(NA, 1, ncol(hu_akt)) %>% dplyr::as_tibble()
colnames(habupdate) <- colnames(hu_akt)

for(i in 1:nrow(sites_habitats_sitecodes)) {
  habupdate <- dplyr::bind_rows(habupdate, 
                                as.data.frame(hvezdice_update(sites_habitats_sitecodes[i,])))
}
habupdate_22 <- habupdate[c(2:nrow(habupdate)),]

write.csv2(habupdate_22, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habupdate_22_", 
           gsub('-','',Sys.Date()), 
           ".csv")

# RESULTS A1 ----
hu <- hvezdice_eval_a1(sites_habitats[1444,5], sites_habitats[1444,1])
habresults_100_110 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 1443:1445) {
  habresults_100_110 <- dplyr::bind_rows(habresults_100_110, 
                                         as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}



habresults_1_500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1_500) <- colnames(hu)
habresults_501_1000 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_501_1000) <- colnames(hu)
habresults_1001_1500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1001_1500) <- colnames(hu)
habresults_1501_1893 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1501_1893) <- colnames(hu)

rm(vmb_shp_sjtsk_a1_read, vmb_shp_sjtsk_a1_read, vmb_hab_dbf_23, vmb_pb_dbf_22, vmb_hab_pb_dbf_22)

for(i in 1:500) {
  habresults_1_500 <- dplyr::bind_rows(habresults_1_500, 
                                       as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1_500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults22_1_500.csv", 
           row.names = FALSE)
for(i in 501:1000) {
  habresults_501_1000 <- dplyr::bind_rows(habresults_501_1000, 
                                          as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_501_1000, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults22_501_1000.csv", 
           row.names = FALSE)
for(i in 1001:1500) {
  habresults_1001_1500 <- dplyr::bind_rows(habresults_1001_1500, 
                                           as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1001_1500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults22_1001_1500.csv", 
           row.names = FALSE)
for(i in 1501:nrow(sites_habitats)) {
  habresults_1501_1893 <- dplyr::bind_rows(habresults_1501_1893, 
                                           as.data.frame(hvezdice_eval_a1(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1501_1893, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults22_1501_1893.csv", 
           row.names = FALSE)

results_habitats <- bind_rows(habresults_1_500[c(2:nrow(habresults_1_500)),], 
                              habresults_501_1000[c(2:nrow(habresults_501_1000)),],
                              habresults_1001_1500[c(2:nrow(habresults_1001_1500)),],
                              habresults_1501_1893[c(2:nrow(habresults_1501_1893)),])

path <- paste0("S:/Složky uživatelů/Gaigr/stanoviste/VMB2/results_habitats_A1_", 
               gsub('-','',Sys.Date()), 
               ".csv")
write.csv2(results_habitats, 
           path, 
           row.names = FALSE,
           fileEncoding = "Windows-1250")

evl_sjtsk %>%
  filter(SITE)

duplicates <- results_habitats %>%
  group_by(SITECODE, HABITAT_CODE) %>%
  filter(n() > 1) %>%
  distinct()



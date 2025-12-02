
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


# VÝPOČET HODNOCENÍ ----
hvezdice_eval <- function(hab_code, evl_site) {
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_akt %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
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
  
  SUM_PLO_BIO_MINIMI <- sum(vmb_target_sjtsk %>%
                              sf::st_drop_geometry() %>%
                              dplyr::filter(DG != "W" & RB != "W") %>%
                              dplyr::pull(PLO_BIO_M2_EVL) %>%
                              sum())
  
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
  
  # MINIMIAREÁL
  if(nrow(vmb_target_sjtsk) > 0) {
    
    minimi_value <- vmb_target_sjtsk %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M2.2" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M3" %in% unique(BIOTOP) ~ 2000/10000,
                                            "M2.1" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M2.3" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M4.2" %in% unique(BIOTOP) ~ 300/10000,
                                            "M4.3" %in% unique(BIOTOP) ~ 1000/10000,
                                            TRUE ~ minimisize %>% 
                                              dplyr::filter(HABITAT == hab_code) %>%
                                              dplyr::pull(MINIMISIZE) %>%
                                              unique() %>%
                                              .[1]/10000)) %>%
      dplyr::pull(MINIMI_SIZE) %>%
      unique()
    
  } else {
    minimi_value <- (minimisize %>% 
                       dplyr::filter(HABITAT == hab_code) %>%
                       dplyr::pull(MINIMISIZE) %>%
                       unique() %>%
                       .[1]/10000)
  }
  
  spat_celistvost <- vmb_target_sjtsk %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    sf::st_buffer(., 50) %>%
    sf::st_union() %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_make_valid() %>%
    base::as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number()) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    dplyr::group_by(ID_COMB) %>%
    dplyr::mutate(COMB_SIZE = sum(PLO_BIO_M2_EVL, na.rm = TRUE)) %>%
    dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE >= 2000 ~ 1,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE >= 300 ~ 1,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "V6" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE < 2000 ~ 0,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE < 300 ~ 0,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          COMB_SIZE >= minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) %>%
                                            unique() %>%
                                            .[1] ~ 1,
                                          COMB_SIZE < minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) %>%
                                            unique() %>%
                                            .[1] ~ 0)) %>%
    dplyr::ungroup()
  
  if(nrow(vmb_target_sjtsk %>%
          sf::st_drop_geometry() %>% 
          dplyr::filter(DG != "W" & RB != "W")) > 0) {
    
    celistvost_minimi <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL) %>%
      sum()
    celistvost <- celistvost_minimi/SUM_PLO_BIO*100
    
    celistvost_num <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(ID_COMB) %>%
      base::unique() %>%
      base::length()
      
  } else {
    celistvost <- NA
    celistvost_num <- NA
  }
  
  # PARAMETR MOZAIKA
  
  # PŘÍPRAVA VRSTVY PRO VÝPOČET PARAMETRU "MOZAIKA"
  vmb_buff <- vmb_shp_sjtsk_akt %>%
    sf::st_filter(., evl_sjtsk %>%
                    dplyr::filter(., SITECODE == evl_site) %>%
                    sf::st_buffer(., 500)) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                    sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                    sf::st_geometry_type(geometry) != "LINESTRING" & 
                    sf::st_geometry_type(geometry) != "MULTILINESTRING" & 
                    sf::st_geometry_type(geometry) != "GEOMETRYCOLLECTION") %>%
    dplyr::filter(FSB_EVAL != "X" &
                    FSB_EVAL != "-" &
                    FSB_EVAL != "-1" &
                    HABITAT != hab_code) %>% 
    dplyr::rename(SEGMENT_ID_buff = SEGMENT_ID) %>%
    dplyr::group_by(SEGMENT_ID_buff) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  vmb_spat <- vmb_target_sjtsk %>%
    dplyr::filter(FSB_EVAL != "X")
  
  spat_union <- vmb_spat %>%
    sf::st_cast(., "POLYGON") %>%
    as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number())
  
  # DÉLKA HRANICE STANOVIŠTĚ S JINÝMI PŘÍRODNÍMI STANOVIŠTI
  if(nrow(vmb_spat) > 0) {
    border_nat <- spat_union %>%
      sf::st_intersection(., vmb_buff) %>%
      dplyr::filter(!SEGMENT_ID_buff %in% vmb_spat$SEGMENT_ID) %>%
      dplyr::group_by(SEGMENT_ID_buff) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # CELKOVÁ DÁLKA HRANIC STANOVIŠTĚ
    border_all <- spat_union %>%
      sf::st_cast(., "LINESTRING") %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # DÉLKA HRANICE STANOVIŠTĚ S HRANICÍ ČR 
    border_hsl <- spat_union %>%
      sf::st_intersection(., czechia_line) %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # VÝPOČET PARAMETRU MOZAIKA
    mozaika_bord <- 1 - border_hsl/border_all
    
    mozaika <- border_nat/(border_all-border_hsl)*mozaika_bord*100

    if(mozaika > 100) {
      mozaika <- 100
    }
    
  } else {
    mozaika <- NA
    mozaika_bord <- NA
  }
  
  # VNITŘNÍ MOZAIKA
  mozaika_inner <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(grepl("X", BIOTOP_SEZ, ignore.case = TRUE)) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    na.omit() %>%
    sum()/SUM_PLO_BIO*100
  
  perc_spat <- sum(vmb_spat$PLO_BIO_M2_EVL)/100/target_area_ha
  

  if (is.na(perc_spat) == TRUE | is.null(perc_spat) == TRUE) {
    mozaika_kompil <- NA
  } else if (perc_spat >= 25) {
    mozaika_kompil <- mozaika
  } else {
    mozaika_kompil <- 100 - mozaika_inner
  } 
  
  # !!!! NUTNO PŘEPSAT PŘI AKTUALIZACI EVL !!!!!
  area_evl_perc <- unique(target_area_ha/(unique(vmb_target_sjtsk$SHAPE_AREA)/10000)*100)
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
  
  # VYPLNĚNOST PARAMETRŮ
  fill_TD <- sum(filter(vmb_target_sjtsk, is.na(TD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_QUAL <- sum(filter(vmb_target_sjtsk, is.na(KVALITA) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_MD <- sum(filter(vmb_target_sjtsk, is.na(MD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_KP <- sum(filter(vmb_target_sjtsk, is.na(MD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  
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
  if(hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(SITECODE == evl_site) %>%
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
      dplyr::filter(SITECODE == evl_site) %>%
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
    dplyr::filter(SITECODE == evl_site) %>%
    dplyr::filter(POKRYVN %in% c("3", "4", "5")) %>%
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
    result <- vmb_qual %>%
      dplyr::reframe(SITECODE = unique(SITECODE)[1],
                       NAZEV = unique(NAZEV)[1],
                       HABITAT_CODE = unique(HABITAT)[1],
                       ROZLOHA = target_area_ha,
                       #ROZLOHA = sum(vmb_target_sjtsk$PLO_BIO_M2_EVL, na.rm = TRUE),
                       KVALITA = unique(QUALITY)[1],
                       TYPICKE_DRUHY = unique(TD_FIN)[1],
                       REPRE = unique(RB_FIN)[1],
                       REPRE_SDF = unique(RB_SDF_FIN)[1],
                       CONSERVATION = unique(CN_FIN)[1],
                       DEGREE_OF_CONSERVATION = unique(DC_FIN)[1],
                       MINIMIAREAL = celistvost,
                       MINIMIAREAL_JADRA = celistvost_num,
                       MINIMIAREAL_HODNOTA = minimi_value,
                       MOZAIKA_VNEJSI = mozaika,
                       MOZAIKA_VNITRNI = (100 - mozaika_inner),
                       MOZAIKA_FIN = mozaika_kompil,
                       RED_LIST = redlist,
                       INVASIVE = invaders,
                       EXPANSIVE = expanders,
                       MRTVE_DREVO = unique(MD_FIN)[1],
                       KALAMITA_POLOM = unique(KP_FIN)[1],
                       RED_LIST_SPECIES = paste(redlist_list, collapse = ", "),
                       INVASIVE_LIST = paste(invaders_list, collapse = ", "),
                       EXPANSIVE_LIST = paste(expanders_list, collapse = ", "),
                       RELATIVE_AREA_PERC = area_relative_perc,
                       EVL_AREA_PERC = area_evl_perc, # NUTNO PŘEPSAT PŘI AKTUALIZACI EVL
                       GOOD_DOC_AREA_HA = area_good_ha,
                       W_AREA_HA = area_w_ha,
                       W_AREA_PERC = area_w_perc,
                       PASEKY_AREA_HA = area_paseky_ha,
                       PASEKY_AREA_PERC = area_paseky_perc,
                       DEGRAD_AREA_HA = area_degrad_ha,
                       DEGRAD_AREA_PERC = area_degrad_perc,
                       VYPLNENOST_TD = fill_TD,
                       VYPLNENOST_KVALITA = fill_QUAL,
                       VYPLNENOST_MD = fill_MD,
                       VYPLNENOST_MOZAIKA = mozaika_bord,
                       PERC_SPAT = perc_spat,
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
    result <- tibble(SITECODE = evl_site,
                     NAZEV = find_evl_CODE_TO_NAME(evl_site),
                     HABITAT_CODE = hab_code,
                     ROZLOHA = target_area_ha,
                     KVALITA = NA,
                     TYPICKE_DRUHY = NA,
                     REPRE = NA,
                     REPRE_SDF = NA,
                     CONSERVATION = NA,
                     DEGREE_OF_CONSERVATION = NA,
                     MINIMIAREAL = NA,
                     MINIMIAREAL_JADRA = NA,
                     MINIMIAREAL_HODNOTA = minimi_value,
                     MOZAIKA_VNEJSI = NA,
                     MOZAIKA_VNITRNI = NA,
                     MOZAIKA_FIN = NA,
                     RED_LIST = NA,
                     INVASIVE = NA,
                     EXPANSIVE = NA,
                     MRTVE_DREVO = NA,
                     KALAMITA_POLOM = NA,
                     RED_LIST_SPECIES = NA_character_,
                     INVASIVE_LIST = NA_character_,
                     EXPANSIVE_LIST = NA_character_,
                     RELATIVE_AREA_PERC = NA,
                     EVL_AREA_PERC = NA,
                     GOOD_DOC_AREA_HA = NA,
                     W_AREA_HA = area_w_ha,
                     W_AREA_PERC = area_w_perc,
                     PASEKY_AREA_HA = area_paseky_ha,
                     PASEKY_AREA_PERC = area_paseky_perc,
                     DEGRAD_AREA_HA = area_degrad_ha,
                     DEGRAD_AREA_PERC = area_degrad_perc,
                     VYPLNENOST_TD = NA,
                     VYPLNENOST_KVALITA = NA,
                     VYPLNENOST_MD = NA,
                     VYPLNENOST_MOZAIKA = NA,
                     PERC_SPAT = NA,
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

n2k_hab_klic <- function(hab_code, evl_site) {
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_akt %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
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
  area_evl_perc <- unique(target_area_ha/(unique(vmb_target_sjtsk$SHAPE_AREA)/10000)*100)
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
    result <- vmb_qual %>%
      dplyr::reframe(SITECODE = unique(SITECODE)[1],
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
    result <- tibble(SITECODE = evl_site,
                     NAZEV = find_evl_CODE_TO_NAME(evl_site),
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

n2k_hab_spat <- function(hab_code, evl_site) {
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_akt %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
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
  
  # MINIMIAREÁL
  if(nrow(vmb_target_sjtsk) > 0) {
    
    minimi_value <- vmb_target_sjtsk %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M2.2" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M3" %in% unique(BIOTOP) ~ 2000/10000,
                                            "M2.1" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M2.3" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M4.2" %in% unique(BIOTOP) ~ 300/10000,
                                            "M4.3" %in% unique(BIOTOP) ~ 1000/10000,
                                            TRUE ~ minimisize %>% 
                                              dplyr::filter(HABITAT == hab_code) %>%
                                              dplyr::pull(MINIMISIZE) %>%
                                              unique() %>%
                                              .[1]/10000)) %>%
      dplyr::pull(MINIMI_SIZE) %>%
      unique()
    
  } else {
    minimi_value <- (minimisize %>% 
                       dplyr::filter(HABITAT == hab_code) %>%
                       dplyr::pull(MINIMISIZE) %>%
                       unique() %>%
                       .[1]/10000)
  }
  
  spat_celistvost <- vmb_target_sjtsk %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    sf::st_buffer(., 50) %>%
    sf::st_union() %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_make_valid() %>%
    base::as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number()) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    dplyr::group_by(ID_COMB) %>%
    dplyr::mutate(COMB_SIZE = sum(PLO_BIO_M2_EVL, na.rm = TRUE)) %>%
    dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE >= 2000 ~ 1,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE >= 300 ~ 1,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "V6" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE < 2000 ~ 0,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE < 300 ~ 0,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          COMB_SIZE >= minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) %>%
                                            unique() %>%
                                            .[1] ~ 1,
                                          COMB_SIZE < minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) %>%
                                            unique() %>%
                                            .[1] ~ 0)) %>%
    dplyr::ungroup()
  
  if(nrow(vmb_target_sjtsk %>%
          sf::st_drop_geometry() %>% 
          dplyr::filter(DG != "W" & RB != "W")) > 0) {
    
    celistvost_minimi <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL) %>%
      sum()
    celistvost <- celistvost_minimi/SUM_PLO_BIO*100
    
    celistvost_num <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(ID_COMB) %>%
      base::unique() %>%
      base::length()
    
  } else {
    celistvost <- NA
    celistvost_num <- NA
  }
  
  # PARAMETR MOZAIKA
  
  # PŘÍPRAVA VRSTVY PRO VÝPOČET PARAMETRU "MOZAIKA"
  vmb_buff <- vmb_shp_sjtsk_akt %>%
    sf::st_filter(., evl_sjtsk %>%
                    dplyr::filter(., SITECODE == evl_site) %>%
                    sf::st_buffer(., 500)) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                    sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                    sf::st_geometry_type(geometry) != "LINESTRING" & 
                    sf::st_geometry_type(geometry) != "MULTILINESTRING" & 
                    sf::st_geometry_type(geometry) != "GEOMETRYCOLLECTION") %>%
    dplyr::filter(FSB_EVAL != "X" &
                    FSB_EVAL != "-" &
                    FSB_EVAL != "-1" &
                    HABITAT != hab_code) %>% 
    dplyr::rename(SEGMENT_ID_buff = SEGMENT_ID) %>%
    dplyr::group_by(SEGMENT_ID_buff) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  vmb_spat <- vmb_target_sjtsk %>%
    dplyr::filter(FSB_EVAL != "X")
  
  spat_union <- vmb_spat %>%
    sf::st_cast(., "POLYGON") %>%
    as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number())
  
  # DÉLKA HRANICE STANOVIŠTĚ S JINÝMI PŘÍRODNÍMI STANOVIŠTI
  if(nrow(vmb_spat) > 0) {
    border_nat <- spat_union %>%
      sf::st_intersection(., vmb_buff) %>%
      dplyr::filter(!SEGMENT_ID_buff %in% vmb_spat$SEGMENT_ID) %>%
      dplyr::group_by(SEGMENT_ID_buff) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # CELKOVÁ DÁLKA HRANIC STANOVIŠTĚ
    border_all <- spat_union %>%
      sf::st_cast(., "LINESTRING") %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # DÉLKA HRANICE STANOVIŠTĚ S HRANICÍ ČR 
    border_hsl <- spat_union %>%
      sf::st_intersection(., czechia_line) %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # VÝPOČET PARAMETRU MOZAIKA
    mozaika_bord <- 1 - border_hsl/border_all
    
    mozaika <- border_nat/(border_all-border_hsl)*mozaika_bord*100
    
    if(mozaika > 100) {
      mozaika <- 100
    }
    
  } else {
    mozaika <- NA
    mozaika_bord <- NA
  }
  
  # VNITŘNÍ MOZAIKA
  mozaika_inner <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(grepl("X", BIOTOP_SEZ, ignore.case = TRUE)) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    na.omit() %>%
    sum()/SUM_PLO_BIO*100
  
  perc_spat <- sum(vmb_spat$PLO_BIO_M2_EVL)/100/target_area_ha
  
  
  if (is.na(perc_spat) == TRUE | is.null(perc_spat) == TRUE) {
    mozaika_kompil <- NA
  } else if (perc_spat >= 25) {
    mozaika_kompil <- mozaika
  } else {
    mozaika_kompil <- 100 - mozaika_inner
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
    result <- vmb_qual %>%
      dplyr::reframe(SITECODE = unique(SITECODE)[1],
                     NAZEV = unique(NAZEV)[1],
                     HABITAT_CODE = unique(HABITAT)[1],
                     ROZLOHA = target_area_ha,
                     #ROZLOHA = sum(vmb_target_sjtsk$PLO_BIO_M2_EVL, na.rm = TRUE),
                     KVALITA = unique(QUALITY)[1],
                     TYPICKE_DRUHY = unique(TD_FIN)[1],
                     REPRE = unique(RB_FIN)[1],
                     REPRE_SDF = unique(RB_SDF_FIN)[1],
                     CONSERVATION = unique(CN_FIN)[1],
                     DEGREE_OF_CONSERVATION = unique(DC_FIN)[1],
                     MINIMIAREAL = celistvost,
                     MINIMIAREAL_JADRA = celistvost_num,
                     MINIMIAREAL_HODNOTA = minimi_value,
                     MOZAIKA_VNEJSI = mozaika,
                     MOZAIKA_VNITRNI = (100 - mozaika_inner),
                     MOZAIKA_FIN = mozaika_kompil,
                     RED_LIST = redlist,
                     INVASIVE = invaders,
                     EXPANSIVE = expanders,
                     MRTVE_DREVO = unique(MD_FIN)[1],
                     KALAMITA_POLOM = unique(KP_FIN)[1],
                     RED_LIST_SPECIES = paste(redlist_list, collapse = ", "),
                     INVASIVE_LIST = paste(invaders_list, collapse = ", "),
                     EXPANSIVE_LIST = paste(expanders_list, collapse = ", "),
                     RELATIVE_AREA_PERC = area_relative_perc,
                     EVL_AREA_PERC = area_evl_perc, # NUTNO PŘEPSAT PŘI AKTUALIZACI EVL
                     GOOD_DOC_AREA_HA = area_good_ha,
                     W_AREA_HA = area_w_ha,
                     W_AREA_PERC = area_w_perc,
                     PASEKY_AREA_HA = area_paseky_ha,
                     PASEKY_AREA_PERC = area_paseky_perc,
                     DEGRAD_AREA_HA = area_degrad_ha,
                     DEGRAD_AREA_PERC = area_degrad_perc,
                     VYPLNENOST_TD = fill_TD,
                     VYPLNENOST_KVALITA = fill_QUAL,
                     VYPLNENOST_MD = fill_MD,
                     VYPLNENOST_MOZAIKA = mozaika_bord,
                     PERC_SPAT = perc_spat,
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
    result <- tibble(SITECODE = evl_site,
                     NAZEV = find_evl_CODE_TO_NAME(evl_site),
                     HABITAT_CODE = hab_code,
                     ROZLOHA = target_area_ha,
                     KVALITA = NA,
                     TYPICKE_DRUHY = NA,
                     REPRE = NA,
                     REPRE_SDF = NA,
                     CONSERVATION = NA,
                     DEGREE_OF_CONSERVATION = NA,
                     MINIMIAREAL = NA,
                     MINIMIAREAL_JADRA = NA,
                     MINIMIAREAL_HODNOTA = minimi_value,
                     MOZAIKA_VNEJSI = NA,
                     MOZAIKA_VNITRNI = NA,
                     MOZAIKA_FIN = NA,
                     RED_LIST = NA,
                     INVASIVE = NA,
                     EXPANSIVE = NA,
                     MRTVE_DREVO = NA,
                     KALAMITA_POLOM = NA,
                     RED_LIST_SPECIES = NA_character_,
                     INVASIVE_LIST = NA_character_,
                     EXPANSIVE_LIST = NA_character_,
                     RELATIVE_AREA_PERC = NA,
                     EVL_AREA_PERC = NA,
                     GOOD_DOC_AREA_HA = NA,
                     W_AREA_HA = area_w_ha,
                     W_AREA_PERC = area_w_perc,
                     PASEKY_AREA_HA = area_paseky_ha,
                     PASEKY_AREA_PERC = area_paseky_perc,
                     DEGRAD_AREA_HA = area_degrad_ha,
                     DEGRAD_AREA_PERC = area_degrad_perc,
                     VYPLNENOST_TD = NA,
                     VYPLNENOST_KVALITA = NA,
                     VYPLNENOST_MD = NA,
                     VYPLNENOST_MOZAIKA = NA,
                     PERC_SPAT = NA,
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

n2k_hab_druhy <- function(hab_code, evl_site) {
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_akt %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
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
  if(hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(SITECODE == evl_site) %>%
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
      dplyr::filter(SITECODE == evl_site) %>%
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
    dplyr::filter(SITECODE == evl_site) %>%
    dplyr::filter(POKRYVN %in% c("3", "4", "5")) %>%
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
    result <- vmb_qual %>%
      dplyr::reframe(SITECODE = unique(SITECODE)[1],
                     NAZEV = unique(NAZEV)[1],
                     HABITAT_CODE = unique(HABITAT)[1],
                     ROZLOHA = target_area_ha,
                     RED_LIST = redlist,
                     INVASIVE = invaders,
                     EXPANSIVE = expanders,
                     RED_LIST_SPECIES = paste(redlist_list, collapse = ", "),
                     INVASIVE_LIST = paste(invaders_list, collapse = ", "),
                     EXPANSIVE_LIST = paste(expanders_list, collapse = ", "),
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
    result <- tibble(SITECODE = evl_site,
                     NAZEV = find_evl_CODE_TO_NAME(evl_site),
                     HABITAT_CODE = hab_code,
                     ROZLOHA = target_area_ha,
                     RED_LIST = NA,
                     INVASIVE = NA,
                     EXPANSIVE = NA,
                     RED_LIST_SPECIES = NA_character_,
                     INVASIVE_LIST = NA_character_,
                     EXPANSIVE_LIST = NA_character_,
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


hvezdice_eval_a1 <- function(hab_code, evl_site) {
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_a1 %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
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
  area_paseky_ha <- paseky_a1 %>%
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
  
  evl_area <- sf::st_area(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
    units::drop_units() %>%
    unique()
  
  area_w_ha <- vmb_target_sjtsk %>%
    dplyr::filter(DG == "W" | RB == "W") %>%
    pull(PLO_BIO_M2_EVL) %>%
    na.omit() %>%
    sum()/10000
  area_w_perc <- area_w_ha/target_area_ha*100
  
  area_paseky_perc <- area_paseky_ha/target_area_ha*100
  
  area_degrad_ha <- sum(area_w_ha, area_paseky_ha, na.rm = TRUE)
  area_degrad_perc <- area_degrad_ha/target_area_ha*100
  
  SUM_PLO_BIO_MINIMI <- sum(vmb_target_sjtsk %>%
                              dplyr::filter(DG != "W" & RB != "W") %>%
                              dplyr::pull(PLO_BIO_M2_EVL) %>%
                              sum())
  
  # KVALITATIVNÍ PARAMETRY HODNOCENÍ
  vmb_qual <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    distinct() %>%
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
      TD_FIN = 3 - sum(na.omit(TD_SEG)),
      # REPREZENTATIVITA
      RB_FIN = sum(na.omit(RB_SEG)),
      # REPREZENTATIVITA SDF
      RB_SDF_FIN = 4 - sum(na.omit(RB_SDF_SEG)),
      # DEGREE OF CONSERVATION
      DC_FIN = sum(na.omit(DC_SEG)),
      # CONSERVATION
      CN_FIN = 3 - sum(na.omit(CN_SEG)),
      # DEGREE OF CONSERVATION
      # MRTVÉ DŘEVO
      MD_FIN = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                TRUE ~ sum(na.omit(MD_SEG))),
      # KALAMITA A POLOM
      KP_FIN = dplyr::case_when(substr(hab_code, 1, 1) != 9 ~ NA_real_,
                                TRUE ~ sum(na.omit(KAL_SEG))),
      # KVALITA
      QUALITY = 4 - sum(na.omit(QUAL_SEG)))
  
  # MINIMIAREÁL
  if(nrow(vmb_target_sjtsk) > 0) {
    
    minimi_value <- vmb_target_sjtsk %>%
      dplyr::mutate(
        MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) ~ 1000/10000,
                                "M2.2" %in% unique(BIOTOP) ~ 1000/10000,
                                "M3" %in% unique(BIOTOP) ~ 2000/10000,
                                "M2.1" %in% unique(BIOTOP) ~ 7000/10000,
                                "M2.3" %in% unique(BIOTOP) ~ 7000/10000,
                                "M4.2" %in% unique(BIOTOP) ~ 300/10000,
                                "M4.3" %in% unique(BIOTOP) ~ 1000/10000,
                                TRUE ~ minimisize %>% 
                                dplyr::filter(HABITAT == hab_code) %>%
                                  dplyr::pull(MINIMISIZE) %>%
                                  unique() %>%
                                  .[1]/10000)
        ) %>%
      dplyr::pull(MINIMI_SIZE) %>%
      unique() %>%
      .[1]
    
  } else {
    minimi_value <- (minimisize %>% 
                       dplyr::filter(HABITAT == hab_code) %>%
                       dplyr::pull(MINIMISIZE) %>%
                       unique() %>%
                       .[1]/10000)
  }
  
  spat_celistvost <- vmb_target_sjtsk %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    sf::st_buffer(., 50) %>%
    sf::st_union() %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_make_valid() %>%
    base::as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number()) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    dplyr::group_by(ID_COMB) %>%
    dplyr::mutate(COMB_SIZE = sum(PLO_BIO_M2_EVL, na.rm = TRUE)) %>%
    dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE >= 2000 ~ 1,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE >= 300 ~ 1,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "V6" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE < 2000 ~ 0,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE < 300 ~ 0,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          COMB_SIZE >= minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) %>%
                                            unique() %>%
                                            .[1] ~ 1,
                                          COMB_SIZE < minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) %>%
                                            unique() %>%
                                            .[1] ~ 0)) %>%
    dplyr::ungroup()
  
  if(nrow(vmb_target_sjtsk %>% dplyr::filter(DG != "W" & RB != "W")) > 0) {
    
    celistvost_minimi <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL) %>%
      sum()
    celistvost <- celistvost_minimi/SUM_PLO_BIO*100
    
    celistvost_num <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(ID_COMB) %>%
      base::unique() %>%
      base::length()
    
  } else {
    celistvost <- NA
    celistvost_num <- NA
  }
  
  # PARAMETR MOZAIKA
  # PŘÍPRAVA VRSTVY PRO VÝPOČET PARAMETRU "MOZAIKA"
  vmb_buff <- vmb_shp_sjtsk_a1 %>%
    sf::st_filter(., evl_sjtsk %>%
                    dplyr::filter(., SITECODE == evl_site) %>%
                    sf::st_buffer(., 500)) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                    sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                    sf::st_geometry_type(geometry) != "LINESTRING" & 
                    sf::st_geometry_type(geometry) != "MULTILINESTRING" & 
                    sf::st_geometry_type(geometry) != "GEOMETRYCOLLECTION") %>%
    dplyr::filter(FSB_EVAL != "X" &
                    FSB_EVAL != "-" &
                    FSB_EVAL != "-1" &
                    HABITAT != hab_code) %>% 
    dplyr::rename(SEGMENT_ID_buff = SEGMENT_ID) %>%
    dplyr::group_by(SEGMENT_ID_buff) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  
  vmb_spat <- vmb_target_sjtsk %>%
    dplyr::filter(FSB_EVAL != "X")
  
  spat_union <- vmb_spat %>%
    sf::st_cast(., "POLYGON") %>%
    as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number())
  
  # DÉLKA HRANICE STANOVIŠTĚ S JINÝMI PŘÍRODNÍMI STANOVIŠTI
  if(nrow(vmb_spat) > 0) {
    border_nat <- spat_union %>%
      sf::st_intersection(., vmb_buff) %>%
      dplyr::filter(!SEGMENT_ID_buff %in% vmb_spat$SEGMENT_ID) %>%
      dplyr::group_by(SEGMENT_ID_buff) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # CELKOVÁ DÁLKA HRANIC STANOVIŠTĚ
    border_all <- spat_union %>%
      sf::st_cast(., "LINESTRING") %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # DÉLKA HRANICE STANOVIŠTĚ S HRANICÍ ČR 
    border_hsl <- spat_union %>%
      sf::st_intersection(., czechia_line) %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # VÝPOČET PARAMETRU MOZAIKA
    mozaika_bord <- 1 - border_hsl/border_all
    
    mozaika <- border_nat/(border_all-border_hsl)*mozaika_bord*100
    
    if(mozaika > 100) {
      mozaika <- 100
    }
    
  } else {
    mozaika <- NA
    mozaika_bord <- NA
  }
  
  # VNITŘNÍ MOZAIKA
  mozaika_inner <- vmb_target_sjtsk %>%
    dplyr::filter(grepl("X", BIOTOP_SEZ, ignore.case = TRUE)) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    na.omit() %>%
    sum()/SUM_PLO_BIO*100
  
  perc_spat <- sum(vmb_spat$PLO_BIO_M2_EVL)/100/target_area_ha
  
  
  if (is.na(perc_spat) == TRUE | is.null(perc_spat) == TRUE) {
    mozaika_kompil <- NA
  } else if (perc_spat >= 25) {
    mozaika_kompil <- mozaika
  } else {
    mozaika_kompil <- 100 - mozaika_inner
  } 
  
  area_evl_perc <- target_area_ha/evl_area/10000*100
  area_relative_perc <- target_area_ha/habitat_areas_a1 %>%
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
  
  # VYPLNĚNOST PARAMETRŮ
  fill_TD <- sum(filter(vmb_target_sjtsk, is.na(TD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_QUAL <- sum(filter(vmb_target_sjtsk, is.na(KVALITA) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_MD <- sum(filter(vmb_target_sjtsk, is.na(MD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_KP <- sum(filter(vmb_target_sjtsk, is.na(MD) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  
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
  if(!is.na(hab_code) && hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(SITECODE == evl_site) %>%
      dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID_1.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  } else {
    invaders_all <- invasive_species %>%
      dplyr::filter(SITECODE == evl_site) %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID_1.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  }
  
  invaders_calc <- invaders_all %>%
    dplyr::group_by(OBJECTID_1.y) %>%
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
    dplyr::filter(SITECODE == evl_site) %>%
    dplyr::filter(POKRYVN %in% c("3", "4", "5")) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::filter(is.na(HABITAT) == FALSE) %>%
    dplyr::group_by(OBJECTID_1.y, DRUH) %>%
    dplyr::filter(DATUM_OD >= DATUM.x) %>%
    dplyr::slice(which.max(DATUM_OD)) %>%
    dplyr::filter(NEGATIVNI == 0) %>%
    dplyr::ungroup()
  
  expanders_calc <- expanders_all %>%
    dplyr::group_by(OBJECTID_1.y) %>%
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
    pull(DATUM.x)
  
  min_date <- vmb_target_date %>%
    min() %>%
    unique()
  
  max_date <- vmb_target_date %>%
    max() %>%
    unique()
  
  mean_date <- mean(vmb_target_date)
  
  median_date <- median(vmb_target_date)
  
  perc_seg_0 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT.y == 0) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  perc_seg_1 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT.y > 0 & ROK_AKT.y <= 2012) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  perc_seg_2 <- vmb_target_sjtsk %>%
    dplyr::filter(ROK_AKT.y > 2012 & ROK_AKT.y <= 2024) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()/target_area_ha/100
  
  if(nrow(vmb_target_sjtsk) == 0) {
    perc_seg_0 <- NA
    perc_seg_1 <- NA
    perc_seg_2 <- NA
  }
  
  
  # VÝSLEDKY
  if (sum(vmb_target_sjtsk$PLO_BIO_M2_EVL, na.rm = TRUE) > 0) {
    
    result <- vmb_qual %>%
      dplyr::summarise(
        SITECODE = evl_site,
        NAZEV = sites_habitats %>% dplyr::filter(site_code == evl_site) %>% pull(site_name),
        HABITAT_CODE = hab_code,
        ROZLOHA = target_area_ha,
        KVALITA = unique(QUALITY)[1],
        TYPICKE_DRUHY = unique(TD_FIN)[1],
        REPRE = unique(RB_FIN)[1],
        REPRE_SDF = unique(RB_SDF_FIN)[1],
        CONSERVATION = unique(CN_FIN)[1],
        DEGREE_OF_CONSERVATION = unique(DC_FIN)[1],
        MINIMIAREAL = celistvost,
        MINIMIAREAL_JADRA = celistvost_num,
        MINIMIAREAL_HODNOTA = minimi_value,
        MOZAIKA_VNEJSI = mozaika,
        MOZAIKA_VNITRNI = (100 - mozaika_inner),
        MOZAIKA_FIN = mozaika_kompil,
        RED_LIST = redlist,
        INVASIVE = invaders,
        EXPANSIVE = expanders,
        MRTVE_DREVO = unique(MD_FIN)[1],
        KALAMITA_POLOM = unique(KP_FIN)[1],
        RED_LIST_SPECIES = paste(redlist_list, collapse = ", "),
        INVASIVE_LIST = paste(invaders_list, collapse = ", "),
        EXPANSIVE_LIST = paste(expanders_list, collapse = ", "),
        RELATIVE_AREA_PERC = area_relative_perc,
        EVL_AREA_PERC = area_evl_perc,
        GOOD_DOC_AREA_HA = area_good_ha,
        W_AREA_HA = area_w_ha,
        W_AREA_PERC = area_w_perc,
        PASEKY_AREA_HA = area_paseky_ha,
        PASEKY_AREA_PERC = area_paseky_perc,
        DEGRAD_AREA_HA = area_degrad_ha,
        DEGRAD_AREA_PERC = area_degrad_perc,
        PERC_SPAT = perc_spat,
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
    result <- tibble(
      SITECODE = evl_site,
      NAZEV = sites_habitats %>% dplyr::filter(site_code == evl_site) %>% pull(site_name),
      HABITAT_CODE = hab_code,
      ROZLOHA = NA,
      KVALITA = NA,
      TYPICKE_DRUHY = NA,
      REPRE = NA,
      REPRE_SDF = NA,
      CONSERVATION = NA,
      DEGREE_OF_CONSERVATION = NA,
      MINIMIAREAL = NA,
      MINIMIAREAL_JADRA = NA,
      MINIMIAREAL_HODNOTA = NA,
      MOZAIKA_VNEJSI = NA,
      MOZAIKA_VNITRNI = NA,
      MOZAIKA_FIN = NA,
      RED_LIST = NA,
      INVASIVE = NA,
      EXPANSIVE = NA,
      MRTVE_DREVO = NA,
      KALAMITA_POLOM = NA,
      RED_LIST_SPECIES = NA,
      INVASIVE_LIST = NA,
      EXPANSIVE_LIST = NA,
      RELATIVE_AREA_PERC = NA,
      EVL_AREA_PERC = NA,
      GOOD_DOC_AREA_HA = NA,
      W_AREA_HA = NA,
      W_AREA_PERC = NA,
      PASEKY_AREA_HA = NA,
      PASEKY_AREA_PERC = NA,
      DEGRAD_AREA_HA = NA,
      DEGRAD_AREA_PERC = NA,
      PASEKY_AREA_HA = area_paseky_ha,
      PASEKY_AREA_PERC = area_paseky_perc,
      DEGRAD_AREA_HA = area_degrad_ha,
      DEGRAD_AREA_PERC = area_degrad_perc,
      PERC_SPAT = NA,
      PERC_0 = NA,
      PERC_1 = NA,
      PERC_2 = NA,
      DATE_MIN = NA,
      DATE_MAX = NA,
      DATE_MEAN = NA,
      DATE_MEDIAN = NA
    ) %>%
      dplyr::distinct()
  }
  
  
  return(result)
  
}

hvezdice_eval_a1("3260", "CZ0323142")
hvezdice_eval_a1(sites_habitats[1205,5], sites_habitats[1205,1])

hvezdice_update <- function(evl_site) {
  
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_akt %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                    sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                    sf::st_geometry_type(geometry) != "LINESTRING" & 
                    sf::st_geometry_type(geometry) != "MULTILINESTRING" & 
                    sf::st_geometry_type(geometry) != "GEOMETRYCOLLECTION POINT" & 
                    sf::st_geometry_type(geometry) != "GEOMETRYCOLLECTION LINESTRING") %>%
    dplyr::mutate(AREA_real = units::drop_units(sf::st_area(geometry))) %>%
    dplyr::filter(AREA_real > 0) %>%
    dplyr::mutate(PLO_BIO_M2_EVL = STEJ_PR/100*AREA_real)
  
  SUM_PLO_BIO <- sum(vmb_target_sjtsk$PLO_BIO_M2_EVL, na.rm = TRUE)
  
  result <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(YEAR = stringr::str_sub(DATUM.x, 1, 4)) %>%
    dplyr::group_by(YEAR) %>%
    dplyr::summarize(SITECODE = unique(SITECODE),
                     PLOCHA_HA = sum(PLO_BIO_M2_EVL, na.rm = TRUE), 
                     PLOCHA_PERC = sum(PLO_BIO_M2_EVL, na.rm = TRUE)/SUM_PLO_BIO) %>%
    dplyr::ungroup() %>%
    dplyr::filter(PLOCHA_HA > 0)
  
  return(result)
  
}

hvezdice_eval_orig <- function(hab_code, evl_site) {
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_orig %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
    dplyr::filter(HABITAT == hab_code) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                    sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                    sf::st_geometry_type(geometry) != "LINESTRING" & 
                    sf::st_geometry_type(geometry) != "MULTILINESTRING") %>%
    dplyr::mutate(AREA_real = units::drop_units(st_area(geometry))) %>%
    dplyr::filter(AREA_real > 0) %>%
    dplyr::mutate(PLO_BIO_M2_EVL = STEJ_PR/100*AREA_real)
  
  # CELKOVÁ PLOCHA HABITATU VČETNĚ PASEK
  SUM_PLO_BIO <- vmb_target_sjtsk %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    sum()
  
  target_area_ha <- SUM_PLO_BIO/10000
  
  area_w_ha <- vmb_target_sjtsk %>%
    dplyr::filter(REPRE == "D") %>%
    pull(PLO_BIO_M2_EVL) %>%
    sum()/10000
  area_w_perc <- area_w_ha/target_area_ha*100
  
  vmb_qual <- vmb_target_sjtsk %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      REPRE_SEG = dplyr::case_when(REPRE == "A" ~ 10,
                                   REPRE == "B" ~ 6.6666666666666666666666,
                                   REPRE == "C" ~ 3.3333333333333333333333,
                                   REPRE == "D" ~ 0),
      ZACH_SEG = dplyr::case_when(ZACH == "A" ~ 2,
                                  ZACH == "B" ~ 1,
                                  ZACH == "C" ~ 0),
      KVALITA_SEG = dplyr::case_when(REPRE == "D" ~ 0,
                                     is.na(KVALITA) == TRUE ~ 0,  
                                     KVALITA == 0 ~ 0,
                                     KVALITA == 1 ~ 3,
                                     KVALITA == 2 ~ 2,
                                     KVALITA == 3 ~ 1,
                                     KVALITA == 4 ~ 0),
      RB_SEG = REPRE_SEG*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL),
      ZA_SEG = ZACH_SEG*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL),
      QUAL_SEG = KVALITA_SEG*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL)) %>%
    dplyr::mutate(
      # TYPICKÉ DRUHY
      RB_FIN = sum(na.omit(RB_SEG)),
      ZA_FIN = sum(na.omit(ZA_SEG)),
      QUALITY = 4 - sum(na.omit(QUAL_SEG)))
  
  vmb_spat <- vmb_target_sjtsk %>%
    dplyr::filter(FSB_EVAL != "X")
  
  spat_union <- vmb_spat %>%
    st_union() %>%
    st_cast(., "POLYGON") %>%
    as.data.frame() %>%
    st_as_sf() %>%
    mutate(ID_COMB = row_number())
  
  vmb_spat <- vmb_target_sjtsk %>%
    dplyr::filter(FSB_EVAL != "X")
  
  # VÝPOČET PARAMETRU CELISTVOST
  if(nrow(vmb_target_sjtsk) > 0) {
    
    minimi_value <- vmb_target_sjtsk %>%
      dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M2.2" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M3" %in% unique(BIOTOP) ~ 2000/10000,
                                            "M2.1" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M2.3" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M4.2" %in% unique(BIOTOP) ~ 300/10000,
                                            "M4.3" %in% unique(BIOTOP) ~ 1000/10000,
                                            TRUE ~ minimisize %>% 
                                              dplyr::filter(HABITAT == hab_code) %>%
                                              dplyr::pull(MINIMISIZE)/10000)) %>%
      dplyr::pull(MINIMI_SIZE) %>%
      unique()
    
  } else {
    
    minimi_value <- minimisize %>% 
      dplyr::filter(HABITAT == hab_code) %>%
      dplyr::pull(MINIMISIZE)/10000
    
  }
  
  # MINIMIAREAL
  spat_celistvost <- vmb_target_sjtsk %>%
    dplyr::filter(REPRE != "D") %>%
    sf::st_buffer(., 50) %>%
    sf::st_union() %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_make_valid() %>%
    base::as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number()) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(REPRE != "D") %>%
    dplyr::group_by(ID_COMB) %>%
    dplyr::mutate(COMB_SIZE = sum(PLO_BIO_M2_EVL, na.rm = TRUE)) %>%
    dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE >= 2000 ~ 1,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE >= 300 ~ 1,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "V6" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE < 2000 ~ 0,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE < 300 ~ 0,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          COMB_SIZE >= minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) ~ 1,
                                          COMB_SIZE < minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) ~ 0)) %>%
    dplyr::ungroup()
  
  if(nrow(vmb_target_sjtsk %>% dplyr::filter(REPRE != "D")) > 0) {
    
    celistvost_minimi <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL) %>%
      sum()
    celistvost <- celistvost_minimi/SUM_PLO_BIO*100
    
    celistvost_num <- spat_celistvost %>%
      dplyr::filter(MINIMI_SIZE == 1) %>%
      dplyr::pull(ID_COMB) %>%
      base::unique() %>%
      base::length()
    
  } else {
    celistvost <- NA
    celistvost_num <- NA
  }
  
  # PŘÍPRAVA VRSTVY PRO VÝPOČET PARAMETRU "MOZAIKA"
  vmb_buff <- vmb_shp_sjtsk_orig %>%
    sf::st_filter(., evl_sjtsk %>%
                    dplyr::filter(., SITECODE == evl_site) %>%
                    sf::st_buffer(., 500)) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                    sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                    sf::st_geometry_type(geometry) != "LINESTRING" & 
                    sf::st_geometry_type(geometry) != "MULTILINESTRING" & 
                    sf::st_geometry_type(geometry) != "GEOMETRYCOLLECTION") %>%
    dplyr::filter(FSB_EVAL != "X" &
                    FSB_EVAL != "-" &
                    FSB_EVAL != "-1" &
                    HABITAT != hab_code) %>% 
    dplyr::rename(SEGMENT_ID_buff = SEGMENT_ID) %>%
    dplyr::group_by(SEGMENT_ID_buff) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # PARAMETR MOZAIKA
  spat_union <- vmb_spat %>%
    sf::st_cast(., "POLYGON") %>%
    as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number())
  
  # DÉLKA HRANICE STANOVIŠTĚ S JINÝMI PŘÍRODNÍMI STANOVIŠTI
  if(nrow(vmb_spat) > 0) {
    border_nat <- spat_union %>%
      sf::st_intersection(., vmb_buff) %>%
      dplyr::filter(!SEGMENT_ID_buff %in% vmb_spat$SEGMENT_ID) %>%
      dplyr::group_by(SEGMENT_ID_buff) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # CELKOVÁ DÁLKA HRANIC STANOVIŠTĚ
    border_all <- spat_union %>%
      sf::st_cast(., "LINESTRING") %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # DÉLKA HRANICE STANOVIŠTĚ S HRANICÍ ČR 
    border_hsl <- spat_union %>%
      sf::st_intersection(., czechia_line) %>%
      sf::st_length() %>% 
      units::drop_units() %>%
      sum()
    
    # VÝPOČET PARAMETRU MOZAIKA
    mozaika_bord <- 1 - border_hsl/border_all
    
    mozaika <- border_nat/(border_all-border_hsl)*mozaika_bord*100
    
    if(mozaika > 100) {
      mozaika <- 100
    }
    
  } else {
    mozaika <- NA
    mozaika_bord <- NA
  }
  
  # VNITŘNÍ MOZAIKA
  mozaika_inner <- vmb_qual %>%
    dplyr::filter(grepl("X", BIOTOP_SEZ, ignore.case = TRUE)) %>%
    dplyr::pull(PLO_BIO_M2_EVL) %>%
    na.omit() %>%
    sum()/SUM_PLO_BIO*100
  
  perc_spat <- sum(vmb_spat$PLO_BIO_M2_EVL)/100/target_area_ha
  
  
  if (is.na(perc_spat) == TRUE | is.null(perc_spat) == TRUE) {
    mozaika_kompil <- NA
  } else if (perc_spat >= 25) {
    mozaika_kompil <- mozaika
  } else {
    mozaika_kompil <- 100 - mozaika_inner
  } 
  
  # CELKOVÁ ROZLOHA STANOVIŠTĚ V EVL EVL
  target_area_ha <- sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)/10000
  
  if(nrow(vmb_target_sjtsk) == 0) {
    target_area_ha <- 0
  }
  
  # VYPLNĚNOST PARAMETRŮ
  fill_REPRE <- sum(filter(vmb_target_sjtsk, is.na(REPRE) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)
  fill_QUAL <- sum(filter(vmb_target_sjtsk, is.na(KVALITA) == FALSE)$PLO_BIO_M2_EVL)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)

  # RED LIST SPECIES
  redlist_list <- red_list_species %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::filter(DATUM_OD >= DATUM.x) %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  redlist <- redlist_list %>%
    length()/log(sum(vmb_target_sjtsk$PLO_BIO_M2_EVL))*4
  
  if(redlist > 10) {
    redlist <- 10
  } 
  
  if(length(redlist_list) == 0) {
    redlist_list <- NA_character_
  } 
  
  if(nrow(vmb_target_sjtsk) == 0) {
    redlist_list <- NA_character_
    redlist <- NA
  }
  
  # INVASIVE SPECIES
  if(hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(SEGMENT_ID, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x & DATUM_OD < 2005) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  } else {
    invaders_all <- invasive_species %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(SEGMENT_ID, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x & DATUM_OD < 2005) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  }
  
  invaders_calc <- invaders_all %>%
    dplyr::group_by(SEGMENT_ID, BIOTOP) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  invaders_list <- invaders_all %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  
  invaders <- sum(invaders_calc$PLO_BIO_M2_EVL, na.rm = TRUE)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL, na.rm = TRUE)*100
  
  if(length(invaders_list) == 0 |
     nrow(vmb_target_sjtsk) == 0) {
    invaders_list <- NA
  }
  
  # EXPANZNÍ DRUHY
  expanders_all <- expansive_species %>%
    dplyr::filter(POKRYVN %in% c("3", "4", "5")) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::filter(is.na(HABITAT) == FALSE) %>%
    dplyr::group_by(SEGMENT_ID, DRUH) %>%
    dplyr::filter(DATUM_OD >= DATUM.x & DATUM_OD < 2005) %>%
    dplyr::slice(which.max(DATUM_OD)) %>%
    dplyr::filter(NEGATIVNI == 0) %>%
    dplyr::ungroup()
  
  expanders_calc <- expanders_all %>%
    dplyr::group_by(SEGMENT_ID, BIOTOP) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  expanders_list <- expanders_all %>%
    dplyr::pull(DRUH) %>%
    unique() 
  
  expanders <- sum(expanders_calc$PLO_BIO_M2_EVL, na.rm = TRUE)/sum(vmb_target_sjtsk$PLO_BIO_M2_EVL, na.rm = TRUE)*100
  
  
  if(length(expanders_list) == 0 &
     nrow(vmb_target_sjtsk) == 0) {
    expanders <- NA
    expanders_list <- NA
  } else if (length(expanders_list) == 0 &
             nrow(vmb_target_sjtsk) > 0) {
    expanders <- 0
    expanders_list <- NA
  }
  
  perc_spat <- sum(vmb_spat$PLO_BIO_M2_EVL)/100/target_area_ha
  
  if(nrow(vmb_target_sjtsk) == 0) {
    perc_spat <- NA
  }
  
  area_good_ha <- vmb_target_sjtsk %>%
    dplyr::filter(REPRE == "A" | REPRE == "B" | ZACH == "A"| ZACH == "B") %>%
    pull(PLO_BIO_M2_EVL) %>%
    sum()/10000
  
  if(nrow(vmb_target_sjtsk) == 0 & is.na(target_area_ha)) {
    target_area_ha <- 0
    area_w_ha <- 0
    area_w_perc <- 0
    area_good_ha <- 0
  }
  
  vmb_target_date <- vmb_target_sjtsk %>%
    pull(DATUM.x)
  
  min_date <- vmb_target_date %>%
    min() %>%
    unique()
  
  max_date <- vmb_target_date %>%
    max() %>%
    unique()
  
  mean_date <- mean(vmb_target_date)
  
  median_date <- median(vmb_target_date)
  
  # VÝSLEDKY
  if(nrow(vmb_target_sjtsk) > 0) {
    result <- vmb_qual %>%
      dplyr::summarise(SITECODE = unique(SITECODE),
                       NAZEV = unique(NAZEV),
                       HABITAT_CODE = unique(HABITAT),
                       ROZLOHA = target_area_ha,
                       REPRE = unique(RB_FIN),
                       KVALITA = unique(QUALITY),
                       MINIMIAREAL = celistvost,
                       MINIMIAREAL_JADRA = celistvost_num,
                       MINIMIAREAL_HODNOTA = minimi_value,
                       MOZAIKA = mozaika_kompil,
                       RED_LIST = redlist,
                       INVASIVE = invaders,
                       EXPANSIVE = expanders,
                       RED_LIST_SPECIES = paste(redlist_list, collapse = ", "),
                       INVASIVE_LIST = paste(invaders_list, collapse = ", "),
                       EXPANSIVE_LIST = paste(expanders_list, collapse = ", "),
                       GOOD_DOC_AREA_HA = area_good_ha,
                       W_AREA_HA = area_w_ha,
                       W_AREA_PERC = area_w_perc,
                       DEGRAD_AREA_HA = area_w_ha,
                       DEGRAD_AREA_PERC = area_w_perc,
                       VYPLNENOST_REPRE = fill_REPRE,
                       VYPLNENOST_KVALITA = fill_QUAL,
                       VYPLNENOST_MOZAIKA = mozaika_bord,
                       PERC_SPAT = perc_spat,
                       DATE_MIN = min_date,
                       DATE_MAX = max_date,
                       DATE_MEAN = mean_date,
                       DATE_MEDIAN = median_date
      )
  } else {
    result <- tibble(SITECODE = evl_site,
                     NAZEV = sites_habitats %>% dplyr::filter(KOD == evl_site) %>% pull(NAZEV),
                     HABITAT_CODE = hab_code,
                     ROZLOHA = target_area_ha,
                     REPRE = NA,
                     KVALITA = NA,
                     MINIMIAREAL = NA,
                     MINIMIAREAL_JADRA = NA,
                     MINIMIAREAL_HODNOTA = minimi_value,
                     MOZAIKA = NA,
                     RED_LIST = NA,
                     INVASIVE = NA,
                     EXPANSIVE = NA,
                     RED_LIST_SPECIES = NA,
                     INVASIVE_LIST = NA,
                     EXPANSIVE_LIST = NA,
                     GOOD_DOC_AREA_HA = NA,
                     W_AREA_HA = NA,
                     W_AREA_PERC = NA,
                     DEGRAD_AREA_HA = NA,
                     DEGRAD_AREA_PERC = NA,
                     VYPLNENOST_REPRE = NA,
                     VYPLNENOST_KVALITA = NA,
                     VYPLNENOST_MOZAIKA = NA,
                     PERC_SPAT = NA,
                     DATE_MIN = NA,
                     DATE_MAX = NA,
                     DATE_MEAN = NA,
                     DATE_MEDIAN = NA)
  }
  
  result
  
}

# HODNOCENI MAPA ----
# SEGMENTY S INFORMACÍ O ROZLOZE, KVALITĚ, TYPICKÝCH DRUZÍCH, MRTVÉ DŘEVO, 
# INVAZKY BIN, EXPANZKY BIN, INVAZKY LIST, EXPANZKY LIST, CELISTVOST BIN
hvezdice_eval_orig_geom <- function(hab_code, evl_site) {
  
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_orig %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
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
  area_paseky_ha <- find_habitat_PASEKY(evl_site, hab_code)
  
  SUM_PLO_BIO <- sum(vmb_target_sjtsk %>%
                       dplyr::pull(PLO_BIO_M2_EVL) %>%
                       sum(),
                     area_paseky_ha*10000,
                     na.rm = TRUE)
  
  
  # KVALITATIVNÍ PARAMETRY HODNOCENÍ
  vmb_eval <- vmb_target_sjtsk %>%
    dplyr::mutate(
      KVALITA_SEG = dplyr::case_when(REPRE == "W" ~ 4,
                                     is.na(KVALITA) == TRUE ~ 4,  
                                     TRUE ~ KVALITA),
      TYP_DRUHY_SEG = NA,
      MRTVE_DREVO_SEG = NA,
      KALAMITA_SEG = NA
    ) %>%
    dplyr::select(SEGMENT_ID,
                  HABITAT,
                  BIOTOP,
                  STEJ_PR,
                  PLO_BIO_M2_EVL,
                  KVALITA_SEG,
                  TYP_DRUHY_SEG,
                  MRTVE_DREVO_SEG,
                  KALAMITA_SEG)
  
  # MINIMIAREÁL
  if(nrow(vmb_target_sjtsk) > 0) {
    
    minimi_value <- vmb_target_sjtsk %>%
      dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M2.2" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M3" %in% unique(BIOTOP) ~ 2000/10000,
                                            "M2.1" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M2.3" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M4.2" %in% unique(BIOTOP) ~ 300/10000,
                                            "M4.3" %in% unique(BIOTOP) ~ 1000/10000,
                                            TRUE ~ minimisize %>% 
                                              dplyr::filter(HABITAT == hab_code) %>%
                                              dplyr::pull(MINIMISIZE)/10000)) %>%
      dplyr::pull(MINIMI_SIZE) %>%
      unique()
    
  } else {
    
    minimi_value <- (minimisize %>% 
                       dplyr::filter(HABITAT == hab_code) %>%
                       dplyr::pull(MINIMISIZE)/10000)
    
  }
  
  spat_celistvost <- vmb_target_sjtsk %>%
    dplyr::filter(REPRE != "W") %>%
    sf::st_buffer(., 50) %>%
    sf::st_union() %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_make_valid() %>%
    base::as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number()) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(REPRE != "W") %>%
    dplyr::group_by(ID_COMB) %>%
    dplyr::mutate(COMB_SIZE = sum(PLO_BIO_M2_EVL, na.rm = TRUE)) %>%
    dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE >= 2000 ~ 1,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE >= 300 ~ 1,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "V6" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE < 2000 ~ 0,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE < 300 ~ 0,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          COMB_SIZE >= minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) ~ 1,
                                          COMB_SIZE < minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) ~ 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SEGMENT_ID, MINIMI_SIZE) %>%
    dplyr::rename(MINIMIAREAL = MINIMI_SIZE)
  
  
  # VNITŘNÍ MOZAIKA
  mozaika_inner <- vmb_target_sjtsk %>%
    dplyr::mutate(VNITRNI_MOZAIKA = dplyr::case_when(grepl("X", BIOTOP_SEZ, ignore.case = TRUE) ~ "X",
                                                     TRUE ~ "nat")) %>%
    dplyr::select(SEGMENT_ID, VNITRNI_MOZAIKA) %>%
    sf::st_drop_geometry()
  
  
  
  # RED LIST SPECIES
  redlist <- red_list_species %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_RL = toString(unique(DRUH)),
                  DRUHY_RL_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_RL, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry()
  
  
  # INVASIVE SPECIES
  if(hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  } else {
    invaders_all <- invasive_species %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  }
  
  invaders <- invaders_all %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_INV = toString(unique(DRUH)),
                  DRUHY_INV_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_INV, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry() 
  
  # EXPANZNÍ DRUHY
  expanders_all <- expansive_species %>%
    dplyr::filter(POKRYVN %in% c("3", "4", "5")) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::filter(is.na(HABITAT) == FALSE) %>%
    dplyr::group_by(OBJECTID, DRUH) %>%
    dplyr::filter(DATUM_OD >= DATUM.x) %>%
    dplyr::slice(which.max(DATUM_OD)) %>%
    dplyr::filter(NEGATIVNI == 0) %>%
    dplyr::ungroup()
  
  expanders <- expanders_all %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_EXP = toString(unique(DRUH)),
                  DRUHY_EXP_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_EXP, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry() 
  
  # VÝSLEDKY
  
  result <- vmb_eval %>%
    dplyr::left_join(., spat_celistvost, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::left_join(., mozaika_inner, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::left_join(., invaders, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::left_join(., expanders, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::left_join(., redlist, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(label = paste0("SEGMENT_ID: ", as.character(SEGMENT_ID),"<br>",
                                "Habitat: ", as.character(HABITAT), "<br>",
                                "Biotop: ", as.character(BIOTOP),"<br>",
                                "zastoupení v mozaice: ", as.character(STEJ_PR),"<br>",
                                "Plocha stanoviště: ", as.character(round(PLO_BIO_M2_EVL, 2))," m2", "<br>",
                                "Kvalita: ", as.character(KVALITA_SEG),"<br>",
                                "Typické druhy: ", as.character(TYP_DRUHY_SEG),"<br>",
                                "Mrtvé dřevo: ", as.character(MRTVE_DREVO_SEG),"<br>",
                                "Kalamita: ", as.character(KALAMITA_SEG),"<br>",
                                "Minimiareál: ", as.character(MINIMIAREAL),"<br>",
                                "Invazní druhy: ", as.character(DRUHY_INV),"<br>",
                                "Ohrožené druhy: ", as.character(DRUHY_RL)))
  
  return(result)
  
}

hvezdice_eval_a1_geom <- function(hab_code, evl_site) {
  
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_a1 %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
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
  area_paseky_ha <- find_habitat_PASEKY(evl_site, hab_code)
  
  SUM_PLO_BIO <- sum(vmb_target_sjtsk %>%
                       dplyr::pull(PLO_BIO_M2_EVL) %>%
                       sum(),
                     area_paseky_ha*10000,
                     na.rm = TRUE)

  
  # KVALITATIVNÍ PARAMETRY HODNOCENÍ
  vmb_eval <- vmb_target_sjtsk %>%
    dplyr::mutate(
      SITECODE = evl_site,
      TYP_DRUHY_SEG = dplyr::case_when(DG == "W" ~ 0,
                                       RB == "W" ~ 0,
                                       is.na(TD) == TRUE ~ 0,
                                       TD == "N" ~ 0,
                                       TD == "MP" ~ 1,
                                       TD == "P" ~ 2),
      KVALITA_SEG = dplyr::case_when(DG == "W" ~ 4,
                                     RB == "W" ~ 4,
                                     is.na(KVALITA) == TRUE ~ 4,  
                                     TRUE ~ KVALITA),
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
                                      MD == 4 ~ 2)
    ) %>%
    dplyr::select(SEGMENT_ID,
                  HABITAT,
                  BIOTOP,
                  STEJ_PR,
                  PLO_BIO_M2_EVL,
                  KVALITA_SEG,
                  TYP_DRUHY_SEG,
                  MRTVE_DREVO_SEG,
                  KALAMITA_SEG)
  
  # MINIMIAREÁL
  if(nrow(vmb_target_sjtsk) > 0) {
    
    minimi_value <- vmb_target_sjtsk %>%
      dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M2.2" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M3" %in% unique(BIOTOP) ~ 2000/10000,
                                            "M2.1" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M2.3" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M4.2" %in% unique(BIOTOP) ~ 300/10000,
                                            "M4.3" %in% unique(BIOTOP) ~ 1000/10000,
                                            TRUE ~ minimisize %>% 
                                              dplyr::filter(HABITAT == hab_code) %>%
                                              dplyr::pull(MINIMISIZE)/10000)) %>%
      dplyr::pull(MINIMI_SIZE) %>%
      unique()
    
  } else {
    
    minimi_value <- (minimisize %>% 
                       dplyr::filter(HABITAT == hab_code) %>%
                       dplyr::pull(MINIMISIZE)/10000)
    
  }
  
  spat_celistvost <- vmb_target_sjtsk %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    sf::st_buffer(., 50) %>%
    sf::st_union() %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_make_valid() %>%
    base::as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number()) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    dplyr::group_by(ID_COMB) %>%
    dplyr::mutate(COMB_SIZE = sum(PLO_BIO_M2_EVL, na.rm = TRUE)) %>%
    dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE >= 2000 ~ 1,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE >= 300 ~ 1,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "V6" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE < 2000 ~ 0,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE < 300 ~ 0,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          COMB_SIZE >= minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) ~ 1,
                                          COMB_SIZE < minimisize %>% 
                                            dplyr::filter(HABITAT == hab_code) %>%
                                            dplyr::pull(MINIMISIZE) ~ 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SEGMENT_ID, MINIMI_SIZE) %>%
    dplyr::rename(MINIMIAREAL = MINIMI_SIZE)
  
  
   # VNITŘNÍ MOZAIKA
  mozaika_inner <- vmb_target_sjtsk %>%
    dplyr::mutate(VNITRNI_MOZAIKA = dplyr::case_when(grepl("X", BIOTOP_SEZ, ignore.case = TRUE) ~ "X",
                                                     TRUE ~ "nat")) %>%
    dplyr::select(SEGMENT_ID, VNITRNI_MOZAIKA) %>%
    sf::st_drop_geometry()
  


  # RED LIST SPECIES
  redlist <- red_list_species %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_RL = toString(unique(DRUH)),
                  DRUHY_RL_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_RL, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry()

  
  # INVASIVE SPECIES
  if(hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID_1.y, DRUH) %>%
      dplyr::filter(OBJECTID_1.y >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  } else {
    invaders_all <- invasive_species %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID_1.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM.x) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  }

  invaders <- invaders_all %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_INV = toString(unique(DRUH)),
                  DRUHY_INV_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_INV, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry() 

  # EXPANZNÍ DRUHY
  expanders_all <- expansive_species %>%
    dplyr::filter(POKRYVN %in% c("3", "4", "5")) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::filter(is.na(HABITAT) == FALSE) %>%
    dplyr::group_by(OBJECTID_1.y, DRUH) %>%
    dplyr::filter(DATUM_OD >= DATUM.x) %>%
    dplyr::slice(which.max(DATUM_OD)) %>%
    dplyr::filter(NEGATIVNI == 0) %>%
    dplyr::ungroup()
  
  expanders <- expanders_all %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_EXP = toString(unique(DRUH)),
                  DRUHY_EXP_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_EXP, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry() 
  
  # VÝSLEDKY
  
  result <- vmb_eval %>%
    dplyr::left_join(., spat_celistvost, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::left_join(., mozaika_inner, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::left_join(., invaders, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::left_join(., expanders, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::left_join(., redlist, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(label = paste0("SEGMENT_ID: ", as.character(SEGMENT_ID),"<br>",
                                "Habitat: ", as.character(HABITAT), "<br>",
                                "Biotop: ", as.character(BIOTOP),"<br>",
                                "zastoupení v mozaice: ", as.character(STEJ_PR),"<br>",
                                "Plocha stanoviště: ", as.character(round(PLO_BIO_M2_EVL, 2))," m2", "<br>",
                                "Kvalita: ", as.character(KVALITA_SEG),"<br>",
                                "Typické druhy: ", as.character(TYP_DRUHY_SEG),"<br>",
                                "Mrtvé dřevo: ", as.character(MRTVE_DREVO_SEG),"<br>",
                                "Kalamita: ", as.character(KALAMITA_SEG),"<br>",
                                "Minimiareál: ", as.character(MINIMIAREAL),"<br>",
                                "Invazní druhy: ", as.character(DRUHY_INV),"<br>",
                                "Ohrožené druhy: ", as.character(DRUHY_RL)))
  
  return(result)
  
}

hvezdice_eval_geom <- function(hab_code, evl_site) {
  
  # VÝBĚR KOMBINACE EVL A PŘEDMĚTU OCHRANY, PŘEPOČÍTÁNÍ PLOCHY BIOTOPU
  vmb_target_sjtsk <- vmb_shp_sjtsk_akt %>%
    sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == evl_site)) %>%
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
  area_paseky_ha <- find_habitat_PASEKY(evl_site, hab_code)
  
  SUM_PLO_BIO <- sum(vmb_target_sjtsk %>%
                       dplyr::pull(PLO_BIO_M2_EVL) %>%
                       sum(),
                     area_paseky_ha*10000,
                     na.rm = TRUE)
  
  
  # KVALITATIVNÍ PARAMETRY HODNOCENÍ
  vmb_eval <- vmb_target_sjtsk %>%
    rowwise() %>%
    dplyr::mutate(
      SITECODE = evl_site,
      NAZEV = find_evl_CODE_TO_NAME(evl_site),
      TYP_DRUHY_SEG = dplyr::case_when(DG == "W" ~ 0,
                                       RB == "W" ~ 0,
                                       is.na(TD) == TRUE ~ 0,
                                       TD == "N" ~ 0,
                                       TD == "MP" ~ 1,
                                       TD == "P" ~ 2),
      KVALITA_SEG = dplyr::case_when(DG == "W" ~ 4,
                                     RB == "W" ~ 4,
                                     is.na(KVALITA) == TRUE ~ 4,  
                                     TRUE ~ KVALITA),
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
      mg_stav = dplyr::case_when(grepl("0", MGN) ~ "žádný",
                                 grepl("V", MGN) ~ "vhodný",
                                 grepl("N", MGN) ~ "nevhodný"),
      mg_navrh = dplyr::case_when(grepl("Sn", MGN) ~ "biotop nebo jeho stav neumožňuje stanovit vhodný management ani výslovně a důvodně doporučit ponechání samovolnému vývoji",
                                  grepl("Sx", MGN) ~ "biotop je již v takovém stavu, že náprava by byla extrémně obtížná, zbytečná či nemožná; doporučuje se ponechat samovolnému vývoji",
                                  grepl("S", MGN) ~ "biotop nebo jeho stav nevyžaduje žádný management; doporučuje se ponechat samovolnému vývoji",
                                  grepl("Mm", MGN) ~ "provádět management urychleně a s vyšší naléhavostí",
                                  grepl("M", MGN) ~ "doporučuje se provádět vhodný management"),
      MANAGEMENT = dplyr::case_when(is.na(MGN) ~ NA_character_,
                                    is.na(mg_stav) == FALSE & is.na(mg_navrh) == FALSE ~ paste0(mg_stav, "; ", mg_navrh),
                                    is.na(mg_stav) == TRUE & is.na(mg_navrh) == FALSE ~ toString(mg_navrh),
                                    is.na(mg_stav) == FALSE & is.na(mg_navrh) == TRUE ~ toString(mg_stav)),
      DGP = toString(DGP)
      ) %>%
    dplyr::select(SEGMENT_ID,
                  REGION_ID,
                  SITECODE,
                  NAZEV,
                  HABITAT,
                  BIOTOP,
                  STEJ_PR,
                  FSB,
                  FSB_EVAL,
                  PLO_BIO_M2_EVL,
                  KVALITA_SEG,
                  DG,
                  DGP,
                  TYP_DRUHY_SEG,
                  SF,
                  SEC,
                  BM,
                  ZMENA,
                  RB,
                  RBB,
                  SD,
                  MRTVE_DREVO_SEG,
                  KALAMITA_SEG,
                  MANAGEMENT,
                  POZNAMKA,
                  MAPOVATEL,
                  DATUM)
  
  # MINIMIAREÁL
  if(nrow(vmb_target_sjtsk) > 0) {
    
    minimi_value <- vmb_target_sjtsk %>%
      dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M2.2" %in% unique(BIOTOP) ~ 1000/10000,
                                            "M3" %in% unique(BIOTOP) ~ 2000/10000,
                                            "M2.1" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M2.3" %in% unique(BIOTOP) ~ 7000/10000,
                                            "M4.2" %in% unique(BIOTOP) ~ 300/10000,
                                            "M4.3" %in% unique(BIOTOP) ~ 1000/10000,
                                            TRUE ~ minimisize %>% 
                                              dplyr::filter(HABITAT == hab_code) %>%
                                              dplyr::pull(MINIMISIZE)/10000)) %>%
      dplyr::pull(MINIMI_SIZE) %>%
      unique()
    
  } else {
    
    minimi_value <- (minimisize %>% 
      dplyr::filter(HABITAT == hab_code) %>%
      dplyr::pull(MINIMISIZE)/10000)
    
  }
  
  spat_celistvost <- vmb_target_sjtsk %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    sf::st_buffer(., 50) %>%
    sf::st_union() %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_make_valid() %>%
    base::as.data.frame() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(ID_COMB = row_number()) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(DG != "W" & RB != "W") %>%
    dplyr::group_by(ID_COMB) %>%
    dplyr::mutate(COMB_SIZE = sum(PLO_BIO_M2_EVL, na.rm = TRUE)) %>%
    dplyr::mutate(MINIMI_SIZE = case_when("V6" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE >= 2000 ~ 1,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE >= 7000 ~ 1,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE >= 300 ~ 1,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE >= 1000 ~ 1,
                                          "V6" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M2.2" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          "M3" %in% unique(BIOTOP) & COMB_SIZE < 2000 ~ 0,
                                          "M2.1" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M2.3" %in% unique(BIOTOP) & COMB_SIZE < 7000 ~ 0,
                                          "M4.2" %in% unique(BIOTOP) & COMB_SIZE < 300 ~ 0,
                                          "M4.3" %in% unique(BIOTOP) & COMB_SIZE < 1000 ~ 0,
                                          COMB_SIZE >= find_habitat_MINIMISIZE(hab_code)[1] ~ 1,
                                          COMB_SIZE < find_habitat_MINIMISIZE(hab_code)[1] ~ 0)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SEGMENT_ID, MINIMI_SIZE) %>%
    dplyr::rename(MINIMIAREAL = MINIMI_SIZE)
  
  
  # VNITŘNÍ MOZAIKA
  mozaika_inner <- vmb_target_sjtsk %>%
    dplyr::mutate(VNITRNI_MOZAIKA = dplyr::case_when(grepl("X", BIOTOP_SEZ, ignore.case = TRUE) ~ "X",
                                                     TRUE ~ "nat")) %>%
    dplyr::select(SEGMENT_ID, VNITRNI_MOZAIKA) %>%
    sf::st_drop_geometry()
  
  
  
  # RED LIST SPECIES
  redlist <- red_list_species %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_RL = toString(unique(DRUH)),
                  DRUHY_RL_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_RL, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry()
  
  
  # INVASIVE SPECIES
  if(hab_code == 6510) {
    invaders_all <- invasive_species %>%
      dplyr::filter(DRUH != "Arrhenatherum elatius") %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup()
  } else {
    invaders_all <- invasive_species %>%
      sf::st_intersection(., vmb_target_sjtsk) %>%
      dplyr::filter(is.na(HABITAT) == FALSE) %>%
      dplyr::group_by(OBJECTID.y, DRUH) %>%
      dplyr::filter(DATUM_OD >= DATUM) %>%
      dplyr::slice(which.max(DATUM_OD)) %>%
      dplyr::filter(NEGATIVNI == 0) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(DRUH_POKR = paste0(DRUH, " (", POKRYVN, ")"))
  }
  
  invaders <- invaders_all %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_INV = toString(unique(DRUH_POKR)),
                  DRUHY_INV_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_INV, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry() 
  
  # EXPANZIVNÍ DRUHY
  expanders_all <- expansive_species %>%
    dplyr::filter(POKRYVN %in% c("3", "3-4", "4", "4-5", "5")) %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::filter(is.na(HABITAT) == FALSE) %>%
    dplyr::group_by(OBJECTID.y, DRUH) %>%
    dplyr::filter(DATUM_OD >= DATUM) %>%
    dplyr::slice(which.max(DATUM_OD)) %>%
    dplyr::filter(NEGATIVNI == 0) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DRUH_POKR = paste0(DRUH, " (", POKRYVN, ")"))
  
  expanders <- expanders_all %>%
    sf::st_intersection(., vmb_target_sjtsk) %>%
    dplyr::group_by(SEGMENT_ID) %>%
    dplyr::mutate(DRUHY_EXP = toString(unique(DRUH_POKR)),
                  DRUHY_EXP_POCET = length(unique(DRUH))) %>%
    dplyr::select(DRUHY_EXP, SEGMENT_ID) %>%
    distinct() %>%
    sf::st_drop_geometry() 
  
  # VÝSLEDKY
  if(sum(vmb_target_sjtsk$PLO_BIO_M2_EVL) > 0 & 
     is.na(sum(vmb_target_sjtsk$PLO_BIO_M2_EVL)) == FALSE) {
    
    result <- vmb_eval %>%
      dplyr::left_join(., spat_celistvost, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
      dplyr::left_join(., mozaika_inner, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
      dplyr::left_join(., invaders, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
      dplyr::left_join(., expanders, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
      dplyr::left_join(., redlist, by = c("SEGMENT_ID" = "SEGMENT_ID")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(label = paste0("SEGMENT_ID: ", as.character(SEGMENT_ID),"<br>",
                                   "Habitat: ", as.character(HABITAT), "<br>",
                                   "Biotop: ", as.character(BIOTOP),"<br>",
                                   "Vyhraněnost: ", as.character(), "<br>",
                                   "Zastoupení v mozaice: ", as.character(STEJ_PR), " %","<br>",
                                   "Plocha stanoviště: ", as.character(round(PLO_BIO_M2_EVL, 2))," m2", "<br>",
                                   "Kvalita: ", as.character(KVALITA_SEG), "<br>",
                                   "Degradace: ", as.character(DG), "<br>",
                                   "Příčina degradace: ", as.character(DGP), "<br>",
                                   "Reprezentativnost: ", as.character(RB), "<br>",
                                   "Struktura a funkce: ", as.character(SF), "<br>",
                                   "Typické druhy: ", as.character(TYP_DRUHY_SEG), "<br>",
                                   "Mrtvé dřevo: ", as.character(MRTVE_DREVO_SEG), "<br>",
                                   "Kalamita: ", as.character(KALAMITA_SEG), "<br>",
                                   "Minimiareál: ", as.character(MINIMIAREAL), "<br>",
                                   "Invazní druhy: ", as.character(DRUHY_INV), "<br>",
                                   "Druhy Červeného seznamu: ", as.character(DRUHY_RL), "<br>",
                                   "Management: ", as.character(MANAGEMENT), "<br>",
                                   "Přechodový biotop: ", as.character(RBB), "<br>",
                                   "Mapovatel: ", as.character(MAPOVATEL), "<br>",
                                   "Datum: ", as.character(DATUM)))
    
  } else {
    
    result <- tibble(SITECODE = evl_site,
                     NAZEV = find_evl_CODE_TO_NAME(evl_site),
                     HABITAT = hab_code,
                     SEGMENT_ID = NA,
                     REGION_ID = NA,
                     BIOTOP = NA,
                     STEJ_PR = NA,
                     FSB = NA,
                     FSB_EVAL = NA,
                     PLO_BIO_M2_EVL = NA,
                     KVALITA_SEG = NA,
                     DG = NA,
                     DGP = NA,
                     TYP_DRUHY_SEG = NA,
                     SF = NA,
                     SEC = NA,
                     BM = NA,
                     ZMENA = NA,
                     RB = NA,
                     RBB = NA,
                     SD = NA,
                     MRTVE_DREVO_SEG = NA,
                     KALAMITA_SEG = NA,
                     MANAGEMENT = NA,
                     POZNAMKA = NA,
                     MAPOVATEL = NA,
                     DATUM = NA,
                     MINIMIAREAL = NA,
                     VNITRNI_MOZAIKA = NA,
                     DRUHY_INV = NA,
                     DRUHY_EXP = NA,
                     DRUHY_RL = NA,
                     geometry = NULL,
                     label = NA)
    
  }
  
  
  return(result)
  
}

vmb_shp_sjtsk_akt %>%
  filter(is.na(SEC) == FALSE) %>%
  pull(ROK_AKT.x) %>%
  unique

# 91E0
dopln_91 <- openxlsx::read.xlsx("91E0.xlsx")
hu <- hvezdice_eval("91E0", "CZ0214008")
habresults_100_110 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 1:nrow(dopln_91)) {
  habresults_100_110 <- dplyr::bind_rows(habresults_100_110, 
                                         as.data.frame(hvezdice_eval("91E0", dopln_91[i,1])))
}
write.csv2(habresults_100_110, "hodnoceni91E0.csv", fileEncoding = "Windows-1250", row.names = FALSE)

# RESULTS 2024 ----
n2k_hab_klic(3130, "CZ0210027")
hu <- n2k_hab_klic(sites_habitats[1753,5], sites_habitats[1753,1])
habresults_100_110 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 87:95) {
  habresults_100_110 <- dplyr::bind_rows(habresults_100_110, 
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
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults_x_1_500.csv", 
           row.names = FALSE)
for(i in 501:1000) {
  habresults_x_501_1000 <- dplyr::bind_rows(habresults_x_501_1000, 
                                          as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_501_1000, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults_x_501_1000.csv", 
           row.names = FALSE)
for(i in 1001:1500) {
  habresults_x_1001_1500 <- dplyr::bind_rows(habresults_x_1001_1500, 
                                           as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1001_1500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults_x_1001_1500.csv", 
           row.names = FALSE)
for(i in 1501:nrow(sites_habitats)) {
  habresults_x_1501_1893 <- dplyr::bind_rows(habresults_x_1501_1893, 
                                           as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1501_1893, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults_x_1501_1893.csv", 
           row.names = FALSE)

results_habitats_x <- bind_rows(habresults_x_1_500[c(2:nrow(habresults_x_1_500)),], 
                                habresults_x_501_1000[c(2:nrow(habresults_x_501_1000)),],
                                habresults_x_1001_1500[c(2:nrow(habresults_x_1001_1500)),],
                                habresults_x_1501_1893[c(2:nrow(habresults_x_1501_1893)),])

path <- paste0("S:/Složky uživatelů/Gaigr/stanoviste/VMB0/results_habitats_24_", 
               gsub('-','',Sys.Date()), 
               ".csv")
write.csv2(results_habitats_x, 
           path, 
           row.names = FALSE,
           fileEncoding = "Windows-1250")

results_habitats_l <- results_habitats  %>%
  dplyr::mutate(across(c(4:20, 25:41),
                       round, 3))%>%
  rowwise() %>%
  dplyr::mutate(NAZEV_HABITATU = find_habitat_NAME_CZ(HABITAT_CODE))

for(i in c(4:21, 25:41)) {
  results_habitats_l[,i] <- as.character(as.numeric(unlist(results_habitats_l[,i])))
}
results_habitats_long <- tidyr::pivot_longer(results_habitats_l,
                                             cols = c(4:41),
                                             names_to = "PAR_NAZEV",
                                             values_to = "PAR_HODNOTA") %>%
  dplyr::mutate(ROK_HODNOCENI = 2024) %>%
  dplyr::select(-c(DATE_MEAN, DATE_MEDIAN)) %>%
  dplyr::select(SITECODE,
                NAZEV,
                HABITAT_CODE,
                NAZEV_HABITATU,
                DATE_MIN, 
                DATE_MAX,
                PAR_NAZEV,
                PAR_HODNOTA,
                ROK_HODNOCENI) %>%
  dplyr::mutate(REVIZE = "",
                POZN_REV = "")
  
write.csv2(results_habitats_long, 
           paste0(
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_long_23_", 
           gsub('-','',Sys.Date()), 
           ".csv"),
           row.names = FALSE)
openxlsx::write.xlsx(results_habitats_long, 
                     paste0(
                       "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_long_22_", 
                       gsub('-','',Sys.Date()), 
                       ".xlsx"))

results_habitats_read <-read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_22_20230127.csv")
results_habitats_read[is.na(results_habitats_read)] <- 0

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

# PATCH A1 ----
novelizace <- openxlsx::read.xlsx("S:/Složky uživatelů/Gaigr/stanoviste/evl/novelizace.xlsx", sheet = 1)
sites_patch <- sites_habitats %>%
  dplyr::filter(site_code %in% novelizace$sitecode)
hu <- hvezdice_eval_a1(sites_patch[1,5], sites_patch[1,1])
habresults_patch <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_patch) <- colnames(hu)
for(i in 1:nrow(sites_patch)) {
  habresults_patch <- dplyr::bind_rows(habresults_patch, 
                                         as.data.frame(hvezdice_eval_a1(sites_patch[i,5], sites_patch[i,1])))
}
path <- paste0("S:/Složky uživatelů/Gaigr/stanoviste/VMB2/results_habitats_A1_patch_", 
               gsub('-','',Sys.Date()), 
               ".csv")
write.csv2(habresults_patch[c(2:nrow(habresults_patch)),], 
           path, 
           row.names = FALSE,
           fileEncoding = "Windows-1250")
# END PATCH ----

results_habitats_l <- results_habitats %>%
  dplyr::filter(NAZEV != "Třeboňsko – střed") %>%
  dplyr::mutate(across(c(4:20, 25:41),
                       round, 3))%>%
  rowwise() %>%
  dplyr::mutate(NAZEV_HABITATU = find_habitat_NAME_CZ(HABITAT_CODE))

for(i in c(4:21, 25:41)) {
  results_habitats_l[,i] <- as.character(as.numeric(unlist(results_habitats_l[,i])))
}
results_habitats_long <- tidyr::pivot_longer(results_habitats_l,
                                             cols = c(4:37),
                                             names_to = "PAR_NAZEV",
                                             values_to = "PAR_HODNOTA") %>%
  dplyr::mutate(ROK_HODNOCENI = 2024) %>%
  dplyr::select(-c(DATE_MEAN, DATE_MEDIAN)) %>%
  dplyr::select(SITECODE,
                NAZEV,
                HABITAT_CODE,
                NAZEV_HABITATU,
                DATE_MIN, 
                DATE_MAX,
                PAR_NAZEV,
                PAR_HODNOTA,
                ROK_HODNOCENI) %>%
  dplyr::mutate(REVIZE = "",
                POZN_REV = "")

write.csv2(results_habitats_long, 
           paste0(
             "S:/Složky uživatelů/Gaigr/stanoviste/results_habitats_long_A124_", 
             gsub('-','',Sys.Date()), 
             ".csv"),
           row.names = FALSE)
openxlsx::write.xlsx(results_habitats_long, 
                     paste0(
                       "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_long_22_", 
                       gsub('-','',Sys.Date()), 
                       ".xlsx"))

results_habitats_read <-read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_22_20230127.csv")
results_habitats_read[is.na(results_habitats_read)] <- 0

# SDF FINAL ----
sdf_read_old <- read.csv2("S:/Složky uživatelů/Gaigr/stanoviste/VMB2/results_habitats_A1_20240820.csv", 
                      fileEncoding = "Windows-1250") %>%
  dplyr::distinct() %>%
  dplyr::mutate(RELATIVE_AREA_PERC = RELATIVE_AREA_PERC*100000000,
                prior = 0)
sdf_patch_read <- read.csv2("S:/Složky uživatelů/Gaigr/stanoviste/VMB2/results_habitats_A1_patch_20240828.csv", 
                      fileEncoding = "Windows-1250") %>%
  dplyr::distinct() %>%
  dplyr::mutate(RELATIVE_AREA_PERC = RELATIVE_AREA_PERC*100000000,
                prior = 1)

sdf_read <- sdf_read_old %>%
  bind_rows(., sdf_patch_read) %>%
  group_by(SITECODE, HABITAT_CODE) %>%
  arrange(-prior) %>%
  slice(1) %>%
  ungroup()

cz_rel_area_median <- sdf_read %>%
  pull(RELATIVE_AREA_PERC) %>%
  na.omit() %>%
  median()
cz_rel_area_3q <- sdf_read %>%
  pull(RELATIVE_AREA_PERC) %>%
  na.omit() %>%
  quantile(., 0.75)

sdf_cz <- sdf_read %>%
  dplyr::mutate(cz_rel_area_median = cz_rel_area_median,
                cz_rel_area_3q = cz_rel_area_3q,
                REPRESENTATIVITA_SDF = dplyr::case_when(REPRE_SDF > 2.5 ~ "C"),
                GLOBAL_CZ = dplyr::case_when(RELATIVE_AREA_PERC < cz_rel_area_median ~ "C",
                                             RELATIVE_AREA_PERC >= cz_rel_area_median & RELATIVE_AREA_PERC <= cz_rel_area_3q ~ "B",
                                             RELATIVE_AREA_PERC > cz_rel_area_3q ~ "A")) %>%
  dplyr::left_join(., biogeo_evl, by = c("SITECODE" = "SITECODE")) %>%
  dplyr::left_join(., eu_ha_areas_calc, by = c("SITECODE" = "SITECODE", "HABITAT_CODE" = "HABITAT_CODE")) %>%
  dplyr::mutate(BG = dplyr::case_when(BG != BG_EVL ~ BG_EVL,
                                      TRUE ~ BG)) %>%
  dplyr::mutate(RELATIVE_EU_PERC = ROZLOHA/COV_BG_HA/100) %>%
  distinct()

eu_rel_area_median  <- sdf_cz %>%
  pull(RELATIVE_EU_PERC) %>%
  na.omit() %>%
  median()
eu_rel_area_3q <- sdf_cz %>%
  pull(RELATIVE_EU_PERC) %>%
  na.omit() %>%
  quantile(., 0.75)

sdf <- sdf_cz %>%
  dplyr::mutate(eu_rel_area_median = eu_rel_area_median,
                eu_rel_area_3q = eu_rel_area_3q) %>%
  dplyr::mutate(GLOBAL_EU = dplyr::case_when(RELATIVE_EU_PERC < eu_rel_area_median ~ "C",
                                             RELATIVE_EU_PERC >= eu_rel_area_median & RELATIVE_EU_PERC <= eu_rel_area_3q ~ "B",
                                             RELATIVE_EU_PERC > eu_rel_area_3q ~ "A")) %>%
  dplyr::distinct()

path_sdf <- paste0("S:/Složky uživatelů/Gaigr/stanoviste/VMB2/sdf_pracovni_", 
               gsub('-','',Sys.Date()), 
               ".csv")
write.csv2(sdf, 
           path_sdf, 
           row.names = FALSE,
           fileEncoding = "Windows-1250")


# RESULTS 2006 ----
hu <- hvezdice_eval_orig(sites_habitats[26,5], sites_habitats[26,1])
habresults_100_110 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 125:135) {
  habresults_100_110 <- dplyr::bind_rows(habresults_100_110, 
                                         as.data.frame(hvezdice_eval_orig(sites_habitats[i,5], sites_habitats[i,1])))
}

habresults_1_500 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1_500) <- colnames(hu)
habresults_501_1000 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_501_1000) <- colnames(hu)
habresults_1001_1500 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1001_1500) <- colnames(hu)
habresults_1501_1893 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_1501_1893) <- colnames(hu)
habresults_901_1893 <- matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_901_1893) <- colnames(hu)

rm(vmb_shp_sjtsk_orig_read, vmb_hab_dbf_orig, vmb_pb_dbf_orig, vmb_hab_pb_dbf_orig)

for(i in 1:500) {
  habresults_1_500 <- dplyr::bind_rows(habresults_1_500, 
                                       as.data.frame(hvezdice_eval_orig(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1_500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults06_1_500.csv", 
           row.names = FALSE)
for(i in 501:1000) {
  habresults_501_1000 <- dplyr::bind_rows(habresults_501_1000, 
                                          as.data.frame(hvezdice_eval_orig(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_501_1000, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults06_501_1000.csv", 
           row.names = FALSE)
for(i in 1001:1500) {
  habresults_1001_1500 <- dplyr::bind_rows(habresults_1001_1500, 
                                           as.data.frame(hvezdice_eval_orig(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1001_1500, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults06_1001_1500.csv", 
           row.names = FALSE)
for(i in 1501:nrow(sites_habitats)) {
  habresults_1501_1893 <- dplyr::bind_rows(habresults_1501_1893, 
                                           as.data.frame(hvezdice_eval_orig(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_1501_1893, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habresults06_1501_1893.csv", 
           row.names = FALSE)

#write.csv2(habresults_1_900, "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_1.csv", row.names = FALSE)
#write.csv2(habresults_901_1893, "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_2.csv", row.names = FALSE)
results_habitats <- bind_rows(habresults_1_500[c(2:nrow(habresults_1_500)),], 
                              habresults_501_1000[c(2:nrow(habresults_501_1000)),],
                              habresults_1001_1500[c(2:nrow(habresults_1001_1500)),],
                              habresults_1501_1893[c(2:nrow(habresults_1501_1893)),])
write.csv2(results_habitats, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_06_20230921.csv", 
           row.names = FALSE)

results_habitats_read <-  read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_A1_20230805.csv")
results_habitats_read[is.na(results_habitats_read)] <- 0

write.csv2(results_habitats_read, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_A1_20230811.csv", 
           row.names = FALSE,
           fileEncoding = "Windows-1250")

# SPAT RESULTS ----
hu <- hvezdice_eval_geom(sites_habitats[1,5], sites_habitats[1,1])
habresults_100_110 <- hu[1,] %>% dplyr::mutate(SITECODE = NA)
#habresults_100_110 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble() %>% st_as_sf()
#colnames(habresults_100_110) <- colnames(hu)
for(i in c(30, 1263:1267)) {
  habresults_100_110 <- dplyr::bind_rows(habresults_100_110, 
                                         hvezdice_eval_geom(sites_habitats[i,5], 
                                                                         sites_habitats[i,1]))
}

habresults_1_500 <- hu[1,] %>% dplyr::mutate(SITECODE = NA)
habresults_501_1000 <- hu[1,] %>% dplyr::mutate(SITECODE = NA)
habresults_1001_1500 <- hu[1,] %>% dplyr::mutate(SITECODE = NA)
habresults_1501_1893 <- hu[1,] %>% dplyr::mutate(SITECODE = NA)

rm(vmb_shp_sjtsk_akt_read, vmb_hab_dbf_23, vmb_pb_dbf_22, vmb_hab_pb_dbf_22)

for(i in 1:500) {
  habresults_1_500 <- dplyr::bind_rows(habresults_1_500, 
                                       hvezdice_eval_geom(sites_habitats[i,5], sites_habitats[i,1]))
}


for(i in 501:1000) {
  habresults_501_1000 <- dplyr::bind_rows(habresults_501_1000, 
                                          hvezdice_eval_geom(sites_habitats[i,5], sites_habitats[i,1]))
}

for(i in 1001:1500) {
  habresults_1001_1500 <- dplyr::bind_rows(habresults_1001_1500, 
                                           hvezdice_eval_geom(sites_habitats[i,5], sites_habitats[i,1]))
}

for(i in 1501:nrow(sites_habitats)) {
  habresults_1501_1893 <- dplyr::bind_rows(habresults_1501_1893, 
                                           hvezdice_eval_geom(sites_habitats[i,5], sites_habitats[i,1]))
}


results_habitats_spat <- bind_rows(habresults_1_500[c(2:nrow(habresults_1_500)),], 
                                   habresults_501_1000[c(2:nrow(habresults_501_1000)),],
                                   habresults_1001_1500[c(2:nrow(habresults_1001_1500)),],
                                   habresults_1501_1893[c(2:nrow(habresults_1501_1893)),])

path <- paste0("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_spat_", 
               gsub('-','',Sys.Date()), 
               ".gpkg")
sf::st_write(results_habitats_spat,
             path,
             append = FALSE, 
             layer_options = "ENCODING=WINDOWS-1250")

dplyr::filter(results_1vmb, results_1vmb$SITECODE %in% results_0vmb$SITECODE)

# POROVNÁNÍ ----
results_0vmb <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_06_20230210.csv",
                          fileEncoding = "Windows-1250") %>%
  dplyr::rename_with(~str_c(., "_00"), .cols = c(4:29)) %>%
  distinct()
results_1vmb <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_A1_20240510.csv",
                          fileEncoding = "Windows-1250") %>%
  dplyr::rename_with(~str_c(., "_A1"), .cols = c(4:45)) %>%
  dplyr::select(-NAZEV) %>% 
  distinct()
results_2022 <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/results_habitats_23_20231025.csv",
                          fileEncoding = "Windows-1250") %>%
  dplyr::rename_with(~str_c(., "_22"), .cols = c(4:45)) %>%
  dplyr::select(-NAZEV) %>% 
  distinct() %>%
  dplyr::mutate(INVASIVE_22 = 100+INVASIVE_22)
results_limits <- openxlsx::read.xlsx("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/SDF_cilove_stavy_20240425.xlsx") %>%
  dplyr::mutate(ROZLOHA_SDF = as.numeric(ROZLOHA_SDF))
SDO_sites <- read.csv2("SDO_II_predmetolokality.csv", fileEncoding = "Windows-1250")

results <- results_0vmb %>%
  dplyr::left_join(., results_1vmb, by = c("SITECODE", "HABITAT_CODE")) %>%
  dplyr::left_join(., results_2022, by = c("SITECODE", "HABITAT_CODE")) %>%
  dplyr::left_join(., results_limits, by = c("SITECODE", "HABITAT_CODE")) %>%
  dplyr::mutate(ROZLOHA_CIL = dplyr::case_when(ZDROJ_ROZLOHA == "AVMB" ~ plyr::round_any(ROZLOHA_A1, 0.01, f = floor),
                                               ZDROJ_ROZLOHA == "SDF 2019" ~ plyr::round_any(ROZLOHA_SDF, 0.01, f = floor),
                                               ZDROJ_ROZLOHA == "SDF 2024" ~ plyr::round_any(ROZLOHA_SDF, 0.01, f = floor),
                                               ZDROJ_ROZLOHA %in% c("MINIMIAREÁL", "MINIMIAREAL") ~ plyr::round_any(MINIMIAREAL_HODNOTA_A1, 0.01, f = floor)),
                KVALITA_CIL = dplyr::case_when(KVALITA_A1 > 2 ~ 2,
                                               is.na(KVALITA_A1) ~ 2,
                                               TRUE ~ plyr::round_any(KVALITA_A1, 0.1, f = ceiling)),
                ZDROJ_CS = dplyr::case_when(ROZLOHA_CIL < MINIMIAREAL_HODNOTA_A1 ~ "MINIMIAREAL",
                                            TRUE ~ ZDROJ_ROZLOHA)) %>%
  dplyr::mutate(ROZLOHA_CIL = dplyr::case_when(ZDROJ_ROZLOHA == "VMB2" ~ plyr::round_any(ROZLOHA_A1, 0.01, f = floor),
                                               ROZLOHA_CIL < MINIMIAREAL_HODNOTA_A1 ~ MINIMIAREAL_HODNOTA_A1,
                                               TRUE ~ ROZLOHA_CIL),
                ROZLOHA_STAV_00 = dplyr::case_when(is.na(ROZLOHA_00) ~ NA_character_,
                                                        is.na(ROZLOHA_CIL) ~ NA_character_,
                                                        ROZLOHA_00 < ROZLOHA_CIL ~ "nesplněno",
                                                        ROZLOHA_00 >= ROZLOHA_CIL ~ "splněno"),
                ROZLOHA_STAV_A1 = dplyr::case_when(is.na(ROZLOHA_A1) ~ NA_character_,
                                                        is.na(ROZLOHA_CIL) ~ NA_character_,
                                                        ROZLOHA_A1 < ROZLOHA_CIL ~ "nesplněno",
                                                        ROZLOHA_A1 >= ROZLOHA_CIL ~ "splněno"),
                ROZLOHA_STAV_22 = dplyr::case_when(is.na(ROZLOHA_22) ~ NA_character_,
                                                        is.na(ROZLOHA_CIL) ~ NA_character_,
                                                        ROZLOHA_22 < ROZLOHA_CIL ~ "nesplněno",
                                                        ROZLOHA_22 >= ROZLOHA_CIL ~ "splněno"),
                KVALITA_STAV_00 = dplyr::case_when(is.na(KVALITA_00) ~ NA_character_,
                                                        is.na(KVALITA_CIL) ~ NA_character_,
                                                        KVALITA_00 > KVALITA_CIL ~ "nesplněno",
                                                        KVALITA_00 <= KVALITA_CIL ~ "splněno"),
                KVALITA_STAV_A1 = dplyr::case_when(is.na(KVALITA_A1) ~ NA_character_,
                                                   is.na(KVALITA_CIL) ~ NA_character_,
                                                   round(as.numeric(KVALITA_22), 2) > KVALITA_CIL ~ "nesplněno",
                                                   round(as.numeric(KVALITA_22), 2) <= KVALITA_CIL ~ "splněno"),
                KVALITA_STAV_22 = dplyr::case_when(is.na(KVALITA_22) ~ NA_character_,
                                                   is.na(KVALITA_CIL) ~ NA_character_,
                                                   round(as.numeric(KVALITA_22), 2) > KVALITA_CIL ~ "nesplněno",
                                                  KVALITA_22 <= KVALITA_CIL ~ "splněno"),
                STAV_00 = dplyr::case_when(is.na(KVALITA_00) ~ NA_character_,
                                                is.na(ROZLOHA_00) ~ NA_character_,
                                                is.na(KVALITA_CIL) ~ NA_character_,
                                                is.na(ROZLOHA_CIL) ~ NA_character_,
                                                ROZLOHA_STAV_00 == "nesplněno" &
                                                  KVALITA_STAV_00 == "nesplněno" ~ "špatný",
                                                ROZLOHA_STAV_00 == "nesplněno" |
                                                  KVALITA_STAV_00 == "nesplněno" ~ "zhoršený",
                                                ROZLOHA_STAV_00 == "splněno" &
                                                  KVALITA_STAV_00 == "splněno" ~ "dobrý"),
                STAV_A1 = dplyr::case_when(is.na(KVALITA_A1) ~ NA_character_,
                                                is.na(ROZLOHA_A1) ~ NA_character_,
                                                is.na(KVALITA_CIL) ~ NA_character_,
                                                is.na(ROZLOHA_CIL) ~ NA_character_,
                                                ROZLOHA_STAV_A1 == "nesplněno" &
                                                  KVALITA_STAV_A1 == "nesplněno" ~ "špatný",
                                                ROZLOHA_STAV_A1 == "nesplněno" |
                                                  KVALITA_STAV_A1 == "nesplněno" ~ "zhoršený",
                                                ROZLOHA_STAV_A1 == "splněno" &
                                                  KVALITA_STAV_A1 == "splněno" ~ "dobrý"),
                STAV_22 = dplyr::case_when(is.na(KVALITA_22) ~ NA_character_,
                                                is.na(ROZLOHA_22) ~ NA_character_,
                                                is.na(KVALITA_CIL) ~ NA_character_,
                                                is.na(ROZLOHA_CIL) ~ NA_character_,
                                                ROZLOHA_STAV_22 == "nesplněno" &
                                                  KVALITA_STAV_22 == "nesplněno" ~ "špatný",
                                                ROZLOHA_STAV_22 == "nesplněno" |
                                                  KVALITA_STAV_22 == "nesplněno" ~ "zhoršený",
                                                ROZLOHA_STAV_22 == "splněno" &
                                                  KVALITA_STAV_22 == "splněno" ~ "dobrý"),
                TREND_ROZLOHA_00A1 = dplyr::case_when(is.na(ROZLOHA_00) ~ NA_character_,
                                                      is.na(ROZLOHA_A1) ~ NA_character_,
                                                      ROZLOHA_00*0.95 > ROZLOHA_A1 ~ "negativní",
                                                      ROZLOHA_00*0.95 <= ROZLOHA_A1 &
                                                        ROZLOHA_00*1.05 >= ROZLOHA_A1 ~ "stabilní",
                                                      ROZLOHA_00*1.05 < ROZLOHA_A1 ~ "pozitivní"),
                TREND_ROZLOHA_A122 = dplyr::case_when(is.na(ROZLOHA_A1) ~ NA_character_,
                                                      is.na(ROZLOHA_22) ~ NA_character_,
                                                      ROZLOHA_A1*0.95 > ROZLOHA_22 ~ "negativní",
                                                      ROZLOHA_A1*0.95 <= ROZLOHA_22 &
                                                        ROZLOHA_A1*1.05 >= ROZLOHA_22 ~ "stabilní",
                                                      ROZLOHA_A1*1.05 < ROZLOHA_22 ~ "pozitivní"),
                TREND_KVALITA_00A1 = dplyr::case_when(is.na(KVALITA_00) ~ NA_character_,
                                                      is.na(KVALITA_A1) ~ NA_character_,
                                                      KVALITA_00*1.05 < KVALITA_A1 ~ "negativní",
                                                      KVALITA_00*1.05 >= KVALITA_A1 &
                                                        KVALITA_00*0.95 <= KVALITA_A1 ~ "stabilní",
                                                      KVALITA_00*0.95 > KVALITA_A1 ~ "pozitivní"),
                TREND_KVALITA_A122 = dplyr::case_when(is.na(KVALITA_A1) ~ NA_character_,
                                                      is.na(KVALITA_22) ~ NA_character_,
                                                      KVALITA_A1*1.05 < KVALITA_22 ~ "negativní",
                                                      KVALITA_A1*1.05 >= KVALITA_22 &
                                                        KVALITA_A1*0.95 <= KVALITA_22 ~ "stabilní",
                                                      KVALITA_A1*0.95 > KVALITA_22 ~ "pozitivní"),
                TREND_DEGRAD_00A1 = dplyr::case_when(is.na(DEGRAD_AREA_PERC_00) ~ NA_character_,
                                                      is.na(DEGRAD_AREA_PERC_A1) ~ NA_character_,
                                                      DEGRAD_AREA_PERC_00*1.05 < DEGRAD_AREA_PERC_A1 ~ "negativní",
                                                      DEGRAD_AREA_PERC_00*1.05 >= DEGRAD_AREA_PERC_A1 &
                                                        DEGRAD_AREA_PERC_00*0.95 <= DEGRAD_AREA_PERC_A1 ~ "stabilní",
                                                      DEGRAD_AREA_PERC_00*0.95 > DEGRAD_AREA_PERC_A1 ~ "pozitivní"),
                TREND_DEGRAD_A122 = dplyr::case_when(is.na(DEGRAD_AREA_PERC_A1) ~ NA_character_,
                                                      is.na(DEGRAD_AREA_PERC_22) ~ NA_character_,
                                                      DEGRAD_AREA_PERC_A1*1.05 < DEGRAD_AREA_PERC_22 ~ "negativní",
                                                      DEGRAD_AREA_PERC_A1*1.05 >= DEGRAD_AREA_PERC_22 &
                                                        DEGRAD_AREA_PERC_A1*0.95 <= DEGRAD_AREA_PERC_22 ~ "stabilní",
                                                      DEGRAD_AREA_PERC_A1*0.95 > DEGRAD_AREA_PERC_22 ~ "pozitivní"),
                TREND_MINIMIAREAL_00A1 = dplyr::case_when(is.na(MINIMIAREAL_00) ~ NA_character_,
                                                          is.na(MINIMIAREAL_A1) ~ NA_character_,
                                                          MINIMIAREAL_00*0.95 > MINIMIAREAL_A1 ~ "negativní",
                                                      MINIMIAREAL_00*0.95 <= MINIMIAREAL_A1 &
                                                        MINIMIAREAL_00*1.05 >= MINIMIAREAL_A1 ~ "stabilní",
                                                      MINIMIAREAL_00*1.05 < MINIMIAREAL_A1 ~ "pozitivní"),
                TREND_MINIMIAREAL_A122 = dplyr::case_when(is.na(MINIMIAREAL_A1) ~ NA_character_,
                                                          is.na(MINIMIAREAL_22) ~ NA_character_,
                                                          MINIMIAREAL_A1*0.95 > MINIMIAREAL_22 ~ "negativní",
                                                      MINIMIAREAL_A1*0.95 <= MINIMIAREAL_22 &
                                                        MINIMIAREAL_A1*1.05 >= MINIMIAREAL_22 ~ "stabilní",
                                                      MINIMIAREAL_A1*1.05 < MINIMIAREAL_22 ~ "pozitivní"),
                TREND_MOZAIKA_00A1 = dplyr::case_when(MOZAIKA_00*0.95 > MOZAIKA_FIN_A1 ~ "negativní",
                                                          MOZAIKA_00*0.95 <= MOZAIKA_FIN_A1 &
                                                            MOZAIKA_00*1.05 >= MOZAIKA_FIN_A1 ~ "stabilní",
                                                          MOZAIKA_00*1.05 < MOZAIKA_FIN_A1 ~ "pozitivní"),
                TREND_MOZAIKA_A122 = dplyr::case_when(MOZAIKA_FIN_A1*0.95 > MOZAIKA_FIN_22 ~ "negativní",
                                                          MOZAIKA_FIN_A1*0.95 <= MOZAIKA_FIN_22 &
                                                            MOZAIKA_FIN_A1*1.05 >= MOZAIKA_FIN_22 ~ "stabilní",
                                                          MOZAIKA_FIN_A1*1.05 < MOZAIKA_FIN_22 ~ "pozitivní"),
                TREND_TYPICKE_DRUHY_A122 = dplyr::case_when(TYPICKE_DRUHY_A1*1.05 < TYPICKE_DRUHY_22 ~ "negativní",
                                                      TYPICKE_DRUHY_A1*1.05 >= TYPICKE_DRUHY_22 &
                                                        TYPICKE_DRUHY_A1*0.95 <= TYPICKE_DRUHY_22 ~ "stabilní",
                                                      TYPICKE_DRUHY_A1*0.95 > TYPICKE_DRUHY_22 ~ "pozitivní"),
                MRTVE_DREVO_A122 = dplyr::case_when(MRTVE_DREVO_A1*0.95 > MRTVE_DREVO_22 ~ "negativní",
                                                MRTVE_DREVO_A1*0.95 <= MRTVE_DREVO_22 &
                                                  MRTVE_DREVO_A1*1.05 >= MRTVE_DREVO_22 ~ "stabilní",
                                                MRTVE_DREVO_A1*1.05 < MRTVE_DREVO_22 ~ "pozitivní"),
                RED_LIST_A122 = dplyr::case_when(RED_LIST_A1*0.95 > RED_LIST_22 ~ "negativní",
                                                RED_LIST_A1*0.95 <= RED_LIST_22 &
                                                  RED_LIST_A1*1.05 >= RED_LIST_22 ~ "stabilní",
                                                RED_LIST_A1*1.05 < RED_LIST_22 ~ "pozitivní"),
                INVASIVE_A122 = dplyr::case_when(INVASIVE_A1*0.95 > INVASIVE_22 ~ "negativní",
                                                INVASIVE_A1*0.95 <= INVASIVE_22 &
                                                  INVASIVE_A1*1.05 >= INVASIVE_22 ~ "stabilní",
                                                INVASIVE_A1*1.05 < INVASIVE_22 ~ "pozitivní"),
                EXPANSIVE_A122 = dplyr::case_when(EXPANSIVE_A1*0.95 > EXPANSIVE_22 ~ "negativní",
                                                EXPANSIVE_A1*0.95 <= EXPANSIVE_22 &
                                                  EXPANSIVE_A1*1.05 >= EXPANSIVE_22 ~ "stabilní",
                                                EXPANSIVE_A1*1.05 < EXPANSIVE_22 ~ "pozitivní")) %>%
  dplyr::left_join(., rp_n2k, by = c("SITECODE" = "sitecode")) %>%
  dplyr::left_join(., n2k_oop, by = c("SITECODE" = "sitecode")) %>%
  dplyr::distinct()

write.csv2(results %>%
             #dplyr::rename(ROZLOHA_VYHL = ROZLOHA_SDF) %>%
             dplyr::mutate(SPOJENI = paste0(SITECODE, "_", HABITAT_CODE)) %>%
             dplyr::select(SPOJENI,
                           c(1:3), 
                           ROZLOHA_22,
                           ROZLOHA_A1, ROZLOHA_SDF, MINIMIAREAL_HODNOTA_A1, ZDROJ_CS, ROZLOHA_CIL, 
                           KVALITA_A1, KVALITA_CIL, 
                           ROZLOHA_STAV_A1, KVALITA_STAV_A1, STAV_A1,
                           oop, rp) %>%
             dplyr::mutate(ROZLOHA_CIL = round(as.numeric(ROZLOHA_CIL), 2),
                           ROZLOHA_A1 = round(as.numeric(ROZLOHA_A1), 4),
                           KVALITA_A1 = round(as.numeric(KVALITA_A1), 2),
                           KVALITA_CIL = round(as.numeric(KVALITA_CIL), 2),
                           HABITAT_CODE = as.character(HABITAT_CODE),
                           SDO_II = dplyr::case_when(SITECODE %in% SDO_sites$Kód.lokality ~ "ano",
                                                     TRUE ~ "ne"),
                           PARDUBICKY_KRAJ = dplyr::case_when(grepl("Krajský úřad Pardubického kraje", oop, ignore.case = FALSE) ~ "ano",
                                                             TRUE ~ "ne"),
                           KRALOVEHRADECKY = dplyr::case_when(grepl("Krajský úřad Královéhradeckého kraje", oop, ignore.case = FALSE) ~ "ano",
                                                              TRUE ~ "ne")),
           paste0("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habitaty_vyhodnoceni_sum_",
                  gsub("-", "", Sys.Date()),
                  ".csv"),
           row.names = FALSE,
           fileEncoding = "Windows-1250")
write.csv2(results %>%
             #dplyr::select( c(1:3), ROZLOHA_22, c(114:141)) %>%
             dplyr::mutate(ROZLOHA_CIL = round(as.numeric(ROZLOHA_CIL), 2),
                           KVALITA_CIL = round(as.numeric(KVALITA_CIL), 2)),
           paste0("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habitaty_vyhodnoceni_",
                  gsub("-", "", Sys.Date()),
                  ".csv"),
           row.names = FALSE,
           fileEncoding = "Windows-1250")
results <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habitaty_vyhodnoceni_20230228_vse.csv",
                     fileEncoding = "Windows-1250")
results_sum <- results %>%
tidyr::pivot_longer(cols = c(STAV_00, STAV_A1, STAV_22),
                    names_to = "ROK",
                    values_to = "HODNOCENI") %>% 
  group_by(ROK, HODNOCENI) %>%
  dplyr::summarise(POCET = n(),
                   PROCENTO = POCET/1893*100,
                   PROCENTO_BEZ_NA = POCET/(1893-558)*100,
                   ROK = dplyr::case_when(ROK == "STAV_00" ~ "0_VMB",
                                          ROK == "STAV_A1" ~ "1_AVMB",
                                          ROK == "STAV_22" ~ "2_AVMB2022")) %>%
  distinct()

results_cil_0 <- results %>%
  dplyr::select(SITECODE, HABITAT_CODE, ZDROJ_CS, ROZLOHA_CIL, KVALITA_CIL) %>%
  dplyr::rename(ROZLOHA = ROZLOHA_CIL, 
                KVALITA = KVALITA_CIL) 
for(i in c(4:5)) {
  results_cil_0[,i] <- as.character(unlist(results_cil_0[,i]))
}
results_cil <- results_cil_0 %>%
  tidyr::pivot_longer(cols = c(4:ncol(.)),
                      names_to = "parametr_nazev",
                      values_to = "parametr_limit") 

results_0 <- results %>%
  dplyr::select(c(1:3), 
                ROZLOHA_A1, 
                KVALITA_A1, 
                TYPICKE_DRUHY_A1,
                MINIMIAREAL_A1,
                MINIMIAREAL_JADRA_A1,
                MINIMIAREAL_HODNOTA_A1,
                MOZAIKA_VNEJSI_A1,
                MOZAIKA_VNITRNI_A1,
                MOZAIKA_FIN_A1,
                RED_LIST_A1,
                INVASIVE_A1,
                EXPANSIVE_A1,
                MRTVE_DREVO_A1,
                KALAMITA_POLOM_A1,
                RED_LIST_SPECIES_A1,
                INVASIVE_LIST_A1,
                EXPANSIVE_LIST_A1) %>%
  mutate(CELKOVE_HODNOCENI = NA)

new_column_names <- sub("_A1$", "", colnames(results_0))
colnames(results_0) <- new_column_names

for(i in c(4:21)) {
  results_0[,i] <- as.character(unlist(results_0[,i]))
}

cis_habitat <- read.csv2("v_cis_habitat.csv", fileEncoding = "Windows-1250") %>%
  dplyr::select(KOD_HABITAT, NAZEV_HABITAT)

results_long <- results_0 %>%
  tidyr::pivot_longer(cols = c(4:ncol(.)),
                      names_to = "parametr_nazev",
                      values_to = "parametr_hodnota") %>%
  dplyr::left_join(., cis_habitat, by = c("HABITAT_CODE" = "KOD_HABITAT")) %>%
  dplyr::full_join(., results_cil, by = c("SITECODE" = "SITECODE", "HABITAT_CODE" = "HABITAT_CODE", "parametr_nazev" = "parametr_nazev" )) %>%
  dplyr::left_join(., results_1vmb %>% dplyr::select(HABITAT_CODE, SITECODE, DATE_MIN_A1, DATE_MAX_A1)) %>%
  dplyr::left_join(., rp_n2k, by = c("SITECODE" = "sitecode")) %>%
  dplyr::left_join(., n2k_oop, by = c("SITECODE" = "sitecode")) %>%
  rowwise() %>%
  dplyr::mutate(stav = dplyr::case_when(parametr_nazev == "ROZLOHA" & parametr_hodnota < parametr_limit ~ "špatný",
                                        parametr_nazev == "ROZLOHA" & parametr_hodnota >= parametr_limit ~ "dobrý",
                                        parametr_nazev == "KVALITA" & parametr_hodnota > parametr_limit ~ "špatný",
                                        parametr_nazev == "KVALITA" & parametr_hodnota <= parametr_limit ~ "dobrý"),
                parametr_jednotka = dplyr::case_when(parametr_nazev == "ROZLOHA" ~ "hektary", 
                                                     parametr_nazev == "KVALITA" ~ "kvalita", 
                                                     parametr_nazev == "TYPICKE_DRUHY" ~ "typické druhy",
                                                     parametr_nazev == "MINIMIAREAL" ~ "% rozlohy v celistvém uspořádání splňující hodnotu minimiareálu",
                                                     parametr_nazev == "MINIMIAREAL_JADRA" ~ "počet lokalit splňujících hodnotu minimiareálu",
                                                     parametr_nazev == "MINIMIAREAL_HODNOTA" ~ "hodnota minimiareálu",
                                                     parametr_nazev == "MOZAIKA_VNEJSI" ~ "% hranice s nepřírodními biotopy",
                                                     parametr_nazev == "MOZAIKA_VNITRNI" ~ "zastoupení nepřírodních biotopů v segmentech stanoviště",
                                                     parametr_nazev == "MOZAIKA_FIN" ~ "mozaikovitost",
                                                     parametr_nazev == "RED_LIST" ~ "druhy červeného seznamu",
                                                     parametr_nazev == "INVASIVE" ~ "% rozlohy zasažené invazními druhy",
                                                     parametr_nazev == "EXPANSIVE" ~ "% rozlohy zasažené expanzivními druhy",
                                                     parametr_nazev == "MRTVE_DREVO" ~ "mrtvé dřevo",
                                                     parametr_nazev == "KALAMITA_POLOM" ~ "kalamita/polom",
                                                     parametr_nazev == "RED_LIST_SPECIES" ~ "seznam druhů červeného seznamu",
                                                     parametr_nazev == "INVASIVE_LIST" ~ "seznam invazních druhů",
                                                     parametr_nazev == "EXPANSIVE_LIST" ~ "seznam expanzivních druhů")) %>%
  dplyr::group_by(SITECODE, HABITAT_CODE) %>%
  dplyr::mutate(stav_count = dplyr::case_when(stav == "dobrý" ~ 1),
                glob_stav = case_when(sum(stav_count, na.rm = TRUE) == 0 ~ "špatný",
                                      sum(stav_count, na.rm = TRUE) == 1 ~ "zhoršený",
                                      sum(stav_count, na.rm = TRUE) == 2 ~ "dobrý"),
                stav = dplyr::case_when(parametr_nazev == "CELKOVE_HODNOCENI" ~ unique(glob_stav),
                                        TRUE ~ stav)) %>%
  dplyr::ungroup() %>% 
  dplyr::select(SITECODE, NAZEV, HABITAT_CODE, NAZEV_HABITAT, DATE_MIN_A1, DATE_MAX_A1, 
                parametr_nazev, parametr_hodnota, parametr_limit, parametr_jednotka, ZDROJ_CS, stav, oop, rp)
  str_repl
write.csv2(results_long %>%
             mutate(parametr_hodnota = as.numeric(parametr_hodnota),
                    parametr_limit = as.numeric(parametr_limit)),
           paste0("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/stanoviste_vyhodnoceni_",
                  gsub("-", "", Sys.Date()),
                  ".csv"),
           row.names = FALSE,
           fileEncoding = "Windows-1250")

kuk <- results_long %>%
  mutate(kuk = parametr_nazev == "ROZLOHA" & parametr_hodnota < parametr_limit)
  

ggplot(data = results_sum %>%
         dplyr::filter(is.na(HODNOCENI) == FALSE), 
       aes(x = ROK, y = POCET, fill = HODNOCENI)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("seagreen3", "red", "orange", "grey")) +
  scale_y_continuous(expand = expand_scale(c(0, 0.1))) +
  xlab("\ndatová sada") +
  ylab("počet předmětolokalit\n") +
  theme_bw()

write.csv2(results_sum,
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habitaty_shrnuti.csv",
           row.names = FALSE)

# KRAJE ----
results_kraje <- results %>%
  dplyr::mutate(oop = stringr::str_split(as.character(oop), ", ")) %>%
  tidyr::unnest() %>%
  dplyr::filter(grepl("kraje", oop, ignore.case = TRUE) | grepl("magis", oop, ignore.case = TRUE))

results_kraje_sum <- results_kraje %>%
  group_by(oop, STAV_A1) %>%
  dplyr::summarise(POCET = n(),
                   NAZEV = toString(unique(NAZEV)),
                   TYP_STAN = toString(unique(HABITAT_CODE))) %>%
  dplyr::arrange(-POCET)
  
ggplot(data = results_kraje_sum, aes(x = oop, y = POCET, fill = STAV_A1)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("green", "red", "orange", "grey"),
                    name = "STAV") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("\nkrajský úřad") +
  ylab("počet EVL\n") +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90))


# NASTAVENÍ LIMITNÍCH HODNOT ----
limits <- matrix(as.integer(NA), nrow(sites_habitats), ncol(hablimits)+2) %>% 
  dplyr::as_tibble() 
colnames(limits) <- c("SITECODE", "NAZEV", colnames(hablimits))
limits <- limits %>%
  mutate(SITECODE = as.character(SITECODE),
         NAZEV = as.character(NAZEV),
         HABITAT_CODE = as.character(HABITAT_CODE),
         TD_LIM = as.integer(TD_LIM),
         QUAL_LIM = as.integer(QUAL_LIM),
         MINIMIAREAL_LIM = as.integer(MINIMIAREAL_LIM),
         MOZAIKA_LIM = as.integer(MOZAIKA_LIM),
         CELISTVOST_LIM = as.integer(CELISTVOST_LIM),
         KONEKTIVITA_LIM = as.integer(KONEKTIVITA_LIM),
         REDLIST_LIM = as.logical(REDLIST_LIM),
         INVASIVE_LIM = as.integer(INVASIVE_LIM),
         EXPANSIVE_LIM = as.integer(INVASIVE_LIM),
         CONFLICT = as.integer(CONFLICT),
         REG = as.character(REG))

for(i in 1:nrow(sites_habitats)) {
  limits[i,1] <- sites_habitats[i,1]
  limits[i,2] <- sites_habitats[i,2]
  limits[i,3:14] <- (find_habitat_LIMIT(sites_habitats[i,5]))
}

# HODNOTY PARAMETRŮ VZTAŽENÉ K LIMITNÍM HODNOTÁM ----
results_habitats_limits <- left_join(results_habitats_read, limits)
results_habitats_values <- results_habitats_limits %>%
  mutate(TD_DIF = TYPICKE_DRUHY - TD_LIM,
         QUAL_DIF_ORIG = KVALITA_ORIG - QUAL_LIM,
         QUAL_DIF = KVALITA - QUAL_LIM,
         MINIMIAREAL_DIF = MINIMIAREAL - MINIMIAREAL_LIM,
         MOZAIKA_DIF = MOZAIKA - MOZAIKA_LIM,
         CELISTVOST_DIF = CELISTVOST - CELISTVOST_LIM,
         KONEKTIVITA_DIF = KONEKTIVITA - KONEKTIVITA_LIM,
         INVASIVE_DIF = INVASIVE - INVASIVE_LIM,
         EXPANSIVE_DIF = EXPANSIVE - INVASIVE_LIM) %>%
  dplyr::mutate(TD_ABSL = case_when(TD_DIF < 0 ~ 0,
                                    is.na(TD_DIF) ~ 0,
                                    TD_DIF >= 0 ~ 1),
                QUAL_ABSL = case_when(QUAL_DIF < 0 ~ 0,
                                      is.na(QUAL_DIF) ~ 0,
                                      QUAL_DIF >= 0 ~ 1),
                QUAL_ABSL_ORIG = case_when(QUAL_DIF_ORIG < 0 ~ 0,
                                           is.na(QUAL_DIF_ORIG) ~ 0,
                                           QUAL_DIF_ORIG >= 0 ~ 1),
                MINIMIAREAL_ABSL = case_when(MINIMIAREAL_DIF < 0 ~ 0,
                                             is.na(MINIMIAREAL_DIF) ~ 0,
                                             MINIMIAREAL_DIF >= 0 ~ 1),
                MOZAIKA_ABSL = case_when(MOZAIKA_DIF < 0 ~ 0,
                                         is.na(MOZAIKA_DIF) ~ 0,
                                         MOZAIKA_DIF >= 0 ~ 1),
                CELISTVOST_ABSL = case_when(CELISTVOST_DIF < 0 ~ 0,
                                            is.na(CELISTVOST_DIF) ~ 0,
                                            CELISTVOST_DIF >= 0 ~ 1),
                KONEKTIVITA_ABSL = case_when(KONEKTIVITA_DIF < 0 ~ 0,
                                             is.na(KONEKTIVITA_DIF) ~ 0,
                                             KONEKTIVITA_DIF >= 0 ~ 1),
                INVASIVE_ABSL = case_when(INVASIVE_DIF < 0 ~ 0,
                                          is.na(INVASIVE_DIF) ~ 0,
                                          INVASIVE_DIF >= 0 ~ 1),
                EXPANSIVE_ABSL = case_when(EXPANSIVE_DIF < 0 ~ 0,
                                           is.na(EXPANSIVE_DIF) ~ 0,
                                           EXPANSIVE_DIF >= 0 ~ 1)) %>%
  dplyr::group_by(NAZEV, HABITAT_CODE) %>%
  dplyr::mutate(SITECODE = as.character(NAZEV),
                OVERALL_SUM = sum(TD_DIF, QUAL_DIF, MINIMIAREAL_DIF, 
                                  MOZAIKA_DIF, CELISTVOST_DIF, KONEKTIVITA_DIF, 
                                  INVASIVE_DIF, EXPANSIVE_DIF),
                PAR_KLIC = sum(QUAL_ABSL, MINIMIAREAL_ABSL),
                PAR_ALL = case_when(sum(TD_ABSL, QUAL_ABSL, MINIMIAREAL_ABSL, MOZAIKA_ABSL, 
                                        CELISTVOST_ABSL, KONEKTIVITA_ABSL, 
                                        INVASIVE_ABSL, EXPANSIVE_ABSL) >= 5 &
                                      sum(QUAL_ABSL, MINIMIAREAL_ABSL) == 2 ~ 1,
                                    sum(TD_ABSL, QUAL_ABSL, MINIMIAREAL_ABSL, MOZAIKA_ABSL, 
                                        CELISTVOST_ABSL, KONEKTIVITA_ABSL, 
                                        INVASIVE_ABSL, EXPANSIVE_ABSL) >= 4 &
                                      sum(QUAL_ABSL, MINIMIAREAL_ABSL) >= 1 ~ 0.5,
                                    sum(TD_ABSL, QUAL_ABSL, MINIMIAREAL_ABSL, MOZAIKA_ABSL, 
                                        CELISTVOST_ABSL, KONEKTIVITA_ABSL, 
                                        INVASIVE_ABSL, EXPANSIVE_ABSL) <= 3 |
                                      sum(QUAL_ABSL, MINIMIAREAL_ABSL) == 0 ~ 0),
                OBJ_AREA = dplyr::case_when(MINIMIAREAL_DIF < 0 ~ "Enlarge the area",
                                            TRUE ~ NA_character_),
                OBJ_IMPROVE = dplyr::case_when(TD_DIF < 0 |
                                                 QUAL_DIF < 0 ~ "Improve the habitats condition",
                                               TRUE ~ NA_character_),
                OBJ_PRESENCE = dplyr::case_when(ROZLOHA == 0 & 
                                                  HABITAT_CODE != 8310 ~ "Re-establish the habitat",
                                                TRUE ~ NA_character_),
                OBJ_OTHER = dplyr::case_when(MOZAIKA_DIF < 0 | 
                                               CELISTVOST_DIF < 0 | 
                                               KONEKTIVITA_DIF < 0 |
                                               INVASIVE_DIF < 0 | 
                                               EXPANSIVE_DIF < 0 ~ "Other",
                                             TRUE ~ NA_character_),
                OBJ_LIST = dplyr::case_when(is.na(OBJ_AREA) == TRUE ~ toString(na.omit(c(OBJ_AREA,
                                                                                         OBJ_IMPROVE,
                                                                                         OBJ_PRESENCE,
                                                                                         OBJ_OTHER))))) %>%
  ungroup()

write.csv2(results_habitats_values, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habitaty_vyhodnoceni_20220902.csv", 
           row.names = FALSE)

# DATATABLE ----
# VÝSLEDKY HODNOCENÍ S BAREVNĚ VYZNAČENÝMI HODNOTAMI PRARAMETRŮ VE VZTAHU K LIMITNÍM HODNOTÁM
library(DT)
# KRÁTKÁ TABULKA PRO JEDNODUCHOU PREZENTACI
habitats_datatable_sum <- DT::datatable(results_habitats_values %>%
                                          dplyr::select(-c(TD_LIM,
                                                           QUAL_LIM,
                                                           MINIMIAREAL_LIM,
                                                           MOZAIKA_LIM,
                                                           CELISTVOST_LIM,
                                                           KONEKTIVITA_LIM,
                                                           INVASIVE_LIM,
                                                           EXPANSIVE_LIM,
                                                           TD_ABSL,
                                                           QUAL_ABSL,
                                                           MINIMIAREAL_ABSL,
                                                           MOZAIKA_ABSL,
                                                           CELISTVOST_ABSL,
                                                           KONEKTIVITA_ABSL,
                                                           INVASIVE_ABSL,  
                                                           EXPANSIVE_ABSL)) %>%
                                          dplyr::mutate(across(c(ROZLOHA,
                                                                 TYPICKE_DRUHY,
                                                                 KVALITA,
                                                                 MINIMIAREAL,
                                                                 MOZAIKA,
                                                                 CELISTVOST,
                                                                 KONEKTIVITA,
                                                                 RED_LIST,
                                                                 INVASIVE,
                                                                 EXPANSIVE,
                                                                 VYPLNENOST_TD,
                                                                 VYPLNENOST_KVALITA,
                                                                 OVERALL_SUM),
                                                               round, 3)) %>%
                                          as.data.frame(),
                                        extensions = 'Buttons',
                                        options = list(
                                          autowidth = TRUE,
                                          dom = 'Bfrtip',
                                          columnDefs = list(list(targets = 18:28, visible = FALSE)),
                                          buttons = c('csv', 'excel'),
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$('body').css({'font-family': 'Calibri'});",
                                            "}"
                                          )),
                                        rownames = FALSE,
                                        filter = "top") %>%
  DT::formatStyle('SITECODE', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('HABITAT_CODE', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('TYPICKE_DRUHY', 'TD_DIF', 
                  backgroundColor = styleInterval(0, 
                                                  c('red', 'green'))) %>%
  DT::formatStyle('KVALITA', 'QUAL_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('MINIMIAREAL', 'MINIMIAREAL_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('MOZAIKA', 'MOZAIKA_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('CELISTVOST', 'CELISTVOST_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('KONEKTIVITA', 'KONEKTIVITA_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('INVASIVE', 'INVASIVE_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('EXPANSIVE', 'EXPANSIVE_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('PAR_ALL',
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) 
habitats_datatable_sum

# KOMPLETNÍ VÝSLEDKY VČETNĚ LIMITNÍCH HODNOT
habitats_datatable_all <- DT::datatable(results_habitats_values %>%
                                          dplyr::select(-c(TD_ABSL,
                                                           QUAL_ABSL,
                                                           MINIMIAREAL_ABSL,
                                                           MOZAIKA_ABSL,
                                                           CELISTVOST_ABSL,
                                                           KONEKTIVITA_ABSL,
                                                           INVASIVE_ABSL,  
                                                           EXPANSIVE_ABSL)) %>%
                                          mutate(across(c(ROZLOHA,
                                                          TYPICKE_DRUHY,
                                                          KVALITA,
                                                          MINIMIAREAL,
                                                          MOZAIKA,
                                                          CELISTVOST,
                                                          KONEKTIVITA,
                                                          RED_LIST,
                                                          INVASIVE,
                                                          EXPANSIVE,
                                                          TD_LIM,
                                                          QUAL_LIM,
                                                          MINIMIAREAL_LIM,
                                                          MOZAIKA_LIM,
                                                          CELISTVOST_LIM,
                                                          KONEKTIVITA_LIM,
                                                          INVASIVE_LIM,
                                                          EXPANSIVE_LIM,
                                                          VYPLNENOST_TD,
                                                          VYPLNENOST_KVALITA,
                                                          OVERALL_SUM), round, 3)) %>%
                                          as.data.frame(),
                                        extensions = 'Buttons',
                                        options = list(
                                          dom = 'Bfrtip',
                                          buttons = c('csv', 'excel'),
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$('body').css({'font-family': 'Calibri'});",
                                            "}"
                                          )),
                                        rownames = FALSE,
                                        filter = "top") %>%
  DT::formatStyle('SITECODE', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('NAZEV', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('HABITAT_CODE', 'PAR_ALL', 
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green"))) %>%
  DT::formatStyle('TYPICKE_DRUHY', 'TD_DIF', 
                  backgroundColor = styleInterval(0, 
                                                  c('red', 'green'))) %>%
  DT::formatStyle('KVALITA', 'QUAL_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('MINIMIAREAL', 'MINIMIAREAL_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('MOZAIKA', 'MOZAIKA_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('CELISTVOST', 'CELISTVOST_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('KONEKTIVITA', 'KONEKTIVITA_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('INVASIVE', 'INVASIVE_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('EXPANSIVE', 'EXPANSIVE_DIF',
                  backgroundColor = styleInterval(0, 
                                                  c("red", "green"))) %>%
  DT::formatStyle('PAR_ALL',
                  backgroundColor = styleEqual(c(0, 0.5, 1), 
                                               c("red", "orange", "green")))
habitats_datatable_all

# EXPORT GRAFŮ ----
results_habitats_values_plot <- as.data.frame(results_habitats_values)
results_habitats_values_plot[is.na(results_habitats_values_plot)] <- 0

for(i in 1:nrow(sites_habitats)) {
  file_name_prep <- paste(results_habitats_read[i,2], results_habitats_read[i,3], sep = "_")
  file_name <- paste("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/", "hodnoceni_", file_name_prep, ".png", sep = "")
  result <- hvezdice_plot(as.data.frame(results_habitats_read[i,]))
  ggplot2::ggsave(result, filename = file_name, height = 8, width = 12, units = "in")
}

results_habitats_values_plot %>% 
  dplyr::select(TD_LIM,
                QUAL_LIM,
                MINIMIAREAL_LIM,
                MOZAIKA_LIM,
                CELISTVOST_LIM,
                KONEKTIVITA_LIM,
                INVASIVE_LIM,
                EXPANSIVE_LIM,
                CONFLICT)
results_habitats$SITECODE %in% SDO_sites$site_code 

# SDO II LINK ----
SDO_sites <- read.csv2("SDO_II_predmetolokality.csv", fileEncoding = "Windows-1250")
results_habitats_values_SDO <- results_habitats_values %>%
  mutate(SDO_II = case_when(SITECODE %in% SDO_sites$site_code &
                              HABITAT_CODE %in% SDO_sites$Předmět.ochrany...kód ~ 1,
                            TRUE ~ 0))
results_habitats_values_SDO <- results_habitats_read %>%
  filter(SITECODE %in% SDO_sites$site_code) %>%
  filter(HABITAT_CODE %in% SDO_sites$Předmět.ochrany...kód)
write.csv2(results_habitats_values_SDO, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habitaty_vyhodnoceni_20220902_SDOII.csv", 
           row.names = FALSE)

# MISC ----
results_habitats_values <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habitaty_vyhodnoceni_20220902.csv")

results_habitats_values %>%
  filter(HABITAT_CODE == 9410) %>%
  dplyr::select(MRTVE_DREVO, KALAMITA_POLOM) %>%
  plot()

results_habitats_values %>%
  filter(HABITAT_CODE == 9410) %>%
  pull(MRTVE_DREVO) %>%
  hist()

results_habitats_values %>%
  filter(HABITAT_CODE == 6510) %>%
  pull(QUAL_DIF_ORIG) %>%
  hist(., breaks = 20)

results_habitats_values %>%
  filter(HABITAT_CODE == 6510) %>%
  pull(QUAL_DIF) %>%
  hist(., breaks = 20)

ggplot(data = results_habitats_read, aes(x = KVALITA_ORIG, y = KVALITA)) +
  geom_point() +
  theme_bw()

ggplot(data = results_habitats_read, aes(x = log(ROZLOHA), y = CELISTVOST)) +
  geom_point() +
  theme_bw()

ggplot(data = results_habitats_read, aes(x = REPRE_SDF)) +
  geom_histogram(bins = 25, fill = "#8CC88C", color = "#006B4D", alpha = 0.9) +
  geom_vline(xintercept = mean(na.omit(results_habitats_read$REPRE_SDF)), linetype = "dotted",
             colour = "palegreen4", size = 1) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  ylab("počet předmětolokalit\n") +
  theme_bw()
ggplot(data = results_habitats_read, aes(x = REPRE)) +
  geom_histogram(bins = 25, fill = "#8CC88C", color = "#006B4D", alpha = 0.9) +
  geom_vline(xintercept = mean(na.omit(results_habitats_read$REPRE)), linetype = "dotted",
             colour = "palegreen4", size = 1) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  ylab("počet předmětolokalit\n") +
  theme_bw()
ggplot(data = results_habitats_read, aes(x = TYPICKE_DRUHY)) +
  geom_histogram(bins = 25, fill = "#8CC88C", color = "#006B4D", alpha = 0.9) +
  geom_vline(xintercept = mean(na.omit(results_habitats_read$TYPICKE_DRUHY)), linetype = "dotted",
             colour = "palegreen4", size = 1) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  ylab("počet předmětolokalit\n") +
  theme_bw()
ggplot(data = results_habitats_read, aes(x = KVALITA)) +
  geom_histogram(bins = 25, fill = "#8CC88C", color = "#006B4D", alpha = 0.9) +
  geom_vline(xintercept = mean(na.omit(results_habitats_read$KVALITA)), linetype = "dotted",
             colour = "palegreen4", size = 1) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  ylab("počet předmětolokalit\n") +
  theme_bw()
ggplot(data = results_habitats_read, aes(x = KVALITA_ORIG)) +
  geom_histogram(bins = 25, fill = "#8CC88C", color = "#006B4D", alpha = 0.9) +
  geom_vline(xintercept = mean(na.omit(results_habitats_read$KVALITA_ORIG)), linetype = "dotted",
             colour = "palegreen4", size = 1) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  ylab("počet předmětolokalit\n") +
  theme_bw()
ggplot(data = results_habitats_read, aes(x = CELISTVOST)) +
  geom_histogram(bins = 25, fill = "#8CC88C", color = "#006B4D", alpha = 0.9) +
  geom_vline(xintercept = mean(na.omit(results_habitats_read$CELISTVOST)), linetype = "dotted",
             colour = "palegreen4", size = 1) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  ylab("počet předmětolokalit\n") +
  theme_bw()

vmb_test <- vmb_shp_sjtsk_akt %>%
  sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == "CZ0214008")) %>%
  dplyr::filter(HABITAT == "91E0") %>%
  sf::st_make_valid() %>%
  dplyr::mutate(AREA_real = units::drop_units(sf::st_area(geometry))) %>%
  dplyr::mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA) %>%
  sf::st_make_valid() %>%
  dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                  sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                  sf::st_geometry_type(geometry) != "LINESTRING" & 
                  sf::st_geometry_type(geometry) != "MULTILINESTRING" & 
                  sf::st_geometry_type(geometry) != "GEOMETRYCOLLECTION")
sum(vmb_test$PLO_BIO_M2_EVL)/10000
vmb_testo <- vmb_shp_sjtsk_orig %>%
  sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == "CZ0720033")) %>%
  dplyr::filter(HABITAT == "6210p") %>%
  sf::st_make_valid() %>%
  dplyr::mutate(AREA_real = units::drop_units(sf::st_area(geometry))) %>%
  #dplyr::filter(AREA_real > 0) %>%
  dplyr::mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA)
sum(vmb_testo$PLO_BIO_M2_EVL)/10000

st_geometry_type(vmb_test) == "GEOMETRYCOLLECTION"
vmb_test  %>% st_union() %>% plot
  st_cast(., "POLYGON")

vmb_sumava <- vmb_shp_sjtsk_akt %>%
  sf::st_filter(., evl_sjtsk %>%
                  dplyr::filter(., SITECODE == "CZ0314024") %>%
                  sf::st_buffer(., 1000))
st_write(vmb_sumava, "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/vmb_sumava.gpkg")

vmb_shp_sjtsk_orig$DATUM.x[1]+356 >= red_list_species$DATUM_OD[1]
rltest <- st_intersection(red_list_species, vmb_shp_sjtsk_orig[c(1:50),])
rltest %>% filter(DATUM_OD >= DATUM.x)

st_crs(vmb_shp_sjtsk_a1) == st_crs(expansive_species)

pozary_22 <- hvezdice_eval(6230, "CZ0320037")
pozary_00 <- hvezdice_eval_orig(6230, "CZ0320037")
pozary_a1 <- hvezdice_eval_a1(6230, "CZ0320037")

pozary <- pozary_00 %>%
  dplyr::rename_with(~str_c(., "_00"), .cols = c(4:29)) %>%
  dplyr::left_join(., pozary_a1 %>%
                     dplyr::rename_with(~str_c(., "_A1"), .cols = c(4:45)), by = c("SITECODE", "HABITAT_CODE")) %>%
  dplyr::left_join(., pozary_22 %>%
                     dplyr::rename_with(~str_c(., "_22"), .cols = c(4:45)), by = c("SITECODE", "HABITAT_CODE")) %>%
  dplyr::left_join(., results_limits, by = c("SITECODE", "HABITAT_CODE")) %>%
  dplyr::mutate(ROZLOHA_STAV_00 = dplyr::case_when(is.na(ROZLOHA_00) ~ NA_character_,
                                                        is.na(ROZLOHA_CIL) ~ NA_character_,
                                                        ROZLOHA_00 < ROZLOHA_CIL ~ "nesplněno",
                                                        ROZLOHA_00 >= ROZLOHA_CIL ~ "splněno"),
                ROZLOHA_STAV_A1 = dplyr::case_when(is.na(ROZLOHA_A1) ~ NA_character_,
                                                        is.na(ROZLOHA_CIL) ~ NA_character_,
                                                        ROZLOHA_A1 < ROZLOHA_CIL ~ "nesplněno",
                                                        ROZLOHA_A1 >= ROZLOHA_CIL ~ "splněno"),
                ROZLOHA_STAV_22 = dplyr::case_when(is.na(ROZLOHA_22) ~ NA_character_,
                                                        is.na(ROZLOHA_CIL) ~ NA_character_,
                                                        ROZLOHA_22 < ROZLOHA_CIL ~ "nesplněno",
                                                        ROZLOHA_22 >= ROZLOHA_CIL ~ "splněno"),
                KVALITA_STAV_00 = dplyr::case_when(is.na(KVALITA_00) ~ NA_character_,
                                                        is.na(KVALITA_CIL) ~ NA_character_,
                                                        KVALITA_00 > KVALITA_CIL ~ "nesplněno",
                                                        KVALITA_00 <= KVALITA_CIL ~ "splněno"),
                KVALITA_STAV_A1 = dplyr::case_when(is.na(KVALITA_A1) ~ NA_character_,
                                                        is.na(KVALITA_CIL) ~ NA_character_,
                                                        KVALITA_A1 > KVALITA_CIL ~ "nesplněno",
                                                        KVALITA_A1 <= KVALITA_CIL ~ "splněno"),
                KVALITA_STAV_22 = dplyr::case_when(is.na(KVALITA_22) ~ NA_character_,
                                                        is.na(KVALITA_CIL) ~ NA_character_,
                                                        KVALITA_22 > KVALITA_CIL ~ "nesplněno",
                                                        KVALITA_22 <= KVALITA_CIL ~ "splněno"),
                HODNOCENI_00 = dplyr::case_when(is.na(KVALITA_00) ~ NA_character_,
                                                is.na(ROZLOHA_00) ~ NA_character_,
                                                is.na(KVALITA_CIL) ~ NA_character_,
                                                is.na(ROZLOHA_CIL) ~ NA_character_,
                                                ROZLOHA_STAV_00 == "nesplněno" &
                                                  KVALITA_STAV_00 == "nesplněno" ~ "špatný",
                                                ROZLOHA_STAV_00 == "nesplněno" |
                                                  KVALITA_STAV_00 == "nesplněno" ~ "zhoršený",
                                                ROZLOHA_STAV_00 == "splněno" &
                                                  KVALITA_STAV_00 == "splněno" ~ "dobrý"),
                HODNOCENI_A1 = dplyr::case_when(is.na(KVALITA_A1) ~ NA_character_,
                                                is.na(ROZLOHA_A1) ~ NA_character_,
                                                is.na(KVALITA_CIL) ~ NA_character_,
                                                is.na(ROZLOHA_CIL) ~ NA_character_,
                                                ROZLOHA_STAV_A1 == "nesplněno" &
                                                  KVALITA_STAV_A1 == "nesplněno" ~ "špatný",
                                                ROZLOHA_STAV_A1 == "nesplněno" |
                                                  KVALITA_STAV_A1 == "nesplněno" ~ "zhoršený",
                                                ROZLOHA_STAV_A1 == "splněno" &
                                                  KVALITA_STAV_A1 == "splněno" ~ "dobrý"),
                HODNOCENI_22 = dplyr::case_when(is.na(KVALITA_22) ~ NA_character_,
                                                is.na(ROZLOHA_22) ~ NA_character_,
                                                is.na(KVALITA_CIL) ~ NA_character_,
                                                is.na(ROZLOHA_CIL) ~ NA_character_,
                                                ROZLOHA_STAV_22 == "nesplněno" &
                                                  KVALITA_STAV_22 == "nesplněno" ~ "špatný",
                                                ROZLOHA_STAV_22 == "nesplněno" |
                                                  KVALITA_STAV_22 == "nesplněno" ~ "zhoršený",
                                                ROZLOHA_STAV_22 == "splněno" &
                                                  KVALITA_STAV_22 == "splněno" ~ "dobrý"),
                TREND_ROZLOHA_00A1 = dplyr::case_when(is.na(ROZLOHA_00) ~ NA_character_,
                                                      is.na(ROZLOHA_A1) ~ NA_character_,
                                                      ROZLOHA_00*0.95 < ROZLOHA_A1 ~ "negativní",
                                                      ROZLOHA_00*0.95 >= ROZLOHA_A1 &
                                                        ROZLOHA_00*1.05 <= ROZLOHA_A1 ~ "stabilní",
                                                      ROZLOHA_00*1.05 < ROZLOHA_A1 ~ "pozitivní"),
                TREND_ROZLOHA_A122 = dplyr::case_when(is.na(ROZLOHA_A1) ~ NA_character_,
                                                      is.na(ROZLOHA_22) ~ NA_character_,
                                                      ROZLOHA_A1*0.95 < ROZLOHA_22 ~ "negativní",
                                                      ROZLOHA_A1*0.95 >= ROZLOHA_22 &
                                                        ROZLOHA_A1*1.05 <= ROZLOHA_22 ~ "stabilní",
                                                      ROZLOHA_A1*1.05 > ROZLOHA_22 ~ "pozitivní"),
                TREND_KVALITA_00A1 = dplyr::case_when(is.na(KVALITA_00) ~ NA_character_,
                                                      is.na(KVALITA_A1) ~ NA_character_,
                                                      KVALITA_00*1.05 < KVALITA_A1 ~ "negativní",
                                                      KVALITA_00*1.05 >= KVALITA_A1 &
                                                        KVALITA_00*0.95 <= KVALITA_A1 ~ "stabilní",
                                                      KVALITA_00*0.95 > KVALITA_A1 ~ "pozitivní"),
                TREND_KVALITA_A122 = dplyr::case_when(is.na(KVALITA_A1) ~ NA_character_,
                                                      is.na(KVALITA_22) ~ NA_character_,
                                                      KVALITA_A1*1.05 < KVALITA_22 ~ "negativní",
                                                      KVALITA_A1*1.05 >= KVALITA_22 &
                                                        KVALITA_A1*0.95 <= KVALITA_22 ~ "stabilní",
                                                      KVALITA_A1*0.95 > KVALITA_22 ~ "pozitivní"),
                TREND_DEGRAD_00A1 = dplyr::case_when(is.na(DEGRAD_AREA_PERC_00) ~ NA_character_,
                                                     is.na(DEGRAD_AREA_PERC_A1) ~ NA_character_,
                                                     DEGRAD_AREA_PERC_00*1.05 < DEGRAD_AREA_PERC_A1 ~ "negativní",
                                                     DEGRAD_AREA_PERC_00*1.05 >= DEGRAD_AREA_PERC_A1 &
                                                       DEGRAD_AREA_PERC_00*0.95 <= DEGRAD_AREA_PERC_A1 ~ "stabilní",
                                                     DEGRAD_AREA_PERC_00*0.95 > DEGRAD_AREA_PERC_A1 ~ "pozitivní"),
                TREND_DEGRAD_A122 = dplyr::case_when(is.na(DEGRAD_AREA_PERC_A1) ~ NA_character_,
                                                     is.na(DEGRAD_AREA_PERC_22) ~ NA_character_,
                                                     DEGRAD_AREA_PERC_A1*1.05 < DEGRAD_AREA_PERC_22 ~ "negativní",
                                                     DEGRAD_AREA_PERC_A1*1.05 >= DEGRAD_AREA_PERC_22 &
                                                       DEGRAD_AREA_PERC_A1*0.95 <= DEGRAD_AREA_PERC_22 ~ "stabilní",
                                                     DEGRAD_AREA_PERC_A1*0.95 > DEGRAD_AREA_PERC_22 ~ "pozitivní"),
                TREND_MINIMIAREAL_00A1 = dplyr::case_when(is.na(MINIMIAREAL_00) ~ NA_character_,
                                                          is.na(MINIMIAREAL_A1) ~ NA_character_,
                                                          MINIMIAREAL_00*0.95 > MINIMIAREAL_A1 ~ "negativní",
                                                          MINIMIAREAL_00*0.95 <= MINIMIAREAL_A1 &
                                                            MINIMIAREAL_00*1.05 >= MINIMIAREAL_A1 ~ "stabilní",
                                                          MINIMIAREAL_00*1.05 < MINIMIAREAL_A1 ~ "pozitivní"),
                TREND_MINIMIAREAL_A122 = dplyr::case_when(is.na(MINIMIAREAL_A1) ~ NA_character_,
                                                          is.na(MINIMIAREAL_22) ~ NA_character_,
                                                          MINIMIAREAL_A1*0.95 > MINIMIAREAL_22 ~ "negativní",
                                                          MINIMIAREAL_A1*0.95 <= MINIMIAREAL_22 &
                                                            MINIMIAREAL_A1*1.05 >= MINIMIAREAL_22 ~ "stabilní",
                                                          MINIMIAREAL_A1*1.05 < MINIMIAREAL_22 ~ "pozitivní"),
                TREND_MOZAIKA_00A1 = dplyr::case_when(MOZAIKA_00*0.95 > MOZAIKA_FIN_A1 ~ "negativní",
                                                      MOZAIKA_00*0.95 <= MOZAIKA_FIN_A1 &
                                                        MOZAIKA_00*1.05 >= MOZAIKA_FIN_A1 ~ "stabilní",
                                                      MOZAIKA_00*1.05 < MOZAIKA_FIN_A1 ~ "pozitivní"),
                TREND_MOZAIKA_A122 = dplyr::case_when(MOZAIKA_FIN_A1*0.95 > MOZAIKA_FIN_22 ~ "negativní",
                                                      MOZAIKA_FIN_A1*0.95 <= MOZAIKA_FIN_22 &
                                                        MOZAIKA_FIN_A1*1.05 >= MOZAIKA_FIN_22 ~ "stabilní",
                                                      MOZAIKA_FIN_A1*1.05 < MOZAIKA_FIN_22 ~ "pozitivní"),
                TREND_TYPICKE_DRUHY_A122 = dplyr::case_when(TYPICKE_DRUHY_A1*1.05 < TYPICKE_DRUHY_22 ~ "negativní",
                                                            TYPICKE_DRUHY_A1*1.05 >= TYPICKE_DRUHY_22 &
                                                              TYPICKE_DRUHY_A1*0.95 <= TYPICKE_DRUHY_22 ~ "stabilní",
                                                            TYPICKE_DRUHY_A1*0.95 > TYPICKE_DRUHY_22 ~ "pozitivní"),
                MRTVE_DREVO_A122 = dplyr::case_when(MRTVE_DREVO_A1*0.95 > MRTVE_DREVO_22 ~ "negativní",
                                                    MRTVE_DREVO_A1*0.95 <= MRTVE_DREVO_22 &
                                                      MRTVE_DREVO_A1*1.05 >= MRTVE_DREVO_22 ~ "stabilní",
                                                    MRTVE_DREVO_A1*1.05 < MRTVE_DREVO_22 ~ "pozitivní"),
                RED_LIST_A122 = dplyr::case_when(RED_LIST_A1*0.95 > RED_LIST_22 ~ "negativní",
                                                 RED_LIST_A1*0.95 <= RED_LIST_22 &
                                                   RED_LIST_A1*1.05 >= RED_LIST_22 ~ "stabilní",
                                                 RED_LIST_A1*1.05 < RED_LIST_22 ~ "pozitivní"),
                INVASIVE_A122 = dplyr::case_when(INVASIVE_A1*0.95 > INVASIVE_22 ~ "negativní",
                                                 INVASIVE_A1*0.95 <= INVASIVE_22 &
                                                   INVASIVE_A1*1.05 >= INVASIVE_22 ~ "stabilní",
                                                 INVASIVE_A1*1.05 < INVASIVE_22 ~ "pozitivní"),
                EXPANSIVE_A122 = dplyr::case_when(EXPANSIVE_A1*0.95 > EXPANSIVE_22 ~ "negativní",
                                                  EXPANSIVE_A1*0.95 <= EXPANSIVE_22 &
                                                    EXPANSIVE_A1*1.05 >= EXPANSIVE_22 ~ "stabilní",
                                                  EXPANSIVE_A1*1.05 < EXPANSIVE_22 ~ "pozitivní"))


write.csv2(pozary %>%
             dplyr::select( c(1:3), ROZLOHA_22, c(114:141)) %>%
             dplyr::mutate(ROZLOHA_CIL = round(as.numeric(ROZLOHA_CIL), 2),
                           KVALITA_CIL = round(as.numeric(KVALITA_CIL), 2)),
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/pozary.csv",
           row.names = FALSE)
write.csv2(pozary %>%
             #dplyr::select( c(1:3), ROZLOHA_22, c(114:141)) %>%
             dplyr::mutate(ROZLOHA_CIL = round(as.numeric(ROZLOHA_CIL), 2),
                           KVALITA_CIL = round(as.numeric(KVALITA_CIL), 2)),
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/pozary_vsechno.csv",
           row.names = FALSE)

# TEST ----
kun_6210 <- hvezdice_eval_geom(sites_habitats[216,5], sites_habitats[216,1])
podmilesy_6430 <- hvezdice_eval_geom(6430, "CZ0420160")
podmilesy_a1_6430 <- hvezdice_eval_a1_geom(6430, "CZ0420160")
sumava_91D0 <- hvezdice_eval_geom("91D0", "CZ0314024")

kvetenice_6430 <- hvezdice_eval_geom("91H0", "CZ0624065")
kvetenice_6430_a1 <- hvezdice_eval_a1_geom("91H0", "CZ0624065")
kvetenice_6430_orig <- hvezdice_eval_orig_geom("91H0", "CZ0624065")
sum(kvetenice_6430$PLO_BIO_M2_EVL)
sum(kvetenice_6430_a1$PLO_BIO_M2_EVL)
sum(kvetenice_6430_orig$PLO_BIO_M2_EVL)

zehun_6210_o <- hvezdice_eval_orig_geom(6210, "CZ0214050")
zehun_6210 <- hvezdice_eval_a1_geom(6210, "CZ0214050")

plot()

map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
  addPolygons(data = sf::st_transform(evl_sjtsk %>%
                                        dplyr::filter(NAZEV == "Žehuňsko"), 
                                      CRS("+init=epsg:4326")) %>%
                sf::st_make_valid(),
              color = "blue",
              fill = NULL,
              weight = 10,
              opacity = 0.3,
              group = "EVL") %>%
  addPolygons(data = sf::st_transform(zehun_6210_o, CRS("+init=epsg:4326")) %>%
                sf::st_make_valid(),
              color = "green",
              fill = "green",
              weight = 1,
              opacity = 1,
              popup = ~label,
              group = "VMB1") %>%
  addPolygons(data = sf::st_transform(zehun_6210, CRS("+init=epsg:4326")) %>%
                sf::st_make_valid(),
              color = "darkgreen",
              fill = "darkgreen",
              weight = 1,
              opacity = 1,
              popup = ~label,
              group = "VMB2") %>%
  addLayersControl(baseGroups = c("Esri WorldTopoMap", "Esri WorldImagery"),
                   overlayGroups = c("VMB1",
                                     "VMB2",
                                     "EVL"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMeasure(primaryLengthUnit = "meters") %>% 
  addScaleBar(position = "bottomleft")
map

analysis <- evl_sjtsk %>%
  dplyr::filter(SITECODE %in% c("CZ0624065", "CZ0214003", "CZ0420160", "CZ0410401")) %>%
  st_intersection(., vmb_shp_sjtsk_akt)
  
analysis_a1 <- evl_sjtsk %>%
  dplyr::filter(SITECODE %in% c("CZ0624065", "CZ0214003", "CZ0420160", "CZ0410401")) %>%
  st_intersection(., vmb_shp_sjtsk_a1)

analysis_orig <- evl_sjtsk %>%
  dplyr::filter(SITECODE %in% c("CZ0624065", "CZ0214003", "CZ0420160", "CZ0410401")) %>%
  st_intersection(., vmb_shp_sjtsk_orig)

st_write(habresults_100_110[c(2:nrow(habresults_100_110)),], 
         "S:/Složky uživatelů/Gaigr/darko.shp", 
         append = FALSE)
vmb_shp_sjtsk_a1 %>%
  st_drop_geometry() %>%
  filter(BIOTOP == "T1.1")

vmb_shp_sjtsk_akt %>%
  st_drop_geometry() %>%
  filter(BIOTOP == "T1.1") 

sites_habitats %>%
  filter(feature_type == "stanoviště") %>%
  group_by(feature_code) %>%
  summarise(pocet = n()) %>%
  arrange(pocet)
results_1vmb %>%
  group_by(HABITAT_CODE) %>%
  summarise(pocet = n()) %>%
  arrange(pocet)

sites_habitats %>%
  filter(feature_type == "stanoviště" & !feature_code %in% c("3140", "91T0")) %>%
  nrow()
results_1vmb %>%
filter(!HABITAT_CODE %in% c("3140", "91T0")) %>%
  nrow()
sites_habitats %>%
  filter(feature_code %in% results_1vmb$HABITAT_CODE & site_code %in% results_1vmb$SITECODE) %>%
  nrow()

pejri <- openxlsx::read.xlsx("netopyri_evl.xlsx") %>%
  dplyr::mutate(oop = stringr::str_split(as.character(oop), ", ")) %>%
  tidyr::unnest() %>%
  dplyr::filter(grepl("kraje", oop, ignore.case = TRUE) | grepl("magis", oop, ignore.case = TRUE))

pejri_sum <- pejri %>%
  filter(PAR_NAZEV == "CELKOVE_HODNOCENI") %>%
  group_by(oop, PAR_STAV) %>%
  dplyr::summarise(POCET = n(),
                   NAZEV = toString(unique(NAZEV)),
                   DRUH = toString(unique(DRUH))) %>%
  dplyr::arrange(-POCET)

ggplot(data = pejri_sum, aes(x = oop, y = POCET, fill = PAR_STAV)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("green", "grey", "red", "orange"),
                    name = "STAV") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  xlab("\nkrajský úřad") +
  ylab("počet EVL\n") +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90))

hvezdice_eval_a1("6210p", "CZ0214017")
hvezdice_eval_a1("6210", "CZ0214017")

50/habitat_areas_a1 %>%
  dplyr::filter(., HABITAT == "6510") %>%
  pull(TOTAL_AREA_ALL)/10000*100

vmbtest <- vmb_shp_sjtsk_a1 %>%
  sf::st_intersection(dplyr::filter(evl_sjtsk, SITECODE == "CZ0214017")) %>%
  dplyr::filter(HABITAT == "6210") %>%
  sf::st_make_valid() %>%
  dplyr::filter(sf::st_geometry_type(geometry) != "POINT" & 
                  sf::st_geometry_type(geometry) != "MULTIPOINT" & 
                  sf::st_geometry_type(geometry) != "LINESTRING" & 
                  sf::st_geometry_type(geometry) != "MULTILINESTRING") %>%
  dplyr::mutate(AREA_real = units::drop_units(sf::st_area(geometry))) %>%
  dplyr::filter(AREA_real > 0) %>%
  dplyr::mutate(PLO_BIO_M2_EVL = STEJ_PR/100*AREA_real)

vmb_vlivy <- vmbtest %>%
  separate_wider_delim(DGP, delim = ";", names_sep = '_', too_few = "align_start")
    
silnice <- sf::st_read("F:/TN-ROAD/TN_ROAD.gpkg")
silnice_line <- silnice %>%
  st_cast(., "LINESTRING")
sf::st_write(silnice_line,
             "F:/TN_ROAD_L.shp")    
# END -----
kuk <- read.csv2("C:/Users/jonas.gaigr/Downloads/nalezy-po-filtru (7).csv") %>% 
  sf::st_as_sf(., coords = c("X", "Y"), crs = "+init=epsg:5514")
  
n2k <- bind_rows(evl, po)
st_intersection(kuk, n2k)

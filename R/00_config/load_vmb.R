
load_vmb <- function(vmb_x = 1, clean = TRUE) {
  
  if(vmb_x == 1) {
    
    # VMB - ZÁKLADNÍ MAPOVÁNÍ
    vmb_shp_sjtsk_orig_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/20060501_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
        )
    vmb_hab_dbf_orig <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/HAB20060501_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
        )
    vmb_pb_dbf_orig <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/PB20060501_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
        ) %>%
      dplyr::filter(
        !OBJECTID %in% vmb_hab_dbf_orig$OBJECTID
        )
    
    vmb_hab_pb_dbf_orig <- 
      dplyr::bind_rows(
        vmb_hab_dbf_orig, 
        vmb_pb_dbf_orig
        ) %>%
      dplyr::group_by(
        SEGMENT_ID
        ) %>%
      dplyr::mutate(
        moz_num = n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)
        ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
        ) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_orig <- 
      vmb_shp_sjtsk_orig_read %>%
      dplyr::left_join(
        vmb_hab_dbf_orig, 
        by = "SEGMENT_ID"
        ) %>%
      dplyr::left_join(
        vmb_hab_pb_dbf_orig,
        by = "SEGMENT_ID"
        ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
          ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT
          )
        )
    
    assign(
      "vmb_shp_sjtsk_orig", 
      vmb_shp_sjtsk_orig, 
      envir = .GlobalEnv
      )
    
  } else if(vmb_x == 2) {
    
    # VMB1 - PRVNÍ AKTUALIZACE
    vmb_shp_sjtsk_a1_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Aktualizace1_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
        )
    
    vmb_hab_dbf_a1 <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Biotop/Aktualizace1_Hab_biotop.dbf", 
        options = "ENCODING=WINDOWS-1250"
        )
    
    vmb_pb_dbf_a1 <-
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Biotop/Aktualizace1_Biotop.dbf",
        options = "ENCODING=WINDOWS-1250"
        ) %>%
      dplyr::filter(
        !OBJECTID_1 %in% vmb_hab_dbf_a1$OBJECTID_1
        )
    
    vmb_hab_pb_dbf_a1 <- 
      dplyr::bind_rows(
        vmb_hab_dbf_a1, 
        vmb_pb_dbf_a1) %>%
      dplyr::group_by(SEGMENT_ID
                      ) %>%
      dplyr::mutate(
        moz_num = n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_
          )
        ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
        ) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_a1 <- 
      vmb_shp_sjtsk_a1_read %>%
      dplyr::left_join(
        vmb_hab_dbf_a1, 
        by = "SEGMENT_ID"
        ) %>%
      dplyr::left_join(
        vmb_hab_pb_dbf_a1, 
        by = "SEGMENT_ID"
        ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
          ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT
          )
        )
    
    paseky_a1 <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_a1_results_20240814.csv")
    
    assign(
      "vmb_shp_sjtsk_a1",
      vmb_shp_sjtsk_a1, 
      envir = .GlobalEnv
      )
    
    assign(
      "paseky_a1", 
      paseky_a1, 
      envir = .GlobalEnv
      )
    
  } else if(vmb_x == 0) {
    # VMB - MÁJOVÁ VRSTVA AKTUÁLNÍ
    vmb_shp_sjtsk_akt_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Aktualni_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
        )
    vmb_hab_dbf_akt <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/HAB_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
        )
    vmb_pb_dbf_akt <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/PB_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
        ) %>%
      dplyr::filter(
        !OBJECTID %in% vmb_hab_dbf_akt$OBJECTID
        )
    
    vmb_hab_pb_dbf_akt <- 
      dplyr::bind_rows(
        vmb_hab_dbf_akt,
        vmb_pb_dbf_akt
        ) %>%
      dplyr::group_by(
        SEGMENT_ID
        ) %>%
      dplyr::mutate(
        moz_num = n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_
          )
        ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
        ) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_akt <- 
      vmb_shp_sjtsk_akt_read %>%
      dplyr::left_join(
        ., 
        vmb_hab_dbf_akt, 
        by = "SEGMENT_ID"
        ) %>%
      dplyr::left_join(
        ., 
        vmb_hab_pb_dbf_akt, 
        by = "SEGMENT_ID"
        ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
          ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT),
          REGION_ID = REGION_ID.x
        ) %>%
      dplyr::rename(
        DATUM = DATUM.x
        )
    
    paseky_23 <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv")
    
    
    assign(
      "vmb_shp_sjtsk_akt", 
      vmb_shp_sjtsk_akt, 
      envir = .GlobalEnv
      )
    assign(
      "paseky", 
      paseky_23, 
      envir = .GlobalEnv
      )
    
  } 
  
  if(vmb_x == 1 & clean == TRUE) {
    
    rm(
      vmb_shp_sjtsk_orig_read, 
      vmb_hab_dbf_orig, 
      vmb_pb_dbf_orig,
      vmb_hab_pb_dbf_orig
      )
    
  } else if(vmb_x == 2 & clean == TRUE) {
    
    rm(
      vmb_shp_sjtsk_a1_read, 
      vmb_hab_dbf_a1, 
      vmb_pb_dbf_a1, 
      vmb_hab_pb_dbf_a1
      )
    
  } else if(vmb_x == 0 & clean == TRUE) {
    
    rm(
      vmb_shp_sjtsk_akt_read, 
      vmb_hab_dbf_akt, 
      vmb_pb_dbf_akt, 
      vmb_hab_pb_dbf_akt
      )
    
  }
  
}

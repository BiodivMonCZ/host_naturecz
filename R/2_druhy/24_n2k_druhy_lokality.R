#----------------------------------------------------------#
# Priprava agregovanych indikatoru ----
#----------------------------------------------------------#
n2k_druhy_lok_pre <- 
  n2k_druhy_lim %>%
  dplyr::group_by(
    kod_chu, 
    DRUH, 
    KOD_LOKAL, 
    POLE, 
    ROK, 
    ID_IND
    ) %>%
  dplyr::reframe(
    SKUPINA = unique(
      SKUPINA
      ),
    NAZEV_LOK = toString(
      unique(
        LOKALITA
        )
      ),
    ID_ND_AKCE = toString(
      unique(
        IDX_ND_AKCE
        )
      ),
    DATUM = max(
      DATUM, 
      na.rm = TRUE
      ),
    HOD_IND = toString(na.omit(unique(HOD_IND))),        
    TYP_IND = unique(TYP_IND),
    LIM_IND = unique(LIM_IND),
    JEDNOTKA = unique(JEDNOTKA),
    LIM_INDLIST = unique(LIM_INDLIST),
    STAV_IND = dplyr::case_when(
      IND_GRP == "minmax" & grepl("POP_POSK", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
      IND_GRP == "minmax" & grepl("POP_", ID_IND) == TRUE ~ max(as.numeric(STAV_IND), na.rm = TRUE),
      IND_GRP == "minmax" & grepl("POP_", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
      IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE)
      ),
    KLIC = unique(KLIC),
    IND_GRP = unique(IND_GRP),
    UROVEN = unique(UROVEN),
    CILMON = max(
      CILMON, 
      na.rm = TRUE
      )
  ) %>%
  dplyr::mutate_all(
    ~ ifelse(
      is.infinite(
        .
        ),
      NA,
      .)
    ) %>% 
  dplyr::ungroup() %>%
  dplyr::distinct()

#----------------------------------------------------------#
# Napojeni na limity ----
#----------------------------------------------------------#
n2k_druhy_lok <- n2k_druhy_lok_pre %>%
  dplyr::group_by(
    kod_chu, 
    DRUH, 
    KOD_LOKAL, 
    ROK
    ) %>%
  dplyr::mutate(
    IND_SUM = STAV_IND[limity$UROVEN %in% c("lok") &
                         is.na(LIM_IND) == FALSE] %>%
      na.omit() %>%
      as.numeric() %>%
      sum() %>%
      as.character(),
    IND_SUMKLIC = STAV_IND[KLIC == "ano" &
                            limity$UROVEN %in% c("lok") &
                            is.na(LIM_IND) == FALSE] %>%
      na.omit() %>%
      as.numeric() %>%
      sum() %>%
      as.character(),
    IND_SUMOST = STAV_IND[KLIC == "ne" &
                            limity$UROVEN %in% c("lok") &
                            is.na(LIM_IND) == FALSE] %>%
      na.omit() %>%
      as.numeric() %>%
      sum() %>%
      as.character(),
    IND_LEN = limity$ID_IND[limity$DRUH %in% DRUH &
                              limity$UROVEN %in% c("lok") &
                              is.na(limity$LIM_IND) == FALSE] %>%
      unique() %>% 
      na.omit() %>%
      length(),
    IND_LENKLIC = limity$ID_IND[limity$DRUH %in% DRUH & 
                                  limity$KLIC == "ano" &
                                  limity$UROVEN %in% c("lok") &
                                  is.na(limity$LIM_IND) == FALSE] %>%
      unique() %>%
      na.omit() %>%
      length(),
    IND_LENOST = limity$ID_IND[limity$DRUH %in% DRUH & 
                                 limity$KLIC == "ne" &
                                 limity$UROVEN %in% c("lok") &
                                 is.na(limity$LIM_IND) == FALSE] %>%
      unique() %>% 
      na.omit() %>%
      length()
    ) %>%
  # pokud sumklic spatny tak spatny, pokud sum ost tak spatny, pokud sumost tak zhorseny, pokud nic z toho tak dobry, TRUE ~ neznamy pres napojeni na limity
  dplyr::mutate(
    CELKOVE = dplyr::case_when(
      is.na(CILMON) == TRUE ~ NA_real_,
      unique(IND_SUMKLIC) < IND_LENKLIC ~ 0,
      unique(IND_SUMOST) < (IND_LENOST - 2) ~ 0,
      unique(IND_SUMOST) < (IND_LENOST - 1) ~ 0.5,
      unique(IND_SUMKLIC) >= IND_LENKLIC & 
        unique(IND_SUMOST) > (IND_LENOST - 1) ~ 1,
      TRUE ~ NA_real_
      )
    ) %>%
  dplyr::mutate(
    STAV_IND = dplyr::case_when(
      ID_IND == "CELKOVE_HODNOCENI" ~ CELKOVE,
      TRUE ~ STAV_IND
      )
    ) %>%
  dplyr::mutate(
    HOD_IND = dplyr::case_when(
      is.na(STAV_IND) == TRUE ~ "neznámý",
      ID_IND == "CELKOVE_HODNOCENI" & 
        STAV_IND == 0 
      ~ "špatný",
      ID_IND == "CELKOVE_HODNOCENI" & 
        STAV_IND == 0.5 
      ~ "zhoršený",
      ID_IND == "CELKOVE_HODNOCENI" & 
        STAV_IND == 1 
      ~ "dobrý",
      TRUE ~ HOD_IND
      )
    ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%
  dplyr::arrange(ID_ND_AKCE)

#----------------------------------------------------------#
# Vyber ID_AKCE reprezentujici SITMAP_1RAD ----
#----------------------------------------------------------#
n2k_druhy_pole1_idakce <- 
  n2k_druhy_lok %>%
  dplyr::group_by(
    kod_chu, 
    DRUH, 
    POLE
    ) %>%
  dplyr::arrange(
    desc(
      CILMON
      ),
    desc(
      ROK
      ), 
    desc(
      CELKOVE
      ), 
    desc(
      DATUM
      )
    ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(ID_ND_AKCE)

#----------------------------------------------------------#
# Filtr podle POLE ----
#----------------------------------------------------------#
n2k_druhy_pole1eval <- 
  n2k_druhy_lok %>%
  dplyr::filter(
    ID_ND_AKCE %in% n2k_druhy_pole1_idakce
    ) %>%
  dplyr::ungroup()

#----------------------------------------------------------#
# Vyber posledniho ID_AKCE za lokalitu ----
#----------------------------------------------------------#
n2k_druhy_lok_idakce <- 
  n2k_druhy_lok %>%
  dplyr::group_by(
    kod_chu, 
    DRUH, 
    KOD_LOKAL
    ) %>%
  dplyr::arrange(
    dplyr::desc(
      CILMON
      ),
    dplyr::desc(
      ROK
      ), 
    dplyr::desc(
      CELKOVE
      ), 
    dplyr::desc(
      DATUM
      )
    ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::pull(ID_ND_AKCE)

#----------------------------------------------------------#
# Filtr lokalit podle vybraneho ID_AKCE -----
#----------------------------------------------------------#
n2k_druhy_lokeval <- n2k_druhy_lok %>%
  dplyr::filter(
    ID_ND_AKCE %in% n2k_druhy_pole1_idakce
    ) %>%
  dplyr::ungroup()

#----------------------------------------------------------#
# Zapis dat -----
#----------------------------------------------------------#

lok_export <-
  function() {
    
    n2k_druhy_lok_write <-
      n2k_druhy_lokeval %>%
      dplyr::left_join(
        ., 
        evl %>%
          sf::st_drop_geometry() %>%
          dplyr::select(
            SITECODE, 
            NAZEV
          ),
        by = c(
          "kod_chu" = "SITECODE"
        )
      ) %>%
      dplyr::left_join(
        ., 
        n2k_druhy_obdobi_lok,
        by = join_by(
          "kod_chu",
          "KOD_LOKAL",
          "POLE",
          "DRUH",
        )
      ) %>%
      dplyr::left_join(
        .,
        rp_code,
        by = join_by(
          "kod_chu"
        )
      ) %>%
      dplyr::left_join(
        .,
        n2k_oop,
        by = c("kod_chu" = "SITECODE")
      )
    
    sep_isop <- ";"
    quote_env_isop <- FALSE
    encoding_isop <- "UTF-8"
    
    sep <- ","
    quote_env <- TRUE
    encoding <- "Windows-1250"
    
    write.table(
      n2k_druhy_lok_write,
      paste0(
        "Outputs/Data/druhy/",
        "n2k_druhy_lok",
        "_",
        current_year,
        "_",
        gsub(
          "-", 
          "", 
          Sys.Date()
          ),
        "_",
        encoding,
        ".csv"
      ),
      row.names = FALSE,
      sep = sep,
      quote = quote_env,
      fileEncoding = encoding
    )  
    
    write.table(
      n2k_druhy_lok_write,
      paste0(
        "Outputs/Data/druhy/",
        "n2k_druhy_lok",
        "_",
        current_year,
        "_",
        gsub(
          "-", 
          "", 
          Sys.Date()
          ),
        "_",
        encoding_isop,
        ".csv"
      ),
      row.names = FALSE,
      sep = sep_isop,
      quote = quote_env_isop,
      fileEncoding = encoding_isop
    )  
    
  }

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
#----------------------------------------------------------#
# Nacteni temp dat ----
#----------------------------------------------------------#
n2k_druhy_lim <- 
  readr::read_csv(
    "Data/Temp/n2k_druhy_lim.csv"
  )

ncol_druhy_lim <- 
  ncol(
    n2k_druhy_lim
  )

run_n2k_druhy_lok <- function(
    n2k_druhy_lim,
    species_name,
    sites_subjects,
    limity,
    current_year = 2024
    ) {
  
  bad_groups <- n2k_druhy_lim %>%
    dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE, ROK, ID_IND) %>%
    dplyr::summarise(
      n_typ_ind = n_distinct(TYP_IND),
      n_klic    = n_distinct(KLIC),
      n_urowen  = n_distinct(UROVEN),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_typ_ind > 1 | n_klic > 1 | n_urowen > 1) %>%
    dplyr::pull(ID_IND) %>%
    unique()
  
  if (length(bad_groups) == 0) {
    warning(glue::glue("Druh {species_name}: Vsechny indikatory vraceji prave 1 typ, klicovost i uroven"))
  } else {
    warning(glue::glue("Druh {species_name}: indikator {bad_groups} vraci vice nez 1 typ, klicovost ci uroven"))
    
  }
  
  #----------------------------------------------------------#
  # Nacteni skupiny druhu ----
  #----------------------------------------------------------#
  skupina_druhu <- n2k_druhy_lim %>% 
    dplyr::filter(DRUH == species_name) %>% 
    dplyr::pull(SKUPINA) %>% 
    unique() %>% 
    stats::na.omit() %>%
    dplyr::first()
  
  #----------------------------------------------------------#
  # Priprava dilcich objektu -----
  #----------------------------------------------------------#
  pole_skupiny <- c("Brouci", "Motýli", "Vážky", "Rovnokřídlí")
  # Zde je bezpecnejsi osetrit, pokud by druh nebyl v sites_subjects vubec
  is_pole_druh <- species_name %in% sites_subjects$DRUH[sites_subjects$SKUPINA %in% pole_skupiny]
  
  #----------------------------------------------------------#
  # Priprava agregovanych indikatoru ----
  #----------------------------------------------------------#
  n2k_druhy_lim_post <- 
    n2k_druhy_lim %>%
    dplyr::filter(DRUH == species_name) %>%
    dplyr::group_by(
      kod_chu, 
      DRUH, 
      KOD_LOKAL, 
      POLE, 
      ROK, 
      ID_IND
    ) %>%
    dplyr::reframe(
      SKUPINA = unique(SKUPINA),
      NAZEV_LOK = toString(unique(LOKALITA)),
      ID_ND_AKCE = toString(unique(IDX_ND_AKCE)),
      DATUM = max(DATUM, na.rm = TRUE),
      HOD_IND = toString(na.omit(unique(HOD_IND))),        
      TYP_IND = toString(unique(TYP_IND)),
      TYP_IND = ifelse(grepl("val", TYP_IND) == TRUE, "val", TYP_IND),
      LIM_IND = dplyr::first(na.omit(unique(LIM_IND))),
      JEDNOTKA = dplyr::first(na.omit(unique(JEDNOTKA))),
      LIM_INDLIST = dplyr::first(na.omit(unique(LIM_INDLIST))),
      STAV_IND = dplyr::case_when(
        IND_GRP == "minmax" & grepl("POP_POSK", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "minmax" & grepl("POP_", ID_IND) == TRUE ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "minmax" & grepl("POP_", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        TRUE ~ NA_real_
      ),
      KLIC = dplyr::first(na.omit(unique(KLIC))),
      IND_GRP = dplyr::first(na.omit(unique(IND_GRP))),
      UROVEN = dplyr::first(na.omit(unique(UROVEN))),
      CILMON = max(CILMON, na.rm = TRUE)
    ) %>%
    dplyr::mutate_all(
      ~ ifelse(is.infinite(.), NA, .)
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::distinct()
  
  if(is_pole_druh) {
    
    n2k_druhy_lok_pre <-
      n2k_druhy_lim_post %>%
      dplyr::group_by(
        kod_chu, 
        DRUH, 
        KOD_LOKAL, 
        ROK
      )
      
  } else {
    
    n2k_druhy_lok_pre <-
      n2k_druhy_lim_post %>%
      dplyr::group_by(
        kod_chu, 
        DRUH, 
        KOD_LOKAL, 
        ROK
      ) %>%
      dplyr::mutate(
        POLE = toString(unique(POLE))
        ) %>%
      dplyr::distinct(
        kod_chu, DRUH, KOD_LOKAL, ROK, ID_IND, .keep_all = TRUE
        )
    
  }
  
  #----------------------------------------------------------#
  # Napojeni na limity ----
  #----------------------------------------------------------#
  n2k_druhy_lok <- 
    n2k_druhy_lok_pre %>%
    # 1. Seskupeni - definuje, za co pocitame vysledek
    dplyr::group_by(
      kod_chu, 
      DRUH, 
      KOD_LOKAL, 
      ROK
    ) %>%
    # 2. Vypocet souhrnnych statistik pro danou skupinu
    # Nyni se odkazujeme na sloupce UVNITR dataframu, ne na externi 'limity'
    dplyr::mutate(
      # Pocet klicovych indikatoru, ktere maji vyplneny limit (definovane)
      IND_LENKLIC = sum(KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND), na.rm = TRUE),
      
      # Pocet ostatnich indikatoru, ktere maji vyplneny limit (definovane)
      IND_LENOST  = sum(KLIC == "ne" & UROVEN == "lok" & !is.na(LIM_IND), na.rm = TRUE),
      
      # Kolik klicovych indikatoru skutecne splnilo limit (STAV_IND == 1)
      IND_SUMKLIC = sum(STAV_IND == 1 & KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND), na.rm = TRUE),
      
      # Kolik ostatnich indikatoru skutecne splnilo limit
      IND_SUMOST  = sum(STAV_IND == 1 & KLIC == "ne"  & UROVEN == "lok" & !is.na(LIM_IND), na.rm = TRUE)
    ) %>%
    # 3. Vyhodnoceni celkoveho stavu (Logika zustava stejna, ale pracujeme s cisly)
    dplyr::mutate(
      CELKOVE = dplyr::case_when(
        # Pokud chybi cil monitoringu, nelze hodnotit
        is.na(CILMON) ~ NA_real_,
        
        # Pokud je splneno mene klicovych, nez je pozadovano -> 0 (Spatny)
        IND_SUMKLIC < IND_LENKLIC ~ 0,
        
        # Ostatni: Povolujeme urcitou toleranci (dle puvodniho skriptu)
        # Pokud je splneno o 2 a vice mene nez pozadovano -> 0 (Spatny)
        IND_SUMOST < (IND_LENOST - 1) ~ 0, 
        
        # Pokud chybi prave 1 do plneho poctu -> 0.5 (Zhorseny)
        # (Pozn: podminka vyse < (LEN - 1) zachytila rozdily 2, 3, 4..., takze tady zbyva jen rozdil 1)
        IND_SUMOST < IND_LENOST ~ 0.5,
        
        # Pokud mame vsechny klicove A zaroven dostatek ostatnich -> 1 (Dobry)
        IND_SUMKLIC >= IND_LENKLIC & IND_SUMOST >= (IND_LENOST - 1) ~ 1, # Zde jsem upravil logiku na >=, aby to matematicky sedelo k "zbytku"
        
        TRUE ~ NA_real_
      )
    ) %>%
    # 4. Propis do radku CELKOVE_HODNOCENI
    dplyr::mutate(
      STAV_IND = dplyr::case_when(
        ID_IND == "CELKOVE_HODNOCENI" ~ CELKOVE,
        TRUE ~ STAV_IND
      )
    ) %>%
    # 5. Prevod na slovni hodnoceni
    dplyr::mutate(
      HOD_IND = dplyr::case_when(
        is.na(STAV_IND) ~ "neznámý",
        ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0   ~ "špatný",
        ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0.5 ~ "zhoršený",
        ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 1   ~ "dobrý",
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
      desc(CILMON),
      desc(ROK), 
      desc(CELKOVE), 
      desc(DATUM)
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
      dplyr::desc(CILMON),
      dplyr::desc(ROK), 
      dplyr::desc(CELKOVE), 
      dplyr::desc(DATUM)
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
  
  if(is_pole_druh) {
    n2k_druhy_lok_return <- n2k_druhy_lokeval
  } else {
    n2k_druhy_lok_return <- n2k_druhy_lok
  }
  
  return(n2k_druhy_lok_return)
  
}

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
      ) %>%
      dplyr::distinct()
    
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
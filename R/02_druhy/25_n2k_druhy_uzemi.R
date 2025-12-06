#----------------------------------------------------------#
# Nacteni temp dat ----
#----------------------------------------------------------#
n2k_druhy_lok <- 
  readr::read_csv(
    "Data/Temp/n2k_druhy_lok.csv"
  )

ncol_druhy_lok <- 
  ncol(
    n2k_druhy_lok
  )

n2k_druhy_obdobi_chu <- 
  readr::read_csv(
    "Data/Temp/n2k_druhy_obdobi_chu.csv"
  )

n2k_druhy_posledni_chu <- 
  readr::read_csv(
    "Data/Temp/n2k_druhy_posledni_chu.csv"
  )

#----------------------------------------------------------#
# Funkce napoctu ----
#----------------------------------------------------------#
run_n2k_druhy_uzemi <- function(
    n2k_druhy_lok,
    species_name,
    sites_subjects,
    limity,
    biotop_evd, 
    n2k_druhy_obdobi_chu,
    n2k_druhy_posledni_chu,
    current_year = 2024
) {
  
  #----------------------------------------------------------#
  # Nacteni skupiny druhu ----
  #----------------------------------------------------------#
  skupina_druhu <- n2k_druhy_lok %>% 
    dplyr::filter(DRUH == species_name) %>% 
    dplyr::pull(SKUPINA) %>% 
    unique() %>% 
    stats::na.omit() %>%
    dplyr::first()
  
  #----------------------------------------------------------#
  # Priprava dilcich objektu -----
  #----------------------------------------------------------#
  pole_skupiny <- c("Brouci", "Motýli", "Vážky", "Rovnokřídlí")
  is_pole_druh <- species_name %in% sites_subjects$DRUH[sites_subjects$SKUPINA %in% pole_skupiny]
  
  if (is_pole_druh) {
    # Logika pro POLE
    n2k_druhy_chu_temp <- n2k_druhy_lok %>%
      dplyr::filter(
        DRUH == species_name
        ) %>%
      dplyr::group_by(
        kod_chu, 
        DRUH
        ) %>%
      dplyr::reframe(
        ROK = toString(unique(ROK)),
        POLE = toString(unique(POLE)),
        NAZEV_LOK = toString(unique(NAZEV_LOK)),
        ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
        CILMON_CHU = max(CILMON, na.rm = TRUE),
        POP_POCETPOLE1 = sum(ID_IND == "CELKOVE_HODNOCENI" & CILMON == 1, na.rm = TRUE),
        POP_POCETPOLE1D = sum(
          ID_IND == "CELKOVE_HODNOCENI" & 
            HOD_IND != "neznámý" & HOD_IND != "zhoršený" & HOD_IND != "špatný" & 
            STAV_IND != "NA" & STAV_IND != "0" & is.na(STAV_IND) == FALSE & 
            STAV_IND != 0.5 & STAV_IND != 0 & CILMON == 1, 
          na.rm = TRUE
          ),
        STA_HABPOKRYVPRE = {
          k_chu <- unique(kod_chu)
          x <- biotop_evd$BIOTOP_PROCENTO[biotop_evd$SITECODE == k_chu & biotop_evd$DRUH == species_name]
          if (length(x) == 0) NA_real_ else unique(x)
        }
      ) %>%
      dplyr::mutate(
        POP_PROCPOLE1D = round(POP_POCETPOLE1D/POP_POCETPOLE1*100, 3),
        STA_HABPOKRYV = ifelse(is.na(STA_HABPOKRYVPRE) == TRUE, NA, STA_HABPOKRYVPRE*100)
      ) %>%
      dplyr::select(-STA_HABPOKRYVPRE)
    
  } else { 
    # Logika pro LOKALITY / OSTATNI
    n2k_druhy_chu_temp <- n2k_druhy_lok %>%
      dplyr::filter(DRUH == species_name) %>%
      dplyr::group_by(
        kod_chu, 
        DRUH
        ) %>%
      dplyr::reframe(
        ROK = toString(unique(ROK)), POLE = toString(unique(POLE)), NAZEV_LOK = toString(unique(NAZEV_LOK)), 
        ID_ND_AKCE = toString(unique(ID_ND_AKCE)), CILMON_CHU = max(CILMON, na.rm = TRUE),
        # CELKOVE_HODNOCENI = NA -- TOTO JIZ NENI POTREBA
        POP_PRESENCE = dplyr::case_when(
          any(ID_IND == "POP_PRESENCE" & STAV_IND == 1, na.rm = TRUE) ~ "ano",
          any(ID_IND == "POP_PRESENCE" & STAV_IND == 0, na.rm = TRUE) & !any(ID_IND == "POP_PRESENCE" & STAV_IND == 1, na.rm = TRUE) ~ "ne",
          TRUE ~ NA_character_
        ), 
        POP_POCETMAX = sum(dplyr::case_when(ID_IND == "POP_POCETMAX" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETMIN = sum(dplyr::case_when(ID_IND == "POP_POCETMIN" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETSUM = sum(dplyr::case_when(ID_IND == "POP_POCET" & CILMON == 1 ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE) %>% max(),
        POP_POCETDOB = sum(dplyr::case_when(ID_IND == "POP_POCET" & CELKOVE == 1 & CILMON == 1 ~ as.numeric(HOD_IND), TRUE ~ 0), na.rm = TRUE) %>% max(),
        POP_POCETOST = sum(dplyr::case_when(ID_IND == "POP_POCET" & CELKOVE != 1 & CILMON == 1 ~ as.numeric(HOD_IND), TRUE ~ 0), na.rm = TRUE),
        POP_PROCDOB = dplyr::case_when(is.na(POP_POCETDOB) | is.na(POP_POCETSUM) ~ NA_real_, POP_POCETSUM == 0 ~ 0, TRUE ~ round(POP_POCETDOB / POP_POCETSUM * 100, 3)),
        POP_POCETZIM = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETZIM1 = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETZIM2 = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETZIM3 = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETZIMREF = mean(POP_POCETZIM1, POP_POCETZIM2, POP_POCETZIM3, na.rm = TRUE),
        POP_VITALZIM = ifelse(POP_POCETZIMREF == 0, NA_real_, round(POP_POCETZIM/POP_POCETZIMREF, 3)),
        POP_POCETLETS1 = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETLETS2 = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETLET = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETLET1 = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETLET2 = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETLET3 = sum(dplyr::case_when(ID_IND == "POP_POCET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETLETREF = mean(POP_POCETLET1, POP_POCETLET2, POP_POCETLET3, na.rm = TRUE),
        POP_VITALLET = round(POP_POCETLET/POP_POCETLETREF, 3),
        POP_REPROCHI = round(POP_POCETLETS2/POP_POCETLETS1, 3),
        LOK_POCETSUM = sum(ID_IND == "CELKOVE_HODNOCENI" & CILMON == 1, na.rm = TRUE),
        LOK_POCETDOB = sum(ID_IND == "CELKOVE_HODNOCENI" & CILMON == 1 & HOD_IND == "dobrý", na.rm = TRUE),
        LOK_PROCDOBR = dplyr::case_when(is.na(LOK_POCETDOB) | is.na(LOK_POCETSUM) ~ NA_real_, LOK_POCETSUM == 0 ~ NA_real_, TRUE ~ round(LOK_POCETDOB / LOK_POCETSUM * 100, 3))
      )
  }
  
  #--------------------------------------------------#
  # Prevod na long format ----
  #--------------------------------------------------#
  n2k_druhy_chu_komb_long <- n2k_druhy_chu_temp %>%
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.numeric), 
        .fns = as.character
      )
    ) %>%
    tidyr::pivot_longer(
      cols = -c(kod_chu, DRUH, ROK, POLE, NAZEV_LOK, ID_ND_AKCE, CILMON_CHU), 
      names_to = "ID_IND",
      values_to = "HOD_IND"
    ) %>%
    dplyr::mutate(
      HOD_IND = as.character(HOD_IND)
    )
  
  #----------------------------------------------------------#
  # Napojeni na limity ----
  #----------------------------------------------------------#
  n2k_druhy_chu_pre <- n2k_druhy_chu_komb_long %>%
    dplyr::right_join(
      .,
      limity %>%
        dplyr::filter(UROVEN == "chu"),
      by = c("DRUH" = "DRUH", "ID_IND" = "ID_IND")
    ) %>%
    # Vsechny radky by jiz mely mit ROK (diky inner joinu), ale pro jistotu ponechame fill 
    # v pripade, ze by limity pridaly prazdne radky
    dplyr::group_by(kod_chu, DRUH) %>%
    tidyr::fill(ROK, POLE, NAZEV_LOK, ID_ND_AKCE, CILMON_CHU, .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(kod_chu, DRUH, ID_IND) %>%
    dplyr::arrange(HOD_IND) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      STAV_IND = dplyr::case_when(
        is.na(HOD_IND) == TRUE ~ NA_real_,
        TYP_IND == "min" & as.numeric(HOD_IND) < as.numeric(LIM_IND) ~ 0,
        TYP_IND == "min" & as.numeric(HOD_IND) >= as.numeric(LIM_IND) ~ 1,
        TYP_IND == "max" & as.numeric(HOD_IND) > as.numeric(LIM_IND) ~ 0,
        TYP_IND == "max" & as.numeric(HOD_IND) <= as.numeric(LIM_IND) ~ 1,
        TYP_IND == "val" & HOD_IND != LIM_IND ~ 0,
        TYP_IND == "val" & HOD_IND == LIM_IND ~ 1)) %>%
    dplyr::mutate(
      IND_GRP = dplyr::case_when(
        TYP_IND %in% c("min", "max") ~ "minmax",
        TRUE ~ TYP_IND),
      KLIC = dplyr::case_when(
        is.na(HOD_IND) == TRUE ~ "ne",
        TRUE ~ KLIC
      )
    ) %>%
    dplyr::distinct() %>%
    dplyr::select(-dplyr::starts_with("..."))
  
  
  #----------------------------------------------------------#
  # Konsolidace uzemi a VYPOCET CELKOVE_HODNOCENI -----
  #----------------------------------------------------------#
  
  # Vypocet dilcich stavu (STAV_IND) a pridani metadat obdobi
  n2k_druhy_chu_vypocet <- n2k_druhy_chu_pre %>%
    dplyr::group_by(kod_chu, DRUH, ID_IND, KLIC) %>%
    dplyr::reframe(
      ROK = toString(unique(ROK)),
      POLE = toString(unique(POLE)),
      NAZEV_LOK = toString(unique(NAZEV_LOK)),
      ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
      HOD_IND = toString(na.omit(unique(HOD_IND))),        
      TYP_IND = unique(TYP_IND),
      LIM_IND = unique(LIM_IND),
      JEDNOTKA = unique(JEDNOTKA),
      LIM_INDLIST = unique(LIM_INDLIST),
      STAV_IND = dplyr::case_when(
        is.na(HOD_IND) == TRUE ~ NA_real_,
        IND_GRP == "minmax" & grepl("POP_POSK", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "minmax" & grepl("POP_", ID_IND) == TRUE ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "minmax" & grepl("POP_", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE)
      ),
      KLIC = unique(KLIC),
      UROVEN = unique(UROVEN),
      IND_GRP = unique(IND_GRP),
      CILMON_CHU = max(CILMON_CHU, na.rm = TRUE)
    ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      STAV_IND = ifelse(is.infinite(STAV_IND), 0, STAV_IND)
    ) %>%
    dplyr::group_by(kod_chu, DRUH) %>%
    dplyr::mutate(
      IND_SUMKLIC = sum(STAV_IND[KLIC == "ano" & UROVEN == "chu" & !is.na(LIM_IND)], na.rm = TRUE),
      LENIND_SUMKLIC = length(unique(na.omit(ID_IND[KLIC == "ano" & UROVEN == "chu" & !is.na(LIM_IND)]))),
      LENIND_NAKLIC = sum(KLIC == "ano" & UROVEN == "chu" & is.na(HOD_IND), na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      # Vypocet celkoveho stavu (numeric 0/0.5/1)
      CELKOVE = dplyr::case_when(
        IND_SUMKLIC < (LENIND_SUMKLIC - 1 - LENIND_NAKLIC) ~ 0,
        IND_SUMKLIC < (LENIND_SUMKLIC - LENIND_NAKLIC) ~ 0.5,
        IND_SUMKLIC >= (LENIND_SUMKLIC - LENIND_NAKLIC) ~ 1,
        TRUE ~ NA_real_
      )
    )
  
  # Vytvoreni radku CELKOVE_HODNOCENI z vypoctu CELKOVE
  metadata_chu <- n2k_druhy_chu_vypocet %>%
    dplyr::group_by(kod_chu, DRUH) %>%
    dplyr::summarise(
      ROK = toString(unique(ROK)),
      POLE = toString(unique(POLE)),
      NAZEV_LOK = toString(unique(NAZEV_LOK)),
      ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
      UROVEN = "chu",
      CILMON_CHU = max(CILMON_CHU, na.rm = TRUE),
      STAV_IND = unique(CELKOVE) %>% max(na.rm = TRUE), 
      HOD_IND = dplyr::case_when(
        STAV_IND == 0 ~ "špatný",
        STAV_IND == 0.5 ~ "zhoršený",
        STAV_IND == 1 ~ "dobrý",
        is.na(STAV_IND) ~ "neznámý"
      )
    ) %>%
    dplyr::mutate(
      ID_IND = "CELKOVE_HODNOCENI",
      KLIC = NA_character_,
      TYP_IND = NA_character_,
      LIM_IND = NA_character_,
      JEDNOTKA = NA_character_,
      LIM_INDLIST = NA_character_
    ) %>%
    dplyr::select(kod_chu, DRUH, ROK, POLE, NAZEV_LOK, ID_ND_AKCE, ID_IND, HOD_IND, KLIC, UROVEN, TYP_IND, LIM_IND, JEDNOTKA, LIM_INDLIST, STAV_IND, CILMON_CHU)
  
  # Krok 3: Spojeni radku indikatoru s radky celkoveho hodnoceni
  n2k_druhy_chu_final <- dplyr::bind_rows(
    n2k_druhy_chu_vypocet %>% 
      dplyr::select(-c(IND_SUMKLIC, LENIND_SUMKLIC, LENIND_NAKLIC, CELKOVE)), # Odstranime pomocne sloupce
    metadata_chu
  )
  
  n2k_druhy_chu_final <- n2k_druhy_chu_final %>%
    # Puvodni sekce pro prepis HOD_IND (TEXT) a STAV_IND (SLOVNI)
    dplyr::mutate(
      HOD_IND = dplyr::case_when(
        HOD_IND == "NaN" ~ NA_character_,
        is.na(HOD_IND) == TRUE & ID_IND != "CELKOVE_HODNOCENI" ~ NA_character_,
        TRUE ~ HOD_IND
      )
    ) %>%
    dplyr::mutate(
      STAV_IND = dplyr::case_when(
        UROVEN != "chu" ~ "nehodnocen",
        ID_IND != "CELKOVE_HODNOCENI" & is.na(UROVEN) == TRUE ~ "nehodnocen",
        ID_IND != "CELKOVE_HODNOCENI" & is.na(LIM_IND) == TRUE ~ "nehodnocen",
        is.na(STAV_IND) == TRUE ~ "neznámý",
        is.infinite(STAV_IND) == TRUE ~ "neznámý",
        HOD_IND == " " ~ "neznámý",
        ID_IND == "STA_HABPOKRYV" ~ "neznámý",
        is.na(HOD_IND) == TRUE & ID_IND != "CELKOVE_HODNOCENI" ~ "neznámý",
        STAV_IND == 0 ~ "špatný",
        STAV_IND == 0.5 ~ "zhoršený",
        STAV_IND == 1 ~ "dobrý",
        STAV_IND == "0" ~ "špatný",
        STAV_IND == "0.5" ~ "zhoršený",
        STAV_IND == "1" ~ "dobrý",
        TRUE ~ as.character(STAV_IND)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::arrange(DRUH, kod_chu, POLE) %>%
    dplyr::filter(is.na(ROK) == FALSE & ROK != "NA")
  
  return(n2k_druhy_chu_final)
  
}

#----------------------------------------------------------#
# Napocet a temp zapis ---- 
#----------------------------------------------------------#

#species_list <- unique(subset(n2k_load, SKUPINA == "Obojživelníci")$DRUH)
#species_list <- unique(n2k_load$DRUH)
species_list <- c("Pulsatilla patens", "Bombina variegata", "Osmoderma barnabita", "Lampetra planeri")
#species_list <- "Bombina variegata"

n2k_druhy_uzemi <- 
  lapply(species_list, function(sp) {
  run_n2k_druhy_uzemi(
    n2k_druhy_lok, sp, sites_subjects, limity, biotop_evd, 
    current_year = 2024)
    }
  ) %>%
  dplyr::bind_rows()

readr::write_csv(
  n2k_druhy_uzemi,
  paste0("Data/Temp/n2k_druhy_uzemi", ".csv")
)


n2k_druhy_chu_write <-
  n2k_druhy_uzemi %>%
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
  dplyr::left_join(.,
                   rp_code,
                   by = join_by(
                     "kod_chu"
                   )
  ) %>%
  dplyr::left_join(
    ., 
    n2k_oop,
    by = c(
      "kod_chu" = "SITECODE"
    )
  ) %>%
  dplyr::mutate(
    typ_predmetu_hodnoceni = "Druh",
    feature_code = NA,
    trend = "neznámý",
    datum_hodnoceni = Sys.Date()
  ) %>%
  dplyr::filter(
    CILMON_CHU == 1
  ) %>%
  dplyr::rename(
    nazev_chu = NAZEV, 
    druh = DRUH, #feature_code, 
    hodnocene_obdobi_od = HODNOCENE_OBDOBI_OD, 
    hodnocene_obdobi_do = HODNOCENE_OBDOBI_DO, 
    parametr_nazev = ID_IND, 
    parametr_hodnota = HOD_IND, 
    parametr_limit = LIM_IND, 
    parametr_jednotka = JEDNOTKA, 
    stav = STAV_IND
  ) %>%
  dplyr::select(
    typ_predmetu_hodnoceni, 
    kod_chu, 
    nazev_chu, 
    druh, 
    feature_code, 
    hodnocene_obdobi_od, 
    hodnocene_obdobi_do, 
    oop, 
    parametr_nazev, 
    parametr_hodnota, 
    parametr_limit, 
    parametr_jednotka, 
    stav, 
    trend, 
    datum_hodnoceni, 
    pracoviste, 
    ID_ND_AKCE
  ) %>%
  dplyr::left_join(
    ., 
    indikatory_id, 
    by = c("parametr_nazev" = "ind_r")
  ) %>%
  dplyr::mutate(
    parametr_nazev = ind_id,
    pracoviste = gsub(
      ",",
      "",
      pracoviste
    ),
    metodika = 15087
  ) %>%
  # navazani pouze na predmety ochrany, dropnuti nepredmetu
  dplyr::right_join(
    .,
    sites_subjects %>%
      dplyr::select(
        site_code,
        nazev_lat
      ),
    by = c(
      "kod_chu" = "site_code",
      "druh" = "nazev_lat"
    )
  ) %>%
  dplyr::select(
    -c(
      ind_id,
      ind_popis, 
      #oop,
      ID_ND_AKCE
    )
  ) %>%
  dplyr::distinct()

#----------------------------------------------------------#
# Zapis dat -----
#----------------------------------------------------------#
  
chu_export <-
  function() {
    
    # oddelovace a enkodovani pro import do ISOP
    sep_isop <- ";"
    quote_env_isop <- FALSE
    encoding_isop <- "UTF-8"
    
    # oddelovace a enkodovani pro import do MS Excel
    sep <- ","
    quote_env <- TRUE
    encoding <- "Windows-1250"
    
    write.table(
      n2k_druhy_chu,
      paste0("Outputs/Data/druhy/",
             "n2k_druhy_chu",
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
      n2k_druhy_chu,
      paste0("Outputs/Data/druhy/",
             "n2k_druhy_chu",
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
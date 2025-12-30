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
    limity, # Ponechano pro budouci pouziti
    current_year = 2024
) {
  
  #----------------------------------------------------------#
  # 1. Validace a priprava dat ----
  #----------------------------------------------------------#
  
  # Kontrola konzistence metadat (Bad Groups)
  bad_groups <- n2k_druhy_lim %>%
    dplyr::filter(DRUH == species_name) %>% 
    dplyr::group_by(kod_chu, KOD_LOKAL, POLE, ROK, ID_IND) %>%
    dplyr::summarise(
      n_meta = dplyr::n_distinct(paste(TYP_IND, KLIC, UROVEN)),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_meta > 1) %>%
    dplyr::pull(ID_IND) %>%
    unique()
  
  if (length(bad_groups) > 0) {
    warning(glue::glue("Druh {species_name}: Indikatory s nekonzistentnimi metadaty: {paste(bad_groups, collapse = ', ')}"))
  }
  
  # Identifikace skupiny druhu
  skupina_druhu <- n2k_druhy_lim %>% 
    dplyr::filter(DRUH == species_name) %>% 
    dplyr::pull(SKUPINA) %>% 
    unique() %>% 
    stats::na.omit() %>% 
    head(1)
  
  pole_skupiny <- c("Brouci", "Motýli", "Vážky", "Rovnokřídlí")
  
  # Zjisteni, zda se ma filtrovat podle POLE (transekt/plocha)
  is_pole_druh <- species_name %in% sites_subjects$DRUH[sites_subjects$SKUPINA %in% pole_skupiny]
  
  #----------------------------------------------------------#
  # 2. Agregace dilcich indikatoru ----
  #----------------------------------------------------------#
  
  n2k_druhy_lim_post <- n2k_druhy_lim %>%
    dplyr::filter(DRUH == species_name) %>%
    dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE, ROK, ID_IND) %>%
    dplyr::reframe(
      # Metadata
      SKUPINA = dplyr::first(SKUPINA),
      NAZEV_LOK = paste(unique(LOKALITA), collapse = ", "),
      ID_ND_AKCE = paste(unique(IDX_ND_AKCE), collapse = ", "),
      DATUM = max(DATUM, na.rm = TRUE),
      CILMON = max(CILMON, na.rm = TRUE),
      # Atributy indikatoru
      TYP_IND = dplyr::first(TYP_IND),
      KLIC = dplyr::first(KLIC),
      UROVEN = dplyr::first(UROVEN),
      IND_GRP = dplyr::first(IND_GRP),
      JEDNOTKA = dplyr::first(JEDNOTKA),
      # Limity
      LIM_IND     = dplyr::first(stats::na.omit(unique(LIM_IND))),
      LIM_INDLIST = dplyr::first(stats::na.omit(unique(LIM_INDLIST))),
      # Vytahneme originalni hodnotu. Pokud je jich vice (aggregate), spojime je, 
      # ale pro POP_ indikatory je to typicky jedna hodnota.
      HOD_IND_VAL = dplyr::first(stats::na.omit(HOD_IND)),
      # Vypocet hodnoty (STAV_IND) dle typu (minmax vs val)
      STAV_IND_RAW = dplyr::case_when(
        IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        # U POP_ (populace) bereme maximum, pokud to neni poskozeni (POP_POSK)
        IND_GRP == "minmax" & grepl("POP_", ID_IND) & !grepl("POP_POSK", ID_IND) ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "minmax" ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::mutate(
      STAV_IND = ifelse(is.infinite(STAV_IND_RAW), NA, STAV_IND_RAW),
      HOD_IND_TEXT = dplyr::case_when(
        is.na(HOD_IND_VAL) ~ "neznámý",
        TRUE ~ as.character(HOD_IND_VAL)
      )
    ) %>%
    dplyr::ungroup()
  
  #----------------------------------------------------------#
  # 3. Vypocet CELKOVE_HODNOCENI ----
  #----------------------------------------------------------#
  
  # Nastaveni seskupovacich promennych
  group_vars <- c("kod_chu", "DRUH", "KOD_LOKAL", "ROK")
  
  if(!is_pole_druh) {
    # Pokud neni druh vazany na pole, sloucime vizualne POLE, ale grupujeme bez nej
    n2k_druhy_lim_post <- n2k_druhy_lim_post %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::mutate(POLE = paste(unique(POLE), collapse = ",")) %>%
      dplyr::ungroup()
  } else {
    group_vars <- c(group_vars, "POLE")
  }
  
  # Vytvoreni hodnotici tabulky
  n2k_eval <- n2k_druhy_lim_post %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      # Pocet OCEKAVANYCH indikatoru (maji definovany limit)
      # Do jmenovatele vstupuji jen ty, ktere maji !is.na(LIM_IND)
      N_KEY_EXPECTED = sum(KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND), na.rm = TRUE),
      N_OTH_EXPECTED = sum(KLIC == "ne" & UROVEN == "lok" & !is.na(LIM_IND), na.rm = TRUE),
      # Pocet SPLNENYCH indikatoru (STAV_IND == 1)
      N_KEY_PASSED = sum(KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND) & STAV_IND == 1, na.rm = TRUE),
      N_OTH_PASSED = sum(KLIC == "ne" & UROVEN == "lok" & !is.na(LIM_IND) & STAV_IND == 1, na.rm = TRUE),
      # Metadata pro razeni
      MAX_CILMON = max(CILMON, na.rm = TRUE),
      MAX_DATUM  = max(DATUM, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # --- Logika vyhodnoceni ---
      # Pocet selhani v ostatnich indikatorech
      N_OTH_FAIL = N_OTH_EXPECTED - N_OTH_PASSED,
      CELKOVE = dplyr::case_when(
        # Pokud neni znam cil monitoringu, nelze hodnotit
        is.na(MAX_CILMON) ~ NA_real_,
        # 1. Klicove indikatory: Musi byt splneny vsechny
        N_KEY_EXPECTED > 0 & N_KEY_PASSED < N_KEY_EXPECTED ~ 0,
        # 2. Ostatni indikatory: Tolerance selhani
        # Vice nez 1 selhani -> Spatny (0)
        N_OTH_FAIL > 1 ~ 0,
        # Prave 1 selhani -> Zhorseny (0.5) (Pravidlo "-1 is OK")
        N_OTH_FAIL == 1 ~ 0.5,
        # 0 selhani -> Dobry (1)
        TRUE ~ 1
      )
    )
  
  #----------------------------------------------------------#
  # 4. Vyber reprezentativni navstevy (ID_AKCE) ----
  #----------------------------------------------------------#
  
  best_visits <- n2k_eval %>%
    dplyr::group_by(kod_chu, DRUH, KOD_LOKAL) %>% 
    dplyr::arrange(
      dplyr::desc(MAX_CILMON),
      dplyr::desc(ROK),
      dplyr::desc(MAX_DATUM),
      dplyr::desc(CELKOVE) 
    ) %>%
    dplyr::slice(1) %>%
    dplyr::select(kod_chu, DRUH, KOD_LOKAL, ROK, BEST_POLE = dplyr::any_of("POLE"), WINNING_CELKOVE = CELKOVE)
  
  #----------------------------------------------------------#
  # 5. Finalni slozeni vystupu ----
  #----------------------------------------------------------#
  
  # Filtrace puvodnich dat na vitezne navstevy
  result <- n2k_druhy_lim_post %>%
    dplyr::inner_join(best_visits, by = c("kod_chu", "DRUH", "KOD_LOKAL", "ROK")) 
  
  # Pokud se resi pole, dofiltrujeme konkretni pole
  if(is_pole_druh) {
    result <- result %>% dplyr::filter(POLE == BEST_POLE)
  }
  
  # Doplneni radku s celkovym hodnocenim
  final_rows <- result %>%
    dplyr::mutate(
      STAV_IND = dplyr::case_when(
        ID_IND == "CELKOVE_HODNOCENI" ~ WINNING_CELKOVE,
        TRUE ~ STAV_IND
      ),
      HOD_IND = dplyr::case_when(
        ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0    ~ "špatný",
        ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 0.5 ~ "zhoršený",
        ID_IND == "CELKOVE_HODNOCENI" & STAV_IND == 1    ~ "dobrý",
        TRUE ~ as.character(HOD_IND_TEXT)
      )
    ) %>%
    # Odstraneni pomocnych sloupcu
    dplyr::select(-WINNING_CELKOVE, -dplyr::any_of("BEST_POLE"), -STAV_IND_RAW, -HOD_IND_TEXT, -HOD_IND_VAL) %>%
    dplyr::arrange(kod_chu, KOD_LOKAL, ID_IND)
  
  return(final_rows)
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
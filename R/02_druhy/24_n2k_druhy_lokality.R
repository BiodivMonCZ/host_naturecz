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
      
      # --- FIX 3: ZACHOVANI VSECH LIMITU (nikoli jen first) ---
      LIM_IND = paste(unique(stats::na.omit(LIM_IND)), collapse = ", "),
      LIM_INDLIST = paste(unique(stats::na.omit(LIM_INDLIST)), collapse = ", "),
      
      # Vytahneme originalni hodnotu.
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
    # --- FIX 1: ODSTRANENI DUPLICIT PRO NE-POLE DRUHY ---
    # Pokud neni druh vazany na pole, sloucime vizualne POLE a ponechame 
    # pro kazdy indikator (ID_IND) jen jeden radek.
    n2k_druhy_lim_post <- n2k_druhy_lim_post %>%
      # Musime grupovat i podle ID_IND, abychom neztratili ruzne indikatory
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "ID_IND")))) %>%
      dplyr::mutate(POLE = paste(unique(POLE), collapse = ", ")) %>%
      dplyr::slice(1) %>% # Ponecha jen jeden radek pro danou kombinaci (Lokalita+Rok+Indikator)
      dplyr::ungroup()
  } else {
    group_vars <- c(group_vars, "POLE")
  }
  
  # Vytvoreni hodnotici tabulky (zde jiz data neobsahuji duplicity transektu)
  n2k_eval <- n2k_druhy_lim_post %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      # Zde ponechavame n_distinct pro jistotu, ale diky FIX 1 by nyni fungoval i sum
      
      # Pocet OCEKAVANYCH indikatoru
      N_KEY_EXPECTED = dplyr::n_distinct(ID_IND[KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != ""]),
      N_OTH_EXPECTED = dplyr::n_distinct(ID_IND[KLIC == "ne" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != ""]),
      
      # Pocet SPLNENYCH indikatoru
      N_KEY_PASSED = dplyr::n_distinct(ID_IND[KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != "" & STAV_IND == 1]),
      N_OTH_PASSED = dplyr::n_distinct(ID_IND[KLIC == "ne" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != "" & STAV_IND == 1]),
      
      # Metadata pro razeni
      MAX_CILMON = max(CILMON, na.rm = TRUE),
      MAX_DATUM  = max(DATUM, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      N_OTH_FAIL = N_OTH_EXPECTED - N_OTH_PASSED,
      CELKOVE = dplyr::case_when(
        is.na(MAX_CILMON) ~ NA_real_,
        N_KEY_EXPECTED > 0 & N_KEY_PASSED < N_KEY_EXPECTED ~ 0,
        N_OTH_FAIL > 1 ~ 0,
        N_OTH_FAIL == 1 ~ 0.5,
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
  
  # A. Detailni radky (indikatory)
  result_details <- n2k_druhy_lim_post %>%
    dplyr::inner_join(best_visits, by = c("kod_chu", "DRUH", "KOD_LOKAL", "ROK")) 
  
  if(is_pole_druh) {
    result_details <- result_details %>% dplyr::filter(POLE == BEST_POLE)
  }
  
  # B. --- FIX 2: VYTVORENI RADKU CELKOVEHO HODNOCENI ---
  # Vytvorime novy dataframe pro souhrnne radky na zaklade viteznych navstev
  # Pouzijeme metadata z result_details (prvni radek skupiny), aby sedely sloupce jako Datum atd.
  
  result_summary <- result_details %>%
    dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, ROK) %>%
    dplyr::slice(1) %>% # Vezmeme "hlavicku" z prvniho indikatoru
    dplyr::ungroup() %>%
    dplyr::mutate(
      ID_IND = "CELKOVE_HODNOCENI",
      # Prepiseme hodnoty vysledkem z best_visits (WINNING_CELKOVE uz tam je diky joinu)
      STAV_IND = WINNING_CELKOVE,
      HOD_IND = dplyr::case_when(
        WINNING_CELKOVE == 0   ~ "špatný",
        WINNING_CELKOVE == 0.5 ~ "zhoršený",
        WINNING_CELKOVE == 1   ~ "dobrý",
        TRUE ~ "nehodnoceno"
      ),
      # Vycistime sloupce specificke pro dilci indikatory, aby to nepletlo
      TYP_IND = NA_character_,
      KLIC = NA_character_,
      UROVEN = "lok", # Celkove hodnoceni je vzdy za lokalitu
      LIM_IND = NA_character_,
      LIM_INDLIST = NA_character_,
      JEDNOTKA = NA_character_
    ) %>%
    # Odstranime pomocne sloupce pred spojenim
    dplyr::select(-WINNING_CELKOVE, -dplyr::any_of("BEST_POLE"), -STAV_IND_RAW, -HOD_IND_TEXT, -HOD_IND_VAL)
  
  # C. Spojeni detailu a souhrnu
  final_rows <- result_details %>%
    dplyr::select(-WINNING_CELKOVE, -dplyr::any_of("BEST_POLE"), -STAV_IND_RAW, -HOD_IND = HOD_IND_TEXT, -HOD_IND_VAL) %>%
    # Prejmenovani sloupce HOD_IND_TEXT na HOD_IND pro sjednoceni
    dplyr::bind_rows(result_summary) %>%
    dplyr::arrange(kod_chu, KOD_LOKAL, dplyr::desc(ID_IND == "CELKOVE_HODNOCENI"), ID_IND) %>%
    dplyr::distinct()
  
  return(final_rows)
}

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
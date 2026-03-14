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
  # Zistujeme, zda pro jeden identifikator ID_IND neexistuji rozporuplna metadata
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
  
  # Identifikace skupiny druhu (napr. Rostliny, Bezobratli)
  skupina_druhu <- n2k_druhy_lim %>% 
    dplyr::filter(DRUH == species_name) %>% 
    dplyr::pull(SKUPINA) %>% 
    unique() %>% 
    stats::na.omit() %>% 
    head(1)
  
  pole_skupiny <- c("Brouci", "Motýli", "Vážky", "Rovnokřídlí")
  
  # Zjisteni, zda se ma filtrovat podle POLE (zda je druh vazany na transekt nebo plochu)
  is_pole_druh <- species_name %in% sites_subjects$DRUH[sites_subjects$SKUPINA %in% pole_skupiny]
  if(species_name == "Eriogaster catax") {
    is_pole_druh == FALSE
  }
  
  #----------------------------------------------------------#
  # 2. Agregace dilcich indikatoru ----
  #----------------------------------------------------------#
  
  # Zde redukujeme data na uroven unikatniho indikatoru v ramci lokality a roku
  n2k_druhy_lim_post <- n2k_druhy_lim %>%
    dplyr::filter(DRUH == species_name) %>%
    dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE, ROK, ID_IND) %>%
    dplyr::reframe(
      # Metadata - zachovame prvni nalezene hodnoty
      SKUPINA = dplyr::first(SKUPINA),
      NAZEV_LOK = paste(unique(LOKALITA), collapse = ", "),
      ID_ND_AKCE = paste(unique(IDX_ND_AKCE), collapse = ", "),
      ID_ND_LOK = unique(IDX_ND_LOK)[1],
      DATUM = max(DATUM, na.rm = TRUE),
      CILMON = max(CILMON, na.rm = TRUE),
      # Atributy indikatoru
      TYP_IND = dplyr::first(TYP_IND),
      KLIC = dplyr::first(KLIC),
      UROVEN = dplyr::first(UROVEN),
      IND_GRP = dplyr::first(IND_GRP),
      JEDNOTKA = dplyr::first(JEDNOTKA),
      
      # Zde resime sloucenim vsech limitu do retezce, aby nedochazelo ke ztrate informaci
      LIM_IND = paste(unique(stats::na.omit(LIM_IND)), collapse = ", "),
      LIM_INDLIST = paste(unique(stats::na.omit(LIM_INDLIST)), collapse = ", "),
      
      # Vytahneme originalni hodnotu mereni
      HOD_IND_VAL = dplyr::first(stats::na.omit(HOD_IND)),
      
      # Vypocet ciselne hodnoty stavu (STAV_IND) dle typu vyhodnoceni (minmax vs val)
      STAV_IND_RAW = dplyr::case_when(
        IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        # U POP_ (populace) bereme maximum, pokud to neni poskozeni (POP_POSK)
        IND_GRP == "minmax" & grepl("POP_", ID_IND) & !grepl("POP_POSK", ID_IND) ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "minmax" ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::mutate(
      # Osetreni nekonecnych hodnot
      STAV_IND = ifelse(is.infinite(STAV_IND_RAW), NA, STAV_IND_RAW),
      # Priprava textove interpretace namerene hodnoty
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
    # Pokud druh neni vazany na pole, musime odstranit duplicity zpusobene vice transekty
    # Sloucime nazvy poli a ponechame jen jeden radek pro kazdy indikator
    n2k_druhy_lim_post <- n2k_druhy_lim_post %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "ID_IND")))) %>%
      dplyr::mutate(POLE = paste(unique(POLE), collapse = ", ")) %>%
      dplyr::slice(1) %>% # Ponecha jen jeden unikátni radek
      dplyr::ungroup()
  } else {
    # Pokud je druh vazany na pole, grupujeme i podle POLE
    group_vars <- c(group_vars, "POLE")
  }
  
  # Vytvoreni souhrnne hodnotici tabulky
  n2k_eval <- n2k_druhy_lim_post %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      # Pocet OCEKAVANYCH indikatoru (maji definovany limit a nejsou prazdne)
      N_KEY_EXPECTED = dplyr::n_distinct(ID_IND[KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != ""]),
      N_OTH_EXPECTED = dplyr::n_distinct(ID_IND[KLIC == "ne" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != ""]),
      
      # Pocet SPLNENYCH indikatoru (STAV_IND je 1)
      N_KEY_PASSED = dplyr::n_distinct(ID_IND[KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != "" & STAV_IND == 1]),
      N_OTH_PASSED = dplyr::n_distinct(ID_IND[KLIC == "ne" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != "" & STAV_IND == 1]),
      
      # Metadata pro razeni nejlepsi navstevy
      MAX_CILMON = max(CILMON, na.rm = TRUE),
      MAX_DATUM  = max(DATUM, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Logika semaforu pro celkove hodnoceni
      N_OTH_FAIL = N_OTH_EXPECTED - N_OTH_PASSED,
      CELKOVE = dplyr::case_when(
        is.na(MAX_CILMON) ~ NA_real_,
        # Klicove indikatory musi byt splneny vsechny
        N_KEY_EXPECTED > 0 & N_KEY_PASSED < N_KEY_EXPECTED ~ 0,
        # Ostatni indikatory - tolerance selhani
        N_OTH_FAIL > 1 ~ 0,   # Vice nez 1 chyba = Spatny
        N_OTH_FAIL == 1 ~ 0.5, # Prave 1 chyba = Zhorseny
        TRUE ~ 1               # Bez chyb = Dobry
      )
    )
  
  #----------------------------------------------------------#
  # 4. Vyber reprezentativni navstevy (ID_AKCE) ----
  #----------------------------------------------------------#
  
  # Vybereme jednu nejlepsi navstevu pro danou lokalitu a rok
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
  
  # A. Detailni radky (jednotlive indikatory)
  result_details <- n2k_druhy_lim_post %>%
    dplyr::inner_join(best_visits, by = c("kod_chu", "DRUH", "KOD_LOKAL", "ROK")) 
  
  # Pokud se resi konkretni pole, vyfiltrujeme jen vitezne pole
  if(is_pole_druh) {
    result_details <- result_details %>% dplyr::filter(POLE == BEST_POLE)
  }
  
  # B. Vytvoreni radku pro celkove hodnoceni
  # Tento radek chybi v puvodnich datech, musime ho vygenerovat
  result_summary <- result_details %>%
    dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, ROK) %>%
    dplyr::slice(1) %>% # Pouzijeme prvni radek jako sablonu pro metadata
    dplyr::ungroup() %>%
    dplyr::mutate(
      ID_IND = "CELKOVE_HODNOCENI",
      STAV_IND = WINNING_CELKOVE,
      # Prevod ciselneho skore na textove hodnoceni
      HOD_IND = dplyr::case_when(
        WINNING_CELKOVE == 0   ~ "špatný",
        WINNING_CELKOVE == 0.5 ~ "zhoršený",
        WINNING_CELKOVE == 1   ~ "dobrý",
        TRUE ~ "nehodnoceno"
      ),
      # Vycisteni sloupcu, ktere nedavaji smysl pro celkove hodnoceni
      TYP_IND = NA_character_,
      KLIC = NA_character_,
      UROVEN = "lok",
      LIM_IND = NA_character_,
      LIM_INDLIST = NA_character_,
      JEDNOTKA = NA_character_
    ) %>%
    dplyr::select(-WINNING_CELKOVE, -dplyr::any_of("BEST_POLE"), -STAV_IND_RAW, -HOD_IND_TEXT, -HOD_IND_VAL)
  
  # C. Spojeni detailu a celkoveho hodnoceni do finalni tabulky
  final_rows <- result_details %>%
    # Zde byla opravena chyba syntaxe: nejprve prejmenujeme, pak selektujeme
    dplyr::rename(HOD_IND = HOD_IND_TEXT) %>%
    dplyr::select(
      -WINNING_CELKOVE, 
      -dplyr::any_of("BEST_POLE"), 
      -STAV_IND_RAW, 
      -HOD_IND_VAL
    ) %>%
    dplyr::bind_rows(result_summary) %>%
    # Serazeni tak, aby celkove hodnoceni bylo na konci (nebo na zacatku dle preference, zde abecedne)
    dplyr::arrange(kod_chu, KOD_LOKAL, dplyr::desc(ID_IND == "CELKOVE_HODNOCENI"), ID_IND) %>%
    dplyr::distinct()
  
  return(final_rows)
}
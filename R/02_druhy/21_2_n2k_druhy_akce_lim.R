run_n2k_druhy_lim <- function(
    n2k_druhy,
    species_name,
    sites_subjects,
    limity,
    current_year = 2025
) {
  
  #----------------------------------------------------------#
  # 1. Prevod na long format a napojeni na limity ----- 
  #----------------------------------------------------------#
  
  n2k_druhy_long <- n2k_druhy %>%
    dplyr::mutate(
      # Prevedeme vsechny sloupce od POP_PRESENCE_N na text, abychom mohli pivotovat
      dplyr::across(
        .cols = ncol_orig:ncol(.),
        .fns = ~ as.character(.)
      )
    ) %>%
    # Pivotovani do dlouheho formatu (ID_IND = nazev indikatoru, HOD_IND = hodnota)
    tidyr::pivot_longer(
      .,
      cols = POP_PRESENCE_N:dplyr::last_col(),
      names_to = "ID_IND",
      values_to = "HOD_IND"
    ) %>%
    # Odstraneni nepotrebnych sloupcu metadat
    dplyr::select(
      -c(ZDROJ:PRESNOST)
    ) %>%
    # Pripojeni tabulky limitu (right_join zachova limity i bez dat)
    dplyr::right_join(
      .,
      limity %>%
        dplyr::filter(
          UROVEN == "lok" # Pouze limity pro lokalitu
        ) %>%
        dplyr::filter(
          is.na(LIM_IND) == FALSE # Pouze platne limity
        ),
      by = c("DRUH" = "DRUH",
             "ID_IND" = "ID_IND")
    ) %>%
    # IND_GRP: Sjednoceni typu limitu (min a max se radi do stejne skupiny "minmax")
    dplyr::mutate(
      IND_GRP = dplyr::case_when(
        TYP_IND %in% c("min", "max") ~ "minmax",
        TRUE ~ TYP_IND
      )
    ) 
  
  # ------------------------------------------#
  # 2. Porovnani s limity ----- 
  # ------------------------------------------#
  
  n2k_druhy_lim_pre <- n2k_druhy_long %>%
    dplyr::mutate(
      # 1. Orizneme mezery v hodnotach a limitech
      HOD_IND_trim = stringr::str_trim(HOD_IND),
      LIM_IND_trim = stringr::str_trim(LIM_IND),
      # 2. Regex detekce: Je to cislo? (Volitelne minus, cislice, volitelne tecka a cislice)
      # Pokud data obsahuji desetinnou carku misto tecky, regex vrati FALSE a vznikne NA (coz je pozadovane)
      is_num_hod = stringr::str_detect(HOD_IND_trim, "^-?\\d+(\\.\\d+)?$"),
      is_num_lim = stringr::str_detect(LIM_IND_trim, "^-?\\d+(\\.\\d+)?$"),
      # 3. Podmineny prevod na cisla
      HOD_IND_num = dplyr::if_else(is_num_hod, as.numeric(HOD_IND_trim), NA_real_),
      LIM_IND_num = dplyr::if_else(is_num_lim, as.numeric(LIM_IND_trim), NA_real_)
    ) %>%
    # Odstraneni pomocnych sloupcu pro cistotu
    dplyr::select(-c(HOD_IND_trim, LIM_IND_trim, is_num_hod, is_num_lim)) %>%
    dplyr::mutate(
      # STAV_IND: Vyhodnoceni splneni limitu (1 = splneno, 0 = nesplneno)
      STAV_IND = dplyr::case_when(
        # Pro MIN limit: Hodnota musi byt vetsi nebo rovna
        TYP_IND == "min" & HOD_IND_num < LIM_IND_num ~ 0,
        TYP_IND == "min" & HOD_IND_num >= LIM_IND_num ~ 1,
        # Pro MAX limit: Hodnota musi byt mensi nebo rovna
        TYP_IND == "max" & HOD_IND_num > LIM_IND_num ~ 0,
        TYP_IND == "max" & HOD_IND_num <= LIM_IND_num ~ 1,
        # Pro VAL limit (text): Hodnota se musi rovnat
        TYP_IND == "val" & HOD_IND != LIM_IND ~ 0,
        TYP_IND == "val" & HOD_IND == LIM_IND ~ 1
      )
    ) %>%
    dplyr::select(-c(HOD_IND_num, LIM_IND_num)) %>%
    # Agregace vice limitu pro jeden indikator
    dplyr::group_by(
      ID_ND_NALEZ, 
      ID_IND, 
      IND_GRP
    ) %>%
    dplyr::mutate(
      # is_POP: Indikator, zda se jedna o populacni parametr (zacina POP_)
      is_POP = stringr::str_starts(ID_IND, "POP_") 
    ) %>%
    # Agregace vice hodnot pro jeden indikator (min/max logika)
    dplyr::mutate(
      STAV_IND = dplyr::case_when(
        # Populacni (minmax): Logika OR (MAX) - staci splnit jeden limit
        IND_GRP == "minmax" & is_POP ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        # Ostatni (minmax): Logika AND (MIN) - musi splnit interval (vsechny limity)
        IND_GRP == "minmax" & !is_POP ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        # Kategoricke (val): Logika OR (MAX) - staci se trefit do jedne hodnoty
        IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE)
      )
    ) %>%
    dplyr::select(-is_POP) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Osetreni nekonecnych hodnot vzniklych agragaci prazdnych dat
      STAV_IND = dplyr::case_when(
        is.infinite(STAV_IND) ~ NA,
        TRUE ~ STAV_IND
      )
    ) %>%
    # Vyber nejlepsi varianty pro unikatnost v ramci nalezu (redukce radku)
    dplyr::group_by(
      ID_ND_NALEZ, 
      ID_IND
    ) %>%
    dplyr::arrange(dplyr::desc(STAV_IND)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # ------------------------------------------#
  # 3. Hodnoceni nalezu ----- 
  # ------------------------------------------#
  
  n2k_druhy_lim <- n2k_druhy_lim_pre %>%
    dplyr::group_by(ID_ND_NALEZ) %>%
    dplyr::mutate(
      # CELKOVE_SUM: Soucet vsech splnenych limitu pro nalez
      CELKOVE_SUM = as.character(
        sum(
          STAV_IND, 
          na.rm = TRUE)
      )
    ) %>%
    dplyr::select(-c(ID_IND:IND_GRP)) %>%
    # Pivotovani CELKOVE_SUM zpet do dlouheho formatu
    tidyr::pivot_longer(
      .,
      cols = ncol(.),
      names_to = "ID_IND",
      values_to = "HOD_IND"
    ) %>%
    dplyr::distinct() %>%
    # Spojeni s puvodnimi vysledky
    dplyr::bind_rows(
      ., 
      n2k_druhy_lim_pre
    ) %>%
    dplyr::arrange(ID_ND_NALEZ) %>%
    # Filtrace "sirotcich" limitu, ktere nemaji prirazeny nalez
    dplyr::filter(is.na(ID_ND_NALEZ) == FALSE)
  
  return(n2k_druhy_lim)
}
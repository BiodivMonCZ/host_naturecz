#----------------------------------------------------------#
# Nacteni napoctu z VMB -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Nacteni VMB2 -----
#--------------------------------------------------#
results <- 
  readr::read_csv2( # Nacteni CSV souboru s vysledky VMB2
  "Outputs/Data/stanoviste/results_habitats_A1_20250222.csv",
  locale = readr::locale(encoding = "Windows-1250"), # Nasteveni ceskeho kodovani
  col_types = cols( # Urceni formatu datumu
    DATE_MIN = readr::col_date(format = "%Y-%m-%d"),
    DATE_MAX = readr::col_date(format = "%Y-%m-%d"),
    DATE_MEAN = readr::col_date(format = "%Y-%m-%d"),
    DATE_MEDIAN = readr::col_date(format = "%Y-%m-%d")
  )
  ) %>%
  # Kontrola nacteni habitatu 91E0
  dplyr::mutate(
    HABITAT_CODE = dplyr::case_when(
      HABITAT_CODE == "91" ~ "91E0",
      TRUE ~ HABITAT_CODE
      )
    ) %>%
  # Nahrazeni NA v rozloze nulou, cisteni nazvu od ruznych pomlcek
  dplyr::mutate(
    ROZLOHA = replace_na(ROZLOHA, 0),
    #KVALITA = replace_na(KVALITA, 0),
    #KVALITA = round(KVALITA, 2),
    NAZEV = str_replace_all(
      NAZEV, 
      "–|—",
      "-"
      )
    ) %>%
  dplyr::distinct() # Odstraneni duplicitnich radku

#--------------------------------------------------#
## Nacteni aktualni VMBX -----
#--------------------------------------------------#
results_x <- 
  readr::read_csv2( # Nacteni novejsiho datasetu s vysledky VMBX
    "Outputs/Data/stanoviste/results_habitats_24_20250806.csv",
    locale = readr::locale(encoding = "Windows-1250"),
    col_types = cols(
      DATE_MIN = readr::col_date(format = "%Y-%m-%d"),
      DATE_MAX = readr::col_date(format = "%Y-%m-%d"),
      DATE_MEAN = readr::col_date(format = "%Y-%m-%d"),
      DATE_MEDIAN = readr::col_date(format = "%Y-%m-%d")
    )
    ) %>% 
  # Kontrola nacteni habitatu 91E0
  dplyr::mutate(
    HABITAT_CODE = dplyr::case_when(
      HABITAT_CODE == "91" ~ "91E0",
      TRUE ~ HABITAT_CODE
      )
    ) %>%
  # Nahrazeni NA v rozloze nulou a sjednoceni pomlcek
  dplyr::mutate(
    ROZLOHA = replace_na(ROZLOHA, 0),
    #KVALITA = replace_na(KVALITA, 0),
    #KVALITA = round(KVALITA, 2),
    NAZEV = str_replace_all(NAZEV, "–|—", "-")
    ) %>%
  dplyr::distinct() # Odstraneni duplicit

#--------------------------------------------------#
# Nacteni cilovych stavu ----
#--------------------------------------------------#
limity_stan <- 
  readr::read_csv( # Nacteni CSV s limity indikatoru
    "Data/Input/limity_stanoviste.csv",
    locale = readr::locale(encoding = "Windows-1250")
    ) %>% 
  dplyr::mutate(
    # Urceni posloupnosti ciloveho stavu
    rowname = as.numeric(
      rownames(.)
      ),
    # Kontrola nacteni habitatu 91E0
    HABITAT_CODE = dplyr::case_when(
      HABITAT_CODE == "91" ~ "91E0",
      HABITAT_CODE == "9.10E+01" ~ "91E0",
      HABITAT_CODE == "9,10E+01" ~ "91E0",
      TRUE ~ HABITAT_CODE
      )
    ) %>%
  dplyr::rowwise() %>%
  # Zaokrouhleni limitnich hodnot
  dplyr::mutate(
    LIM_IND = dplyr::case_when(
      ID_IND == "ROZLOHA" ~ safe_floor(LIM_IND, 2),
      ID_IND == "KVALITA" ~ ceiling(LIM_IND * 10) / 10
      ),
    # Sjednoceni zdrojovych oznaceni na jednotne verze
    ZDROJ = dplyr::case_when(
      ZDROJ == "AVMB2" ~ "VMB3",
      ZDROJ == "AVMB1" ~ "VMB2",
      ZDROJ == "AVMB" ~ "VMB2",
      grepl("SDF", ZDROJ) ~ "SDF",
      TRUE ~ ZDROJ
      )
    ) %>%
  dplyr::distinct() # Odstraneni duplicitnich zaznamu

kval <- 
  limity_stan %>%
  dplyr::filter(
    ID_IND == "KVALITA"
    ) %>%
  dplyr::group_by(
    SITECODE, 
    HABITAT_CODE
    ) %>%
  dplyr::arrange(
    rowname, 
    LIM_IND
    ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() 

rozl <- 
  limity_stan %>%
  dplyr::filter(
    ID_IND == "ROZLOHA"
    ) %>%
  dplyr::group_by(
    SITECODE, 
    HABITAT_CODE
    ) %>%
  dplyr::arrange(
    rowname,
    LIM_IND
    ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() 

#lim_new <- 
#  dplyr::bind_rows(
#    rozl, 
#    kval
#    )

#write.csv(
#  lim_new,
#  "limity_stanoviste_20250524.csv",
#  row.names = FALSE,
#  fileEncoding = "Windows-1250"
#  )

#--------------------------------------------------#
# Nacteni informaci o SDO II ----
#--------------------------------------------------#
evl_sdo <- evl %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(sites_subjects, by = c("SITECODE" = "site_code")) %>%
  dplyr::left_join(rp_code, by = c("SITECODE" = "kod_chu")) %>%
  dplyr::mutate(SDO2 = dplyr::case_when(SITECODE %in% sdo_II_sites$sitecode ~ 1,
                                        TRUE ~ 0),
                KRAJE2024 = dplyr::case_when(grepl("Karlo", oop) == TRUE ~ 1,
                                             grepl("Libereckého", oop) == TRUE ~ 1,
                                             grepl("Plz", oop) == TRUE ~ 1,
                                             grepl("Král", oop) == TRUE ~ 1,
                                             grepl("Pardu", oop) == TRUE ~ 1,
                                             TRUE ~ 0)) %>%
  rowwise() %>%
  dplyr::mutate(rev = sum(SDO2, KRAJE2024))

#----------------------------------------------------------#
# Prevod na long format -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Prevod na as.character() -----
## Nutnost pro prevedeni na long format a kombinaci ruznych sloupcu
#--------------------------------------------------#
results <- results  %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 4)),
    across(
      where(~ is.numeric(.x) && !inherits(.x, "Date")),
      as.character
    )
  )

results_x <- results_x %>%
  mutate(
    across( # Zaokrouhleni cisel na 4 desetinna mista
      where(is.numeric), ~ round(.x, 4)
      ), 
    across( # Prevod cisel na text
      where(~ is.numeric(.x) && !inherits(.x, "Date")), 
      as.character
    )
  )


#--------------------------------------------------#
## Long VMBX -----
#--------------------------------------------------#
results_long_x <- results_x %>%
  dplyr::mutate(
    CELKOVE_HODNOCENI = NA # Pridani sloupce pro celkove hodnoceni
    ) %>%
  dplyr::mutate(
    datum_hodnoceni_od = DATE_MIN, # Kopie nejstarsiho data mapovani
    datum_hodnoceni_do = DATE_MAX # Kopie nejmladsiho data mapovani
    ) %>%
  #dplyr::rename(parametr_nazev = PAR_NAZEV,
  #              parametr_hodnota = PAR_HODNOTA) %>%
  # Odstraneni puvodnich sloupcu s casovou informaci o mapavani
  dplyr::select(
    -c(
      DATE_MIN, 
      DATE_MAX, 
      DATE_MEAN, 
      DATE_MEDIAN
      )
    ) %>%
  # Prevod z wide do long formatu
  tidyr::pivot_longer(
    cols = c(4:(ncol(.)-2)),
    names_to = "parametr_nazev",
    values_to = "parametr_hodnota"
    ) 

#--------------------------------------------------#
## Long VMB2 -----
#--------------------------------------------------#
results_long <- results %>%
  dplyr::mutate(
    CELKOVE_HODNOCENI = NA # Pridani sloupce pro celkove hodnoceni
    ) %>%
  dplyr::mutate(
    datum_hodnoceni_od = DATE_MIN, # Kopie nejstarsiho data mapovani
    datum_hodnoceni_do = DATE_MAX # Kopie nejmladsiho data mapovani
    ) %>%
  #dplyr::rename(parametr_nazev = PAR_NAZEV,
  #              parametr_hodnota = PAR_HODNOTA) %>%
  dplyr::select(
    -c(
      DATE_MIN, 
      DATE_MAX, 
      DATE_MEAN, 
      DATE_MEDIAN
      )
    ) %>%
  tidyr::pivot_longer(
    cols = c(4:(ncol(.)-2)),
    names_to = "parametr_nazev",
    values_to = "parametr_hodnota"
    ) %>%
  dplyr::left_join(
    ., 
    cis_habitat, 
    by = c(
      "HABITAT_CODE" = "KOD_HABITAT"
      )
    ) %>%
  dplyr::left_join(
    ., 
    limity_stan, 
    by = c(
      "SITECODE" = "SITECODE",
      "HABITAT_CODE" = "HABITAT_CODE",
      "parametr_nazev" = "ID_IND"
      )
    ) %>%
  dplyr::left_join(
    ., 
    rp_code, 
    by = c(
      "SITECODE" = "kod_chu"
      )
    ) %>%
  dplyr::left_join(
    .,
    indikatory_id, 
    by = c(
      "parametr_nazev" = "ind_r"
      )
    ) %>%
  dplyr::left_join(
    ., 
    n2k_oop, 
    by = c(
      "SITECODE" = "SITECODE"
      )
    ) %>%
  dplyr::left_join(
    ., 
    minimisize, 
    by = c(
      "HABITAT_CODE" = "HABITAT"
      )
    ) %>%
  #dplyr::group_by(HABITAT_CODE, SITECODE, parametr_nazev) %>%
  rowwise() %>%
  dplyr::mutate(
    ZDROJ = dplyr::case_when(
      HABITAT_CODE %in% c("91T0", "3140", "3130", "8310") ~ NA,
      ZDROJ == "EXPERT" ~ ZDROJ,
      ZDROJ == "VMB3" ~ ZDROJ,
      SITECODE %in% sdo_II_sites$sitecode ~ ZDROJ,
      grepl("Karlo", oop) == TRUE ~ ZDROJ,
      grepl("Libereckého", oop) == TRUE & grepl("Správa KRNAP", oop) == FALSE ~ ZDROJ,
      grepl("Plz", oop) == TRUE & grepl("Správa NP", oop) == FALSE ~ ZDROJ,
      grepl("Král", oop) == TRUE ~ ZDROJ,
      grepl("Pardu", oop) == TRUE ~ ZDROJ,
      grepl("Jihočeského", oop) == TRUE & grepl("Správa NP", oop) == FALSE ~ ZDROJ,
      parametr_nazev == "KVALITA" & 
        ZDROJ %in% c("SDF", "VMB2", "VMB3") & 
        LIM_IND <= 2 ~ ZDROJ,
      parametr_nazev == "KVALITA" & 
        (is.na(LIM_IND) == TRUE | LIM_IND == "NA") &
        parametr_hodnota %>% as.numeric() <= 2 ~ "VMB2",
      parametr_nazev == "KVALITA" &
        (is.na(LIM_IND) == TRUE | LIM_IND == "NA") &
        parametr_hodnota %>% as.numeric() > 2 ~ "MINIMI",
      parametr_nazev == "KVALITA" &
        parametr_hodnota %>% as.numeric() > 2 ~ "MINIMI",
      parametr_nazev == "ROZLOHA" &
        ZDROJ == "SDF" &
        LIM_IND >= MINIMISIZE ~ ZDROJ,
      parametr_nazev == "ROZLOHA" &
        ZDROJ == "SDF" &
        LIM_IND < MINIMISIZE ~ "MINIMI",
      parametr_nazev == "ROZLOHA" &
        ZDROJ == "VMB2" &
        as.numeric(parametr_hodnota) < MINIMISIZE ~ "MINIMI",
      parametr_nazev == "ROZLOHA" &
        ZDROJ == "VMB2" &
        LIM_IND < MINIMISIZE ~ "MINIMI",
      parametr_nazev == "ROZLOHA" &
        ZDROJ == "VMB2" &
        as.numeric(parametr_hodnota) >= MINIMISIZE ~ ZDROJ,
      parametr_nazev == "ROZLOHA" &
        as.numeric(parametr_hodnota) < MINIMISIZE ~ "MINIMI",
      (is.na(ZDROJ)  == TRUE | ZDROJ == "NA") &
        as.numeric(parametr_hodnota) == 0 ~ "MINIMI",
      (is.na(ZDROJ) == TRUE | ZDROJ == "NA") &
        as.numeric(parametr_hodnota) == NA ~ "MINIMI",
      (is.na(ZDROJ) == TRUE | ZDROJ == "NA") &
        parametr_hodnota == "NA" ~ "MINIMI",
      parametr_nazev == "KVALITA" &
        (is.na(LIM_IND) == TRUE | LIM_IND == "NA") ~ "MINIMI",
      TRUE ~ ZDROJ
      )
    ) %>%
  dplyr::mutate(
    LIM_IND = dplyr::case_when(
      HABITAT_CODE %in% c("91T0", "3140", "3130", "8310") ~ NA,
      parametr_nazev == "ROZLOHA" &
        SITECODE == "CZ0514672" ~ safe_floor(parametr_hodnota, 2),
      ZDROJ == "EXPERT" ~ LIM_IND,
      parametr_nazev == "ROZLOHA" & 
        ZDROJ == "SDF" ~ safe_floor(parametr_hodnota, 2),
      parametr_nazev == "ROZLOHA" &
        ZDROJ == "VMB3" ~ LIM_IND,
      parametr_nazev == "ROZLOHA" &
        SITECODE %in% sdo_II_sites$sitecode ~ LIM_IND,
      (grepl("Karlo", oop) == TRUE |
         grepl("Libereckého", oop) == TRUE |
         grepl("Plz", oop) == TRUE |
         grepl("Král", oop) == TRUE |
         grepl("Pardu", oop) == TRUE
       ) &
        (grepl("Správa NP", oop) == FALSE &
           grepl("Správa KRNAP", oop) == FALSE) ~ LIM_IND,
      parametr_nazev == "ROZLOHA" &
        ZDROJ == "MINIMI" ~ MINIMISIZE,
      parametr_nazev == "ROZLOHA" &
        ZDROJ == "VMB2" ~ safe_floor(parametr_hodnota, 2),
      parametr_nazev == "KVALITA" &
        is.na(LIM_IND) == TRUE &
        parametr_hodnota <= 2 ~ ceiling(as.numeric(parametr_hodnota) * 10) / 10,
      parametr_nazev == "KVALITA" &
        ZDROJ == "MINIMI" ~ 2,
      parametr_nazev == "KVALITA" &
        (is.na(LIM_IND) == TRUE | LIM_IND == "NA") ~ 2,
      TRUE ~ LIM_IND
      )
    ) %>%
  dplyr::mutate(
    stav = dplyr::case_when(
      is.na(LIM_IND) == TRUE ~ "nehodnocen",
      parametr_nazev == "KVALITA" & is.na(parametr_hodnota) ~ "špatný",
      parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota) < LIM_IND ~ "špatný",
      parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota) >= LIM_IND ~ "dobrý",
      parametr_nazev == "KVALITA" & as.numeric(parametr_hodnota) > LIM_IND ~ "špatný",
      parametr_nazev == "KVALITA" & as.numeric(parametr_hodnota) <= LIM_IND ~ "dobrý"
      ),
    stav_toler = dplyr::case_when(
      is.na(LIM_IND) == TRUE ~ "nehodnocen",
      parametr_nazev == "KVALITA" & is.na(parametr_hodnota) ~ "špatný",
      parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota) == 0 ~ "špatný",
      parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota)*1.05 < LIM_IND ~ "špatný",
      parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota)*1.05 >= LIM_IND ~ "dobrý",
      parametr_nazev == "KVALITA" & as.numeric(parametr_hodnota)*0.95 > LIM_IND ~ "špatný",
      parametr_nazev == "KVALITA" & as.numeric(parametr_hodnota)*0.95 <= LIM_IND ~ "dobrý"
      ),
    parametr_jednotka = dplyr::case_when(
      parametr_nazev == "ROZLOHA" ~ "hektary", 
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
      parametr_nazev == "EXPANSIVE_LIST" ~ "seznam expanzivních druhů"
      )
    ) %>%
  dplyr::group_by(
    SITECODE, 
    HABITAT_CODE
    ) %>%
  dplyr::mutate(
    stav_count = dplyr::case_when(
      stav == "dobrý" ~ 1
      ),
    stav_count_toler = dplyr::case_when(
      stav_toler == "dobrý" ~ 1
      ),
    glob_stav = case_when(
      HABITAT_CODE %in% c("91T0", "3140", "3130", "8310") ~ "nehodnocen",
      sum(stav_count, na.rm = TRUE) == 0 ~ "špatný",
      sum(stav_count, na.rm = TRUE) == 1 ~ "zhoršený",
      sum(stav_count, na.rm = TRUE) == 2 ~ "dobrý"
      ),
    glob_stav_toler = case_when(
      HABITAT_CODE %in% c("91T0", "3140", "3130", "8310") ~ "nehodnocen",
      sum(stav_count_toler, na.rm = TRUE) == 0 ~ "špatný",
      sum(stav_count_toler, na.rm = TRUE) == 1 ~ "zhoršený",
      sum(stav_count_toler, na.rm = TRUE) == 2 ~ "dobrý"
      ),
    stav = dplyr::case_when(
      parametr_nazev == "CELKOVE_HODNOCENI" ~ unique(glob_stav),
      TRUE ~ stav
      )
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    #parametr_nazev = par_nazev,
    typ_predmetu_hodnoceni = "Stanoviště",
    trend = "neznámý",
    datum_hodnoceni = paste0(Sys.Date())
    ) %>%
  dplyr::rename(
    kod_chu = SITECODE, 
    nazev_chu = NAZEV, 
    druh = NAZEV_HABITAT, 
    feature_code = HABITAT_CODE, 
    hodnocene_obdobi_od = datum_hodnoceni_od, 
    hodnocene_obdobi_do = datum_hodnoceni_do,
    parametr_limit = LIM_IND,
    poznamka = ZDROJ
    ) %>%
    dplyr::select(
      typ_predmetu_hodnoceni, 
      kod_chu, 
      nazev_chu, 
      druh, 
      feature_code, 
      hodnocene_obdobi_od, 
      hodnocene_obdobi_do, 
      parametr_nazev, 
      parametr_hodnota, 
      parametr_limit, 
      parametr_jednotka, 
      stav, 
      trend, 
      datum_hodnoceni, 
      oop, 
      pracoviste, 
      poznamka
      ) %>%
  #dplyr::left_join(., indikatory[,c(1:2)], by = c("par_nazev" = "ID")) %>%
  dplyr::mutate(
    SDO2 = dplyr::case_when(
      kod_chu %in% sdo_II_sites$sitecode ~ 1,
      TRUE ~ 0
      ),
    KRAJE2024 = dplyr::case_when(
      grepl("Správa NP", oop) == TRUE ~ 0,
      grepl("Správa KRNAP", oop) == TRUE ~ 0,
      grepl("Karlo", oop) == TRUE ~ 1,
      grepl("Libereckého", oop) == TRUE ~ 1,
      grepl("Plz", oop) == TRUE ~ 1,
      grepl("Král", oop) == TRUE ~ 1,
      grepl("Pardu", oop) == TRUE ~ 1,
      TRUE ~ 0)
    ) %>%
  dplyr::mutate(
    nazev_chu = str_replace_all(nazev_chu, "–|—", "-")
    ) %>%
  dplyr::distinct()

#--------------------------------------------------#
# Srovnani VMBX a VMB2 -----
#--------------------------------------------------#
results_comp <- 
  results_long %>%
  # 1) Pripojeni dat z VMBX (results_long_x) k datovemu ramci z VMB2 (results_long)
  #    - join dle kodu EVL (kod_chu -> SITECODE), kodu habitat (feature_code -> HABITAT_CODE)
  #      a nazvu parametru (parametr_nazev)
  #    - tecka '.' v left_join oznacuje, ze se leva tabulka bere jako aktualni data (results_long)
  dplyr::left_join(
    .,
    results_long_x,
    by = c(
      "kod_chu" = "SITECODE",
      "feature_code" = "HABITAT_CODE",
      "parametr_nazev"
      )
    ) %>%
  # 2) rowwise() zajisti, ze dalsi operace v mutate budou provadeny radek po radku,
  #    coz je dulezite pro pripady, kdy v mutates pouzivame funkce citlive na jednotlive raadky
  dplyr::rowwise() %>%
  # 3) prevod sloupcu s hodnotami na cisla pro porovnani a vypocet trendu
  #    - vytvori se dva sloupce: parametr_hodnota.xnum (hodnota z VMB2) a
  #      parametr_hodnota.ynum (hodnota z VMBX)
  #    - as.numeric prevede NA nebo texty na NA_real_ pokud prevod neni mozne
  mutate(
    parametr_hodnota.xnum = as.numeric(parametr_hodnota.x),
    parametr_hodnota.ynum = as.numeric(parametr_hodnota.y)
    ) %>%
  # 4) urceni stavu (stav) na urovni jednotlivych radku podle pravidel
  #    - pokud neexistuje limit (parametr_limit je NA) => "nehodnocen"
  #    - pro indikator KVALITA: pokud neni hodnota v Y (VMBX) => "spatny"
  #    - pro ROZLOHA porovnani cisel vynuti "spatny" nebo "dobry" podle limitu
  #    - pro KVALITA porovnani numericke hodnoty s limitem (vyska kvality)
  #    POZOR: poradi vetvi v case_when je dulezite — prvni splnena veta se pouzije.
  dplyr::mutate(
    stav = dplyr::case_when(
      is.na(parametr_limit) == TRUE ~ "nehodnocen",
      parametr_nazev == "KVALITA" & is.na(parametr_hodnota.ynum) ~ "špatný",
      parametr_nazev == "ROZLOHA" & parametr_hodnota.ynum < parametr_limit ~ "špatný",
      parametr_nazev == "ROZLOHA" & parametr_hodnota.ynum >= parametr_limit ~ "dobrý",
      parametr_nazev == "KVALITA" & parametr_hodnota.ynum > parametr_limit ~ "špatný",
      parametr_nazev == "KVALITA" & parametr_hodnota.ynum <= parametr_limit ~ "dobrý"
      )
    ) %>%
  # 5) Skupinove zpracovani po kod_chu a feature_code (tj. po EVL a konkr. predmetu/habitatu)
  dplyr::group_by(
    kod_chu, 
    feature_code
  ) %>%
  dplyr::mutate(
    # 5a) stav_count: pomocna promenna pro spocitani, zda dany radek prispi jako "dobry" (1) nebo ne (NA)
    stav_count = dplyr::case_when(
      stav == "dobrý" ~ 1
    ),
    # 5b) glob_stav: agregovane celkove hodnoceni pro dany predmet (habitat) v ramci konkretniho EVL
    #     - pokud je druh specificky nevhodny pro hodnoceni (feature_code v uvedenem seznamu) => "nehodnocen"
    #     - jinak se spocita soucet 'dobrych' parametru:
    #         0 => "spatny", 1 => "zhorseny", 2 => "dobry"
    #     - pouziva se sum(..., na.rm = TRUE), aby NA v stav_count nezkreslily soucet
    glob_stav = case_when(
      feature_code %in% c("91T0", "3140", "3130", "8310") ~ "nehodnocen",
      sum(stav_count, na.rm = TRUE) == 0 ~ "špatný",
      sum(stav_count, na.rm = TRUE) == 1 ~ "zhoršený",
      sum(stav_count, na.rm = TRUE) == 2 ~ "dobrý"
    ),
    # 5c) pro radky, ktere reprezentuji 'CELKOVE_HODNOCENI', nahradime hodnotu 'stav' agregovanym glob_stav
    #     - unique(glob_stav) vraci jedinecny prvek glob_stav v ramci skupiny (ocekavane jednouchy prvek)
    #     - jinak zustane povodni stav
    stav = dplyr::case_when(
      parametr_nazev == "CELKOVE_HODNOCENI" ~ unique(glob_stav),
      TRUE ~ stav
    )
  ) %>%
  # 6) ukonceni grupovani, vratime se k rizenemu (nekontrolovanemu) rezimu
  dplyr::ungroup() %>%
  # 7) urceni trendu mezi predchozim (x) a aktualnim (y) merenim
  #    - nejprve se kontroluje mnozstvi mozných scenaru pro kategorie ('CELKOVE_HODNOCENI'):
  #        * pokud textove hodnoty odpovidaji, => "stabilni"
  #        * nasledne jsou vzdy vyhodnoceny vety typu "zhoršujici se" / "zlepsujici se"
  #          vychodzici z porovnani kategorii "dobry", "zhoršený", "špatný"
  #    - pokud neni CELKOVE_HODNOCENI, porovnavaji se cisla (xnum, ynum):
  #        * presna shoda => "stabilni"
  #        * pokud se y lezi v rozsahu +-5% od x => "stabilni" (tolerancni pasmo)
  #        * pro hodnoty KVALITA je zvyseni nad +5% interpretovano jako "zhoršujici se"
  #          (pozor: logika je zavisla na tom, jake znamena smer zmeny pro dany indikator)
  #        * analogicky pro ROZLOHA: pokles pod -5% => "zhoršující se"; vzrust nad +5% => "zlepšující se"
  #    - POZOR NA: poradi vetvi je dulezite, prvni splnena veta je pouzita.
  dplyr::mutate(
    trend = dplyr::case_when(
      parametr_hodnota.y == parametr_hodnota.x ~ "stabilní",
      parametr_nazev == "CELKOVE_HODNOCENI" &
        parametr_hodnota.y %in% c("zhoršený", "špatný") &
        parametr_hodnota.x == "dobrý" ~ "zhoršující se",
      parametr_nazev == "CELKOVE_HODNOCENI" &
        parametr_hodnota.y == "špatný" &
        parametr_hodnota.x %in% c("dobrý", "zhoršený") ~ "zhoršující se",
      parametr_nazev == "CELKOVE_HODNOCENI" &
        parametr_hodnota.y %in% c("dobrý", "zhoršený") &
        parametr_hodnota.x == "špatný" ~ "zlepšující se",
      parametr_nazev == "CELKOVE_HODNOCENI" &
        parametr_hodnota.y == "dobrý" &
        parametr_hodnota.x %in% c("zhoršený", "špatný") ~ "zlepšující se",
      parametr_hodnota.ynum == parametr_hodnota.xnum ~ "stabilní",
      parametr_hodnota.ynum <= parametr_hodnota.xnum*1.05 &
        parametr_hodnota.ynum >= parametr_hodnota.xnum*0.95 ~ "stabilní",
      parametr_nazev == "KVALITA" & 
        parametr_hodnota.ynum > parametr_hodnota.xnum*1.05 ~ "zhoršující se",
      parametr_nazev == "KVALITA" & 
        parametr_hodnota.ynum < parametr_hodnota.xnum*0.95 ~ "zlepšující se",
      parametr_nazev == "ROZLOHA" & 
        parametr_hodnota.ynum < parametr_hodnota.xnum*0.95 ~ "zhoršující se",
      parametr_nazev == "ROZLOHA" & 
        parametr_hodnota.ynum > parametr_hodnota.xnum*1.05 ~ "zlepšující se")
    ) %>%
  # 8) do sloupce parametr_hodnota ulozime ciselnou verzi z VMBX (ynum),
  #    timto se sjednoti typ sloupce pro nasledne exporty
  dplyr::mutate(
    parametr_hodnota = parametr_hodnota.ynum
  ) %>%
  # 9) Formatovani datovych sloupcu s daty hodnoceni do standardniho retezce "YYYY-MM-DD"
  #    - pouziva se lubridate::ymd pro spolehlivy prevod (i z faktor/text), nasledne base::format
  dplyr::mutate(
    datum_hodnoceni_od = base::format(lubridate::ymd(datum_hodnoceni_od), "%Y-%m-%d"),
    datum_hodnoceni_do = base::format(lubridate::ymd(datum_hodnoceni_do), "%Y-%m-%d")
  ) %>%
  # 10) Vyber finalnich sloupcu pro dalsi zpracovani / export
  dplyr::select(
    typ_predmetu_hodnoceni,
    kod_chu,
    nazev_chu,
    druh,
    feature_code,
    datum_hodnoceni_od,
    datum_hodnoceni_do,
    parametr_nazev,
    parametr_hodnota,
    parametr_limit,
    parametr_jednotka,
    stav,
    trend,
    datum_hodnoceni,
    oop,
    pracoviste,
    poznamka,
    ) %>%
  # 11) Pripojeni popisu indikatoru z tabulky indikatory_id
  #     - pripojujeme podle parametru (parametr_nazev == ind_r)
  dplyr::left_join(
    .,
    indikatory_id %>%
      dplyr::select(
        ind_r,
        ind_popis,
        ind_id
      ),
    by = c("parametr_nazev" = "ind_r")
  ) %>%
  # 12) Kodovani slovnich hodnot 'stav' a 'trend' na ciselne kody pro systemovy export
  #     - nastaveni hodnot podle domluvenych ciselnych kodu (napr. 11 = dobry)
  #     - take se mapuji jednotky na ciselne kody pro vystup (napr. "ha" -> "7")
  dplyr::mutate(
    stav = dplyr::case_when(
      stav == "dobrý" ~ 11,
      stav == "zhoršený" ~ 12,
      stav == "špatný" ~ 13,
      stav == "neznámý" ~ 1,
      stav == "nehodnocen" ~ 8,
      ),
    trend = dplyr::case_when(
      trend == "zhoršující se" ~ 4,
      trend == "zlepšující se" ~ 2,
      trend == "stabilní" ~ 3,
      trend == "neznámý" ~ 1,
    ),
    parametr_jednotka = dplyr::case_when(
      parametr_jednotka == "ha" ~ "7",
      parametr_jednotka == "kvalita" ~ "140",
      parametr_jednotka == "mrtvé dřevo" ~ "140",
      parametr_jednotka == "typické druhy" ~ "140",
      TRUE ~ parametr_jednotka
    )
  )

nerealne <- results_long %>%
  dplyr::filter(
    parametr_nazev == "ROZLOHA"
    ) %>%
  dplyr::group_by(
    kod_chu
    ) %>%
  dplyr::reframe(
    SUMA_LIMIT = sum(
      parametr_limit, 
      na.rm = TRUE
      )
    ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    ., 
    evl %>%
      sf::st_drop_geometry() %>% 
      dplyr::mutate(
        rozloha_evl = SHAPE.AREA/10000
        ) %>%
      dplyr::select(
        SITECODE, 
        NAZEV, 
        rozloha_evl, 
        oop
        ),
    by = c("kod_chu" = "SITECODE")
    ) %>%
  dplyr::mutate(
    NEREALNE_CILE = dplyr::case_when(
      SUMA_LIMIT > rozloha_evl ~ 1,
      TRUE ~ 0
      )
    ) %>%
  arrange(-NEREALNE_CILE)

# Zapis vysledku ----
hab_export <-
  function() {
    
    n2k_stanoviste_write <-
      results_comp %>%
      dplyr::mutate(
        parametr_nazev = ind_id,
        feature_code = as.character(feature_code)
      ) %>%
      dplyr::select(-ind_popis, -ind_id) %>%
      dplyr::filter(!is.na(parametr_nazev)) %>%
      mutate(
        parametr_hodnota = ifelse(
          !is.na(parametr_hodnota),
          gsub("\\.", ",", as.character(parametr_hodnota)),
          NA_character_
        ),
        `Poznámka` = NA_character_
      )
    
    ind_order_xlsx <- c("celkové hodnocení", "rozloha", "kvalita")
    
    export_data_xlsx <- 
      n2k_stanoviste_write %>%
      dplyr::rename(
        `kód EVL` = kod_chu,
        `název EVL` = nazev_chu,
        `typ předmětu hodnocení` = typ_predmetu_hodnoceni,
        `předmět hodnocení` = druh,
        `kód předmětu hodn.` = feature_code,
        `počátek hodnoceného období` = datum_hodnoceni_od,
        `konec hodnoceného období` = datum_hodnoceni_do,
        `indikátor` = parametr_nazev,
        `hodnota` = parametr_hodnota,
        `limit` = parametr_limit,
        `jednotka` = parametr_jednotka,
        `datum hodnocení` = datum_hodnoceni,
        `OOP` = oop,
        `pracoviště AOPK` = pracoviste,
        `Způsob určení limitu` = poznamka
      ) %>%
      dplyr::mutate(
        ind_order_xlsx = match(`indikátor`, ind_order_xlsx, nomatch = length(ind_order_xlsx) + 1)
      ) %>%
      dplyr::arrange(`kód předmětu hodn.`, `název EVL`, ind_order_xlsx) %>%
      dplyr::select(-ind_order_xlsx) %>%   # pomocný sloupec odstranit
      dplyr::filter(`indikátor` %in% ind_order_xlsx)
    
    sep_isop <- ";"
    quote_env_isop <- FALSE
    encoding_isop <- "UTF-8"
    
    sep <- ","
    quote_env <- TRUE
    encoding <- "Windows-1250"
    
    
    # --- export .xlsx verze ---
    openxlsx::write.xlsx(
      export_data_xlsx,   # nebo export_data, pokud chceš i ostatní indikátory
      file = paste0(
        "Outputs/Data/stanoviste/stanoviste_",
        gsub("-", "", Sys.Date()),
        ".xlsx"
      )
    )
    
    cat("Export kompletní: ", " .xlsx soubor vytvořen\n")
    
    # --- export Windows-1250 verze ---
    write.table(
      n2k_stanoviste_write,
      paste0("Outputs/Data/stanoviste/",
             "n2k_stanoviste",
             "_",
             current_year,
             "_",
             gsub("-", "", Sys.Date()),
             "_",
             encoding,
             ".csv"
      ),
      row.names = FALSE,
      sep = sep,
      quote = quote_env,
      fileEncoding = encoding
    )  
    
    cat("Export kompletní: ", " Windows-1250 soubor vytvořen\n")
    
    # --- export UTF-8 verze po částech ---
    chunk_size <- 10000
    num_chunks <- ceiling(nrow(n2k_stanoviste_write) / chunk_size)
    
    for (i in seq_len(num_chunks)) {
      start_row <- ((i - 1) * chunk_size) + 1
      end_row <- min(i * chunk_size, nrow(n2k_stanoviste_write))
      
      chunk <- n2k_stanoviste_write[start_row:end_row, ]
      
      file_name <- paste0(
        "Outputs/Data/stanoviste/",
        "n2k_stanoviste",
        "_",
        current_year,
        "_",
        gsub("-", "", Sys.Date()),
        "_",
        encoding_isop,
        "_part",
        i,
        ".csv"
      )
      
      write.table(
        chunk,
        file_name,
        row.names = FALSE,
        sep = sep_isop,
        quote = quote_env_isop,
        fileEncoding = encoding_isop
      )
    }
    
    cat("Export kompletní: ", num_chunks, " UTF-8 soubory vytvořeny\n")
    
  }

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
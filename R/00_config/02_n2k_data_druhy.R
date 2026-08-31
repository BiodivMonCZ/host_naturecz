#----------------------------------------------------------#
# ZDROJOVA DATA A CISELNIKY - DRUHY -----
#----------------------------------------------------------#
# Vse, co potrebuje vetev hodnoceni druhu (R/02_druhy) a co nepotrebuji
# stanoviste: cilove stavy populaci, ciselniky druhu a export z NDOP,
# ze ktereho vznika 'n2k_load'.
#
# Vytvarene objekty: cilove_stavy, CIS_CILMON, cis_pocet_kat,
# cis_ryby_delky, biotop_evd, n2k_export, volna_export, ncol_orig, n2k_load.

#----------------------------------------------------------#
# Zavislost na obecnem configu ----
#----------------------------------------------------------#
# Datovy skript stoji na objektech z 00_n2k_config.R (limity, ciselniky,
# vrstvy EVL/PO, slozka_lokal, pomocne funkce). Pokud config jeste nebezel,
# spusti se automaticky; uz nactena data se znovu necetou.

if (!exists("limity")) {
  source("R/00_config/00_n2k_config.R", encoding = "UTF-8")
}

#------------------------------------------#
### Cilove stavy populaci pro uzemi (SDO) ----
#------------------------------------------#
# Druhy indikator Tabulky 2 metodiky obojzivelniku: "pocet jedincu (klouzavy
# prumer za posledni 3 roky)" se porovnava s cilovym stavem pro dane uzemi.
# Zdroj: BiodivMonCZ/digitalizaceSDO, Outputs/Data/sdo_cilove_druhy.csv
# (staženo 2026-08-20), sloupec navrzena_hodnota.
#
# Snapshot je ZAMERNE vendorovan do Data/Input/ - hodnoceni musi byt
# reprodukovatelne a nesmi zaviset na siti ani na cizim 'main'.
#
# Soubor je v UTF-8, ale ceske textove sloupce jsou uz ze zdroje poskozene
# na U+FFFD. Sloupce pouzivane nize (sitecode, nazev_lat, navrzena_hodnota,
# varovani) jsou ASCII a nedotcene - na poskozenych sloupcich nestavime nic.
cilove_stavy <- readr::read_csv(
  "Data/Input/sdo_cilove_druhy_20260820.csv",
  locale = readr::locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  dplyr::mutate(
    # Lissotriton montandoni je v SDO veden pod synonymem Triturus montandoni
    # (sdf_code 2001). Bez tohoto mapovani by se join TISE minul a druh by
    # zustal bez cilove hodnoty.
    DRUH = dplyr::case_when(
      nazev_lat == "Triturus montandoni" ~ "Lissotriton montandoni",
      TRUE ~ nazev_lat
    )
  ) %>%
  dplyr::filter(!is.na(navrzena_hodnota)) %>%
  dplyr::select(
    kod_chu = sitecode,
    DRUH,
    POP_CILSTAV = navrzena_hodnota,
    CIL_VAROVANI = varovani
  ) %>%
  # Tyz SDO byval nacten dvakrat pod dvema variantami nazvu zdrojoveho PDF
  # (s diakritikou a bez ni). U 171 ze 174 dvojic sitecode x druh je hodnota
  # shodna, u 3 se lisi - bereme NIZSI hodnotu, tedy mirnejsi cil, aby
  # nejednoznacnost zdroje nesla k tizi hodnoceneho uzemi.
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::summarise(
    POP_CILSTAV = min(POP_CILSTAV, na.rm = TRUE),
    CIL_VAROVANI = any(CIL_VAROVANI, na.rm = TRUE),
    .groups = "drop"
  )

#--------------------------------------------------#
## Ciselniky - druhy ---- 
#--------------------------------------------------#
#------------------------------------------#
### Zdroj cileného monitoringu ---- 
#------------------------------------------#
CIS_CILMON <- readr::read_csv(
  "Data/Input/cil_mon_zdroj.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#------------------------------------------#
### Ciselnik poctu navazanych na relativni kategorii pocetnost ---- 
#------------------------------------------#
cis_pocet_kat <- readr::read_csv(
  "Data/Input/cis_pocet_kat.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#------------------------------------------#
### Ciselnik kategorii delkovych struktur ryb a mihuli ---- 
#------------------------------------------#
cis_ryby_delky <- readr::read_csv(
  "Data/Input/cis_ryby_delky_strukt.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#--------------------------------------------------#
### Ciselnik biotopu EVD hmyzu ---- 
#--------------------------------------------------#
biotop_evd <- readr::read_csv(
  "Data/Input/biotopy_evd_hmyz.csv"
)

#------------------------------------------------------#
## Zdrojova data - export z NDOP ----
# export obsahuje data o vyskytu citlivych druhu: 
# kompletni pouze pro overene uzivatele,
# bez vyskytu citlivych druhu na vyzadani na jonas.gaigr@aopk.gov.cz
#------------------------------------------------------#
n2k_export <- readr::read_csv(
  paste0(
    slozka_lokal,
    "export_data_evl.csv"
  ), 
  locale = readr::locale(encoding = "UTF-8")
)

volna_export <- readr::read_csv(
  paste0(
    slozka_lokal,
    "export_data_zprap.csv"
  ), 
  locale = readr::locale(encoding = "UTF-8")
)

ncol_orig <- ncol(n2k_export)

n2k_load <- n2k_export %>%
  dplyr::bind_rows(
    .,
    volna_export
  ) %>%
  dplyr::distinct() %>%
  dplyr::rename(
    POLE = POLE_1_RAD
  ) %>% 
  dplyr::mutate(
    # Převedení DRUHu na kategorickou veličinu
    DRUH = as.factor(DRUH),
    # Převedení datumu do vhodného formátu
    DATUM = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    # Redukce data na den
    DEN = as.numeric(substring(DATUM_OD, 1, 2)),
    # Redukce data na měsíc
    MESIC = as.numeric(substring(DATUM_OD, 4, 5)),
    # Redukce data na rok
    ROK = as.numeric(
      substring(
        DATUM_OD, 
        7, 
        11
      )
    ),
    # Izolace kódu EVL
    kod_chu = substr(
      EVL, 
      1, 
      9
    ),
    # Izolace názvu lokality
    nazev_chu = substr(
      as.character(
        EVL
      ),
      12,
      nchar(
        as.character(
          EVL
        )
      )
    ),
    KOD_LOKRYB = readr::parse_character(
      stringr::str_extract(
        STRUKT_POZN,
        "(?<=<naz_tok>).*(?=</naz_tok>)"
      )
    ),
    KOD_LOKAL = dplyr::case_when(
      SKUPINA == "Letouni" ~ LOKALITA,
      SKUPINA == "Mechy" ~ LOKALITA,
      SKUPINA == "Motýli" ~ substring(
        KOD_LOKALITY, 
        1, 
        10
      ),
      SKUPINA == "Ryby a mihule" &
        is.na(KOD_LOKRYB) == FALSE 
      ~ KOD_LOKRYB,
      KOD_LOKALITY == "amp216" ~ "CZ0723412",
      KOD_LOKALITY == "amp222" ~ "CZ0724089_9",
      KOD_LOKALITY == "amp231" ~ "CZ0724089_19",
      KOD_LOKALITY == "amp185" ~ "CZ0623345",
      KOD_LOKALITY == "amp101" ~ "CZ0423006",
      KOD_LOKALITY == "amp71" ~ "CZ0323158",
      KOD_LOKALITY == "amp59" ~ "CZ0323144",
      KOD_LOKALITY == "amp15" ~ "CZ0213790",
      KOD_LOKALITY == "amp102" ~ "CZ0423215",
      KOD_LOKALITY == "amp254" ~ "CZ0813455",
      KOD_LOKALITY == "amp227" ~ "CZ0723410",
      KOD_LOKALITY == "amp336 (CZ_5)" ~ "CZ0714073_5",
      KOD_LOKALITY == "amp205 (CZ_3)" ~ "CZ0714073_3",
      KOD_LOKALITY == "amp337" ~ "CZ0713383",
      KOD_LOKALITY == "amp129" ~ "CZ0523011",
      KOD_LOKALITY == "amp334 (CZ_3)" ~ "CZ0523010_3",
      KOD_LOKALITY == "amp138 (CZ_2)" ~ "CZ0523010_2",
      KOD_LOKALITY == "amp335 (cz_1)" ~ "CZ0523010_1",
      KOD_LOKALITY == "amp102" ~ "CZ0423215",
      KOD_LOKALITY == "amp116" ~ "CZ0513249",
      KOD_LOKALITY == "amp101" ~ "CZ0423006",
      KOD_LOKALITY == "amp15" ~ "CZ0213790",
      KOD_LOKALITY == "amp30" ~ "CZ0213077",
      KOD_LOKALITY == "amp314 (cz_2)" ~ "CZ0213064_2",
      KOD_LOKALITY == "amp316 (cz_3)" ~ "CZ0213064_3",
      KOD_LOKALITY == "amp315 (cz_1)" ~ "CZ0213064_1",
      KOD_LOKALITY == "CZ0213008" ~ "CZ0213008_1",
      KOD_LOKALITY == "amp244" ~ "CZ0813457",
      KOD_LOKALITY == "amp207" ~ "CZ0713385",
      KOD_LOKALITY == "amp64" ~ "CZ0323143",
      KOD_LOKALITY == "amp24" ~ "CZ0213787",
      KOD_LOKALITY == "amp279" ~ "CZ0613335_03",
      KOD_LOKALITY == "amp110" ~ "CZ0513244",
      KOD_LOKALITY == "amp339" ~ "CZ0213066_2",
      KOD_LOKALITY == "amp340" ~ "CZ0213066_1",
      KOD_LOKALITY == "amp27" ~ "CZ0213058",
      KOD_LOKALITY == "amp226" ~ "CZ0724429_3",
      KOD_LOKALITY %in% c("amp211", "amp211" , "amp211", "amp107", "amp99",
                          "amp53", "amp252", "amp281", "amp280", "amp22",
                          "amp81", "amp25", "CZ0813450", "CZ0713397") ~ NA_character_,
      kod_chu == "CZ0623367" ~ "CZ0623367",
      is.na(KOD_LOKALITY) == TRUE & SKUPINA != "Cévnaté rostliny" ~ kod_chu,
      TRUE ~ NA_character_)
  ) %>%
  dplyr::mutate(
    KOD_LOKAL = dplyr::case_when(
      is.na(KOD_LOKAL) == TRUE ~ KOD_LOKALITY,
      TRUE ~ KOD_LOKAL
    )
  ) %>%
  dplyr::select(
    -KOD_LOKRYB
  )%>%
  dplyr::mutate(
    KOD_LOKAL = dplyr::case_when(
      is.na(KOD_LOKAL) == TRUE ~ LOKALITA,
      TRUE ~ KOD_LOKAL
    )
  ) %>%
  # identifikace dat cileneho monitoringu
  dplyr::mutate(
    CILMON = dplyr::case_when(
      ZDROJ %in% CIS_CILMON ~ 1,
      PROJEKT == "Monitoring druhů ČR" ~ 1,
      DRUH %in% c(
        "Carabus menetriesi pacholei", 
        "Bolbelasmus unicornis"
      ) 
      ~ 1,
      TRUE ~ 0)
  ) 
#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

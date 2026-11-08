#----------------------------------------------------------#
# Srovnani vysledku obou druhu JOINEM PRES NAZVY INDIKATORU -
#----------------------------------------------------------#
# Vstup: mezivysledky kaskady v Data/Temp/ (beh Test/01_run_test_2sp.R)
#   n2k_druhy_lok.csv  - uroven dilci plochy (DP), Tabulka 1 metodiky
#   n2k_druhy_chu.csv  - uroven uzemi (EVL),      Tabulka 2 metodiky
#
# Vystup: slozka z promenne prostredi SROV_OUT, jinak bezneho behu
#   srovnani_indikatoru_DP.csv   - FULL JOIN pres ID_IND, uroven DP
#   srovnani_indikatoru_EVL.csv  - FULL JOIN pres ID_IND, uroven EVL
#   verdikty.csv                 - rozdeleni CELKOVE_HODNOCENI
#   limity_srovnani.csv          - definice limitu obou druhu vedle sebe
#
# Join je ZAMERNE full (dplyr::full_join): indikator, ktery ma jen jeden
# z druhu, musi byt videt - prave to je casta chyba v tabulce limitu.
#----------------------------------------------------------#

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

SP <- c(TC = "Triturus cristatus", BV = "Bombina variegata")
OUT <- Sys.getenv("SROV_OUT", unset = "Outputs/Test/TC_BV_20260909/srovnani")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

#----------------------------------------------------------#
# Ciselnik popisu indikatoru ----
#----------------------------------------------------------#
ind_popis <- readr::read_csv(
  "Data/Input/cis_indikatory_popis.csv",
  locale = readr::locale(encoding = "Windows-1250"),
  show_col_types = FALSE
) %>%
  dplyr::select(ID_IND = ind_r, POPIS = ind_popis) %>%
  dplyr::filter(!is.na(ID_IND)) %>%
  dplyr::distinct(ID_IND, .keep_all = TRUE)

#----------------------------------------------------------#
# Definice limitu obou druhu vedle sebe ----
#----------------------------------------------------------#
limity <- readr::read_csv(
  "Data/Input/limity_vse.csv",
  locale = readr::locale(encoding = "Windows-1250"),
  show_col_types = FALSE
)

lim_sp <- function(sp) {
  limity %>%
    dplyr::filter(DRUH == sp) %>%
    dplyr::group_by(ID_IND, UROVEN) %>%
    dplyr::summarise(
      TYP  = paste(sort(unique(na.omit(TYP_IND))), collapse = " | "),
      LIM  = paste(sort(unique(na.omit(as.character(LIM_IND)))), collapse = " | "),
      JEDN = paste(sort(unique(na.omit(JEDNOTKA))), collapse = " | "),
      KLIC = paste(sort(unique(na.omit(KLIC))), collapse = " | "),
      .groups = "drop"
    )
}

limity_srovnani <- dplyr::full_join(
  lim_sp(SP[["TC"]]) %>% dplyr::rename_with(~paste0("TC_", .x), -c(ID_IND, UROVEN)),
  lim_sp(SP[["BV"]]) %>% dplyr::rename_with(~paste0("BV_", .x), -c(ID_IND, UROVEN)),
  by = c("ID_IND", "UROVEN")
) %>%
  dplyr::left_join(ind_popis, by = "ID_IND") %>%
  dplyr::mutate(
    JEN_U = dplyr::case_when(
      is.na(TC_TYP) ~ "jen Bombina variegata",
      is.na(BV_TYP) ~ "jen Triturus cristatus",
      TRUE ~ ""
    ),
    LIMIT_SHODNY = dplyr::if_else(
      !is.na(TC_LIM) & !is.na(BV_LIM) & TC_LIM == BV_LIM, "ano", "NE"
    )
  ) %>%
  dplyr::select(ID_IND, POPIS, UROVEN, JEN_U, LIMIT_SHODNY,
                dplyr::starts_with("TC_"), dplyr::starts_with("BV_")) %>%
  dplyr::arrange(UROVEN, ID_IND)

readr::write_excel_csv(limity_srovnani, file.path(OUT, "limity_srovnani.csv"))

#----------------------------------------------------------#
# Uroven DP (Tabulka 1) ----
#----------------------------------------------------------#
lok <- readr::read_csv("Data/Temp/n2k_druhy_lok.csv", show_col_types = FALSE)

# STAV_IND je na urovni DP ciselny (0 / 0.5 / 1); CELKOVE_HODNOCENI nese
# slovni verdikt ve sloupci HOD_IND.
dp_sp <- function(sp) {
  lok %>%
    dplyr::filter(DRUH == sp) %>%
    dplyr::mutate(ST = suppressWarnings(as.numeric(STAV_IND))) %>%
    dplyr::group_by(ID_IND) %>%
    dplyr::summarise(
      ZAZNAMU    = dplyr::n(),
      DP         = dplyr::n_distinct(KOD_LOKAL),
      EVL        = dplyr::n_distinct(kod_chu),
      # kolikrat ma indikator vubec zmerenou hodnotu
      S_HODNOTOU = sum(!is.na(HOD_IND)),
      SPLNENO    = sum(!is.na(ST) & ST == 1),
      CASTECNE   = sum(!is.na(ST) & ST == 0.5),
      NESPLNENO  = sum(!is.na(ST) & ST == 0),
      NEHODNOCEN = sum(is.na(ST)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      PODIL_SPLNENO = ifelse(
        SPLNENO + CASTECNE + NESPLNENO > 0,
        round(100 * SPLNENO / (SPLNENO + CASTECNE + NESPLNENO), 1),
        NA_real_
      )
    )
}

srovnani_DP <- dplyr::full_join(
  dp_sp(SP[["TC"]]) %>% dplyr::rename_with(~paste0("TC_", .x), -ID_IND),
  dp_sp(SP[["BV"]]) %>% dplyr::rename_with(~paste0("BV_", .x), -ID_IND),
  by = "ID_IND"
) %>%
  dplyr::left_join(ind_popis, by = "ID_IND") %>%
  dplyr::left_join(
    limity_srovnani %>%
      dplyr::filter(UROVEN == "lok") %>%
      dplyr::select(ID_IND, LIM_TC = TC_LIM, KLIC_TC = TC_KLIC,
                    LIM_BV = BV_LIM, KLIC_BV = BV_KLIC),
    by = "ID_IND"
  ) %>%
  dplyr::mutate(
    JEN_U = dplyr::case_when(
      is.na(TC_ZAZNAMU) ~ "jen Bombina variegata",
      is.na(BV_ZAZNAMU) ~ "jen Triturus cristatus",
      TRUE ~ ""
    )
  ) %>%
  dplyr::select(
    ID_IND, POPIS, JEN_U,
    LIM_TC, LIM_BV, KLIC_TC, KLIC_BV,
    dplyr::starts_with("TC_"), dplyr::starts_with("BV_")
  ) %>%
  dplyr::arrange(dplyr::desc(ID_IND == "CELKOVE_HODNOCENI"), ID_IND)

readr::write_excel_csv(srovnani_DP, file.path(OUT, "srovnani_indikatoru_DP.csv"))

#----------------------------------------------------------#
# Uroven EVL (Tabulka 2) ----
#----------------------------------------------------------#
chu <- readr::read_csv("Data/Temp/n2k_druhy_chu.csv", show_col_types = FALSE)

# Na urovni EVL je STAV_IND uz slovni.
evl_sp <- function(sp) {
  chu %>%
    dplyr::filter(DRUH == sp) %>%
    dplyr::group_by(ID_IND) %>%
    dplyr::summarise(
      ZAZNAMU    = dplyr::n(),
      EVL        = dplyr::n_distinct(kod_chu),
      S_HODNOTOU = sum(!is.na(HOD_IND)),
      DOBRY      = sum(STAV_IND == "dobrý", na.rm = TRUE),
      ZHORSENY   = sum(STAV_IND == "zhoršený", na.rm = TRUE),
      SPATNY     = sum(STAV_IND == "špatný", na.rm = TRUE),
      NEZNAMY    = sum(STAV_IND == "neznámý", na.rm = TRUE),
      NEHODNOCEN = sum(STAV_IND == "nehodnocen", na.rm = TRUE),
      .groups = "drop"
    )
}

srovnani_EVL <- dplyr::full_join(
  evl_sp(SP[["TC"]]) %>% dplyr::rename_with(~paste0("TC_", .x), -ID_IND),
  evl_sp(SP[["BV"]]) %>% dplyr::rename_with(~paste0("BV_", .x), -ID_IND),
  by = "ID_IND"
) %>%
  dplyr::left_join(ind_popis, by = "ID_IND") %>%
  dplyr::left_join(
    limity_srovnani %>%
      dplyr::filter(UROVEN == "chu") %>%
      dplyr::select(ID_IND, LIM_TC = TC_LIM, LIM_BV = BV_LIM),
    by = "ID_IND"
  ) %>%
  dplyr::mutate(
    JEN_U = dplyr::case_when(
      is.na(TC_ZAZNAMU) ~ "jen Bombina variegata",
      is.na(BV_ZAZNAMU) ~ "jen Triturus cristatus",
      TRUE ~ ""
    )
  ) %>%
  dplyr::select(
    ID_IND, POPIS, JEN_U, LIM_TC, LIM_BV,
    dplyr::starts_with("TC_"), dplyr::starts_with("BV_")
  ) %>%
  dplyr::arrange(dplyr::desc(ID_IND == "CELKOVE_HODNOCENI"), ID_IND)

readr::write_excel_csv(srovnani_EVL, file.path(OUT, "srovnani_indikatoru_EVL.csv"))

#----------------------------------------------------------#
# Verdikty ----
#----------------------------------------------------------#
verdikt_dp <- lok %>%
  dplyr::filter(ID_IND == "CELKOVE_HODNOCENI") %>%
  dplyr::distinct(DRUH, kod_chu, KOD_LOKAL, ROK, HOD_IND) %>%
  dplyr::count(DRUH, HOD_IND, name = "POCET") %>%
  dplyr::mutate(UROVEN = "DP")

# Na urovni EVL nese verdikt STAV_IND, ne HOD_IND: pro neznamy stav zustava
# HOD_IND = NA (25_n2k_druhy_uzemi.R, blok kolem r. 509), takze pocitani pres
# HOD_IND by 'neznámý' vykazalo jako NA.
verdikt_evl <- chu %>%
  dplyr::filter(ID_IND == "CELKOVE_HODNOCENI") %>%
  dplyr::distinct(DRUH, kod_chu, STAV_IND) %>%
  dplyr::count(DRUH, HOD_IND = STAV_IND, name = "POCET") %>%
  dplyr::mutate(UROVEN = "EVL")

verdikty <- dplyr::bind_rows(verdikt_dp, verdikt_evl) %>%
  dplyr::select(UROVEN, DRUH, VERDIKT = HOD_IND, POCET) %>%
  dplyr::arrange(UROVEN, DRUH, VERDIKT)

readr::write_excel_csv(verdikty, file.path(OUT, "verdikty.csv"))

#----------------------------------------------------------#
# Vypis do konzole ----
#----------------------------------------------------------#
cat("\n=========== VERDIKTY ===========\n")
print(as.data.frame(verdikty))

cat("\n=========== UROVEN DP: indikatory ===========\n")
print(as.data.frame(srovnani_DP %>% dplyr::select(
  ID_IND, JEN_U, LIM_TC, LIM_BV,
  TC_DP, TC_SPLNENO, TC_CASTECNE, TC_NESPLNENO, TC_NEHODNOCEN,
  BV_DP, BV_SPLNENO, BV_CASTECNE, BV_NESPLNENO, BV_NEHODNOCEN
)))

cat("\n=========== UROVEN EVL: indikatory ===========\n")
print(as.data.frame(srovnani_EVL %>% dplyr::select(
  ID_IND, JEN_U, LIM_TC, LIM_BV,
  TC_EVL, TC_DOBRY, TC_ZHORSENY, TC_SPATNY, TC_NEZNAMY, TC_NEHODNOCEN,
  BV_EVL, BV_DOBRY, BV_ZHORSENY, BV_SPATNY, BV_NEZNAMY, BV_NEHODNOCEN
)))

cat("\n=========== LIMITY: rozdily mezi druhy ===========\n")
print(as.data.frame(limity_srovnani %>%
  dplyr::filter(JEN_U != "" | LIMIT_SHODNY == "NE") %>%
  dplyr::select(ID_IND, UROVEN, JEN_U, TC_TYP, TC_LIM, BV_TYP, BV_LIM)))

cat("\n=== HOTOVO ===\n")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

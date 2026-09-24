#----------------------------------------------------------#
# Analyza pricin: neznamy stav a selhani POP_POCETPRUM3 ----
#----------------------------------------------------------#
# Vstup: mezivysledky behu 2026-09-09 v Data/Temp/
# Vystup: Outputs/Test/TC_BV_20260909/analyza/
#----------------------------------------------------------#

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

OUT <- "Outputs/Test/TC_BV_20260909/analyza"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

lok <- readr::read_csv("Data/Temp/n2k_druhy_lok.csv", show_col_types = FALSE)
chu <- readr::read_csv("Data/Temp/n2k_druhy_chu.csv", show_col_types = FALSE)

#==========================================================#
# A. PRICINY "neznamy" NA UROVNI EVL
#==========================================================#
# Tabulka 2 vraci NA (-> "neznamy") vyhradne tehdy, kdyz je NA indikator
# LOK_PROCDOBR (25_n2k_druhy_uzemi.R, blok 5b: STAV_PROCDOBR je NA => CELKOVE
# spadne do `TRUE ~ NA_real_`). LOK_PROCDOBR je NA, kdyz LOK_POCETSUM == 0,
# tedy kdyz v uzemi NENI ANI JEDNA DP s verdiktem z CILENEHO MONITORINGU
# (podminka `CILMON == 1` v definici LOK_POCETSUM).

verdikt <- chu %>%
  dplyr::filter(ID_IND == "CELKOVE_HODNOCENI") %>%
  dplyr::distinct(kod_chu, DRUH, VERDIKT = STAV_IND)

# Hodnoty obou citatelu Tabulky 2 za uzemi
dilc <- chu %>%
  dplyr::filter(ID_IND %in% c("LOK_DILCPOCET", "LOK_DILCDOBRE", "LOK_PROCDOBR")) %>%
  dplyr::select(kod_chu, DRUH, ID_IND, HOD_IND) %>%
  tidyr::pivot_wider(names_from = ID_IND, values_from = HOD_IND) %>%
  dplyr::mutate(dplyr::across(c(LOK_DILCPOCET, LOK_DILCDOBRE, LOK_PROCDOBR),
                              ~ suppressWarnings(as.numeric(.x))))

# Co je v uzemi k dispozici na urovni DP
dp_stat <- lok %>%
  dplyr::filter(ID_IND == "CELKOVE_HODNOCENI") %>%
  dplyr::distinct(kod_chu, DRUH, KOD_LOKAL, ROK, CILMON, HOD_IND) %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::summarise(
    DP_CELKEM    = dplyr::n_distinct(KOD_LOKAL),
    DP_CILMON1   = dplyr::n_distinct(KOD_LOKAL[CILMON == 1]),
    DP_CILMON0   = dplyr::n_distinct(KOD_LOKAL[CILMON == 0]),
    RADKU_CILMON1 = sum(CILMON == 1),
    .groups = "drop"
  )

neznamy <- verdikt %>%
  dplyr::left_join(dilc, by = c("kod_chu", "DRUH")) %>%
  dplyr::left_join(dp_stat, by = c("kod_chu", "DRUH")) %>%
  dplyr::mutate(
    DP_CELKEM  = dplyr::coalesce(DP_CELKEM, 0L),
    DP_CILMON1 = dplyr::coalesce(DP_CILMON1, 0L),
    DP_CILMON0 = dplyr::coalesce(DP_CILMON0, 0L),
    PRICINA = dplyr::case_when(
      VERDIKT != "neznámý"                 ~ "-- hodnoceno --",
      DP_CELKEM == 0                       ~ "A. zadna DP s verdiktem v uzemi",
      DP_CILMON1 == 0 & DP_CILMON0 > 0     ~ "B. DP existuji, ale zadna z cileneho monitoringu (CILMON=0)",
      DP_CILMON1 > 0                       ~ "C. DP z cileneho monitoringu existuji - necekane!",
      TRUE                                 ~ "D. jine"
    )
  )

cat("\n==========================================================\n")
cat("A. PRICINY \"neznamy\" NA UROVNI EVL\n")
cat("==========================================================\n\n")
print(as.data.frame(
  neznamy %>%
    dplyr::count(DRUH, VERDIKT, PRICINA, name = "EVL") %>%
    dplyr::arrange(DRUH, VERDIKT, dplyr::desc(EVL))
), right = FALSE)

cat("\n--- Kontrola: LOK_DILCPOCET u neznamych uzemi ---\n\n")
print(as.data.frame(
  neznamy %>%
    dplyr::filter(VERDIKT == "neznámý") %>%
    dplyr::count(DRUH, LOK_DILCPOCET, LOK_PROCDOBR_NA = is.na(LOK_PROCDOBR),
                 name = "EVL")
), right = FALSE)

cat("\n--- Kolik DP (CILMON=0) se tim zahazuje ---\n\n")
print(as.data.frame(
  neznamy %>%
    dplyr::filter(VERDIKT == "neznámý") %>%
    dplyr::group_by(DRUH) %>%
    dplyr::summarise(
      EVL               = dplyr::n(),
      DP_zahozenych     = sum(DP_CILMON0),
      EVL_bez_jakekoli_DP = sum(DP_CELKEM == 0),
      .groups = "drop"
    )
), right = FALSE)

readr::write_excel_csv(neznamy, file.path(OUT, "priciny_neznamy_EVL.csv"))

#==========================================================#
# B. PRICINY SELHANI POP_POCETPRUM3
#==========================================================#
# POP_POCETPRUM3 = klouzavy prumer za posledni 3 roky ze souctu maximalnich
# pocetnosti na jednotlivych DP, porovnavany jako `min` s cilovym stavem
# uzemi (POP_CILSTAV z SDO).

prum3 <- chu %>%
  dplyr::filter(ID_IND == "POP_POCETPRUM3") %>%
  dplyr::transmute(
    kod_chu, DRUH,
    HOD = suppressWarnings(as.numeric(HOD_IND)),
    CIL = suppressWarnings(as.numeric(LIM_IND)),
    JEDNOTKA,
    STAV_IND
  ) %>%
  dplyr::mutate(
    PODIL = ifelse(!is.na(HOD) & !is.na(CIL) & CIL > 0, round(HOD / CIL, 3), NA_real_),
    VAROVANI_SDO = grepl("VAROVANI", dplyr::coalesce(JEDNOTKA, ""), fixed = TRUE)
  ) %>%
  dplyr::left_join(dp_stat, by = c("kod_chu", "DRUH"))

cat("\n==========================================================\n")
cat("B. POP_POCETPRUM3 - proc tak casto nesplneno\n")
cat("==========================================================\n\n")

cat("--- Stav indikatoru ---\n\n")
print(as.data.frame(prum3 %>% dplyr::count(DRUH, STAV_IND, name = "EVL")),
      right = FALSE)

cat("\n--- Jak daleko od ciloveho stavu (HOD / CIL) ---\n\n")
print(as.data.frame(
  prum3 %>%
    dplyr::filter(!is.na(PODIL)) %>%
    dplyr::group_by(DRUH, SPLNENO = STAV_IND) %>%
    dplyr::summarise(
      EVL     = dplyr::n(),
      min     = min(PODIL),
      q25     = quantile(PODIL, 0.25),
      median  = median(PODIL),
      q75     = quantile(PODIL, 0.75),
      max     = max(PODIL),
      .groups = "drop"
    )
), right = FALSE)

cat("\n--- Rozsah selhani: jak hluboko pod cilem ---\n\n")
print(as.data.frame(
  prum3 %>%
    dplyr::filter(STAV_IND == "špatný", !is.na(PODIL)) %>%
    dplyr::mutate(PASMO = cut(
      PODIL,
      breaks = c(-Inf, 0.01, 0.05, 0.1, 0.25, 0.5, 1),
      labels = c("0-1 % cile", "1-5 %", "5-10 %", "10-25 %", "25-50 %", "50-100 %")
    )) %>%
    dplyr::count(DRUH, PASMO, name = "EVL")
), right = FALSE)

cat("\n--- Varovani zdroje SDO u ciloveho stavu ---\n\n")
print(as.data.frame(prum3 %>% dplyr::count(DRUH, VAROVANI_SDO, STAV_IND, name = "EVL")),
      right = FALSE)

cat("\n--- Souvisi selhani s poctem monitorovanych DP? ---\n\n")
print(as.data.frame(
  prum3 %>%
    dplyr::filter(!is.na(STAV_IND)) %>%
    dplyr::group_by(DRUH, STAV_IND) %>%
    dplyr::summarise(
      EVL               = dplyr::n(),
      DP_cilmon1_median = median(dplyr::coalesce(DP_CILMON1, 0L)),
      DP_celkem_median  = median(dplyr::coalesce(DP_CELKEM, 0L)),
      CIL_median        = median(CIL, na.rm = TRUE),
      HOD_median        = median(HOD, na.rm = TRUE),
      .groups = "drop"
    )
), right = FALSE)

cat("\n--- 15 nejhorsich uzemi ---\n\n")
print(as.data.frame(
  prum3 %>%
    dplyr::filter(STAV_IND == "špatný") %>%
    dplyr::arrange(PODIL) %>%
    dplyr::select(kod_chu, DRUH, HOD, CIL, PODIL, DP_CILMON1, VAROVANI_SDO) %>%
    head(15)
), right = FALSE)

readr::write_excel_csv(prum3, file.path(OUT, "priciny_pop_pocetprum3.csv"))

cat("\n=== HOTOVO ===\n")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

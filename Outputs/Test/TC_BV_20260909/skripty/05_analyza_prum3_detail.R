#----------------------------------------------------------#
# POP_POCETPRUM3 - detailni rozbor priciny selhani ---------
#----------------------------------------------------------#
# Doplnuje Test/04_analyza_priciny.R. Zamerene na tri hypotezy:
#   H1 klouzavy prumer stoji na MENE nez 3 letech
#   H2 pouzite roky jsou STARE (okno -12 let, ne posledni 3 kalendarni roky)
#   H3 HOD = 0 jsou skutecne zaznamy NEPRITOMNOSTI, ne chybejici data
#   H4 cilovy stav je radove jinde nez cokoli, co monitoring kdy zaznamenal
#----------------------------------------------------------#

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

OUT <- "Outputs/Test/TC_BV_20260909/analyza"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

poc <- readr::read_csv("Data/Temp/n2k_druhy_pocetnost.csv", show_col_types = FALSE)
chu <- readr::read_csv("Data/Temp/n2k_druhy_chu.csv", show_col_types = FALSE)

prum3 <- chu %>%
  dplyr::filter(ID_IND == "POP_POCETPRUM3") %>%
  dplyr::transmute(
    kod_chu, DRUH,
    HOD = suppressWarnings(as.numeric(HOD_IND)),
    CIL = suppressWarnings(as.numeric(LIM_IND)),
    STAV_IND
  )

det <- prum3 %>%
  dplyr::left_join(poc, by = c("kod_chu", "DRUH"))

cat("\n==========================================================\n")
cat("H1. Na kolika letech stoji \"klouzavy prumer za 3 roky\"\n")
cat("==========================================================\n\n")
print(as.data.frame(
  det %>%
    dplyr::filter(!is.na(STAV_IND), STAV_IND != "nehodnocen") %>%
    dplyr::count(DRUH, STAV_IND, POP_POCETPRUM3_LET, name = "EVL") %>%
    dplyr::arrange(DRUH, STAV_IND, POP_POCETPRUM3_LET)
), right = FALSE)

#----------------------------------------------------------#
# H2. Jak stare jsou pouzite roky ----
#----------------------------------------------------------#
# Rekonstrukce rady po rocich ze faze 1 (n2k_druhy.csv), stejnym postupem
# jako faze 1b v 27_n2k_druhy_zapis.R, aby bylo videt, KTERE roky vstoupily.
d1 <- data.table::fread(
  "Data/Temp/n2k_druhy.csv",
  select = c("kod_chu", "DRUH", "KOD_LOKAL", "ROK", "CILMON",
             "POCET", "POP_POCET", "POP_POCETNOSTNAL"),
  encoding = "UTF-8"
)

rada <- d1 %>%
  tibble::as_tibble() %>%
  dplyr::filter(CILMON == 1, !is.na(POP_POCET)) %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, ROK) %>%
  dplyr::summarise(POP_POCETDP = max(as.numeric(POP_POCET), na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::group_by(kod_chu, DRUH, ROK) %>%
  dplyr::summarise(SUMROK = sum(POP_POCETDP, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::slice_max(order_by = ROK, n = 3, with_ties = FALSE) %>%
  dplyr::summarise(
    ROKY      = paste(sort(ROK), collapse = ", "),
    ROK_MAX   = max(ROK),
    ROK_MIN   = min(ROK),
    LET       = dplyr::n(),
    .groups   = "drop"
  )

det2 <- det %>% dplyr::left_join(rada, by = c("kod_chu", "DRUH"))

cat("\n==========================================================\n")
cat("H2. Jak stary je NEJNOVEJSI rok vstupujici do prumeru\n")
cat("    (hodnoceny rok = 2025)\n")
cat("==========================================================\n\n")
print(as.data.frame(
  det2 %>%
    dplyr::filter(!is.na(ROK_MAX), !is.na(STAV_IND), STAV_IND != "nehodnocen") %>%
    dplyr::mutate(STARI = 2025 - ROK_MAX) %>%
    dplyr::group_by(DRUH, STAV_IND) %>%
    dplyr::summarise(
      EVL            = dplyr::n(),
      stari_median   = median(STARI),
      stari_max      = max(STARI),
      do_2_let       = sum(STARI <= 2),
      starsi_5_let   = sum(STARI > 5),
      .groups = "drop"
    )
), right = FALSE)

#----------------------------------------------------------#
# H3. Jsou nuly skutecne nepritomnosti? ----
#----------------------------------------------------------#
cat("\n==========================================================\n")
cat("H3. EVL s POP_POCETPRUM3 = 0 - co je v datech\n")
cat("==========================================================\n\n")

nuly <- det2 %>% dplyr::filter(!is.na(HOD), HOD == 0)

cat("pocet EVL s prumerem 0:", nrow(nuly), "\n\n")

nul_det <- d1 %>%
  tibble::as_tibble() %>%
  dplyr::semi_join(nuly, by = c("kod_chu", "DRUH")) %>%
  dplyr::filter(CILMON == 1) %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::summarise(
    zaznamu          = dplyr::n(),
    s_POP_POCET      = sum(!is.na(POP_POCET)),
    POP_POCET_nula   = sum(!is.na(POP_POCET) & as.numeric(POP_POCET) == 0),
    s_surovym_POCET  = sum(!is.na(POCET)),
    kat_0            = sum(!is.na(POP_POCETNOSTNAL) & POP_POCETNOSTNAL == 0),
    .groups = "drop"
  )
print(as.data.frame(nul_det), right = FALSE)

#----------------------------------------------------------#
# H4. Cilovy stav vs. historicke maximum monitoringu ----
#----------------------------------------------------------#
# Nejlepsi rok, jaky kdy monitoring v uzemi zaznamenal, vs. cilovy stav.
# Kdyby byl cil nastaven realne, mel by byt dosazitelny alespon v nejlepsim
# zaznamenanem roce.
hist_max <- d1 %>%
  tibble::as_tibble() %>%
  dplyr::filter(CILMON == 1, !is.na(POP_POCET)) %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, ROK) %>%
  dplyr::summarise(DP = max(as.numeric(POP_POCET), na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(kod_chu, DRUH, ROK) %>%
  dplyr::summarise(SUMROK = sum(DP, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::summarise(HIST_MAX = max(SUMROK), .groups = "drop")

det3 <- det2 %>%
  dplyr::left_join(hist_max, by = c("kod_chu", "DRUH")) %>%
  dplyr::mutate(
    CIL_DOSAZITELNY = dplyr::case_when(
      is.na(CIL) | is.na(HIST_MAX) ~ NA,
      HIST_MAX >= CIL ~ TRUE,
      TRUE ~ FALSE
    )
  )

cat("\n==========================================================\n")
cat("H4. Byl cilovy stav KDY dosazen (nejlepsi rok za 12 let)?\n")
cat("==========================================================\n\n")
print(as.data.frame(
  det3 %>%
    dplyr::filter(STAV_IND == "špatný") %>%
    dplyr::count(DRUH, CIL_DOSAZITELNY, name = "EVL")
), right = FALSE)

cat("\n--- Podil cile, ktery nejlepsi rok vubec dosahl ---\n\n")
print(as.data.frame(
  det3 %>%
    dplyr::filter(STAV_IND == "špatný", !is.na(HIST_MAX), !is.na(CIL), CIL > 0) %>%
    dplyr::mutate(PODIL_MAX = round(HIST_MAX / CIL, 3)) %>%
    dplyr::group_by(DRUH) %>%
    dplyr::summarise(
      EVL    = dplyr::n(),
      median = median(PODIL_MAX),
      q75    = quantile(PODIL_MAX, 0.75),
      max    = max(PODIL_MAX),
      nikdy_nedosazen = sum(PODIL_MAX < 1),
      .groups = "drop"
    )
), right = FALSE)

readr::write_excel_csv(det3, file.path(OUT, "pop_pocetprum3_detail.csv"))

cat("\n=== HOTOVO ===\n")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

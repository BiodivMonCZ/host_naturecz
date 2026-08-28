#----------------------------------------------------------#
# Srovnani testovaciho behu s puvodnim behem ----------------
# Triturus cristatus, vsechny tri urovne hodnoceni
#----------------------------------------------------------#
# NOVY  = testovaci beh z dnesniho dne (HEAD vetve 202608-obojzivelnici,
#         tj. vcetne oprav H-21 a H-22)
# PUVODNI = posledni zakomitovany beh na vetvi (commit 9a108a2, 2026-08-25),
#         tedy stav PRED opravami H-21 a H-22
#
# Vystup: souhrn za celou populaci + detailni srovnani pro nahodny vzorek
#         10 EVL (kod_chu) a 50 dilcich ploch (KOD_LOKAL).
#----------------------------------------------------------#

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
})

SPECIES  <- "Triturus cristatus"
SEED     <- 20260828
N_EVL    <- 10
N_LOK    <- 50

OUT <- Sys.getenv("TC_OUT", unset = "Test/out")
dir.create(file.path(OUT, "srovnani"), recursive = TRUE, showWarnings = FALSE)

w1250 <- readr::locale(encoding = "Windows-1250")

read_export <- function(path) {
  readr::read_csv(path, locale = w1250, show_col_types = FALSE,
                  progress = FALSE, guess_max = 50000)
}

new_stamp <- format(Sys.Date(), "%Y%m%d")

#----------------------------------------------------------#
# 1. Nacteni ----
#----------------------------------------------------------#
message("--- nacitam exporty ---")

nal_new <- read_export(sprintf("Outputs/Data/druhy/n2k_druhy_nal_2025_%s_Windows-1250.csv.gz", new_stamp)) %>%
  filter(DRUH == SPECIES)
lok_new <- read_export(sprintf("Outputs/Data/druhy/n2k_druhy_lok_2025_%s_Windows-1250.csv.gz", new_stamp)) %>%
  filter(DRUH == SPECIES)
chu_new <- read_export(sprintf("Outputs/Data/druhy/n2k_druhy_chu_2025_%s_Windows-1250.csv.gz", new_stamp)) %>%
  filter(druh == SPECIES)

nal_old <- read_export("Test/baseline/n2k_druhy_nal_2025_20260820_Windows-1250.csv.gz") %>%
  filter(DRUH == SPECIES)
lok_old <- read_export("Test/baseline/n2k_druhy_lok_2025_20260821_Windows-1250.csv.gz") %>%
  filter(DRUH == SPECIES)
chu_old <- read_export("Test/baseline/n2k_druhy_chu_2025_20260821_Windows-1250.csv.gz") %>%
  filter(druh == SPECIES)

# Ciselnik ISOP kodu -> nazev indikatoru v R (pro uroven EVL)
cis_ind <- readr::read_csv("Data/Input/cis_indikatory_popis.csv",
                           locale = w1250, show_col_types = FALSE) %>%
  filter(!is.na(ind_id)) %>%
  distinct(ind_id, ind_r, ind_popis)

ind_name <- function(x) {
  m <- cis_ind$ind_r[match(as.character(x), as.character(cis_ind$ind_id))]
  ifelse(is.na(m), as.character(x), m)
}

#----------------------------------------------------------#
# 2. Souhrn za celou populaci ----
#----------------------------------------------------------#
message("--- souhrn za celou populaci ---")

sum_lok <- bind_rows(
  lok_old %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>%
    count(stav = HOD_IND) %>% mutate(beh = "puvodni"),
  lok_new %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>%
    count(stav = HOD_IND) %>% mutate(beh = "novy")
) %>%
  pivot_wider(names_from = beh, values_from = n, values_fill = 0L) %>%
  arrange(stav)

sum_chu <- bind_rows(
  chu_old %>% filter(parametr_nazev == 10) %>%
    count(stav) %>% mutate(beh = "puvodni"),
  chu_new %>% filter(parametr_nazev == 10) %>%
    count(stav) %>% mutate(beh = "novy")
) %>%
  pivot_wider(names_from = beh, values_from = n, values_fill = 0L) %>%
  arrange(stav)

sum_rozsah <- tibble::tibble(
  metrika = c("radku nal", "radku lok", "radku chu",
              "DP (kod_chu x KOD_LOKAL)", "EVL v lok", "EVL v chu exportu",
              "indikatoru na urovni EVL"),
  puvodni = c(
    nrow(nal_old), nrow(lok_old), nrow(chu_old),
    n_distinct(paste(lok_old$kod_chu, lok_old$KOD_LOKAL)),
    n_distinct(lok_old$kod_chu),
    n_distinct(chu_old$kod_chu),
    n_distinct(chu_old$parametr_nazev)
  ),
  novy = c(
    nrow(nal_new), nrow(lok_new), nrow(chu_new),
    n_distinct(paste(lok_new$kod_chu, lok_new$KOD_LOKAL)),
    n_distinct(lok_new$kod_chu),
    n_distinct(chu_new$kod_chu),
    n_distinct(chu_new$parametr_nazev)
  )
)

#----------------------------------------------------------#
# 3. Vzorek 10 EVL + 50 DP ----
#----------------------------------------------------------#
message("--- losuji vzorek ---")

set.seed(SEED)

# EVL se losuji z uzemi, ktera maji hodnoceni na urovni EVL (tj. dostala se do
# exportu chu). Losovat ze vsech 191 uzemi v `lok` nema smysl - vetsina z nich
# zadny radek na urovni EVL nema (CILMON_CHU != 1 nebo uzemi neni v seznamu
# predmetu ochrany), takze by se vzorek z vetsi casti skladal z prazdnych
# porovnani.
evl_all <- sort(union(chu_old$kod_chu, chu_new$kod_chu))
evl_smp <- sort(sample(evl_all, min(N_EVL, length(evl_all))))

dp_all <- sort(union(
  paste(lok_old$kod_chu, lok_old$KOD_LOKAL, sep = "|"),
  paste(lok_new$kod_chu, lok_new$KOD_LOKAL, sep = "|")
))
dp_smp <- sort(sample(dp_all, min(N_LOK, length(dp_all))))

readr::write_csv(tibble::tibble(kod_chu = evl_smp),
                 file.path(OUT, "srovnani", "vzorek_evl.csv"))
readr::write_csv(
  tibble::tibble(dp = dp_smp) %>%
    separate(dp, c("kod_chu", "KOD_LOKAL"), sep = "\\|", extra = "merge"),
  file.path(OUT, "srovnani", "vzorek_dp.csv")
)

#----------------------------------------------------------#
# 4. Uroven EVL - srovnani vzorku ----
#----------------------------------------------------------#
message("--- uroven EVL ---")

chu_key <- function(x) {
  x %>%
    transmute(
      kod_chu,
      indikator = ind_name(parametr_nazev),
      parametr_nazev = as.character(parametr_nazev),
      hodnota = as.character(parametr_hodnota),
      limit = as.character(parametr_limit),
      jednotka = as.character(parametr_jednotka),
      stav
    )
}

# Porovnani NA-bezpecne: `dif()` je TRUE i kdyz je jedna strana NA a druha ne.
dif <- function(a, b) (is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b)

chu_cmp <- full_join(
  chu_key(chu_old) %>% mutate(.puv = TRUE) %>%
    rename_with(~paste0(.x, "_puv"), -c(kod_chu, indikator, .puv)),
  chu_key(chu_new) %>% mutate(.novy = TRUE) %>%
    rename_with(~paste0(.x, "_novy"), -c(kod_chu, indikator, .novy)),
  by = c("kod_chu", "indikator")
) %>%
  mutate(
    .puv  = coalesce(.puv, FALSE),
    .novy = coalesce(.novy, FALSE),
    zmena = case_when(
      !.puv &  .novy ~ "pouze novy",
      .puv  & !.novy ~ "pouze puvodni",
      dif(stav_puv, stav_novy) ~ "zmena stavu",
      dif(hodnota_puv, hodnota_novy) ~ "zmena hodnoty",
      TRUE ~ "shoda"
    )
  ) %>%
  select(-.puv, -.novy) %>%
  arrange(kod_chu, indikator)

chu_cmp_smp <- chu_cmp %>% filter(kod_chu %in% evl_smp)

#----------------------------------------------------------#
# 5. Uroven DP - srovnani vzorku ----
#----------------------------------------------------------#
message("--- uroven DP ---")

lok_key <- function(x) {
  x %>%
    transmute(
      kod_chu, KOD_LOKAL,
      indikator = ID_IND,
      ROK = as.character(ROK),
      hodnota = as.character(HOD_IND),
      limit = as.character(LIM_IND),
      stav = as.character(STAV_IND)
    )
}

lok_cmp <- full_join(
  lok_key(lok_old) %>% mutate(.puv = TRUE) %>%
    rename_with(~paste0(.x, "_puv"), -c(kod_chu, KOD_LOKAL, indikator, .puv)),
  lok_key(lok_new) %>% mutate(.novy = TRUE) %>%
    rename_with(~paste0(.x, "_novy"), -c(kod_chu, KOD_LOKAL, indikator, .novy)),
  by = c("kod_chu", "KOD_LOKAL", "indikator")
) %>%
  mutate(
    .puv  = coalesce(.puv, FALSE),
    .novy = coalesce(.novy, FALSE),
    zmena = case_when(
      !.puv &  .novy ~ "pouze novy",
      .puv  & !.novy ~ "pouze puvodni",
      dif(stav_puv, stav_novy) ~ "zmena stavu",
      dif(hodnota_puv, hodnota_novy) ~ "zmena hodnoty",
      dif(ROK_puv, ROK_novy) ~ "zmena roku",
      TRUE ~ "shoda"
    )
  ) %>%
  select(-.puv, -.novy) %>%
  arrange(kod_chu, KOD_LOKAL, indikator)

lok_cmp_smp <- lok_cmp %>%
  filter(paste(kod_chu, KOD_LOKAL, sep = "|") %in% dp_smp)

#----------------------------------------------------------#
# 6. Uroven nalezu - srovnani vzorku ----
#----------------------------------------------------------#
message("--- uroven nalezu ---")

nal_key <- function(x) {
  x %>%
    transmute(
      kod_chu, KOD_LOKAL,
      ID_ND_NALEZ = as.character(ID_ND_NALEZ),
      indikator = ID_IND,
      hodnota = as.character(HOD_IND),
      stav = as.character(STAV_IND)
    )
}

# POZOR: (ID_ND_NALEZ, ID_IND) neni v exportu unikatni klic - jeden nalez muze
# mit pro tyz indikator vic radku (ruzne jednotky / limity). Radky se proto
# porovnavaji jako MNOZINY: kazdemu radku se v ramci klice priradi poradove
# cislo podle setrideneho obsahu, takze se paruji stejne obsahy, ne stejne
# poradi ve zdrojovem souboru.
nal_ord <- function(x) {
  x %>%
    arrange(kod_chu, KOD_LOKAL, ID_ND_NALEZ, indikator, hodnota, stav) %>%
    group_by(kod_chu, KOD_LOKAL, ID_ND_NALEZ, indikator) %>%
    mutate(.i = row_number()) %>%
    ungroup()
}

nal_cmp <- full_join(
  nal_ord(nal_key(nal_old)) %>% mutate(.puv = TRUE) %>%
    rename_with(~paste0(.x, "_puv"), -c(kod_chu, KOD_LOKAL, ID_ND_NALEZ, indikator, .i, .puv)),
  nal_ord(nal_key(nal_new)) %>% mutate(.novy = TRUE) %>%
    rename_with(~paste0(.x, "_novy"), -c(kod_chu, KOD_LOKAL, ID_ND_NALEZ, indikator, .i, .novy)),
  by = c("kod_chu", "KOD_LOKAL", "ID_ND_NALEZ", "indikator", ".i")
) %>%
  mutate(
    .puv  = coalesce(.puv, FALSE),
    .novy = coalesce(.novy, FALSE),
    zmena = case_when(
      !.puv &  .novy ~ "pouze novy",
      .puv  & !.novy ~ "pouze puvodni",
      dif(stav_puv, stav_novy) ~ "zmena stavu",
      dif(hodnota_puv, hodnota_novy) ~ "zmena hodnoty",
      TRUE ~ "shoda"
    )
  ) %>%
  select(-.puv, -.novy)

nal_cmp_smp <- nal_cmp %>%
  filter(paste(kod_chu, KOD_LOKAL, sep = "|") %in% dp_smp) %>%
  arrange(kod_chu, KOD_LOKAL, ID_ND_NALEZ, indikator)

#----------------------------------------------------------#
# 7. Zapis ----
#----------------------------------------------------------#
message("--- zapisuji ---")

wr <- function(x, name) {
  readr::write_excel_csv(x, file.path(OUT, "srovnani", name), na = "")
  invisible(x)
}

wr(sum_rozsah, "souhrn_rozsah.csv")
wr(sum_lok,    "souhrn_stav_DP.csv")
wr(sum_chu,    "souhrn_stav_EVL.csv")

wr(chu_cmp,     "srovnani_EVL_vse.csv")
wr(chu_cmp_smp, "srovnani_EVL_vzorek10.csv")
wr(lok_cmp,     "srovnani_DP_vse.csv")
wr(lok_cmp_smp, "srovnani_DP_vzorek50.csv")
wr(nal_cmp_smp, "srovnani_nalezy_vzorek50.csv")

# Prehledy zmen
wr(count(chu_cmp, zmena), "prehled_zmen_EVL_vse.csv")
wr(count(lok_cmp, zmena), "prehled_zmen_DP_vse.csv")
wr(count(nal_cmp, zmena), "prehled_zmen_nalezy_vse.csv")
wr(count(chu_cmp, indikator, zmena), "prehled_zmen_EVL_dle_indikatoru.csv")
wr(count(lok_cmp, indikator, zmena), "prehled_zmen_DP_dle_indikatoru.csv")

# Uplny seznam DP a EVL, kde se vysledek zmenil (mimo vzorek - pro dohledani)
wr(
  lok_cmp %>% filter(indikator == "CELKOVE_HODNOCENI", zmena != "shoda") %>%
    select(kod_chu, KOD_LOKAL, ROK_novy, stav_puv = hodnota_puv,
           stav_novy = hodnota_novy),
  "zmenene_DP_vse.csv"
)
wr(
  chu_cmp %>% filter(indikator == "CELKOVE_HODNOCENI", zmena != "shoda") %>%
    select(kod_chu, stav_puv, stav_novy),
  "zmenene_EVL_vse.csv"
)
wr(count(chu_cmp_smp, zmena), "prehled_zmen_EVL_vzorek.csv")
wr(count(lok_cmp_smp, zmena), "prehled_zmen_DP_vzorek.csv")
wr(count(nal_cmp_smp, zmena), "prehled_zmen_nalezy_vzorek.csv")

# Celkove hodnoceni pro vzorkovane EVL a DP - prehledna tabulka
wr(
  chu_cmp %>% filter(indikator == "CELKOVE_HODNOCENI", kod_chu %in% evl_smp) %>%
    select(kod_chu, stav_puv, stav_novy, zmena),
  "vzorek10_EVL_celkove.csv"
)
wr(
  lok_cmp %>% filter(indikator == "CELKOVE_HODNOCENI",
                     paste(kod_chu, KOD_LOKAL, sep = "|") %in% dp_smp) %>%
    select(kod_chu, KOD_LOKAL, ROK_puv, ROK_novy,
           stav_puv = hodnota_puv, stav_novy = hodnota_novy, zmena),
  "vzorek50_DP_celkove.csv"
)

#----------------------------------------------------------#
# 8. Vypis na konzoli ----
#----------------------------------------------------------#
cat("\n================ ROZSAH ================\n");        print(as.data.frame(sum_rozsah))
cat("\n========== CELKOVY STAV - DP ===========\n");         print(as.data.frame(sum_lok))
cat("\n========== CELKOVY STAV - EVL ==========\n");         print(as.data.frame(sum_chu))
cat("\n===== ZMENY (vse, uroven nalezu) =======\n");         print(as.data.frame(count(nal_cmp, zmena)))
cat("\n===== ZMENY (vse, uroven DP) ===========\n");         print(as.data.frame(count(lok_cmp, zmena)))
cat("\n===== ZMENY (vse, uroven DP dle ind.) ==\n")
print(as.data.frame(count(lok_cmp, indikator, zmena) %>% filter(zmena != "shoda")))
cat("\n===== ZMENY (vse, uroven EVL) ==========\n");         print(as.data.frame(count(chu_cmp, zmena)))
cat("\n===== ZMENY (vse, uroven EVL dle ind.) =\n")
print(as.data.frame(count(chu_cmp, indikator, zmena)))
cat("\n===== VZOREK 10 EVL - celkove ==========\n")
print(as.data.frame(chu_cmp %>% filter(indikator == "CELKOVE_HODNOCENI", kod_chu %in% evl_smp) %>%
                      select(kod_chu, stav_puv, stav_novy, zmena)))
cat("\n===== VZOREK 50 DP - celkove ===========\n")
print(as.data.frame(lok_cmp %>% filter(indikator == "CELKOVE_HODNOCENI",
                                       paste(kod_chu, KOD_LOKAL, sep = "|") %in% dp_smp) %>%
                      select(kod_chu, KOD_LOKAL, ROK_puv, ROK_novy,
                             stav_puv = hodnota_puv, stav_novy = hodnota_novy, zmena)))
cat("\n===== ZMENY ve vzorku ==================\n")
cat("EVL:\n");    print(as.data.frame(count(chu_cmp_smp, zmena)))
cat("DP:\n");     print(as.data.frame(count(lok_cmp_smp, zmena)))
cat("nalezy:\n"); print(as.data.frame(count(nal_cmp_smp, zmena)))

message("--- HOTOVO: ", file.path(OUT, "srovnani"), " ---")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

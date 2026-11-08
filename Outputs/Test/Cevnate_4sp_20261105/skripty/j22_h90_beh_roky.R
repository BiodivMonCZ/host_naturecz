# Dopad pravidla H-90 na SOUCASNEM kodu: 24 bez pravidla (433b610) x 24 s pravidlem (HEAD)
# nad mezivysledkem 21_2 testovaciho behu, rozdeleno podle roku hodnocene navstevy DP.
# Pouziti: Rscript j22_h90_beh_roky.R <kopie_repozitare> <vystup.rds>
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
a <- commandArgs(trailingOnly = TRUE)
sp <- "<scratchpad>/"
lim <- read_csv(file.path(a[1], "Data/Temp/n2k_druhy_lim.csv"), show_col_types = FALSE, guess_max = 1e6,
                col_types = cols(.default = col_character(), STAV_IND = col_double(), CILMON = col_double(),
                                 ROK = col_double(), DATUM = col_date()))
sites_subjects <- lim %>% distinct(DRUH, SKUPINA)
DRUHY_RYBY_2026 <- c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis")
e_bez <- new.env(); sys.source(paste0(sp, "j21_24_bez.R"), envir = e_bez, keep.source = FALSE)
e_s <- new.env(); sys.source(paste0(sp, "j21_24_s.R"), envir = e_s, keep.source = FALSE)
souhrn <- function(x, beh) x %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% distinct(kod_chu, DRUH, KOD_LOKAL, .keep_all = TRUE) %>%
  transmute(kod_chu, DRUH, KOD_LOKAL, SKUPINA, ROK = as.integer(ROK), CILMON, ROK_CILMON = as.integer(ROK_CILMON),
            !!paste0("v_", beh) := HOD_IND, !!paste0("c_", beh) := CELKOVE_CILMON)
res <- list()
for (d in sort(unique(lim$DRUH))) {
  x <- lim %>% filter(DRUH == d)
  b <- suppressWarnings(e_bez$run_n2k_druhy_lok(x, d, sites_subjects, NULL))
  s <- suppressWarnings(e_s$run_n2k_druhy_lok(x, d, sites_subjects, NULL))
  res[[d]] <- souhrn(b, "bez") %>% full_join(souhrn(s, "s") %>% select(kod_chu, DRUH, KOD_LOKAL, v_s, c_s), by = c("kod_chu", "DRUH", "KOD_LOKAL"))
}
r <- bind_rows(res) %>% mutate(obdobi = case_when(ROK >= 2024 ~ "2024+", ROK >= 2020 ~ "2020-2023", TRUE ~ "do 2019"))
saveRDS(r, a[2])
options(width = 220)
cat("=== vsechny DP: podle obdobi hodnocene navstevy ===\n")
print(as.data.frame(r %>% group_by(DRUH, obdobi) %>% summarise(dp = n(), zmena = sum(v_bez != v_s), podil = round(100 * mean(v_bez != v_s), 1),
  dobry_bez = sum(v_bez == "dobrý"), dobry_s = sum(v_s == "dobrý"), spatny_bez = sum(v_bez == "špatný"), spatny_s = sum(v_s == "špatný"), .groups = "drop")))
cat("\n=== verdikt z ciloveho monitoringu (vstup EVL), podle roku tohoto verdiktu ===\n")
rc <- r %>% filter(!is.na(c_bez)) %>% mutate(obdobi_c = case_when(ROK_CILMON >= 2024 ~ "2024+", ROK_CILMON >= 2020 ~ "2020-2023", TRUE ~ "do 2019"))
print(as.data.frame(rc %>% group_by(DRUH, obdobi_c) %>% summarise(dp = n(), zmena = sum(c_bez != c_s), podil = round(100 * mean(c_bez != c_s), 1),
  dobry_bez = sum(c_bez == "dobrý"), dobry_s = sum(c_s == "dobrý"), spatny_bez = sum(c_bez == "špatný"), spatny_s = sum(c_s == "špatný"),
  neznamy_s = sum(c_s == "neznámý"), .groups = "drop")))

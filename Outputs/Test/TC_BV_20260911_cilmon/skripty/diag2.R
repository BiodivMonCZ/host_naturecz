suppressPackageStartupMessages({ library(tidyverse) })
OUT <- Sys.getenv("SCR")
lim <- readr::read_csv("Data/Temp/n2k_druhy_lim.csv", show_col_types = FALSE)

cat("=== A. Rozsah dedupu slice(1) u nepole druhu ===\n")
post <- lim %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE, ROK, ID_IND) %>%
  dplyr::summarise(CILMON = max(CILMON, na.rm = TRUE), .groups = "drop")
grp <- post %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, ROK, ID_IND) %>%
  dplyr::summarise(n_POLE = dplyr::n(), mn = min(CILMON), mx = max(CILMON), .groups = "drop")
cat("skupin (DP x rok x indikator) celkem:", nrow(grp), "\n")
print(grp %>% dplyr::count(DRUH, vice_POLE = n_POLE > 1, name = "skupin"))
cat("\n-- z toho skupiny, kde se POLE lisi CILMONEM (0 i 1) --\n")
print(grp %>% dplyr::filter(n_POLE > 1, mn == 0, mx == 1) %>% dplyr::count(DRUH, name = "skupin"))
cat("\n-- kolik DP x rok je tim dotceno --\n")
print(grp %>% dplyr::filter(n_POLE > 1, mn == 0, mx == 1) %>%
        dplyr::distinct(kod_chu, DRUH, KOD_LOKAL, ROK) %>% dplyr::count(DRUH, name = "DP_rok"))
cat("\n-- kolik DP (bez ohledu na rok) --\n")
print(grp %>% dplyr::filter(n_POLE > 1, mn == 0, mx == 1) %>%
        dplyr::distinct(kod_chu, DRUH, KOD_LOKAL) %>% dplyr::count(DRUH, name = "DP"))

cat("\n=== B. Co presne ma LOK_PROCDOBR u neznamych EVL ===\n")
f <- readRDS(file.path(OUT, "fix.rds"))
nez <- f$chu %>% dplyr::filter(ID_IND == "CELKOVE_HODNOCENI", is.na(HOD_IND)) %>%
  dplyr::distinct(kod_chu, DRUH)
print(f$chu %>% dplyr::semi_join(nez, by = c("kod_chu","DRUH")) %>%
  dplyr::filter(ID_IND %in% c("LOK_PROCDOBR","LOK_DILCPOCET","LOK_DILCDOBRE","POP_POCETPRUM3")) %>%
  dplyr::count(DRUH, ID_IND, HOD_IND, STAV_IND_NA = is.na(STAV_IND), name = "EVL"))

cat("\n=== C. Kolik DP verdiktu se v neznamych EVL zahazuje ===\n")
fc <- f$lok %>% dplyr::filter(ID_IND == "CELKOVE_HODNOCENI")
print(fc %>% dplyr::semi_join(nez, by = c("kod_chu","DRUH")) %>%
  dplyr::count(DRUH, HOD_IND, name = "DP"))
cat("\n-- a kolik z nich ma CILMON = 1 nebo cileny verdikt --\n")
print(fc %>% dplyr::semi_join(nez, by = c("kod_chu","DRUH")) %>%
  dplyr::summarise(.by = DRUH, DP = dplyr::n(),
                   s_CILMON1 = sum(CILMON == 1, na.rm = TRUE),
                   s_cilenym_verdiktem = sum(!is.na(CELKOVE_CILMON))))

suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
sp <- "<scratchpad>/"
L <- read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy_lok.csv"), show_col_types = FALSE, col_types = cols(.default = col_character()))
k <- L %>% filter(ID_IND != "CELKOVE_HODNOCENI", KLIC %in% "ano", UROVEN %in% "lok", !is.na(LIM_IND), LIM_IND != "") %>%
  group_by(kod_chu, DRUH, KOD_LOKAL, ROK) %>% summarise(nezn = sum(is.na(STAV_IND)), nespl = sum(STAV_IND %in% "0"),
  nezn_ind = paste(sort(unique(ID_IND[is.na(STAV_IND)])), collapse = ", "), .groups = "drop")
dp <- L %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% select(kod_chu, DRUH, KOD_LOKAL, ROK, CILMON, HOD_IND, CELKOVE_CILMON) %>% left_join(k, by = c("kod_chu", "DRUH", "KOD_LOKAL", "ROK"))
cat("DP 'neznamy' s alespon jednim NESPLNENYM klicovym (pri prednosti spatneho by byly 'spatny'):\n")
print(dp %>% group_by(DRUH) %>% summarise(neznamy = sum(HOD_IND == "neznámý"), z_toho_s_nesplnenym = sum(HOD_IND == "neznámý" & nespl > 0)))
cat("\nneznamy klicovy indikator u DP 'neznamy':\n")
print(dp %>% filter(HOD_IND == "neznámý") %>% separate_rows(nezn_ind, sep = ", ") %>% count(DRUH, nezn_ind) %>% as.data.frame())

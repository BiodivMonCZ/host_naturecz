# Srovnani vice testovacich behu (izolovane kopie repozitare) - uroven DP a EVL.
# Pouziti: Rscript j06_srovnani_behu.R <OUT> <stitek1>=<kopie1> <stitek2>=<kopie2> ...
# Vstup: Data/Temp/n2k_druhy_lok.csv a n2k_druhy_chu.csv kazde kopie. Vystup: CSV do OUT.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
a <- commandArgs(trailingOnly = TRUE)
OUT <- a[1]; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
behy <- setNames(sub("^[^=]*=", "", a[-1]), sub("=.*$", "", a[-1]))
ss <- openxlsx::read.xlsx("C:/Users/jonas.gaigr/Documents/host_naturecz/Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx", sheet = 1)
names(ss)[c(1,8)] <- c("site_code","nazev_lat"); par <- paste(ss$nazev_lat, ss$site_code)
cti <- function(d, f) read_csv(file.path(d, "Data/Temp", f), show_col_types = FALSE, guess_max = 1e5, col_types = cols(.default = col_character()))
L <- lapply(behy, cti, f = "n2k_druhy_lok.csv"); C <- lapply(behy, cti, f = "n2k_druhy_chu.csv")
lv <- c("dobrý", "zhoršený", "špatný", "neznámý", "nehodnoceno")
options(width = 220)
dp <- bind_rows(lapply(names(L), function(b) L[[b]] %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>%
  transmute(beh = b, DRUH, kod_chu, KOD_LOKAL, CILMON, HOD_IND, CELKOVE_CILMON, PO = paste(DRUH, kod_chu) %in% par)))
t1 <- dp %>% mutate(v = factor(HOD_IND, levels = lv)) %>% count(DRUH, beh, v, .drop = FALSE) %>%
  pivot_wider(names_from = v, values_from = n, values_fill = 0) %>% left_join(dp %>% count(DRUH, beh, name = "DP"), by = c("DRUH", "beh")) %>%
  mutate(beh = factor(beh, levels = names(behy))) %>% arrange(DRUH, beh)
cat("=== DP: verdikty (vsechny DP) ===\n"); print(as.data.frame(t1)); write_csv(t1, file.path(OUT, "dp_verdikty.csv"))
t2 <- dp %>% filter(!is.na(CELKOVE_CILMON)) %>% mutate(v = factor(CELKOVE_CILMON, levels = lv)) %>% count(DRUH, beh, v, .drop = FALSE) %>%
  pivot_wider(names_from = v, values_from = n, values_fill = 0) %>% mutate(beh = factor(beh, levels = names(behy))) %>% arrange(DRUH, beh)
cat("\n=== DP: verdikt z ciloveho monitoringu (CELKOVE_CILMON; vstup pro EVL) ===\n"); print(as.data.frame(t2)); write_csv(t2, file.path(OUT, "dp_verdikty_cilmon.csv"))
cat("\n=== DP: CILMON = 1 (pocet DP s cilenym monitoringem) ===\n"); print(dp %>% count(DRUH, beh, CILMON) %>% pivot_wider(names_from = CILMON, values_from = n, values_fill = 0, names_prefix = "CILMON_"))
evl <- bind_rows(lapply(names(C), function(b) C[[b]] %>% filter(ID_IND == "CELKOVE_HODNOCENI", paste(DRUH, kod_chu) %in% par) %>%
  transmute(beh = b, DRUH, kod_chu, HOD = coalesce(STAV_IND, HOD_IND))))
t3 <- evl %>% count(DRUH, beh, HOD) %>% pivot_wider(names_from = HOD, values_from = n, values_fill = 0) %>% mutate(beh = factor(beh, levels = names(behy))) %>% arrange(DRUH, beh)
cat("\n=== EVL (dvojice predmetu ochrany): verdikty ===\n"); print(as.data.frame(t3)); write_csv(t3, file.path(OUT, "evl_verdikty.csv"))
w <- evl %>% pivot_wider(names_from = beh, values_from = HOD) %>% mutate(across(all_of(names(behy)), ~ coalesce(.x, "(bez výstupu)")))
write_csv(w, file.path(OUT, "evl_prechody.csv"))
for (i in seq_along(behy)[-1]) {
  cat("\n=== EVL: prechody", names(behy)[i - 1], "->", names(behy)[i], "===\n")
  print(w %>% count(DRUH, .data[[names(behy)[i - 1]]], .data[[names(behy)[i]]]) %>% as.data.frame())
}
li <- bind_rows(lapply(names(C), function(b) C[[b]] %>% filter(paste(DRUH, kod_chu) %in% par, grepl("^LOK_|^POP_PROCPOLE", ID_IND)) %>%
  transmute(beh = b, DRUH, kod_chu, ID_IND, HOD_IND, STAV_IND)))
write_csv(li, file.path(OUT, "evl_indikatory_lokalit.csv"))
cat("\n=== EVL: LOK_POCETSUM (hodnocene DP) a LOK_PROCDOBR - soucet / median ===\n")
print(li %>% filter(ID_IND %in% c("LOK_POCETSUM", "LOK_POCETDOB", "LOK_PROCDOBR")) %>% mutate(h = suppressWarnings(as.numeric(HOD_IND))) %>%
        group_by(DRUH, beh, ID_IND) %>% summarise(evl = n(), soucet = sum(h, na.rm = TRUE), median = median(h, na.rm = TRUE), na = sum(is.na(h)), .groups = "drop") %>%
        mutate(beh = factor(beh, levels = names(behy))) %>% arrange(DRUH, ID_IND, beh) %>% as.data.frame())
saveRDS(list(dp = dp, evl = evl, li = li, t1 = t1, t2 = t2, t3 = t3), file.path(OUT, "srovnani.rds"))

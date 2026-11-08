# Proc je POP_ZMENARAD u DP s hodnocenou navstevou 2024+ nezname: chybi predchozi rok s cilenym
# monitoringem, nebo je predchozi rok, ale bez kategorie pocetnosti?
suppressPackageStartupMessages({library(dplyr); library(readr)})
sp <- "<scratchpad>/"
L <- read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy_lok.csv"), show_col_types = FALSE, col_types = cols(.default = col_character()))
lim <- read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy_lim.csv"), show_col_types = FALSE, col_types = cols(.default = col_character()),
                col_select = c(DRUH, KOD_LOKAL, ROK, CILMON, ID_IND, HOD_IND))
dp <- L %>% filter(ID_IND == "POP_ZMENARAD", as.integer(ROK) >= 2024) %>% distinct(DRUH, KOD_LOKAL, ROK, STAV_IND)
roky <- lim %>% filter(CILMON == "1") %>% distinct(DRUH, KOD_LOKAL, ROK_C = as.integer(ROK))
poc <- lim %>% filter(CILMON == "1", ID_IND == "POP_POCETNOST", !is.na(HOD_IND)) %>% distinct(DRUH, KOD_LOKAL, ROK_P = as.integer(ROK))
x <- dp %>% mutate(ROK = as.integer(ROK)) %>%
  left_join(roky %>% group_by(DRUH, KOD_LOKAL) %>% summarise(roky_c = list(ROK_C), .groups = "drop"), by = c("DRUH", "KOD_LOKAL")) %>%
  left_join(poc %>% group_by(DRUH, KOD_LOKAL) %>% summarise(roky_p = list(ROK_P), .groups = "drop"), by = c("DRUH", "KOD_LOKAL")) %>%
  rowwise() %>% mutate(predchozi_cilmon = any(unlist(roky_c) < ROK), predchozi_pocetnost = any(unlist(roky_p) < ROK)) %>% ungroup()
print(x %>% count(DRUH, hodnoceno = !is.na(STAV_IND), predchozi_cilmon, predchozi_pocetnost))
cat("\nrok prvniho ciloveho monitoringu u DP 2024+ bez predchoziho roku:\n")
print(x %>% filter(!predchozi_cilmon %in% TRUE) %>% rowwise() %>% mutate(prvni = suppressWarnings(min(unlist(roky_c)))) %>% ungroup() %>% count(DRUH, prvni))

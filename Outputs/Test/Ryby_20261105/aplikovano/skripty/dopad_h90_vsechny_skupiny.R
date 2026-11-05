# Dopad pravidla H-90 / Z-12 napric skupinami nad poslednim PLNYM behem (export DP
# Outputs/Data/druhy/n2k_druhy_lok_2025_20260828). Pro reprezentativni navstevu kazde DP:
# klicovy indikator s limitem a STAV_IND = NA => po zmene "neznamy" (neznamy ma prednost).
# Varianta "spatny ma prednost" pro srovnani. Jen cteni, nic se nezapisuje do repozitare.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
setwd("C:/Users/jonas.gaigr/Documents/host_naturecz")
sp <- "<scratchpad>/"
x <- read_delim("Outputs/Data/druhy/n2k_druhy_lok_2025_20260828_UTF-8.csv.gz", delim = ";", show_col_types = FALSE,
                col_types = cols(.default = col_character()), na = c("", "NA"))
klic_dp <- c("kod_chu", "DRUH", "KOD_LOKAL", "POLE", "ROK")
cel <- x %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% distinct(across(all_of(klic_dp)), .keep_all = TRUE) %>%
  select(all_of(klic_dp), SKUPINA, CILMON, old = HOD_IND)
kl <- x %>% filter(ID_IND != "CELKOVE_HODNOCENI", KLIC %in% "ano", UROVEN %in% "lok", !is.na(LIM_IND), LIM_IND != "") %>%
  group_by(across(all_of(klic_dp))) %>%
  summarise(n_klic = n_distinct(ID_IND), n_nezn = n_distinct(ID_IND[is.na(STAV_IND)]), n_nespl = n_distinct(ID_IND[STAV_IND %in% "0"]),
            nezn_ind = paste(sort(unique(ID_IND[is.na(STAV_IND)])), collapse = ", "), .groups = "drop")
r <- cel %>% left_join(kl, by = klic_dp) %>% mutate(across(c(n_klic, n_nezn, n_nespl), ~ coalesce(.x, 0L)),
  new = ifelse(n_nezn > 0 & old %in% c("dobrý", "zhoršený", "špatný"), "neznámý", old),
  new_spatny_prednost = ifelse(n_nezn > 0 & old %in% c("dobrý", "zhoršený"), "neznámý", old))
lv <- c("dobrý", "zhoršený", "špatný", "neznámý", "nehodnoceno")
tab <- function(df, col) df %>% mutate(v = factor(.data[[col]], levels = lv)) %>% count(SKUPINA, v, .drop = FALSE) %>%
  pivot_wider(names_from = v, values_from = n, values_fill = 0)
options(width = 220)
cat("=== DP (plny beh 2026-08-28), PRED ===\n"); print(as.data.frame(tab(r, "old")))
cat("\n=== PO (neznamy ma prednost - aplikovano) ===\n"); print(as.data.frame(tab(r, "new")))
cat("\n=== PO, kdyby mel prednost spatny ===\n"); print(as.data.frame(tab(r, "new_spatny_prednost")))
cat("\n=== jen DP s cilenym monitoringem (CILMON = 1), PRED / PO ===\n")
print(as.data.frame(tab(r %>% filter(CILMON == "1"), "old"))); print(as.data.frame(tab(r %>% filter(CILMON == "1"), "new")))
cat("\n=== DP bez jakehokoli klicoveho indikatoru s limitem (pravidlo se jich netyka) ===\n")
print(r %>% group_by(SKUPINA) %>% summarise(dp = n(), bez_klicoveho = sum(n_klic == 0), dobry_bez_klicoveho = sum(n_klic == 0 & old == "dobrý")) %>% as.data.frame())
cat("\n=== ktere klicove indikatory jsou nezname (DP, ktere se zmeni) ===\n")
zm <- r %>% filter(new != old)
print(zm %>% separate_rows(nezn_ind, sep = ", ") %>% count(SKUPINA, nezn_ind, sort = TRUE) %>% as.data.frame())
cat("\n=== po druzich (top 40 dle zmen) ===\n")
print(r %>% group_by(SKUPINA, DRUH) %>% summarise(dp = n(), zmena = sum(new != old), dobry_pred = sum(old == "dobrý"), dobry_po = sum(new == "dobrý"),
      spatny_pred = sum(old == "špatný"), spatny_po = sum(new == "špatný"), .groups = "drop") %>% arrange(desc(zmena)) %>% head(40) %>% as.data.frame())
saveRDS(r, paste0(sp, "dopad_h90_export.rds"))
write_csv(r %>% group_by(SKUPINA) %>% summarise(dp = n(), zmena = sum(new != old), dobry_pred = sum(old == "dobrý"), dobry_po = sum(new == "dobrý"),
  zhorseny_pred = sum(old == "zhoršený"), zhorseny_po = sum(new == "zhoršený"), spatny_pred = sum(old == "špatný"), spatny_po = sum(new == "špatný"),
  neznamy_po = sum(new == "neznámý"), spatny_po_kdyby_prednost = sum(new_spatny_prednost == "špatný")), paste0(sp, "dopad_h90_skupiny.csv"))

# POP_ZMENARAD = NA u DP 2024+, ktere predchozi rok s cilenym monitoringem MAJI: chybi kategorie
# pocetnosti v hodnocenem roce, v poslednim predchozim roce, nebo by se nasla v nekterem starsim roce?
suppressPackageStartupMessages({library(dplyr); library(readr)})
sp <- "<scratchpad>/"
d <- read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5,
              col_select = c(DRUH, KOD_LOKAL, ROK, CILMON_MAX, POP_POCETNOST, POP_ZMENARAD, NEGATIVNI)) %>%
  filter(DRUH %in% c("Bombina bombina", "Triturus cristatus")) %>% distinct()
lp <- d %>% group_by(DRUH, KOD_LOKAL, ROK) %>% summarise(cil = max(CILMON_MAX, na.rm = TRUE), pocetnost = suppressWarnings(max(POP_POCETNOST, na.rm = TRUE)),
  zmena = suppressWarnings(max(POP_ZMENARAD, na.rm = TRUE)), neg = all(NEGATIVNI %in% 1), .groups = "drop") %>%
  mutate(pocetnost = ifelse(is.infinite(pocetnost), NA, pocetnost), zmena = ifelse(is.infinite(zmena), NA, zmena))
x <- lp %>% filter(ROK >= 2024, is.na(zmena)) %>% rowwise() %>% mutate(
  ref = { r <- lp$ROK[lp$DRUH == DRUH & lp$KOD_LOKAL == KOD_LOKAL & lp$cil == 1 & lp$ROK < ROK]; if (length(r)) max(r) else NA_real_ },
  ref_poc = if (is.na(ref)) NA_real_ else lp$pocetnost[lp$DRUH == DRUH & lp$KOD_LOKAL == KOD_LOKAL & lp$ROK == ref][1],
  starsi_s_poc = { r <- lp$ROK[lp$DRUH == DRUH & lp$KOD_LOKAL == KOD_LOKAL & lp$cil == 1 & lp$ROK < ROK & !is.na(lp$pocetnost)]; length(r) > 0 }) %>% ungroup()
print(x %>% mutate(pripad = case_when(is.na(ref) ~ "a) zadny predchozi rok s CILMON",
  is.na(pocetnost) ~ "b) chybi pocetnost v hodnocenem roce",
  is.na(ref_poc) & starsi_s_poc ~ "c) posledni predchozi rok bez pocetnosti, starsi ji ma",
  is.na(ref_poc) ~ "d) zadny predchozi rok s pocetnosti", TRUE ~ "e) jine")) %>% count(DRUH, pripad))
cat("\n b) - je hodnoceny rok negativni zaznam?\n")
print(x %>% filter(!is.na(ref), is.na(pocetnost)) %>% count(DRUH, neg))

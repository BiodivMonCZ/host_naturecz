# Proc chybi abundance (POP_ABUNDANCE) v letech, ktere by daly dynamiku - Lampetra a Misgurnus.
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
sp <- "<scratchpad>/"
x <- readRDS(paste0(sp, "lm_diag.rds")); z <- x$z
options(width = 230)
cat("=== zaznamy ciloveho monitoringu bez abundance: proc ===\n")
r <- z %>% filter(CILMON == 1) %>% mutate(pricina = case_when(
  !is.na(POP_ABUNDANCE) ~ "0 abundance je",
  NEGATIVNI %in% 1 ~ "1 negativni zaznam",
  is.na(POP_POCETODLOV) ~ "2 chybi pocet",
  is.na(plocha_p) & is.na(plocha_pbm) & is.na(plocha_n) ~ "3 chybi plocha (zadny tag plochy)",
  TRUE ~ "4 jine"))
print(as.data.frame(r %>% count(DRUH, pricina) %>% pivot_wider(names_from = pricina, values_from = n, values_fill = 0)))
cat("\n=== negativni zaznamy: hodnota POP_ABUNDANCE a POP_POCETODLOV ===\n")
print(as.data.frame(z %>% filter(CILMON == 1, NEGATIVNI %in% 1) %>% count(DRUH, abund = ifelse(is.na(POP_ABUNDANCE), "NA", as.character(POP_ABUNDANCE)),
                                                                     odlov = ifelse(is.na(POP_POCETODLOV), "NA", as.character(POP_POCETODLOV)))))
cat("\n=== chybi plocha: ktere tagy useku jsou k dispozici (delka, sirka) ===\n")
tg <- function(x, t) stringr::str_match(x, paste0("<", t, ">([^<]*)</", t, ">"))[, 2]
print(as.data.frame(r %>% filter(pricina == "3 chybi plocha (zadny tag plochy)") %>%
  mutate(delka = !is.na(tg(STRUKT_POZN, "delka_prolov_u")), sirka = !is.na(tg(STRUKT_POZN, "pr_sir_t")), metod = metod) %>%
  count(DRUH, metod, delka, sirka)))
cat("\n=== rok zaznamu bez plochy ===\n")
print(as.data.frame(r %>% filter(pricina == "3 chybi plocha (zadny tag plochy)") %>% count(DRUH, ROK) %>% pivot_wider(names_from = ROK, values_from = n, values_fill = 0)))
cat("\n=== vitalita: DP bez mereneho roku - jsou zaznamy pozitivni a maji <velikosti>? ===\n")
lr <- z %>% group_by(DRUH, KOD_LOKAL, ROK) %>% summarise(pozit = any(!NEGATIVNI %in% 1), velik = any(!is.na(velik)), vit = any(!is.na(POP_VITALITA)), .groups = "drop")
print(as.data.frame(lr %>% count(DRUH, pozit, velik, vit)))

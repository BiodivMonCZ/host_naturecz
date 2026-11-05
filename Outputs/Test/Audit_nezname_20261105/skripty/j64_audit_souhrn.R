suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(stringr)})
sp <- "<scratchpad>/"
out <- commandArgs(trailingOnly = TRUE)[1]
det <- bind_rows(readRDS(paste0(sp, "audit_nezname.rds")))
dup <- readRDS(paste0(sp, "audit_duplicity.rds"))
x <- det %>% left_join(dup %>% select(DRUH, kod_chu, KOD_LOKAL, pozdeji_jinde), by = c("DRUH", "kod_chu", "KOD_LOKAL")) %>%
  mutate(pricina2 = case_when(str_starts(pricina, "A") & pozdeji_jinde %in% TRUE ~ "A2 jediny pruzkum, misto pozdeji pod jinym klicem",
                              str_starts(pricina, "A") ~ "A1 jediny pruzkum mista (skutecne prvni)", TRUE ~ pricina))
s <- x %>% count(DRUH, ID_IND, pricina = pricina2) %>% group_by(DRUH, ID_IND) %>% mutate(podil = round(100 * n / sum(n))) %>% ungroup()
write_csv(s, file.path(out, "pricina_neznamych_klicovych.csv"))
write_csv(x %>% select(DRUH, kod_chu, KOD_LOKAL, R, ID_IND, pricina = pricina2, let_any, let_ab, let_vit, pred_cil), file.path(out, "detail_neznamych_klicovych.csv"))
options(width = 200); print(as.data.frame(s))

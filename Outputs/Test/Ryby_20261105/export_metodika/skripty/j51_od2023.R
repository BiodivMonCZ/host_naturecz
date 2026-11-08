suppressPackageStartupMessages({library(dplyr)})
sp <- "<scratchpad>/"
r <- bind_rows(readRDS(paste0(sp, "h90_roky_fin.rds")), readRDS(paste0(sp, "h90_roky_cev.rds")))
s <- function(x, od) x %>% filter(ROK >= od) %>% group_by(DRUH) %>%
  summarise(od = od, dp = n(), zmeneno = sum(v_bez != v_s), podil = round(100 * mean(v_bez != v_s)),
            dobry_bez = sum(v_bez == "dobrý"), dobry_s = sum(v_s == "dobrý"), .groups = "drop")
print(as.data.frame(bind_rows(s(r, 2023), s(r, 2024)) %>% arrange(DRUH, od)))
cat("\nverdikt z ciloveho monitoringu, rok verdiktu >= 2023:\n")
print(as.data.frame(r %>% filter(!is.na(c_bez), ROK_CILMON >= 2023) %>% group_by(DRUH) %>%
  summarise(dp = n(), zmeneno = sum(c_bez != c_s), podil = round(100 * mean(c_bez != c_s)), .groups = "drop")))

# Ulozi souhrny "dopad H-90 podle roku hodnocene navstevy" do CSV (pro Outputs/Test).
suppressPackageStartupMessages({library(dplyr); library(readr)})
sp <- "<scratchpad>/"
out <- commandArgs(trailingOnly = TRUE)[1]
r <- bind_rows(readRDS(paste0(sp, "h90_roky_fin.rds")), readRDS(paste0(sp, "h90_roky_cev.rds")))
write_csv(r %>% group_by(SKUPINA, DRUH, obdobi) %>% summarise(dp = n(), zmeneno = sum(v_bez != v_s),
  dobry_bez = sum(v_bez == "dobrý"), dobry_s = sum(v_s == "dobrý"), zhorseny_bez = sum(v_bez == "zhoršený"), zhorseny_s = sum(v_s == "zhoršený"),
  spatny_bez = sum(v_bez == "špatný"), spatny_s = sum(v_s == "špatný"), neznamy_s = sum(v_s == "neznámý"), .groups = "drop"),
  file.path(out, "h90_podle_roku_soucasny_kod.csv"))
rc <- r %>% filter(!is.na(c_bez)) %>% mutate(obdobi = case_when(ROK_CILMON >= 2024 ~ "2024+", ROK_CILMON >= 2020 ~ "2020-2023", TRUE ~ "do 2019"))
write_csv(rc %>% group_by(SKUPINA, DRUH, obdobi) %>% summarise(dp = n(), zmeneno = sum(c_bez != c_s),
  dobry_bez = sum(c_bez == "dobrý"), dobry_s = sum(c_s == "dobrý"), spatny_bez = sum(c_bez == "špatný"), spatny_s = sum(c_s == "špatný"),
  neznamy_s = sum(c_s == "neznámý"), .groups = "drop"), file.path(out, "h90_podle_roku_soucasny_kod_cilmon.csv"))
write_csv(readRDS(paste0(sp, "h90_roky_export.rds")), file.path(out, "h90_podle_roku_export20260828.csv"))
cat("ok\n")

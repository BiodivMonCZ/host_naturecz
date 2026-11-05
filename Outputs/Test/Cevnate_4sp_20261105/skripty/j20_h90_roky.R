# Dopad pravidla H-90 podle roku hodnocene navstevy DP - export plneho behu 2026-08-28.
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
sp <- "<scratchpad>/"
r <- readRDS(paste0(sp, "dopad_h90_export.rds")) %>%
  mutate(ROK = as.integer(ROK), obdobi = case_when(ROK >= 2024 ~ "2024+", ROK >= 2020 ~ "2020-2023", TRUE ~ "do 2019"),
         zmena = new != old)
options(width = 220)
cat("=== DP podle obdobi hodnocene navstevy: pocet DP, zmenenych na neznamy, podil ===\n")
s <- r %>% group_by(SKUPINA, obdobi) %>%
  summarise(dp = n(), cilmon = sum(CILMON == "1"), zmena = sum(zmena), podil = round(100 * zmena / dp, 1),
            dobry_pred = sum(old == "dobrý"), dobry_po = sum(new == "dobrý"), .groups = "drop") %>%
  arrange(SKUPINA, obdobi)
print(as.data.frame(s))
cat("\n=== jen CILMON = 1, obdobi 2024+ ===\n")
print(as.data.frame(r %>% filter(CILMON == "1", obdobi == "2024+") %>% group_by(SKUPINA) %>%
  summarise(dp = n(), zmena = sum(zmena), podil = round(100 * zmena / dp, 1), dobry_pred = sum(old == "dobrý"), dobry_po = sum(new == "dobrý"),
            spatny_pred = sum(old == "špatný"), spatny_po = sum(new == "špatný"), .groups = "drop")))
cat("\n=== 2024+: ktere klicove indikatory jsou nezname (zmenene DP) ===\n")
print(as.data.frame(r %>% filter(obdobi == "2024+", zmena) %>% separate_rows(nezn_ind, sep = ", ") %>% count(SKUPINA, nezn_ind, sort = TRUE)))
cat("\n=== 2024+: po druzich se zmenou ===\n")
print(as.data.frame(r %>% filter(obdobi == "2024+") %>% group_by(SKUPINA, DRUH) %>% summarise(dp = n(), zmena = sum(zmena), .groups = "drop") %>%
  filter(zmena > 0) %>% arrange(desc(zmena))))
saveRDS(s, paste0(sp, "h90_roky_export.rds"))

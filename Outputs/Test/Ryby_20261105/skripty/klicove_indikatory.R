# Beh PO: kolik verdiktu DP stoji bez jedineho hodnoceneho klicoveho indikatoru
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
sp <- "C:/Users/JONAS~1.GAI/AppData/Local/Temp/claude/f99e9ba18ad8929b/0f8282f7-6255-4065-99ed-5630cf661da9/scratchpad/"
L <- read_csv(paste0(sp, "hn_po/Data/Temp/n2k_druhy_lok.csv"), show_col_types = FALSE, col_types = cols(.default = col_character()))
k <- L %>% filter(ID_IND %in% c("POP_DYN", "POP_VITALITA3")) %>% select(kod_chu, DRUH, KOD_LOKAL, ROK, ID_IND, STAV_IND) %>%
  pivot_wider(names_from = ID_IND, values_from = STAV_IND)
o <- L %>% filter(KLIC == "ne") %>% group_by(kod_chu, DRUH, KOD_LOKAL, ROK) %>% summarise(n_sta = sum(STAV_IND %in% c("0","1")), .groups = "drop")
dp <- L %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% select(kod_chu, DRUH, KOD_LOKAL, ROK, CILMON, HOD = HOD_IND, CELKOVE_CILMON, ROK_CILMON) %>%
  left_join(k, by = c("kod_chu","DRUH","KOD_LOKAL","ROK")) %>% left_join(o, by = c("kod_chu","DRUH","KOD_LOKAL","ROK")) %>%
  mutate(n_key = (POP_DYN %in% c("0","1")) + (POP_VITALITA3 %in% c("0","1")), key_fail = POP_DYN %in% "0" | POP_VITALITA3 %in% "0",
         n_sta = coalesce(n_sta, 0L))
cat("=== DP (vsechny): verdikt x pocet hodnocenych klicovych indikatoru ===\n")
print(dp %>% count(DRUH, HOD, n_key) %>% pivot_wider(names_from = n_key, values_from = n, values_fill = 0, names_prefix = "klic_"))
cat("\n=== DP bez klicoveho i bez stanovistniho indikatoru (verdikt bez jakehokoli hodnoceni) ===\n")
print(dp %>% filter(n_key == 0, n_sta == 0) %>% count(DRUH, HOD, CILMON))
m <- dp %>% filter(CELKOVE_CILMON %in% c("dobrý", "zhoršený", "špatný"))
cat("\nshoda ROK a ROK_CILMON u DP s CELKOVE_CILMON:", round(mean(m$ROK == m$ROK_CILMON), 3), "\n")
var <- m %>% mutate(
  A = CELKOVE_CILMON,
  B = ifelse(n_key == 0, "neznámý", CELKOVE_CILMON),
  C = case_when(key_fail ~ "špatný", n_key < 2 ~ "neznámý", TRUE ~ CELKOVE_CILMON))
cat("\n=== DP s cilenym monitoringem: varianty verdiktu ===\n")
cat("A = dnes (chybejici klicovy se nehodnoti); B = zadny hodnoceny klicovy -> neznamy; C = dobry/zhorseny jen pri obou hodnocenych (kap. 5.1 'oba parametry musi byt hodnoceny jako dobre')\n")
print(var %>% pivot_longer(c(A, B, C), names_to = "varianta", values_to = "v") %>% count(DRUH, varianta, v) %>% pivot_wider(names_from = v, values_from = n, values_fill = 0))
pass <- function(d, n) ifelse(n == 0, NA, ifelse(n == 3, d >= 2, d / n >= 0.75))
evl <- var %>% pivot_longer(c(A, B, C), names_to = "varianta", values_to = "v") %>% group_by(DRUH, kod_chu, varianta) %>%
  summarise(n = sum(v %in% c("dobrý","zhoršený","špatný")), d = sum(v == "dobrý"), .groups = "drop") %>% mutate(lok_dobre = pass(d, n))
cat("\n=== EVL: agregovane lokality (75 %, pri trech dve) podle varianty ===\n")
print(evl %>% count(DRUH, varianta, lok_dobre) %>% pivot_wider(names_from = lok_dobre, values_from = n, values_fill = 0, names_prefix = "dobre_"))
saveRDS(list(dp = dp, var = var, evl = evl), paste0(sp, "klicove.rds"))

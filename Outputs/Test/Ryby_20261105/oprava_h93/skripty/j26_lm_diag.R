# Diagnoza: proc jsou DP Lampetra planeri a Misgurnus fossilis temer vsechny "neznamy".
# Vstup: beh hn_fin (kod 06a849d). Rada za lokalitu a rok ze zaznamu 21_1 (n2k_druhy.csv).
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(stringr)})
sp <- "<scratchpad>/"
D <- c("Lampetra planeri", "Misgurnus fossilis", "Cottus gobio")
z <- read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5,
              col_select = c(IDX_ND_AKCE, DRUH, KOD_LOKAL, kod_chu, ROK, CILMON, NEGATIVNI, POCET, STRUKT_POZN, POZNAMKA,
                             POP_POCETODLOV, POP_POCETODLOVZDROJ, POP_ABUNDANCENAL, POP_ABUNDANCE, POP_DELKYJEDINCI, POP_VITALITA, POP_DYN, POP_VITALITA3)) %>%
  filter(DRUH %in% D)
tag <- function(x, t) { v <- str_squish(str_match(x, paste0("<", t, ">([^<]*)</", t, ">"))[, 2]); v[!is.na(v) & v == ""] <- NA; v }
z <- z %>% mutate(plocha_p = tag(STRUKT_POZN, "plocha_prolov_p"), plocha_pbm = tag(STRUKT_POZN, "plocha_prolov_pbm"), plocha_n = tag(STRUKT_POZN, "plocha_prolov_n"),
                  delka_u = tag(STRUKT_POZN, "delka_prolov_u"), metod = tag(STRUKT_POZN, "metod_lov"), velik = tag(STRUKT_POZN, "velikosti"))
zc <- z %>% filter(CILMON == 1)
options(width = 230)
cat("=== zaznamy ciloveho monitoringu: co je vyplneno (podil zaznamu) ===\n")
print(as.data.frame(zc %>% group_by(DRUH) %>% summarise(zaznamu = n(), negativni = sum(NEGATIVNI %in% 1),
  odlov = round(mean(!is.na(POP_POCETODLOV)), 2), abundance = round(mean(!is.na(POP_ABUNDANCE)), 2),
  plocha_p = round(mean(!is.na(plocha_p)), 2), plocha_pbm = round(mean(!is.na(plocha_pbm)), 2), plocha_n = round(mean(!is.na(plocha_n)), 2),
  velikosti = round(mean(!is.na(velik)), 2), velikosti_u_pozit = round(mean(!is.na(velik[!NEGATIVNI %in% 1])), 2),
  vitalita = round(mean(!is.na(POP_VITALITA)), 2))))
cat("\n=== metoda lovu (cileny monitoring) ===\n")
print(as.data.frame(zc %>% count(DRUH, metod, sort = TRUE) %>% group_by(DRUH) %>% slice_head(n = 6)))
# rada za lokalitu a rok (vsechny zaznamy, jako 21_1)
lr <- z %>% group_by(DRUH, KOD_LOKAL, ROK) %>% summarise(cil = max(CILMON), abund = any(!is.na(POP_ABUNDANCE)), mereno = any(!is.na(POP_VITALITA)),
  pozit = any(!NEGATIVNI %in% 1), .groups = "drop")
L <- read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy_lok.csv"), show_col_types = FALSE, col_types = cols(.default = col_character())) %>% filter(DRUH %in% D)
dp <- L %>% filter(ID_IND == "CELKOVE_HODNOCENI", !is.na(CELKOVE_CILMON)) %>% transmute(DRUH, KOD_LOKAL, kod_chu, R = as.integer(ROK_CILMON), V = CELKOVE_CILMON)
dd <- dp %>% left_join(lr, by = c("DRUH", "KOD_LOKAL"), relationship = "many-to-many") %>% filter(ROK <= R) %>%
  group_by(DRUH, KOD_LOKAL, kod_chu, R, V) %>% summarise(roky = n_distinct(ROK), roky_abund = sum(abund), roky_mereno = sum(mereno),
  roky_cil = sum(cil == 1), .groups = "drop")
cat("\n=== DP s cilenym monitoringem: pocet let dat na lokalite do hodnoceneho roku ===\n")
print(as.data.frame(dd %>% count(DRUH, roky) %>% pivot_wider(names_from = roky, values_from = n, values_fill = 0, names_prefix = "let_")))
cat("\n=== ... z toho let s abundanci (POP_DYN: C/M >= 2, L >= 3) ===\n")
print(as.data.frame(dd %>% count(DRUH, roky_abund) %>% pivot_wider(names_from = roky_abund, values_from = n, values_fill = 0, names_prefix = "abund_")))
cat("\n=== ... z toho let s merenim delek (POP_VITALITA3 >= 2) ===\n")
print(as.data.frame(dd %>% count(DRUH, roky_mereno) %>% pivot_wider(names_from = roky_mereno, values_from = n, values_fill = 0, names_prefix = "mereno_")))
cat("\n=== pricina neznameho POP_DYN: chybi roky, nebo roky jsou, ale bez abundance? ===\n")
prah <- function(d) ifelse(d == "Lampetra planeri", 3, 2)
print(as.data.frame(dd %>% mutate(dyn_ok = roky_abund >= prah(DRUH), roky_ok = roky >= prah(DRUH)) %>% count(DRUH, roky_ok, dyn_ok)))
saveRDS(list(z = z, dd = dd, lr = lr), paste0(sp, "lm_diag.rds"))

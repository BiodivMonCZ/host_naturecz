# Audit neznamych klicovych indikatoru u verdiktu z ciloveho monitoringu (CELKOVE_CILMON).
# Pro kazdou DP a neznamy klicovy indikator urci pricinu:
#   A prvni pruzkum - na lokalite neni drivejsi pruzkum a ani v okoli 100 m v teze EVL
#   B tataz mista pod jinym klicem - drivejsi pruzkum druhu do 100 m v teze EVL, ale jiny KOD_LOKAL
#   C chybi vstup - drivejsi pruzkumy na lokalite jsou, ale bez potrebne hodnoty
#   D jine
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
sp <- "<scratchpad>/"
ex <- readRDS(paste0(sp, "export_5druhu.rds")) %>% select(ID_ND_NALEZ, X, Y)
beh <- list(ryby = "hn_exp", oboj = "hn_fin")
druhy <- list(ryby = c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis"), oboj = c("Triturus cristatus", "Bombina bombina"))
vys <- list(); det <- list()
for (g in names(beh)) {
  d <- beh[[g]]
  rec <- read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5,
                  col_select = c(ID_ND_NALEZ, DRUH, KOD_LOKAL, kod_chu, ROK, CILMON, NEGATIVNI, any_of(c("POP_ABUNDANCE", "POP_VITALITA", "POP_POCETNOST", "POCET", "POCITANO_CLEAN", "REL_POC")))) %>%
    filter(DRUH %in% druhy[[g]]) %>% distinct(ID_ND_NALEZ, DRUH, .keep_all = TRUE) %>%
    mutate(ID_ND_NALEZ = as.character(ID_ND_NALEZ)) %>% left_join(ex, by = "ID_ND_NALEZ")
  lim <- read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy_lim.csv"), show_col_types = FALSE, guess_max = 1e5, col_types = cols(.default = col_character()),
                  col_select = c(DRUH, KOD_LOKAL, kod_chu, ROK, CILMON, ID_IND, STAV_IND, KLIC, UROVEN, LIM_IND)) %>% filter(DRUH %in% druhy[[g]])
  klic <- lim %>% filter(KLIC %in% "ano", UROVEN %in% "lok", !is.na(LIM_IND)) %>% distinct(DRUH, ID_IND)
  lok <- read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy_lok.csv"), show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
    filter(DRUH %in% druhy[[g]], ID_IND == "CELKOVE_HODNOCENI", !is.na(CELKOVE_CILMON)) %>%
    distinct(DRUH, kod_chu, KOD_LOKAL, R = as.integer(ROK_CILMON), V = CELKOVE_CILMON)
  # stav klicovych indikatoru v cilenem behu: zaznamy CILMON == 1 v roce R
  st <- lim %>% filter(CILMON == "1") %>% inner_join(klic, by = c("DRUH", "ID_IND")) %>%
    mutate(R = as.integer(ROK), s = suppressWarnings(as.numeric(STAV_IND))) %>%
    group_by(DRUH, kod_chu, KOD_LOKAL, R, ID_IND) %>% summarise(s = if (all(is.na(s))) NA_real_ else max(s, na.rm = TRUE), .groups = "drop")
  x <- lok %>% left_join(st, by = c("DRUH", "kod_chu", "KOD_LOKAL", "R")) %>% filter(is.na(s))
  # roky na lokalite a v okoli
  roky_lok <- rec %>% group_by(DRUH, KOD_LOKAL) %>% summarise(r_any = list(sort(unique(ROK))),
    r_cil = list(sort(unique(ROK[CILMON == 1]))),
    r_ab = list(sort(unique(ROK[!is.na(.data[[if (g == "ryby") "POP_ABUNDANCE" else "POP_POCETNOST"]])]))),
    r_vit = if (g == "ryby") list(sort(unique(ROK[!is.na(POP_VITALITA)]))) else list(integer(0)),
    x = median(X, na.rm = TRUE), y = median(Y, na.rm = TRUE), .groups = "drop")
  okoli <- function(dr, ch, kl, xx, yy, R, jen_cil) {
    if (is.na(xx)) return(NA)
    o <- rec %>% filter(DRUH == dr, kod_chu == ch, KOD_LOKAL != kl, ROK < R, !is.na(X)) %>% filter(!jen_cil | CILMON == 1)
    any(sqrt((o$X - xx)^2 + (o$Y - yy)^2) <= 100)
  }
  x <- x %>% left_join(roky_lok, by = c("DRUH", "KOD_LOKAL")) %>% rowwise() %>% mutate(
    pred_any = sum(unlist(r_any) < R), pred_cil = sum(unlist(r_cil) < R), pred_ab = sum(unlist(r_ab) < R),
    let_ab = sum(unlist(r_ab) <= R), let_vit = sum(unlist(r_vit) <= R), let_any = sum(unlist(r_any) <= R),
    soused = okoli(DRUH, kod_chu, KOD_LOKAL, x, y, R, g == "oboj")) %>% ungroup()
  prah <- function(dr) ifelse(dr == "Lampetra planeri", 3, 2)
  x <- x %>% mutate(pricina = case_when(
    ID_IND == "POP_DYN" & let_any < prah(DRUH) & soused %in% TRUE ~ "B tataz mista pod jinym klicem",
    ID_IND == "POP_DYN" & let_any < prah(DRUH) ~ "A malo pruzkumu na lokalite",
    ID_IND == "POP_DYN" & let_ab < prah(DRUH) ~ "C chybi abundance (pocet / plocha)",
    ID_IND == "POP_VITALITA3" & let_any < 2 & soused %in% TRUE ~ "B tataz mista pod jinym klicem",
    ID_IND == "POP_VITALITA3" & let_any < 2 ~ "A malo pruzkumu na lokalite",
    ID_IND == "POP_VITALITA3" & let_vit < 2 ~ "C chybi delky",
    ID_IND == "POP_ZMENARAD" & pred_cil == 0 & soused %in% TRUE ~ "B tataz mista pod jinym klicem",
    ID_IND == "POP_ZMENARAD" & pred_cil == 0 ~ "A prvni cileny pruzkum na lokalite",
    ID_IND == "POP_ZMENARAD" ~ "C chybi kategorie pocetnosti (v roce nebo v referenci)",
    TRUE ~ "D jine"))
  det[[g]] <- x
  vys[[g]] <- x %>% count(DRUH, ID_IND, pricina)
  cat("\n=== ", g, ": DP s verdiktem z ciloveho monitoringu a pocty neznamych klicovych indikatoru ===\n")
  print(as.data.frame(lok %>% count(DRUH, name = "DP_cilmon") %>% left_join(x %>% distinct(DRUH, KOD_LOKAL, kod_chu) %>% count(DRUH, name = "DP_s_neznamym"), by = "DRUH")))
}
options(width = 200)
cat("\n=== PRICINY ===\n"); print(as.data.frame(bind_rows(vys) %>% arrange(DRUH, ID_IND, pricina)))
saveRDS(det, paste0(sp, "audit_nezname.rds"))

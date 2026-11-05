# Simulace: kolik DP s cilenym monitoringem by melo hodnotitelne OBA klicove indikatory
# (POP_DYN a POP_VITALITA3) pri jednotlivych upravach. Rada let = jako 21_1 (vsechny zaznamy lokality).
#  A  abundance = 0, je-li pocet 0 (negativni zaznam bez plochy)         - oprava
#  B  Misgurnus: dynamika z poctu (1. + 2. odlov), ne z hustoty           - volba (A-3)
#  C  Lampetra 2018-2020: pocet larev z delek v POZNAMKA ("Min: 40(8x)") - volba
#  D  dynamika uz od 2 let i u Lampetra / vitalita uz z 1 pruzkumu        - jen pro srovnani (A-1, A-4)
suppressPackageStartupMessages({library(dplyr); library(stringr); library(tidyr)})
sp <- "<scratchpad>/"
x <- readRDS(paste0(sp, "lm_diag.rds")); z <- x$z; dp <- x$dd %>% select(DRUH, KOD_LOKAL, kod_chu, R, V)
min_pocet <- function(p) {
  s <- str_match(p, "Min\\.?:\\s*([0-9(),x \\s]+)")[, 2]
  vapply(s, function(t) {
    if (is.na(t)) return(NA_real_)
    it <- str_match_all(t, "([0-9]+)\\s*(?:\\((\\d+)x\\))?")[[1]]
    if (nrow(it) == 0) return(NA_real_)
    sum(ifelse(is.na(it[, 3]), 1, as.numeric(it[, 3])))
  }, numeric(1), USE.NAMES = FALSE)
}
z <- z %>% mutate(
  ab0 = !is.na(POP_ABUNDANCE),
  abA = ab0 | (POP_POCETODLOV %in% 0),
  abB = if_else(DRUH == "Misgurnus fossilis", !is.na(POP_POCETODLOV), abA),
  minC = if_else(DRUH == "Lampetra planeri" & is.na(POP_POCETODLOV) & !NEGATIVNI %in% 1, min_pocet(POZNAMKA), NA_real_),
  abC = abA | (!is.na(minC) & !is.na(plocha_p)),
  abBC = abB | (!is.na(minC) & !is.na(plocha_p)),
  mer = !is.na(POP_VITALITA))
cat("Lampetra: zaznamu bez poctu, z nichz POZNAMKA da pocet larev:", sum(!is.na(z$minC)), "z",
    sum(z$DRUH == "Lampetra planeri" & is.na(z$POP_POCETODLOV) & !z$NEGATIVNI %in% 1), "\n")
lr <- z %>% group_by(DRUH, KOD_LOKAL, ROK) %>% summarise(across(c(ab0, abA, abB, abC, abBC, mer), any), .groups = "drop")
s <- dp %>% left_join(lr, by = c("DRUH", "KOD_LOKAL"), relationship = "many-to-many") %>% filter(ROK <= R) %>% arrange(ROK) %>%
  group_by(DRUH, KOD_LOKAL, kod_chu, R, V) %>%
  summarise(across(c(ab0, abA, abB, abC, abBC), sum), mer = sum(mer), .groups = "drop")
prah <- function(d) ifelse(d == "Lampetra planeri", 3, 2)
v <- s %>% mutate(vit = mer >= 2, vit1 = mer >= 1,
  dnes = ab0 >= prah(DRUH) & vit, A = abA >= prah(DRUH) & vit, A_B = abB >= prah(DRUH) & vit,
  A_C = abC >= prah(DRUH) & vit, A_B_C = abBC >= prah(DRUH) & vit,
  D_dyn2 = abBC >= 2 & vit, D_dyn2_vit1 = abBC >= 2 & vit1)
options(width = 220)
cat("\n=== DP s cilenym monitoringem, ktere maji hodnotitelne OBA klicove indikatory ===\n")
print(as.data.frame(v %>% group_by(DRUH) %>% summarise(DP = n(), across(c(dnes, A, A_B, A_C, A_B_C, D_dyn2, D_dyn2_vit1), sum))))
cat("\n=== jen dynamika hodnotitelna ===\n")
print(as.data.frame(s %>% group_by(DRUH) %>% summarise(DP = n(), dnes = sum(ab0 >= prah(DRUH)), A = sum(abA >= prah(DRUH)), A_B = sum(abB >= prah(DRUH)),
  A_C = sum(abC >= prah(DRUH)), A_B_C = sum(abBC >= prah(DRUH)), dyn_od_2_let = sum(abBC >= 2))))
cat("\n=== jen vitalita hodnotitelna (>= 2 / >= 1 mereny pruzkum) ===\n")
print(as.data.frame(s %>% group_by(DRUH) %>% summarise(DP = n(), vit2 = sum(mer >= 2), vit1 = sum(mer >= 1))))

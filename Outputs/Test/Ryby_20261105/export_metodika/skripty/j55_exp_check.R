suppressPackageStartupMessages({library(dplyr); library(readr)})
sp <- "<scratchpad>/"
L <- function(d) read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy_lok.csv"), show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
  filter(DRUH %in% c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis"))
a <- L("hn_pocet"); b <- L("hn_exp")
for (ind in c("POP_DYN", "POP_VITALITA3", "STA_MIGBARVYS", "POP_PRESENCE", "CELKOVE_HODNOCENI")) {
  j <- full_join(a %>% filter(ID_IND == ind) %>% select(DRUH, kod_chu, KOD_LOKAL, h1 = HOD_IND, s1 = STAV_IND, c1 = CELKOVE_CILMON),
                 b %>% filter(ID_IND == ind) %>% select(DRUH, kod_chu, KOD_LOKAL, h2 = HOD_IND, s2 = STAV_IND, c2 = CELKOVE_CILMON),
                 by = c("DRUH", "kod_chu", "KOD_LOKAL"))
  cat(sprintf("%-18s radku %4d | hodnota jina %d | stav jiny %d | chybi v jednom %d\n", ind, nrow(j),
              sum(!mapply(identical, j$h1, j$h2)), sum(!mapply(identical, j$s1, j$s2)), sum(is.na(j$h1) & is.na(j$s1) & !is.na(j$h2) | is.na(j$h2) & is.na(j$s2) & !is.na(j$h1))))
}
C <- function(d) read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy_chu.csv"), show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
  filter(DRUH %in% c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis"), ID_IND %in% c("STA_MIGPROSTUP", "LOK_PROCDOBR", "CELKOVE_HODNOCENI"))
k <- full_join(C("hn_pocet") %>% select(DRUH, kod_chu, ID_IND, h1 = HOD_IND, s1 = STAV_IND), C("hn_exp") %>% select(DRUH, kod_chu, ID_IND, h2 = HOD_IND, s2 = STAV_IND),
               by = c("DRUH", "kod_chu", "ID_IND"))
cat("\nEVL: radku", nrow(k), "| hodnota jina", sum(!mapply(identical, k$h1, k$h2)), "| stav jiny", sum(!mapply(identical, k$s1, k$s2)), "\n")
print(k %>% count(ID_IND, znamo = !is.na(s2)))

args <- commandArgs(TRUE); a <- args[1]; b <- args[2]
suppressMessages(library(dplyr))
rd <- function(d, f) readr::read_csv(file.path(d, "temp", f), col_types = readr::cols(.default = "c"), show_col_types = FALSE) %>% filter(DRUH == "Eriogaster catax")
for (f in c("n2k_druhy_lok.csv", "n2k_druhy_chu.csv")) {
  la <- rd(a, f); lb <- rd(b, f)
  cat("\n==", f, ": radku pred", nrow(la), "po", nrow(lb), "\n")
  cat("CELKOVE_HODNOCENI pred:\n"); print(table(la$HOD_IND[la$ID_IND == "CELKOVE_HODNOCENI"], useNA = "ifany"))
  cat("CELKOVE_HODNOCENI po:\n"); print(table(lb$HOD_IND[lb$ID_IND == "CELKOVE_HODNOCENI"], useNA = "ifany"))
  k <- intersect(c("kod_chu", "KOD_LOKAL"), names(la))
  j <- full_join(la %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% select(all_of(k), pred = HOD_IND),
                 lb %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% select(all_of(k), po = HOD_IND), by = k)
  cat("prechody:\n"); print(count(j, pred, po))
  for (ind in c("POP_POCET", "STA_HABPOKRYV")) {
    cat(ind, "pred:", nrow(la[la$ID_IND == ind, ]), "radku; po:", nrow(lb[lb$ID_IND == ind, ]), "radku; STAV_IND po:\n")
    print(table(lb$STAV_IND[lb$ID_IND == ind], useNA = "ifany"))
  }
}

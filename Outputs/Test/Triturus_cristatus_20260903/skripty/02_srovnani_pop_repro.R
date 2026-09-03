suppressPackageStartupMessages({library(readr); library(dplyr)})
sc <- Sys.getenv("SCRATCH")
rd <- function(f) suppressWarnings(read_csv(f, col_types = cols(.default = "c"), progress = FALSE))

for (uroven in c("lok", "chu")) {
  fa <- file.path(sc, "pred", paste0("n2k_druhy_", uroven, ".csv"))
  fb <- file.path(sc, "po",   paste0("n2k_druhy_", uroven, ".csv"))
  a <- rd(fa); b <- rd(fb)
  cat("\n############ UROVEN ", toupper(uroven), " ############\n", sep = "")
  cat("radku pred:", nrow(a), "  po:", nrow(b), "\n\n")

  cat("--- indikatory ve vystupu ---\n")
  ia <- a %>% count(ID_IND, name = "pred"); ib <- b %>% count(ID_IND, name = "po")
  print(full_join(ia, ib, by = "ID_IND") %>% arrange(ID_IND) %>% as.data.frame(), row.names = FALSE)

  kl <- if (uroven == "lok") c("kod_chu","DRUH","KOD_LOKAL","ROK") else c("kod_chu","DRUH")
  ca <- a %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% select(all_of(kl), HOD_pred = HOD_IND, STAV_pred = STAV_IND)
  cb <- b %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% select(all_of(kl), HOD_po = HOD_IND, STAV_po = STAV_IND)
  cat("\n--- rozdeleni CELKOVE_HODNOCENI ---\n")
  print(full_join(count(ca, HOD_pred, name = "pred"), count(cb, HOD_po, name = "po"),
                  by = c("HOD_pred" = "HOD_po")) %>% rename(hodnoceni = HOD_pred) %>% as.data.frame(), row.names = FALSE)

  j <- full_join(ca, cb, by = kl)
  zm <- j %>% filter(!identical(TRUE, NA) & (is.na(HOD_pred) != is.na(HOD_po) |
                     (!is.na(HOD_pred) & !is.na(HOD_po) & HOD_pred != HOD_po)))
  cat("\nporovnanych jednotek:", nrow(j), "   ZMENENYCH VERDIKTU:", nrow(zm), "\n")
  if (nrow(zm)) print(head(as.data.frame(zm), 20), row.names = FALSE)

  if (uroven == "lok" && "CELKOVE_SUM" %in% a$ID_IND) {
    sa <- a %>% filter(ID_IND == "CELKOVE_SUM") %>% select(all_of(kl), S_pred = HOD_IND)
    sb <- b %>% filter(ID_IND == "CELKOVE_SUM") %>% select(all_of(kl), S_po = HOD_IND)
    js <- full_join(sa, sb, by = kl)
    zs <- js %>% filter(is.na(S_pred) != is.na(S_po) | (!is.na(S_pred) & !is.na(S_po) & S_pred != S_po))
    cat("CELKOVE_SUM - porovnano:", nrow(js), "  zmeneno:", nrow(zs), "\n")
  }
}

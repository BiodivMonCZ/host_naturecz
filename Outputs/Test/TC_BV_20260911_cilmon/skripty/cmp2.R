suppressPackageStartupMessages({ library(tidyverse); library(data.table) })
OUT <- Sys.getenv("SCR")
b <- readRDS(file.path(OUT,"base.rds")); f <- readRDS(file.path(OUT,"fix.rds"))
lim <- readr::read_csv("Data/Temp/n2k_druhy_lim.csv", show_col_types = FALSE)
fc <- f$lok %>% dplyr::filter(ID_IND == "CELKOVE_HODNOCENI")

cat("=== A. DP, kde je vitezna navsteva CILMON=0, ale cileny verdikt existuje ===\n")
zvl <- fc %>% dplyr::filter(CILMON == 0, !is.na(CELKOVE_CILMON))
print(as.data.frame(zvl %>% dplyr::select(kod_chu, DRUH, KOD_LOKAL, ROK, CILMON, HOD_IND, CELKOVE_CILMON, ROK_CILMON)))
cat("\n-- roky a CILMON techto DP ve vstupnich datech --\n")
for (i in seq_len(nrow(zvl))) {
  z <- zvl[i, ]
  cat("\n*", z$kod_chu, z$DRUH, z$KOD_LOKAL, " vitez ROK =", z$ROK, "\n")
  print(as.data.frame(lim %>%
    dplyr::filter(kod_chu == z$kod_chu, DRUH == z$DRUH, KOD_LOKAL == z$KOD_LOKAL) %>%
    dplyr::count(ROK, CILMON, name = "zaznamu")))
}

cat("\n=== B. DP, kde se cileny verdikt lisi od plneho ===\n")
print(as.data.frame(fc %>% dplyr::filter(!is.na(CELKOVE_CILMON), HOD_IND != CELKOVE_CILMON) %>%
  dplyr::select(kod_chu, DRUH, KOD_LOKAL, ROK, CILMON, HOD_IND, CELKOVE_CILMON, ROK_CILMON)))

cat("\n=== C. DP, kde se lisi rok vitezne navstevy ===\n")
print(as.data.frame(fc %>% dplyr::filter(!is.na(CELKOVE_CILMON), ROK != ROK_CILMON) %>%
  dplyr::select(kod_chu, DRUH, KOD_LOKAL, ROK, CILMON, HOD_IND, ROK_CILMON, CELKOVE_CILMON)))

cat("\n=== D. NEZNAME EVL (fix) - rozbor priciny ===\n")
verd <- function(x) x$chu %>% dplyr::filter(ID_IND=="CELKOVE_HODNOCENI") %>%
  dplyr::transmute(kod_chu, DRUH, VERDIKT = dplyr::coalesce(HOD_IND, "neznámý"))
dilc <- f$chu %>% dplyr::filter(ID_IND %in% c("LOK_DILCPOCET","LOK_PROCDOBR")) %>%
  dplyr::select(kod_chu, DRUH, ID_IND, HOD_IND) %>%
  tidyr::pivot_wider(names_from = ID_IND, values_from = HOD_IND)
dpstat <- fc %>% dplyr::group_by(kod_chu, DRUH) %>% dplyr::summarise(
  DP_verdikt = dplyr::n(),
  DP_cilene  = sum(!is.na(CELKOVE_CILMON)),
  DP_flag_cilmon1 = sum(CILMON == 1, na.rm = TRUE), .groups = "drop")
nz <- verd(f) %>% dplyr::left_join(dilc, by=c("kod_chu","DRUH")) %>%
  dplyr::left_join(dpstat, by=c("kod_chu","DRUH")) %>%
  dplyr::mutate(dplyr::across(c(DP_verdikt, DP_cilene, DP_flag_cilmon1), ~tidyr::replace_na(.x, 0L)))
cat("\n-- verdikt x existence DP --\n")
print(as.data.frame(nz %>% dplyr::count(DRUH, VERDIKT,
    zadna_DP = DP_verdikt == 0, zadna_cilena_DP = DP_cilene == 0, name="EVL")))
cat("\n-- neznamé: LOK_DILCPOCET a LOK_PROCDOBR --\n")
print(as.data.frame(nz %>% dplyr::filter(VERDIKT=="neznámý") %>%
    dplyr::count(DRUH, DILCPOCET = LOK_DILCPOCET, PROCDOBR_NA = is.na(LOK_PROCDOBR), name="EVL")))
cat("\n-- neznamé: kolik DP verdiktu se zahazuje --\n")
print(as.data.frame(nz %>% dplyr::filter(VERDIKT=="neznámý") %>% dplyr::summarise(.by=DRUH,
    EVL=dplyr::n(), bez_jakekoli_DP=sum(DP_verdikt==0),
    DP_verdiktu_zahozeno=sum(DP_verdikt))))
cat("\n-- zmena poctu neznamych base vs fix --\n")
print(dplyr::full_join(verd(b) %>% dplyr::rename(base=VERDIKT), verd(f) %>% dplyr::rename(fix=VERDIKT),
      by=c("kod_chu","DRUH")) %>% dplyr::count(DRUH, base=="neznámý", fix=="neznámý"))

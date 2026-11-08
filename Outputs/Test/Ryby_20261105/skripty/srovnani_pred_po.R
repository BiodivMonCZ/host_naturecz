# Srovnani behu PRED (zive limity a kod 2f40b14) a PO (limity_ryby_2026 + patch) pro tri druhy ryb.
# Vstup: Data/Temp obou izolovanych kopii. Vystup: CSV do OUT a souhrn do konzole.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(stringr)})
sp <- "C:/Users/JONAS~1.GAI/AppData/Local/Temp/claude/f99e9ba18ad8929b/0f8282f7-6255-4065-99ed-5630cf661da9/scratchpad/"
OUT <- Sys.getenv("SROV_OUT", unset = paste0(sp, "srovnani"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
ss <- openxlsx::read.xlsx("C:/Users/jonas.gaigr/Documents/host_naturecz/Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx", sheet = 1)
names(ss)[c(1,8)] <- c("site_code","nazev_lat"); par <- paste(ss$nazev_lat, ss$site_code)
cti <- function(beh, f) read_csv(paste0(sp, beh, "/Data/Temp/", f), show_col_types = FALSE, guess_max = 1e5, col_types = cols(.default = col_character()))
L <- list(pred = cti("hn_pred", "n2k_druhy_lok.csv"), po = cti("hn_po", "n2k_druhy_lok.csv"))
C <- list(pred = cti("hn_pred", "n2k_druhy_chu.csv"), po = cti("hn_po", "n2k_druhy_chu.csv"))
ver <- function(x) factor(x, levels = c("dobrý", "zhoršený", "špatný", "neznámý", "nehodnoceno"))

# --- uroven DP: verdikty ---
dp <- bind_rows(lapply(names(L), function(b) L[[b]] %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>%
  transmute(beh = b, DRUH, kod_chu, KOD_LOKAL, ROK, CILMON, HOD_IND, CELKOVE_CILMON, PO = paste(DRUH, kod_chu) %in% par)))
cat("=== DP: pocet lokalit a verdikty (vsechny DP / jen dvojice PO) ===\n")
t1 <- dp %>% group_by(beh, DRUH) %>% summarise(DP = n(), DP_PO = sum(PO), dobry = sum(HOD_IND == "dobrý"), zhorseny = sum(HOD_IND == "zhoršený"),
  spatny = sum(HOD_IND == "špatný"), jine = sum(!HOD_IND %in% c("dobrý","zhoršený","špatný")), cilmon_hodnoceno = sum(CELKOVE_CILMON %in% c("dobrý","zhoršený","špatný")),
  .groups = "drop") %>% arrange(DRUH, desc(beh))
print(t1); write_csv(t1, file.path(OUT, "dp_verdikty.csv"))
# --- uroven DP: indikatory (STAV) ---
ind <- bind_rows(lapply(names(L), function(b) L[[b]] %>% filter(ID_IND != "CELKOVE_HODNOCENI") %>%
  transmute(beh = b, DRUH, ID_IND, TYP_IND, KLIC, STAV = case_when(STAV_IND == "1" ~ "splněno", STAV_IND == "0" ~ "nesplněno", TRUE ~ "NA"))))
t2 <- ind %>% count(beh, DRUH, ID_IND, TYP_IND, KLIC, STAV) %>% pivot_wider(names_from = STAV, values_from = n, values_fill = 0) %>% arrange(DRUH, ID_IND, desc(beh))
cat("\n=== DP: indikatory ===\n"); print(as.data.frame(t2), right = FALSE); write_csv(t2, file.path(OUT, "dp_indikatory.csv"))
# --- informativni hodnoty PO ---
info <- L$po %>% filter(ID_IND %in% c("POP_POCETODLOVZDROJ", "STA_PODMINKYODLOVU")) %>% count(DRUH, ID_IND, HOD_IND, sort = TRUE)
cat("\n=== PO: zdroj poctu a podminky odlovu (DP) ===\n"); print(as.data.frame(info), right = FALSE); write_csv(info, file.path(OUT, "po_info.csv"))
# --- uroven EVL ---
evl <- bind_rows(lapply(names(C), function(b) C[[b]] %>% filter(ID_IND == "CELKOVE_HODNOCENI", paste(DRUH, kod_chu) %in% par) %>%
  transmute(beh = b, DRUH, kod_chu, HOD = STAV_IND)))
t3 <- evl %>% pivot_wider(names_from = beh, values_from = HOD) %>% mutate(pred = coalesce(pred, "(bez výstupu)"), po = coalesce(po, "(bez výstupu)"))
cat("\n=== EVL (dvojice PO): prechody pred -> po ===\n"); print(t3 %>% count(DRUH, pred, po)); write_csv(t3, file.path(OUT, "evl_verdikty.csv"))
chu_ind <- C$po %>% filter(paste(DRUH, kod_chu) %in% par, ID_IND %in% c("LOK_PROCDOBR", "STA_MIGPROSTUP")) %>% select(DRUH, kod_chu, ID_IND, HOD_IND, STAV_IND)
cat("\n=== PO: indikatory EVL ===\n"); print(chu_ind %>% count(DRUH, ID_IND, STAV_IND))
write_csv(chu_ind, file.path(OUT, "po_evl_indikatory.csv"))
# --- migrace: souhrnna statistika (rozhodnuti Z-5) ---
mig_lok <- L$po %>% filter(ID_IND == "STA_MIGBARVYS", CILMON == "1") %>% mutate(v = suppressWarnings(as.numeric(HOD_IND))) %>%
  left_join(tibble(DRUH = c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis"), lim = c(10, 5, 10)), by = "DRUH")
cat("\n=== PO: migrace - lokality (reprezentativni navsteva, cileny monitoring) ===\n")
t4 <- mig_lok %>% group_by(DRUH) %>% summarise(lokalit = n(), vyska_znama = sum(!is.na(v)), nad_limitem = sum(v > lim, na.rm = TRUE), do_limitu = sum(v <= lim, na.rm = TRUE), neznamo = sum(is.na(v)))
print(t4); write_csv(t4, file.path(OUT, "po_migrace_lokality.csv"))
t5 <- chu_ind %>% filter(ID_IND == "STA_MIGPROSTUP") %>% count(DRUH, stav = STAV_IND)
cat("\n=== PO: migrace - EVL ===\n"); print(t5); write_csv(t5, file.path(OUT, "po_migrace_evl.csv"))
saveRDS(list(t1 = t1, t2 = t2, t3 = t3, t4 = t4, t5 = t5, info = info, chu_ind = chu_ind), file.path(OUT, "srovnani.rds"))

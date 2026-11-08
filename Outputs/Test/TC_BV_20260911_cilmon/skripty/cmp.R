suppressPackageStartupMessages({ library(tidyverse); library(data.table) })
OUT <- Sys.getenv("SCR")
b <- readRDS(file.path(OUT, "base.rds")); f <- readRDS(file.path(OUT, "fix.rds"))

cat("=========== 1. UROVEN DP: zmenilo se neco? ===========\n")
bl <- b$lok %>% dplyr::select(-dplyr::any_of(c("CELKOVE_CILMON","ROK_CILMON")))
fl <- f$lok %>% dplyr::select(-dplyr::any_of(c("CELKOVE_CILMON","ROK_CILMON")))
cat("radku base:", nrow(bl), " fix:", nrow(fl), "\n")
cat("identicke (po serazeni sloupcu):",
    isTRUE(all.equal(as.data.frame(bl), as.data.frame(fl[, names(bl)]), check.attributes = FALSE)), "\n")

cat("\n=========== 2. Novy sloupec CELKOVE_CILMON ===========\n")
fc <- f$lok %>% dplyr::filter(ID_IND == "CELKOVE_HODNOCENI")
cat("DP-verdiktu celkem:", nrow(fc), "\n")
print(fc %>% dplyr::count(DRUH, CILMON, CELKOVE_CILMON = tidyr::replace_na(CELKOVE_CILMON, "(NA)")) %>%
        tidyr::pivot_wider(names_from = CELKOVE_CILMON, values_from = n, values_fill = 0))
cat("\n-- kde se lisi verdikt DP (vse) od verdiktu z cileneho monitoringu --\n")
print(fc %>% dplyr::filter(!is.na(CELKOVE_CILMON), HOD_IND != CELKOVE_CILMON) %>%
        dplyr::count(DRUH, HOD_IND, CELKOVE_CILMON))
cat("\n-- rok viteznne navstevy: shoduje se ROK a ROK_CILMON? --\n")
print(fc %>% dplyr::filter(!is.na(CELKOVE_CILMON)) %>%
        dplyr::count(DRUH, shoda_roku = ROK == ROK_CILMON))

cat("\n=========== 3. LOK_DILCPOCET / LOK_PROCDOBR na EVL ===========\n")
pick <- function(x, tag) x$chu %>%
  dplyr::filter(ID_IND %in% c("LOK_DILCPOCET","LOK_DILCDOBRE","LOK_PROCDOBR","CELKOVE_HODNOCENI")) %>%
  dplyr::select(kod_chu, DRUH, ID_IND, HOD_IND, STAV_IND) %>%
  dplyr::mutate(beh = tag)
cmp <- dplyr::full_join(
  pick(b,"base") %>% dplyr::select(-beh) %>% dplyr::rename(HOD_base=HOD_IND, STAV_base=STAV_IND),
  pick(f,"fix")  %>% dplyr::select(-beh) %>% dplyr::rename(HOD_fix=HOD_IND,  STAV_fix=STAV_IND),
  by = c("kod_chu","DRUH","ID_IND"))
print(cmp %>% dplyr::group_by(ID_IND) %>%
        dplyr::summarise(n = dplyr::n(),
                         zmeneno = sum(!identical(TRUE, TRUE) | (dplyr::coalesce(HOD_base,"@") != dplyr::coalesce(HOD_fix,"@"))),
                         .groups="drop"))
cat("\n-- zmenene radky --\n")
print(cmp %>% dplyr::filter(dplyr::coalesce(HOD_base,"@") != dplyr::coalesce(HOD_fix,"@")) %>% as.data.frame(), max = 400)

cat("\n=========== 4. VERDIKTY EVL ===========\n")
vb <- b$chu %>% dplyr::filter(ID_IND=="CELKOVE_HODNOCENI") %>% dplyr::select(kod_chu, DRUH, base=HOD_IND)
vf <- f$chu %>% dplyr::filter(ID_IND=="CELKOVE_HODNOCENI") %>% dplyr::select(kod_chu, DRUH, fix=HOD_IND)
v  <- dplyr::full_join(vb, vf, by=c("kod_chu","DRUH"))
cat("-- base --\n"); print(table(v$DRUH, v$base, useNA="ifany"))
cat("-- fix  --\n"); print(table(v$DRUH, v$fix,  useNA="ifany"))
cat("\n-- prechody --\n")
print(v %>% dplyr::filter(dplyr::coalesce(base,"@") != dplyr::coalesce(fix,"@")) %>% dplyr::count(DRUH, base, fix))

cat("\n=========== 5. Neznamé EVL: proc ===========\n")
nez <- v %>% dplyr::filter(fix == "neznámý")
dp  <- f$chu %>% dplyr::filter(ID_IND=="LOK_DILCPOCET") %>% dplyr::select(kod_chu, DRUH, DILCPOCET=HOD_IND)
pr  <- f$chu %>% dplyr::filter(ID_IND=="LOK_PROCDOBR") %>% dplyr::select(kod_chu, DRUH, PROCDOBR=HOD_IND)
nez <- nez %>% dplyr::left_join(dp, by=c("kod_chu","DRUH")) %>% dplyr::left_join(pr, by=c("kod_chu","DRUH"))
print(nez %>% dplyr::count(DRUH, DILCPOCET = tidyr::replace_na(DILCPOCET,"(NA)"),
                           PROCDOBR_NA = is.na(PROCDOBR)))
# maji tato uzemi vubec nejake DP s verdiktem?
dpv <- f$lok %>% dplyr::filter(ID_IND=="CELKOVE_HODNOCENI") %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::summarise(n_DP = dplyr::n(),
                   n_DP_cil = sum(!is.na(CELKOVE_CILMON)),
                   .groups="drop")
nez2 <- nez %>% dplyr::left_join(dpv, by=c("kod_chu","DRUH"))
cat("\n-- neznamé EVL: pocet DP s verdiktem vs. z cileneho monitoringu --\n")
print(nez2 %>% dplyr::summarise(.by = DRUH,
        EVL = dplyr::n(),
        bez_DP_uplne = sum(dplyr::coalesce(n_DP,0L) == 0),
        ma_DP_ale_zadnou_cilenou = sum(dplyr::coalesce(n_DP,0L) > 0 & dplyr::coalesce(n_DP_cil,0L) == 0),
        zahozenych_DP_verdiktu = sum(dplyr::coalesce(n_DP,0L))))

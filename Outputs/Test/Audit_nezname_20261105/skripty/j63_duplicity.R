# DP s jedinym cilenym pruzkumem (pricina A): byl totez misto (do 100 m, tataz EVL) pruzkoumano
# POZDEJI pod jinym klicem? Pak je DP duplikat pokracujici lokality, ne skutecne "prvni pruzkum".
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(stringr)})
sp <- "<scratchpad>/"
det <- readRDS(paste0(sp, "audit_nezname.rds"))
ex <- readRDS(paste0(sp, "export_5druhu.rds")) %>% select(ID_ND_NALEZ, X, Y, KOD_LOKALITY, LOKALITA, ZDROJ)
typ <- function(k) case_when(str_detect(k, "^CZ\\d{7}$") ~ "kod EVL", str_detect(k, "^CZ\\d{7}_?\\d+$") ~ "kod EVL + cislo DP",
                             str_detect(k, "^amp") ~ "amp-kod", TRUE ~ "nazev / jiny")
out <- list()
for (g in names(det)) {
  d <- if (g == "ryby") "hn_exp" else "hn_fin"
  rec <- read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5,
                  col_select = c(ID_ND_NALEZ, DRUH, KOD_LOKAL, kod_chu, ROK, CILMON)) %>%
    distinct(ID_ND_NALEZ, DRUH, .keep_all = TRUE) %>% mutate(ID_ND_NALEZ = as.character(ID_ND_NALEZ)) %>% left_join(ex, by = "ID_ND_NALEZ") %>%
    filter(CILMON == 1, !is.na(X))
  a <- det[[g]] %>% filter(str_starts(pricina, "A")) %>% distinct(DRUH, kod_chu, KOD_LOKAL, R, x, y)
  r <- a %>% rowwise() %>% do({
    aa <- .
    o <- rec %>% filter(DRUH == aa$DRUH, kod_chu == aa$kod_chu, KOD_LOKAL != aa$KOD_LOKAL) %>%
      mutate(dist = sqrt((X - aa$x)^2 + (Y - aa$y)^2)) %>% filter(dist <= 100)
    pozd <- o %>% filter(ROK > aa$R)
    tibble(DRUH = aa$DRUH, kod_chu = aa$kod_chu, KOD_LOKAL = aa$KOD_LOKAL, R = aa$R,
           pozdeji_jinde = nrow(pozd) > 0, roky_pozdeji = paste(sort(unique(pozd$ROK)), collapse = ","),
           klic_pozdeji = paste(head(unique(pozd$KOD_LOKAL), 2), collapse = " | "))
  }) %>% ungroup()
  out[[g]] <- r
}
r <- bind_rows(out)
options(width = 220)
cat("=== DP s jedinym cilenym pruzkumem: pruzkoumano totez misto pozdeji pod jinym klicem? ===\n")
print(as.data.frame(r %>% count(DRUH, R2021 = R == 2021, pozdeji_jinde) %>% pivot_wider(names_from = pozdeji_jinde, values_from = n, values_fill = 0, names_prefix = "pozdeji_")))
cat("\n=== typy klicu: DP (A) -> pozdejsi klic tehoz mista ===\n")
print(as.data.frame(r %>% filter(pozdeji_jinde) %>% mutate(t1 = typ(KOD_LOKAL), t2 = typ(sub(" \\|.*$", "", klic_pozdeji))) %>% count(DRUH, t1, t2, sort = TRUE)))
cat("\n=== priklady (obojzivelnici, R = 2021) ===\n")
print(as.data.frame(r %>% filter(pozdeji_jinde, R == 2021) %>% select(DRUH, kod_chu, KOD_LOKAL, roky_pozdeji, klic_pozdeji) %>% head(12)), right = FALSE)
cat("\n=== 2021: odkud maji zaznamy KOD_LOKAL (vyplneno KOD_LOKALITY?) - obojzivelnici, cileny monitoring ===\n")
d <- read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5, col_select = c(ID_ND_NALEZ, DRUH, KOD_LOKAL, ROK, CILMON)) %>%
  distinct(ID_ND_NALEZ, DRUH, .keep_all = TRUE) %>% mutate(ID_ND_NALEZ = as.character(ID_ND_NALEZ)) %>% left_join(ex, by = "ID_ND_NALEZ") %>%
  filter(CILMON == 1, ROK >= 2019)
print(as.data.frame(d %>% mutate(zdroj = ifelse(str_detect(ZDROJ, "Sledování stavu EVL"), "Sledovani stavu EVL", "Monitoring obojzivelniku"),
  KOD_LOKALITY_vyplnen = !is.na(KOD_LOKALITY), typ_klice = typ(KOD_LOKAL)) %>% count(ROK, zdroj, KOD_LOKALITY_vyplnen, typ_klice) %>% arrange(ROK, desc(n))))
saveRDS(r, paste0(sp, "audit_duplicity.rds"))

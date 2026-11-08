# Detail k auditu: B (jiny klic tehoz mista) a C (chybi vstup), a rozlozeni roku u A.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(stringr)})
sp <- "<scratchpad>/"
det <- readRDS(paste0(sp, "audit_nezname.rds"))
ex <- readRDS(paste0(sp, "export_5druhu.rds")) %>% select(ID_ND_NALEZ, X, Y, LOKALITA, KOD_LOKALITY, ZDROJ, POCET, POCITANO, REL_POC, POZNAMKA)
options(width = 230)
cat("=== A: rok hodnoceneho (jedineho) ciloveho pruzkumu ===\n")
print(as.data.frame(bind_rows(det) %>% filter(str_starts(pricina, "A")) %>% distinct(DRUH, kod_chu, KOD_LOKAL, R) %>% count(DRUH, R) %>%
  pivot_wider(names_from = R, values_from = n, values_fill = 0)))
for (g in names(det)) {
  d <- if (g == "ryby") "hn_exp" else "hn_fin"
  rec <- read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5,
                  col_select = c(ID_ND_NALEZ, DRUH, KOD_LOKAL, kod_chu, ROK, CILMON, NEGATIVNI, any_of(c("POP_POCETNOST", "POP_ABUNDANCE")))) %>%
    distinct(ID_ND_NALEZ, DRUH, .keep_all = TRUE) %>% mutate(ID_ND_NALEZ = as.character(ID_ND_NALEZ)) %>% left_join(ex, by = "ID_ND_NALEZ")
  b <- det[[g]] %>% filter(str_starts(pricina, "B")) %>% distinct(DRUH, kod_chu, KOD_LOKAL, R, x, y)
  par <- b %>% rowwise() %>% do({
    bb <- .
    o <- rec %>% filter(DRUH == bb$DRUH, kod_chu == bb$kod_chu, KOD_LOKAL != bb$KOD_LOKAL, ROK < bb$R, !is.na(X)) %>%
      mutate(dist = sqrt((X - bb$x)^2 + (Y - bb$y)^2)) %>% filter(dist <= 100)
    tibble(DRUH = bb$DRUH, kod_chu = bb$kod_chu, klic = bb$KOD_LOKAL, R = bb$R,
           klic_soused = paste(head(unique(o$KOD_LOKAL), 3), collapse = " | "), roky_soused = paste(sort(unique(o$ROK)), collapse = ","))
  }) %>% ungroup()
  cat("\n=== ", g, ": B - priklady dvojic klicu (tataz mista do 100 m) ===\n")
  print(as.data.frame(par %>% head(14) %>% mutate(klic = substr(klic, 1, 45), klic_soused = substr(klic_soused, 1, 60))), right = FALSE)
  typ <- function(k) case_when(str_detect(k, "^CZ\\d{7}$") ~ "kod EVL", str_detect(k, "^CZ\\d{7}_") ~ "kod EVL_cislo", str_detect(k, "^amp") ~ "amp-kod",
                               str_detect(k, "^[A-Z]{2,}[0-9_-]+") ~ "kod lokality", TRUE ~ "nazev")
  cat("\n", g, ": B - typ klice (hodnocena DP -> drivejsi soused)\n")
  print(as.data.frame(par %>% mutate(t1 = typ(klic), t2 = typ(sub(" \\|.*$", "", klic_soused))) %>% count(t1, t2, sort = TRUE)))
  if (g == "oboj") {
    cc <- det[[g]] %>% filter(str_starts(pricina, "C"))
    zr <- rec %>% filter(CILMON == 1) %>% semi_join(cc, by = c("DRUH", "kod_chu", "KOD_LOKAL"))
    cat("\n=== oboj: C - zaznamy ciloveho monitoringu techto DP: je POP_POCETNOST, POCET, REL_POC? ===\n")
    print(as.data.frame(zr %>% count(DRUH, pocetnost = !is.na(POP_POCETNOST), POCET_je = !is.na(POCET), REL_POC_je = !is.na(REL_POC), POCITANO = coalesce(POCITANO, "(NA)"), neg = NEGATIVNI %in% 1) %>% arrange(DRUH, desc(n)) %>% head(20)))
  }
}

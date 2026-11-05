suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr)})
sp <- "<scratchpad>/"
z <- readRDS(paste0(sp, "lm_diag.rds"))$z
h <- names(read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy.csv"), n_max = 1, show_col_types = FALSE))
cat("sloupce s POCITANO:", grep("POCITANO|PRESENCE", h, value = TRUE), "\n")
p <- read_csv(paste0(sp, "hn_fin/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5,
              col_select = c(IDX_ND_AKCE, DRUH, any_of(c("POCITANO", "POCITANO_CLEAN", "POP_PRESENCE")))) %>% filter(DRUH == "Lampetra planeri") %>% distinct()
l <- z %>% filter(DRUH == "Lampetra planeri", CILMON == 1, !NEGATIVNI %in% 1, is.na(POP_POCETODLOV)) %>% left_join(p, by = c("IDX_ND_AKCE", "DRUH"))
cat("\n=== Lampetra: pozitivni zaznamy cil. monitoringu bez poctu (", nrow(l), ") ===\n")
print(l %>% count(ROK)); print(l %>% count(across(any_of(c("POCITANO", "POCITANO_CLEAN"))), POCET_NA = is.na(POCET)) %>% as.data.frame())
cat("\nukazky POZNAMKA:\n"); print(head(unique(substr(l$POZNAMKA, 1, 300)), 6))
cat("\n=== kontrola: plocha_prolov_p proti delka_prolov_u x pr_sir_t (zaznamy, kde je vse) ===\n")
tg <- function(x, t) suppressWarnings(as.numeric(gsub(",", ".", str_match(x, paste0("<", t, ">([^<]*)</", t, ">"))[, 2])))
k <- z %>% filter(CILMON == 1) %>% mutate(pl = tg(STRUKT_POZN, "plocha_prolov_p"), de = tg(STRUKT_POZN, "delka_prolov_u"), si = tg(STRUKT_POZN, "pr_sir_t")) %>%
  filter(!is.na(pl), !is.na(de), !is.na(si), pl > 0) %>% mutate(pomer = de * si / pl)
print(k %>% group_by(DRUH) %>% summarise(n = n(), median = median(pomer), q10 = quantile(pomer, 0.1), q90 = quantile(pomer, 0.9),
                                         shoda_10pct = round(mean(abs(pomer - 1) <= 0.1), 2)))
cat("\n=== Misgurnus: zaznamy s delkou a sirkou, bez plochy - hodnoty ===\n")
m <- z %>% filter(DRUH == "Misgurnus fossilis", CILMON == 1, is.na(plocha_p), is.na(plocha_pbm)) %>%
  mutate(de = tg(STRUKT_POZN, "delka_prolov_u"), si = tg(STRUKT_POZN, "pr_sir_t"))
print(summary(m$de)); print(summary(m$si)); print(m %>% count(neg = NEGATIVNI %in% 1, ma_de_si = !is.na(de) & !is.na(si)))
cat("\n=== Misgurnus: <velikosti> je, POP_VITALITA ne - ukazky ===\n")
v <- z %>% filter(DRUH == "Misgurnus fossilis", !is.na(velik), is.na(POP_VITALITA))
print(nrow(v)); print(head(unique(v$velik), 8)); print(v %>% count(NEGATIVNI))

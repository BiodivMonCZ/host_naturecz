# Fragmentace lokalit Lampetra a Misgurnus: spojily by souradnice (X, Y) nebo IDX_ND_LOK
# jednolete lokality do delsich rad? Monitoring druhu CR od 2013, dvojice predmetu ochrany.
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr)})
sp <- "<scratchpad>/"
D <- c("Lampetra planeri", "Misgurnus fossilis", "Cottus gobio")
ex <- bind_rows(read_csv("C:/Users/jonas.gaigr/Documents/host_data/export_data_evl.csv", show_col_types = FALSE, guess_max = 1e5,
                         col_types = cols(.default = col_character())),
                read_csv("C:/Users/jonas.gaigr/Documents/host_data/export_data_zprap.csv", show_col_types = FALSE, guess_max = 1e5,
                         col_types = cols(.default = col_character()))) %>%
  filter(DRUH %in% D, PROJEKT == "Monitoring druhů ČR") %>% distinct() %>%
  mutate(ROK = as.integer(substr(DATUM_OD, 7, 10)), kod_chu = substr(EVL, 1, 9), X = as.numeric(X), Y = as.numeric(Y),
         LOK = tolower(str_squish(str_replace_all(str_replace_all(LOKALITA, "[\u2013\u2014\u2212]", "-"), " *- *", " - ")))) %>%
  filter(ROK >= 2013)
ss <- openxlsx::read.xlsx("C:/Users/jonas.gaigr/Documents/host_naturecz/Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx", sheet = 1)
names(ss)[c(1, 8)] <- c("site_code", "nazev_lat")
ex <- ex %>% filter(paste(DRUH, kod_chu) %in% paste(ss$nazev_lat, ss$site_code))
options(width = 220)
cat("=== souradnice: jednotky a presnost ===\n"); print(summary(ex$X)); print(ex %>% count(PRESNOST, sort = TRUE) %>% head(5))
cat("\n=== IDX_ND_LOK: opakuje se napric roky? ===\n")
print(ex %>% group_by(DRUH) %>% summarise(zaznamu = n(), idx_lok = n_distinct(IDX_ND_LOK), lokalit_nazev = n_distinct(paste(kod_chu, LOK)),
  idx_s_vice_roky = sum(tapply(ROK, IDX_ND_LOK, function(r) n_distinct(r) > 1)), .groups = "drop"))
# shluk souradnic: lokality (nazev) v teze EVL, jejichz stredy jsou do 300 m
lok <- ex %>% group_by(DRUH, kod_chu, LOK) %>% summarise(x = median(X), y = median(Y), roky = list(sort(unique(ROK))), n_let = n_distinct(ROK), .groups = "drop")
shluk <- function(d, r) {
  d <- d %>% arrange(x)
  g <- seq_len(nrow(d))
  if (nrow(d) > 1) for (i in 1:(nrow(d) - 1)) for (j in (i + 1):nrow(d)) {
    if (d$x[j] - d$x[i] > r) break
    if (sqrt((d$x[i] - d$x[j])^2 + (d$y[i] - d$y[j])^2) <= r) g[g == g[j]] <- g[i]
  }
  d$shluk <- g; d
}
for (r in c(100, 300)) {
  s <- lok %>% group_by(DRUH, kod_chu) %>% group_modify(~ shluk(.x, r)) %>% ungroup() %>%
    group_by(DRUH, kod_chu, shluk) %>% summarise(n_nazvu = n(), n_let = length(unique(unlist(roky))), .groups = "drop")
  cat("\n=== shluk stredu lokalit do", r, "m (v teze EVL) ===\n")
  print(as.data.frame(s %>% group_by(DRUH) %>% summarise(lokalit_nazev = sum(n_nazvu), shluku = n(), jednolete_nazev = NA,
    shluku_jednoletych = sum(n_let == 1), shluku_2plus = sum(n_let >= 2), shluku_3plus = sum(n_let >= 3))))
}
cat("\n=== pro srovnani: lokality podle nazvu ===\n")
print(as.data.frame(lok %>% group_by(DRUH) %>% summarise(lokalit = n(), jednoletych = sum(n_let == 1), dvou_plus = sum(n_let >= 2), tri_plus = sum(n_let >= 3))))
saveRDS(list(ex = ex, lok = lok), paste0(sp, "lm_fragment.rds"))

# Simulace: lokality slozene ze shluku stredu (100 / 300 m v teze EVL) + opravy A, B.
# Pocet "DP" s cilenym monitoringem a s obema klicovymi indikatory hodnotitelnymi.
suppressPackageStartupMessages({library(dplyr); library(stringr); library(tidyr)})
sp <- "<scratchpad>/"
z <- readRDS(paste0(sp, "lm_diag.rds"))$z; lok <- readRDS(paste0(sp, "lm_fragment.rds"))$lok
norm <- function(x) tolower(str_squish(str_replace_all(str_replace_all(x, "[\u2013\u2014\u2212]", "-"), " *- *", " - ")))
shluk <- function(d, r) {
  d <- d %>% arrange(x); g <- seq_len(nrow(d))
  if (nrow(d) > 1) for (i in 1:(nrow(d) - 1)) for (j in (i + 1):nrow(d)) {
    if (d$x[j] - d$x[i] > r) break
    if (sqrt((d$x[i] - d$x[j])^2 + (d$y[i] - d$y[j])^2) <= r) g[g == g[j]] <- g[i]
  }
  d$shluk <- g; d
}
z <- z %>% mutate(LOK = norm(KOD_LOKAL), abA = !is.na(POP_ABUNDANCE) | POP_POCETODLOV %in% 0,
                  abB = if_else(DRUH == "Misgurnus fossilis", !is.na(POP_POCETODLOV), abA), mer = !is.na(POP_VITALITA))
prah <- function(d) ifelse(d == "Lampetra planeri", 3, 2)
vys <- list()
for (r in c(0, 100, 300)) {
  m <- if (r == 0) lok %>% mutate(shluk = LOK) else lok %>% group_by(DRUH, kod_chu) %>% group_modify(~ shluk(.x, r)) %>% ungroup() %>% mutate(shluk = as.character(shluk))
  zz <- z %>% left_join(m %>% select(DRUH, kod_chu, LOK, shluk), by = c("DRUH", "kod_chu", "LOK")) %>% mutate(shluk = coalesce(shluk, paste0("x_", LOK)))
  yr <- zz %>% group_by(DRUH, kod_chu, shluk, ROK) %>% summarise(cil = any(CILMON == 1), abA = any(abA), abB = any(abB), mer = any(mer), .groups = "drop")
  s <- yr %>% group_by(DRUH, kod_chu, shluk) %>% filter(any(cil)) %>% mutate(R = max(ROK[cil])) %>% filter(ROK <= R) %>%
    summarise(abA = sum(abA), abB = sum(abB), mer = sum(mer), .groups = "drop") %>%
    mutate(okA = abA >= prah(DRUH) & mer >= 2, okB = abB >= prah(DRUH) & mer >= 2)
  vys[[as.character(r)]] <- s %>% group_by(DRUH) %>% summarise(polomer = r, DP = n(), oba_A = sum(okA), oba_A_B = sum(okB),
                                                               podil_A_B = round(100 * mean(okB)), .groups = "drop")
}
print(as.data.frame(bind_rows(vys) %>% arrange(DRUH, polomer)))

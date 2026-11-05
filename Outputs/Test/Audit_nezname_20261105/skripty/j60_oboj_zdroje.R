# Obojzivelnici (T. cristatus, B. bombina): zdroje podle roku, PROJEKT, bohatost STRUKT_POZN a CILMON.
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr)})
sp <- "<scratchpad>/"
D <- c("Triturus cristatus", "Bombina bombina", "Cottus gobio", "Lampetra planeri", "Misgurnus fossilis")
ex <- bind_rows(read_csv("C:/Users/jonas.gaigr/Documents/host_data/export_data_evl.csv", show_col_types = FALSE, col_types = cols(.default = col_character())),
                read_csv("C:/Users/jonas.gaigr/Documents/host_data/export_data_zprap.csv", show_col_types = FALSE, col_types = cols(.default = col_character()))) %>%
  distinct() %>% filter(DRUH %in% D) %>%
  mutate(ROK = as.integer(substr(DATUM_OD, 7, 10)), ntag = str_count(coalesce(STRUKT_POZN, ""), "<[a-zA-Z_]+>"), X = as.numeric(X), Y = as.numeric(Y))
saveRDS(ex, paste0(sp, "export_5druhu.rds"))
cil <- read_csv("C:/Users/jonas.gaigr/Documents/host_naturecz/Data/Input/cil_mon_zdroj.csv", locale = locale(encoding = "Windows-1250"), show_col_types = FALSE)$ZDROJ_NDOP
ex <- ex %>% mutate(CILMON = ZDROJ %in% cil | PROJEKT %in% "Monitoring druhů ČR")
o <- ex %>% filter(DRUH %in% D[1:2], ROK >= 2013)
cat("=== obojzivelnici od 2013: zaznamy podle roku a CILMON ===\n")
print(as.data.frame(o %>% count(ROK, CILMON) %>% pivot_wider(names_from = CILMON, values_from = n, values_fill = 0, names_prefix = "CILMON_")))
cat("\n=== zaznamy BEZ CILMON s >= 5 tagy (vypadaji jako cileny monitoring?) - podle zdroje ===\n")
z <- o %>% filter(!CILMON, ntag >= 5) %>% group_by(ZDROJ, PROJEKT = coalesce(PROJEKT, "(NA)")) %>%
  summarise(n = n(), roky = paste(sort(unique(ROK)), collapse = ","), tagu = median(ntag), .groups = "drop") %>% arrange(desc(n))
for (i in seq_len(min(nrow(z), 25))) cat(sprintf("%5d | tagu %3.0f | %-14s | %-26s | %s\n", z$n[i], z$tagu[i], substr(z$roky[i], 1, 14), substr(z$PROJEKT[i], 1, 26), substr(z$ZDROJ[i], 1, 110)))
cat("\n=== zdroje obojzivelniku s 'obojživel' nebo 'Sledování stavu' v nazvu ===\n")
z2 <- o %>% filter(str_detect(tolower(ZDROJ), "obojživel|sledování stavu")) %>% group_by(ZDROJ, PROJEKT = coalesce(PROJEKT, "(NA)"), CILMON) %>%
  summarise(n = n(), roky = paste(sort(unique(ROK)), collapse = ","), tagu = median(ntag), .groups = "drop") %>% arrange(desc(n))
for (i in seq_len(min(nrow(z2), 25))) cat(sprintf("%5d | CILMON %-5s | tagu %3.0f | %-14s | %-26s | %s\n", z2$n[i], z2$CILMON[i], z2$tagu[i], substr(z2$roky[i], 1, 14), substr(z2$PROJEKT[i], 1, 26), substr(z2$ZDROJ[i], 1, 100)))

# Zdroje (ZDROJ) zaznamu ryb a mihuli v exportu NDOP: rok, PROJEKT, bohatost STRUKT_POZN.
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr)})
sp <- "<scratchpad>/"
ex <- bind_rows(read_csv("C:/Users/jonas.gaigr/Documents/host_data/export_data_evl.csv", show_col_types = FALSE, col_types = cols(.default = col_character())),
                read_csv("C:/Users/jonas.gaigr/Documents/host_data/export_data_zprap.csv", show_col_types = FALSE, col_types = cols(.default = col_character()))) %>%
  distinct() %>% mutate(ROK = as.integer(substr(DATUM_OD, 7, 10)), ntag = str_count(coalesce(STRUKT_POZN, ""), "<[a-zA-Z_]+>"))
r <- ex %>% filter(SKUPINA == "Ryby a mihule")
saveRDS(r, paste0(sp, "ryby_export.rds"))
options(width = 250)
cat("=== zdroje ryb s 'monitoring' / 'sledovani' v nazvu (vsechny roky) ===\n")
z <- r %>% filter(str_detect(tolower(ZDROJ), "monitoring|sledov")) %>%
  group_by(ZDROJ, PROJEKT) %>% summarise(n = n(), roky = paste(sort(unique(ROK)), collapse = ","), med_tagu = median(ntag),
                                         druhu = n_distinct(DRUH), .groups = "drop") %>% arrange(desc(n))
print(as.data.frame(z), right = FALSE)
cat("\n=== zaznamy ryb 2024-2025 podle ZDROJ a PROJEKT ===\n")
print(as.data.frame(r %>% filter(ROK >= 2024) %>% count(ZDROJ, PROJEKT, ROK, sort = TRUE) %>% head(25)), right = FALSE)
cat("\n=== zaznamy ryb s >= 10 tagy podle roku a PROJEKT ===\n")
print(as.data.frame(r %>% filter(ntag >= 10) %>% count(ROK, PROJEKT = coalesce(PROJEKT, "(NA)")) %>% pivot_wider(names_from = PROJEKT, values_from = n, values_fill = 0) %>% arrange(ROK)))
cat("\n=== 'Monitoring ryb' i v jinych skupinach? ===\n")
print(as.data.frame(ex %>% filter(str_detect(ZDROJ, "Monitoring ryb|Monitoring mihul|Monitoring vranky|Monitoring piskoře")) %>% count(SKUPINA)))

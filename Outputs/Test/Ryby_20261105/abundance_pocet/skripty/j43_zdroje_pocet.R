suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr)})
sp <- "<scratchpad>/"
r <- readRDS(paste0(sp, "ryby_export.rds"))
k <- r %>% filter(str_detect(ZDROJ, "^Kolektiv autorů \\(\\d{4}\\) Monitoring ryb")) %>% count(ZDROJ, PROJEKT)
for (i in seq_len(nrow(k))) cat(sprintf("%4d | %s | [%s]\n", k$n[i], k$PROJEKT[i], k$ZDROJ[i]))
cat("\nmezery na okrajich:", sum(k$ZDROJ != str_trim(k$ZDROJ)), "; carky:", sum(str_detect(k$ZDROJ, ",")), "\n")
writeLines(k$ZDROJ, paste0(sp, "zdroje_monitoring_ryb.txt"), useBytes = TRUE)
d <- read_csv(paste0(sp, "hn_fixA/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5,
              col_select = c(DRUH, CILMON, NEGATIVNI, POCET, POCITANO, POCITANO_CLEAN, POP_POCET, POP_POCETODLOV, POP_ABUNDANCE)) %>%
  filter(DRUH %in% c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis"), CILMON == 1)
cat("\nPOCITANO_CLEAN u ciloveho monitoringu:\n")
print(d %>% count(DRUH, POCITANO_CLEAN, pocet_je = !is.na(POCET), pop_pocet_je = !is.na(POP_POCET)) %>% as.data.frame())
cat("\nPOP_POCET vs POP_POCETODLOV (1. + 2. odlov) u pozitivnich zaznamu s obojim:\n")
print(d %>% filter(!NEGATIVNI %in% 1, !is.na(POP_POCET), !is.na(POP_POCETODLOV)) %>% group_by(DRUH) %>%
  summarise(n = n(), shodne = sum(POP_POCET == POP_POCETODLOV), pocet_vetsi = sum(POP_POCET > POP_POCETODLOV),
            median_pomer = median(POP_POCET / pmax(POP_POCETODLOV, 1))) %>% as.data.frame())

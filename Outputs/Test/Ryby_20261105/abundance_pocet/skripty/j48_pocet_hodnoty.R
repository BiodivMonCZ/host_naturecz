suppressPackageStartupMessages({library(dplyr); library(readr)})
sp <- "<scratchpad>/"
cti <- function(d) read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy.csv"), show_col_types = FALSE, guess_max = 1e5,
  col_select = c(ID_ND_NALEZ, DRUH, CILMON, POP_POCET, POP_POCETODLOV, POP_ABUNDANCENAL, POP_ABUNDANCE, POP_DYN)) %>%
  filter(DRUH %in% c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis")) %>% distinct(ID_ND_NALEZ, .keep_all = TRUE)
a <- cti("hn_fixA"); b <- cti("hn_pocet")
j <- inner_join(a, b, by = c("ID_ND_NALEZ", "DRUH"), suffix = c("_h93", "_pocet"))
print(j %>% group_by(DRUH) %>% summarise(zaznamu = n(),
  abundance_jina = sum(!mapply(identical, POP_ABUNDANCENAL_h93, POP_ABUNDANCENAL_pocet)),
  abundance_vyssi = sum(POP_ABUNDANCENAL_pocet > POP_ABUNDANCENAL_h93, na.rm = TRUE),
  abundance_nove_NA = sum(is.na(POP_ABUNDANCENAL_pocet) & !is.na(POP_ABUNDANCENAL_h93)),
  abundance_nove_hodnota = sum(!is.na(POP_ABUNDANCENAL_pocet) & is.na(POP_ABUNDANCENAL_h93)),
  dyn_jina = sum(!mapply(identical, round(POP_DYN_h93, 6), round(POP_DYN_pocet, 6))),
  dyn_preslo_limit = sum((POP_DYN_h93 >= 50) != (POP_DYN_pocet >= 50), na.rm = TRUE)) %>% as.data.frame())
L <- function(d) read_csv(paste0(sp, d, "/Data/Temp/n2k_druhy_lok.csv"), show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
  filter(ID_IND == "POP_DYN") %>% select(DRUH, kod_chu, KOD_LOKAL, HOD_IND, STAV_IND)
k <- inner_join(L("hn_fixA"), L("hn_pocet"), by = c("DRUH", "kod_chu", "KOD_LOKAL"), suffix = c("_h93", "_pocet"))
cat("\nPOP_DYN na urovni DP: hodnota jina / stav jiny\n")
print(k %>% group_by(DRUH) %>% summarise(dp = n(), hodnota_jina = sum(HOD_IND_h93 != HOD_IND_pocet, na.rm = TRUE),
  stav_jiny = sum(!mapply(identical, STAV_IND_h93, STAV_IND_pocet))) %>% as.data.frame())

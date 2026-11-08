#----------------------------------------------------------#
# H-64: jake jednotky nesou pocty vstupujici do POP_POCETPRUM3
#----------------------------------------------------------#
suppressPackageStartupMessages({
  library(dplyr)
})

d <- data.table::fread(
  "Data/Temp/n2k_druhy.csv",
  select = c("kod_chu", "DRUH", "CILMON", "POCITANO", "POCITANO_CLEAN",
             "POP_POCET"),
  encoding = "UTF-8"
)

d <- tibble::as_tibble(d) %>%
  dplyr::filter(CILMON == 1, !is.na(POP_POCET))

cat("\n=== Jednotka (POCITANO_CLEAN) u zaznamu vstupujicich do prumeru ===\n\n")
print(as.data.frame(
  d %>%
    dplyr::count(DRUH, POCITANO_CLEAN, name = "zaznamu") %>%
    dplyr::group_by(DRUH) %>%
    dplyr::mutate(pct = round(100 * zaznamu / sum(zaznamu), 1)) %>%
    dplyr::arrange(DRUH, dplyr::desc(zaznamu)) %>%
    dplyr::ungroup()
), right = FALSE)

cat("\n=== Surove POCITANO (prvnich 15 nejcastejsich) ===\n\n")
print(as.data.frame(
  d %>%
    dplyr::count(DRUH, POCITANO, name = "zaznamu") %>%
    dplyr::arrange(DRUH, dplyr::desc(zaznamu)) %>%
    dplyr::group_by(DRUH) %>%
    dplyr::slice_head(n = 15) %>%
    dplyr::ungroup()
), right = FALSE)

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

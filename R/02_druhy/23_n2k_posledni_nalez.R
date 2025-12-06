#----------------------------------------------------------#
# Nacteni temp dat ----
#----------------------------------------------------------#
n2k_druhy <- 
  readr::read_csv(
    "Data/Temp/n2k_druhy.csv"
  )

# posledni nalez - chu - druh - lokalita - pole ----
n2k_druhy_posledni_lok <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(DATUM, na.rm = TRUE)
  )
# posledni nalez - chu - druh - pole ----
n2k_druhy_posledni_pol <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, POLE) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(DATUM, na.rm = TRUE)
  )
# posledni nalez - chu - druh ----
n2k_druhy_posledni_chu <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(DATUM, na.rm = TRUE)
  )

#----------------------------------------------------------#
# Zapis temp dat ----
#----------------------------------------------------------#
readr::write_csv(
  n2k_druhy_posledni_lok,
  paste0("Data/Temp/n2k_druhy_posledni_lok", ".csv")
)
readr::write_csv(
  n2k_druhy_posledni_pol,
  paste0("Data/Temp/n2k_druhy_posledni_pol", ".csv")
)
readr::write_csv(
  n2k_druhy_posledni_chu,
  paste0("Data/Temp/n2k_druhy_posledni_chu", ".csv")
)

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
# nacteni temp dat ----
n2k_druhy <- 
  readr::read_csv(
    "Data/Temp/n2k_druhy.csv"
    )

# hodnocene obdobi - chu - druh - lokalita - pole ----
n2k_druhy_obdobi_lok <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )
# hodnocene obdobi - chu - druh - pole ----
n2k_druhy_obdobi_pol <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH, POLE) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )
# hodnocene obdobi - chu - druh ----
n2k_druhy_obdobi_chu <- n2k_druhy %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
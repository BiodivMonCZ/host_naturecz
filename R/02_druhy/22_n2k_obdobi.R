# hodnocene obdobi - chu - druh - lokalita - pole ----
n2k_druhy_obdobi_lok <- n2k_load %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(DATUM, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(DATUM, na.rm = TRUE)
  )
# hodnocene obdobi - chu - druh - pole ----
# Pol a chu jsou hrubsi seskupeni nez lok (kod_chu+DRUH+POLE, resp. kod_chu+DRUH,
# jsou vzdy nadmnozinou radku spadajicich pod stejne KOD_LOKAL). Misto dalsich
# dvou pruchodu celym n2k_load tedy min/max dopocitame agregaci JIZ HOTOVEHO
# n2k_druhy_obdobi_lok - min z minim a max z maxim davaji stejny vysledek jako
# min/max primo nad n2k_load, ale bez opakovaneho skenovani cele tabulky.
n2k_druhy_obdobi_pol <- n2k_druhy_obdobi_lok %>%
  dplyr::group_by(kod_chu, DRUH, POLE) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(HODNOCENE_OBDOBI_OD, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(HODNOCENE_OBDOBI_DO, na.rm = TRUE)
  )
# hodnocene obdobi - chu - druh ----
n2k_druhy_obdobi_chu <- n2k_druhy_obdobi_lok %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::reframe(
    HODNOCENE_OBDOBI_OD = min(HODNOCENE_OBDOBI_OD, na.rm = TRUE),
    HODNOCENE_OBDOBI_DO = max(HODNOCENE_OBDOBI_DO, na.rm = TRUE)
  )

#----------------------------------------------------------#
# Zapis temp dat ----
#----------------------------------------------------------#
readr::write_csv(
  n2k_druhy_obdobi_lok,
  paste0("Data/Temp/n2k_druhy_obdobi_lok", ".csv")
)
readr::write_csv(
  n2k_druhy_obdobi_pol,
  paste0("Data/Temp/n2k_druhy_obdobi_pol", ".csv")
)
readr::write_csv(
  n2k_druhy_obdobi_chu,
  paste0("Data/Temp/n2k_druhy_obdobi_chu", ".csv")
)

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

#----------------------------------------------------------#
# posledni nalez - chu - druh - lokalita - pole ----
#----------------------------------------------------------#
n2k_druhy_posledni_lok <- n2k_load %>%
  dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, POLE) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(DATUM, na.rm = TRUE)
  )

#----------------------------------------------------------#
# posledni nalez - chu - druh - pole ----
#----------------------------------------------------------#
# Pol a chu jsou hrubsi seskupeni nez lok (kod_chu+DRUH+POLE, resp. kod_chu+DRUH,
# jsou vzdy nadmnozinou radku spadajicich pod stejne KOD_LOKAL). Misto dalsich
# dvou pruchodu celym n2k_load tedy maximum dopocitame agregaci JIZ HOTOVEHO
# n2k_druhy_posledni_lok - max z maxim dava stejny vysledek jako max primo nad
# n2k_load, ale bez opakovaneho skenovani cele tabulky.
n2k_druhy_posledni_pol <- n2k_druhy_posledni_lok %>%
  dplyr::group_by(kod_chu, DRUH, POLE) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(POSLEDNI_NALEZ, na.rm = TRUE)
  )

#----------------------------------------------------------#
# posledni nalez - chu - druh ----
#----------------------------------------------------------#
n2k_druhy_posledni_chu <- n2k_druhy_posledni_lok %>%
  dplyr::group_by(kod_chu, DRUH) %>%
  dplyr::reframe(
    POSLEDNI_NALEZ = max(POSLEDNI_NALEZ, na.rm = TRUE)
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
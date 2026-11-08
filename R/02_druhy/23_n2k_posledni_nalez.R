#----------------------------------------------------------#
# Posledni nalez - chu / pole / lokalita ----
#----------------------------------------------------------#
# Posledni nalez = max(DATUM) pres tytez skupiny jako HODNOCENE_OBDOBI_DO
# v 22_n2k_obdobi.R (n2k_load, kod_chu x DRUH [x POLE] [x KOD_LOKAL]),
# proto se jen prebira odtud. Pocitaji se i negativni zaznamy a vysledek
# kaskada dal nepouziva - viz nalez H-15 v registru cevnatych rostlin.
n2k_druhy_posledni_lok <- n2k_druhy_obdobi_lok %>%
  dplyr::select(kod_chu, DRUH, KOD_LOKAL, POLE, POSLEDNI_NALEZ = HODNOCENE_OBDOBI_DO)
n2k_druhy_posledni_pol <- n2k_druhy_obdobi_pol %>%
  dplyr::select(kod_chu, DRUH, POLE, POSLEDNI_NALEZ = HODNOCENE_OBDOBI_DO)
n2k_druhy_posledni_chu <- n2k_druhy_obdobi_chu %>%
  dplyr::select(kod_chu, DRUH, POSLEDNI_NALEZ = HODNOCENE_OBDOBI_DO)

#----------------------------------------------------------#
# Zapis temp dat ----
#----------------------------------------------------------#
# Data/Temp/ zaklada uz 22_n2k_obdobi.R.
readr::write_csv(n2k_druhy_posledni_lok, "Data/Temp/n2k_druhy_posledni_lok.csv")
readr::write_csv(n2k_druhy_posledni_pol, "Data/Temp/n2k_druhy_posledni_pol.csv")
readr::write_csv(n2k_druhy_posledni_chu, "Data/Temp/n2k_druhy_posledni_chu.csv")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

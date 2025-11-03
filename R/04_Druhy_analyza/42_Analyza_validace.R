#----------------------------------------------------------#
# Nacteni dat -----
#----------------------------------------------------------#
data_val_raw <-
  readr::read_csv(
    "Data/Input/Revisions/export_isop_obojzivelnici_202510.csv", 
    locale = readr::locale(encoding = "Windows-1250")
  )

data_val_n <-
  data_val_raw %>%
  dplyr::filter(RP != "RP Jižní Morava" & RP != "RP Střední Čechy") %>%
  dplyr::group_by(validace) %>%
  dplyr::reframe(pocet_lokalit = n())

data_val_rp_n <-
  data_val_raw %>%
  dplyr::filter(RP != "RP Jižní Morava" & RP != "RP Střední Čechy") %>%
  dplyr::group_by(RP, validace) %>%
  dplyr::reframe(pocet_lokalit = n())

data_val_hod_n <-
  data_val_raw %>%
  dplyr::filter(RP != "RP Jižní Morava" & RP != "RP Střední Čechy") %>%
  dplyr::filter(validace == "validováno") %>%
  dplyr::group_by(druh, stav) %>%
  dplyr::reframe(pocet_lokalit = n())

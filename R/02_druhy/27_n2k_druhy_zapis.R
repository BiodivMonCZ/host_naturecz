#----------------------------------------------------------#
# Druhovy seznam----- 
#----------------------------------------------------------#

#species_list <- unique(subset(n2k_load, SKUPINA == "Obojživelníci")$DRUH)
species_list <- n2k_load %>% dplyr::filter(SKUPINA == "Ryby a mihule") %>% pull(DRUH) %>% unique()
#species_list <- unique(n2k_load$DRUH)
#species_list <- c("Pulsatilla patens", "Bombina variegata", "Osmoderma barnabita", "Lampetra planeri")

#----------------------------------------------------------#
# Napocet a temp zapis----- 
#----------------------------------------------------------#
#--------------------------------------------------#
# Uroven akce ----- 
#--------------------------------------------------#
n2k_druhy <- lapply(unique(n2k_load$DRUH), function(sp) {
  run_n2k_druhy(n2k_load, sp, sites_subjects, limity, current_year = 2024)
}) %>%
  dplyr::bind_rows() 
readr::write_csv(
  n2k_druhy,
  paste0("Data/Temp/n2k_druhy", ".csv")
)

n2k_druhy_lim <- lapply(species_list, function(sp) {
  run_n2k_druhy_lim(n2k_druhy, sp, sites_subjects, limity, current_year = 2024)
}) %>%
  dplyr::bind_rows()
readr::write_csv(
  n2k_druhy_lim,
  paste0("Data/Temp/n2k_druhy_lim", ".csv")
)

#--------------------------------------------------#
# Uroven lokality ----- 
#--------------------------------------------------#
n2k_druhy_lok <- lapply(species_list, function(sp) {
  run_n2k_druhy_lok(n2k_druhy_lim, sp, sites_subjects, limity, current_year = 2024)
}) %>%
  dplyr::bind_rows() 
readr::write_csv(
  n2k_druhy_lok,
  paste0("Data/Temp/n2k_druhy_lok", ".csv")
)

#--------------------------------------------------#
# Uroven uzemi ----- 
#--------------------------------------------------#
n2k_druhy_uzemi <- 
  lapply(species_list, function(sp) {
    run_n2k_druhy_uzemi(
      n2k_druhy_lok, sp, sites_subjects, limity, biotop_evd, 
      current_year = 2024)
  }
  ) %>%
  dplyr::bind_rows()

readr::write_csv(
  n2k_druhy_uzemi,
  paste0("Data/Temp/n2k_druhy_uzemi", ".csv")
)

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
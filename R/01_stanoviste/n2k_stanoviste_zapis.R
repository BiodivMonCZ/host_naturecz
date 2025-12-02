# RESULTS 2024 ----
hu <- n2k_hab_klic(sites_habitats[89,5], sites_habitats[89,1])
habresults_100_110 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_100_110) <- colnames(hu)
for(i in 87:95) {
  habresults_100_110 <- dplyr::bind_rows(
    habresults_100_110, 
    as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}

habresults_x_1_500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1_500) <- colnames(hu)
habresults_x_501_1000 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_501_1000) <- colnames(hu)
habresults_x_1001_1500 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1001_1500) <- colnames(hu)
habresults_x_1501_1893 <- base::matrix(NA, 1, ncol(hu)) %>% dplyr::as_tibble()
colnames(habresults_x_1501_1893) <- colnames(hu)

rm(vmb_shp_sjtsk_akt_read, vmb_hab_dbf_23, vmb_pb_dbf_23, vmb_hab_pb_dbf_23)

for(i in 1:500) {
  habresults_x_1_500 <- dplyr::bind_rows(habresults_x_1_500, 
                                         as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1_500, 
           "Data/Temp/habresults_x_1_500.csv", 
           row.names = FALSE)
for(i in 501:1000) {
  habresults_x_501_1000 <- dplyr::bind_rows(habresults_x_501_1000, 
                                            as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_501_1000, 
           "Data/Temp/habresults_x_501_1000.csv", 
           row.names = FALSE)
for(i in 1001:1500) {
  habresults_x_1001_1500 <- dplyr::bind_rows(habresults_x_1001_1500, 
                                             as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1001_1500, 
           "Data/Temp/habresults_x_1001_1500.csv", 
           row.names = FALSE)
for(i in 1501:nrow(sites_habitats)) {
  habresults_x_1501_1893 <- dplyr::bind_rows(habresults_x_1501_1893, 
                                             as.data.frame(n2k_hab_klic(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(habresults_x_1501_1893, 
           "Data/Temp/habresults_x_1501_1893.csv", 
           row.names = FALSE)

results_habitats_x <- dplyr::bind_rows(
  habresults_x_1_500[c(2:nrow(habresults_x_1_500)),], 
  habresults_x_501_1000[c(2:nrow(habresults_x_501_1000)),],
  habresults_x_1001_1500[c(2:nrow(habresults_x_1001_1500)),],
  habresults_x_1501_1893[c(2:nrow(habresults_x_1501_1893)),]
)

path <- paste0("Outputs/Data/results_habitats_24_", 
               gsub('-','',Sys.Date()), 
               ".csv")
write.csv2(
  results_habitats_x, 
  path, 
  row.names = FALSE,
  fileEncoding = "Windows-1250"
)

## Clear temp files ----
# Define path to the Temp folder
temp_path <- "Data/Temp"   # or "C:/full/path/to/Data/Temp"

# List all files in the folder
files_to_delete <- list.files(temp_path, full.names = TRUE)

# Remove files
file.remove(files_to_delete)

cat("Removed", length(files_to_delete), "files from", temp_path, "\n")

# Vysledky - long format ----

results_habitats_l <- results_habitats_x %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  ) %>%
  mutate(
    across(where(is.numeric) | where(is.Date) & !c("HABITAT_CODE"), ~ as.character(as.numeric(.x)))
  )

results_habitats_long <- tidyr::pivot_longer(results_habitats_l,
                                             cols = c(4:(ncol(results_habitats_l)-4)),
                                             names_to = "PAR_NAZEV",
                                             values_to = "PAR_HODNOTA") %>%
  dplyr::mutate(ROK_HODNOCENI = 2024) %>%
  dplyr::select(-c(DATE_MEAN, DATE_MEDIAN)) %>%
  dplyr::select(SITECODE,
                NAZEV,
                HABITAT_CODE,
                DATE_MIN, 
                DATE_MAX,
                PAR_NAZEV,
                PAR_HODNOTA,
                ROK_HODNOCENI)

write.csv2(
  results_habitats_long, 
  paste0(
    temp_path,
    "results_habitats_long_24_", 
    gsub(
      '-',
      '',
      Sys.Date()
    ),
    ".csv"
  ),
  row.names = FALSE
)

# AKTUALIZACE EVL ----
okrsky <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/AktualizacniOkrsek")
st_filter(evl_sjtsk, okrsky %>%
            dplyr::filter(ROK_AKT > 2020)) %>%
  pull(NAZEV)

sites_habitats_sitecodes <- sites_habitats %>%
  dplyr::select(site_code) %>%
  dplyr::distinct()

hu_akt <- hvezdice_update(sites_habitats_sitecodes[1,])
habupdate <- matrix(NA, 1, ncol(hu_akt)) %>% dplyr::as_tibble()
colnames(habupdate) <- colnames(hu_akt)

for(i in 1:nrow(sites_habitats_sitecodes)) {
  habupdate <- dplyr::bind_rows(habupdate, 
                                as.data.frame(hvezdice_update(sites_habitats_sitecodes[i,])))
}
habupdate_22 <- habupdate[c(2:nrow(habupdate)),]

write.csv2(habupdate_22, 
           "S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/habupdate_22_", 
           gsub('-','',Sys.Date()), 
           ".csv")

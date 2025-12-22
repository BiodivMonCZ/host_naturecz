# SET WD ----
# HRANICE ČR
czechia <- st_read("HraniceCR.shp")
czechia_line <- st_cast(czechia, "LINESTRING")
# BIOGEOGRAFICKÉ ČLENĚNÍ ČR
#bioregs <- st_read("BiogeoRegions_CR.shp")
#bioregs <- st_transform(bioregs, CRS("+init=epsg:4326"))
# VMB - X a PB 2022
vmb_x_shp_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/X_Segment.shp")
vmb_x_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/X_biotop.dbf")
vmb_pb_shp_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/PB_Segment.shp")
vmb_pb_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/PB_BIOTOP.dbf")

vmb_x_22 <- vmb_x_shp_22 %>%
  dplyr::left_join(vmb_x_dbf_22, by = "SEGMENT_ID")

vmb_pb_22 <- vmb_pb_shp_22 %>%
  dplyr::left_join(vmb_pb_dbf_22, by = "SEGMENT_ID")

vmb_pb_x_22 <- dplyr::bind_rows(vmb_x_22, vmb_pb_22)

# VÝPOČET PASEK ----
paseky_evl <- function(hab_code, evl_site) {
  
  if(substr(hab_code, 1, 1) == 9) {
    vmb_target_sjtsk_update <- 
      vmb_pb_x_22 %>%
      sf::st_intersection(
        .,
        dplyr::filter(
          evl_sjtsk, 
          SITECODE == evl_site
          )
        ) %>%
      dplyr::mutate(
        AREA_real_update = units::drop_units(st_area(geometry))
        ) %>%
      dplyr::mutate(
        PLO_BIO_M2_EVL_update = STEJ_PR/100*AREA_real_update
        ) %>%
      dplyr::rename(
        FSB_update = FSB,
        BIOTOP_update = BIOTOP,
        STEJ_PR_update = STEJ_PR,
        ROK_AKT_update = ROK_AKT.y
        )
    
    vmb_target_sjtsk_orig <- 
      vmb_shp_sjtsk_orig %>%
      sf::st_intersection(
        .,
        dplyr::filter(
          evl_sjtsk, 
          SITECODE == evl_site
          )
        ) %>%
      dplyr::filter(
        HABITAT == hab_code
        ) %>%
      dplyr::mutate(
        AREA_real_orig = units::drop_units(st_area(geometry))
        ) %>%
      dplyr::mutate(
        PLO_BIO_M2_EVL_orig = STEJ_PR/100*AREA_real_orig
        ) %>%
      dplyr::rename(
        FSB_orig = FSB,
        BIOTOP_orig = BIOTOP,
        STEJ_PR_orig = STEJ_PR
        )
    
    vmb_target_sjtsk_intersection <- 
      sf::st_intersection(
        vmb_target_sjtsk_update, 
        vmb_target_sjtsk_orig
        ) %>%
      dplyr::mutate(
        PASEKA = dplyr::case_when(
          BIOTOP_update %in% c("LP", "X10") ~ 1,
          BIOTOP_update %in% c("X11", "X12A", "X12B") & ROK_AKT_update %in% c(2007:2012) ~ 1,
          TRUE ~ 0
          )
        ) %>%
      dplyr::mutate(
        AREA_real_intersection = units::drop_units(st_area(geometry))
        ) %>%
      dplyr::mutate(
        PLO_BIO_M2_EVL_intersection = AREA_real_intersection*STEJ_PR_orig/100*STEJ_PR_update/100
        ) %>%
      dplyr::mutate(
        HOLINA = dplyr::case_when(
          PASEKA == 1 & PLO_BIO_M2_EVL_intersection > 10000 ~ 1,
          TRUE ~ 0
          )
        )
    
    rozloha_paseky <- 
      vmb_target_sjtsk_intersection %>%
      dplyr::filter(PASEKA == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL_intersection) %>%
      sum()/10000
    
    rozloha_holiny <- 
      vmb_target_sjtsk_intersection %>%
      dplyr::filter(HOLINA == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL_intersection) %>%
      sum()/10000
    
    pocet_segmentu <- 
      vmb_target_sjtsk_intersection %>%
      dplyr::filter(PASEKA == 1) %>%
      dplyr::pull(SEGMENT_ID) %>%
      unique() %>%
      length()
    
    result <- 
      tidyr::tibble(
        SITECODE = evl_site,
        HABITAT_CODE = hab_code,
        ROZLOHA_PASEKY = rozloha_paseky,
        ROZLOHA_HOLINY = rozloha_holiny,
        POCET_SEGMENTU_PASEKY = pocet_segmentu
        )
    
  } else {
    result <- 
      tidyr::tibble(
        SITECODE = evl_site,
        HABITAT_CODE = hab_code,
        ROZLOHA_PASEKY = NA,
        ROZLOHA_HOLINY = NA,
        POCET_SEGMENTU_PASEKY = NA
        )
  }
  
  return(result)
  
}

# RESULTS ----
hu_paseky <- paseky_evl(sites_habitats[1,5], sites_habitats[1,1])
paseky_results <- matrix(NA, 1, ncol(hu_paseky)) %>% dplyr::as_tibble()
colnames(paseky_results) <- colnames(hu_paseky)
for(i in 1:nrow(sites_habitats)) {
  paseky_results <- dplyr::bind_rows(paseky_results,
                                     as.data.frame(paseky_evl(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(paseky_results, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv", 
           row.names = FALSE)


#----------------------------------------------------------#
# Sumarizacni funkce pro vypocet pasek ----
#----------------------------------------------------------#
paseky <- function(
    hab_code, 
    evl_site, 
    zakl = "VMB1",
    aktu = "VMB0",
    typ_chu = "EVL"
) {
  
  if(typ_chu == "EVL") {
    
    uzemi <- evl
    
  } else if(typ_chu == "MZCHU") {
    
    uzemi <- mzchu_sjtsk
    
  }
  
  if(aktu == "VMB0") {
    
    vmb_aktu <- vmb_pb_x_akt
    
  } else if(aktu == "VMB2") {
    
    vmb_aktu <- vmb_pb_x_a1
    
  }
  
  if(zakl == "VMB1") {
    
    vmb_zakl <- vmb_shp_sjtsk_orig
    
  } else if(zakl == "VMB2") {
    
    vmb_zakl <- vmb_shp_sjtsk_a1
    
  }
  
  if(substr(hab_code, 1, 1) == 9) {
    vmb_target_sjtsk_update <- 
      vmb_aktu %>%
      sf::st_intersection(
        .,
        dplyr::filter(
          uzemi, 
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
        ROK_AKT_update = ROK_AKT
      )
    
    vmb_target_sjtsk_orig <- 
      vmb_zakl %>%
      sf::st_intersection(
        .,
        dplyr::filter(
          uzemi, 
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
  
}

#----------------------------------------------------------#
# Vypocet GIS vrstvy ----
#----------------------------------------------------------#
paseky_spat(sites_habitats[343,5], sites_habitats[343,1])

# Inicializace progress baru
pb <- progress::progress_bar$new(
  # Přidal jsem :current/:total pro lepší přehled
  format = "  Zpracovávám [:bar] :percent | :current/:total | ETA: :eta", 
  total = nrow(sites_habitats),
  clear = FALSE,
  width = 100
)

# Loop s "odchytáváním" zpráv
for(i in 1:nrow(sites_habitats)) {
  
  # Posuneme bar
  pb$tick()
  
  # Spuštění funkce v obalce, která řeší vizuál
  tryCatch({
    withCallingHandlers({
      
      # Tvoje funkce
      paseky_spat(sites_habitats[i,5], sites_habitats[i,1])
      
    }, message = function(m) {
      # TOTO JE KLÍČOVÉ:
      # 1. Vezmeme text zprávy a odstraníme prázdné znaky na konci
      txt <- trimws(m$message, which = "right")
      
      # 2. Vypíšeme ji skrz progress bar (objeví se nad ním)
      if(nchar(txt) > 0) {
        pb$message(txt) 
      }
      
      # 3. Potlačíme původní zprávu, aby se nevytiskla 2x
      invokeRestart("muffleMessage")
    })
  }, error = function(e) {
    # Pokud nastane chyba, vypíšeme ji také hezky přes bar
    pb$message(paste("!!! CHYBA:", e$message))
  })
}

#----------------------------------------------------------#
# Vypocet sumarizace ----
#----------------------------------------------------------#
hu_paseky <- paseky(sites_habitats[343,5], sites_habitats[343,1])
paseky_results <- matrix(NA, 1, ncol(hu_paseky)) %>% dplyr::as_tibble()
colnames(paseky_results) <- colnames(hu_paseky)
for(i in 1:nrow(sites_habitats)) {
  paseky_results <- 
    dplyr::bind_rows(
    paseky_results,
    as.data.frame(paseky(sites_habitats[i,5], sites_habitats[i,1]))
    )
}
write.csv2(paseky_results, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv", 
           row.names = FALSE)


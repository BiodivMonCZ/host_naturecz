#----------------------------------------------------------#
# Nacteni VMB ----
#----------------------------------------------------------#

data_orig <- load_vmb(vmb_x = 1)
data_akt <- load_vmb(vmb_x = 0)

vmb_shp_sjtsk_orig <- data_orig$vmb_shp_sjtsk_orig

vmb_pb_x_akt <- data_akt$vmb_pb_x_akt

#----------------------------------------------------------#
# Prostorova funkce pro vypocet pasek ----
#----------------------------------------------------------#
paseky_spat <- function(
    hab_code, 
    evl_site, 
    zakl = "VMB1", 
    aktu = "VMB0", 
    typ_chu = "EVL"
) {
  
  #--------------------------------------------------#
  ## Načtení podkladových dat (na základě argumentů) ----
  #--------------------------------------------------#
  if(typ_chu == "EVL") {
    uzemi <- evl
  } else if(typ_chu == "MZCHU") {
    uzemi <- mzchu
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
  
  # Inicializace proměnné result pro případ, že se nespustí hlavní blok
  result <- NULL 
  
  if(substr(hab_code, 1, 1) == 9) {
    
    # Zjistíme CRS referenčního území
    target_crs <- sf::st_crs(uzemi)
    
    # Kontrola a transformace vmb_aktu, pokud nesedí CRS
    if (sf::st_crs(vmb_aktu) != target_crs) {
      # Volitelně: vypíše hlášku, pokud dochází k transformaci
      message("Transformuji vmb_aktu na shodný CRS...") 
      vmb_aktu <- sf::st_transform(vmb_aktu, target_crs)
    }
    
    # Kontrola a transformace vmb_zakl, pokud nesedí CRS
    if (sf::st_crs(vmb_zakl) != target_crs) {
      message("Transformuji vmb_zakl na shodný CRS...")
      vmb_zakl <- sf::st_transform(vmb_zakl, target_crs)
    }
    
    # Filtrace území pro konkrétní site (zrychlí následný intersection)
    uzemi_filter <- dplyr::filter(uzemi, SITECODE == evl_site)
    
    #--------------------------------------------------#
    ## Výpočet pro aktuální VMB ----
    #--------------------------------------------------#
    vmb_target_sjtsk_update <- 
      sf::st_intersection(
        vmb_aktu, 
        uzemi_filter
        ) %>%
      dplyr::mutate(
        AREA_real_update = units::drop_units(sf::st_area(geometry))
      ) %>%
      dplyr::mutate(
        PLO_BIO_M2_EVL_update = STEJ_PR/100 * AREA_real_update
      ) %>%
      dplyr::rename(
        FSB_update = FSB,
        BIOTOP_update = BIOTOP,
        STEJ_PR_update = STEJ_PR,
        ROK_AKT_update = ROK_AKT.x
      )
    
    #--------------------------------------------------#
    ## Výpočet pro základní VMB ----
    #--------------------------------------------------#
    vmb_target_sjtsk_orig <- 
      vmb_zakl %>%
      sf::st_intersection(., uzemi_filter) %>%
      dplyr::filter(HABITAT == hab_code) %>%
      dplyr::mutate(
        AREA_real_orig = units::drop_units(sf::st_area(geometry))
      ) %>%
      dplyr::mutate(
        PLO_BIO_M2_EVL_orig = STEJ_PR/100 * AREA_real_orig
      ) %>%
      dplyr::rename(
        FSB_orig = FSB,
        BIOTOP_orig = BIOTOP,
        STEJ_PR_orig = STEJ_PR
      )
    
    #--------------------------------------------------#
    ## Finální průnik ----
    #--------------------------------------------------#
    result <- 
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
        AREA_real_intersection = units::drop_units(sf::st_area(geometry))
      ) %>%
      dplyr::mutate(
        PLO_BIO_M2_EVL_intersection = AREA_real_intersection * STEJ_PR_orig/100 * STEJ_PR_update/100
      ) %>%
      dplyr::mutate(
        HOLINA = dplyr::case_when(
          PASEKA == 1 & PLO_BIO_M2_EVL_intersection > 10000 ~ 1,
          TRUE ~ 0
        )
      )
    
  } 
  
  # Definice cesty a názvu souboru
  file_path <- paste0("Outputs/Data/stanoviste/paseky/", evl_site, "_", hab_code, ".gpkg")
  
  # Aplikace clean_names na celý objekt (pokud existuje)
  if (!is.null(result) && nrow(result) > 0) {
    result <- janitor::clean_names(result)
  }
  
  #--------------------------------------------------#
  ## Zápis do GeoPackage ----
  #--------------------------------------------------#
  if (!is.null(result) && nrow(result) > 0) {
    
    # 1. EXPLICITNÍ SMAZÁNÍ SOUBORU, POKUD EXISTUJE
    # Toto vyřeší "GDAL Error 1 ... already exists"
    if (file.exists(file_path)) {
      tryCatch({
        file.remove(file_path)
        message(paste("Starý soubor smazán:", file_path))
      }, error = function(e) {
        stop("Nelze smazat existující soubor. Ujistěte se, že není otevřený v QGIS/ArcGIS! ", e)
      })
    }
    
    # 2. Samotný zápis
    sf::st_write(
      obj = result, 
      dsn = file_path, 
      layer = paste0(evl_site, "_", hab_code), 
      driver = "GPKG",
      quiet = TRUE
      # delete_dsn už není potřeba, smazali jsme ho ručně o krok výše
    )
    
    message(paste("Pro", evl_site, "a", hab_code, "vrstva zapsána."))
    
  } else {
    message(paste("Pro", evl_site, "a", hab_code, "nevznikl žádný průnik."))
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


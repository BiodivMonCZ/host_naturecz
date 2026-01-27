# Define folder with your GPKG files
data_folder <- "Outputs/Data/stanoviste/paseky/MZCHU" 

# Load all GPKG files into one data frame
# We drop geometry immediately to save RAM, as we only need the attributes for the summary.
cached_data <- dir_ls(data_folder, glob = "*.gpkg") %>% 
  map_dfr(function(file_path) {
    
    st_read(file_path, quiet = TRUE) %>%
      st_drop_geometry() %>% 
      # Standardize column names to lowercase to avoid case-sensitivity issues
      rename_with(tolower) %>% 
      # Ensure numeric columns are actually numeric
      mutate(
        plo_bio_m2_evl_intersection = as.numeric(plo_bio_m2_evl_intersection),
        paseka = as.numeric(paseka),
        holina = as.numeric(holina)
      )
    
  }, .id = "source_file")

# Create an index for faster filtering (optional but good for large data)
# We assume 'sitecode' and 'habitat' are the key lookups

#----------------------------------------------------------#
# Sumarizacni funkce pro vypocet pasek ----
#----------------------------------------------------------#
paseky <- function(
    hab_code, 
    evl_site, 
    zakl = "VMB1", 
    aktu = "VMB0", 
    typ_chu
) {
  
  # Preserving the original logic: Only process if habitat starts with 9
  if(substr(hab_code, 1, 1) == 9) {
    
    # 1. Filter the pre-loaded data instead of calculating intersection
    # We filter by site and habitat. 
    # Note: We use 'sitecode' and 'habitat' (lowercase) to match the standardized cached_data
    vmb_target_data <- cached_data %>%
      filter(
        sitecode_1 == evl_site) %>%
      filter(
        habitat == hab_code | biotop_orig == hab_code
      )
    
    # 2. Calculate summaries using the existing columns
    # The GPKG files already contain 'paseka', 'holina', and 'plo_bio_m2_evl_intersection'
    
    rozloha_paseky <- vmb_target_data %>%
      filter(paseka == 1) %>%
      pull(plo_bio_m2_evl_intersection) %>%
      sum(na.rm = TRUE) / 10000
    
    rozloha_holiny <- vmb_target_data %>%
      filter(holina == 1) %>%
      pull(plo_bio_m2_evl_intersection) %>%
      sum(na.rm = TRUE) / 10000
    
    pocet_segmentu <- vmb_target_data %>%
      filter(paseka == 1) %>%
      pull(segment_id) %>%
      n_distinct() # Safer than unique() %>% length()
    
    # 3. Construct the result tibble
    result <- tidyr::tibble(
      SITECODE = evl_site,
      HABITAT_CODE = hab_code,
      ROZLOHA_PASEKY = rozloha_paseky,
      ROZLOHA_HOLINY = rozloha_holiny,
      POCET_SEGMENTU_PASEKY = pocet_segmentu
    )
    
  } else {
    
    # Return NAs if not a forest habitat (original structure)
    result <- tidyr::tibble(
      SITECODE = evl_site,
      HABITAT_CODE = hab_code,
      ROZLOHA_PASEKY = NA,
      ROZLOHA_HOLINY = NA,
      POCET_SEGMENTU_PASEKY = NA
    )
  }
  
  return(vmb_target_data)
}

#----------------------------------------------------------#
# Vypocet GIS vrstvy ----
#----------------------------------------------------------#
paseky(sites_habitats_mzchu_test[9,5], sites_habitats[9,1])

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


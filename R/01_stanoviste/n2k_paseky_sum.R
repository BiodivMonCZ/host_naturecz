# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# Paseky sumarizace
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
#
# Jediné, co je potřeba upravovat při opakovaném běhu jsou položky v CONFIG.
#
# data_folder = root dir pro 3 podsložky "VMBx_VMBy" (obsahující GPKG soubory) &
#               "latest_paseky" s výstupy ze skriptu pro výpočet nejnovějších
#               kombinací pasek
# input_data = definuje kombinace předmětů ochrany a sitecode, načítá se ze
#              skriptu 00_n2k_config.R
# tpchu = typ chráněného území, tj. "EVL" nebo "MZCHU"
#
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# CONFIG
data_folder <- "Outputs/Data/stanoviste/paseky/EVL"
input_data <- sites_habitats
tpchu <- "EVL"

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# Load latest combination for paseky
latest_choice <- readr::read_csv(
  file.path(data_folder, "latest_paseky", "latest_choice.csv"),
  show_col_types = FALSE
) %>%
  dplyr::rename_with(tolower) %>%
  dplyr::select(-dplyr::any_of(c("...1", "x"))) %>%
  dplyr::mutate(
    sitecode = as.character(sitecode),
    habitat = as.character(habitat),
    region_id = as.character(region_id),
    pair = as.character(pair)
  )

# Folders with GPKG data
pair_folders <- c(
  VMB1_VMB2 = file.path(data_folder, "VMB1_VMB2"),
  VMB2_VMB0 = file.path(data_folder, "VMB2_VMB0"),
  VMB1_VMB0 = file.path(data_folder, "VMB1_VMB0")
)

# Load data into cache
cached_data <- purrr::imap_dfr(pair_folders, function(folder_path, pair_name) {
  
  fs::dir_ls(folder_path, glob = "*.gpkg") %>%
    purrr::map_dfr(function(file_path) {
      
      sf::st_read(file_path, quiet = TRUE) %>%
        sf::st_drop_geometry() %>%
        dplyr::rename_with(tolower) %>%
        dplyr::mutate(
          pair = as.character(pair_name),
          source_file = basename(file_path),
          
          sitecode_1 = as.character(sitecode),
          region_id = as.character(`region_id.x`),
          habitat = as.character(habitat),
          biotop_orig = as.character(biotop_orig),
          
          plo_bio_m2_evl_intersection = as.numeric(plo_bio_m2_evl_intersection),
          paseka = as.numeric(paseka),
          holina = as.numeric(holina)
        )
    })
})

#----------------------------------------------------------#
# Sumarizacni funkce pro vypocet pasek ----
#----------------------------------------------------------#
paseky <- function(
    hab_code, 
    evl_site,
    typ_chu
) {
  
  # Preserving the original logic: Only process if habitat starts with 9
  if(substr(hab_code, 1, 1) == 9 | substr(hab_code, 1, 1) == "L") {
    
    # 1. Filter the pre-loaded data instead of calculating intersection
    # Select the newest available pair separately for each region_id
    latest_current <- latest_choice %>%
      dplyr::filter(
        sitecode == evl_site,
        habitat == hab_code
      ) %>%
      dplyr::select(
        sitecode,
        habitat,
        region_id,
        pair
      ) %>%
      dplyr::distinct()
    
    vmb_target_data <- cached_data %>%
      dplyr::inner_join(
        latest_current,
        by = c(
          "sitecode_1" = "sitecode",
          "region_id" = "region_id",
          "pair" = "pair"
        )
      ) %>%
      dplyr::filter(
        habitat.x == hab_code | biotop_orig == hab_code
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
      TYP_CHU = as.character(typ_chu),
      SITECODE = as.character(evl_site),
      HABITAT_CODE = as.character(hab_code),
      ROZLOHA_PASEKY = rozloha_paseky,
      ROZLOHA_HOLINY = rozloha_holiny,
      POCET_SEGMENTU_PASEKY = pocet_segmentu
    )
    
  } else {
    
    # Return NAs if not a forest habitat (original structure)
    result <- tidyr::tibble(
      TYP_CHU = as.character(typ_chu),
      SITECODE = as.character(evl_site),
      HABITAT_CODE = as.character(hab_code),
      ROZLOHA_PASEKY = NA,
      ROZLOHA_HOLINY = NA,
      POCET_SEGMENTU_PASEKY = NA
    )
  }
  
  return(result)
}

#----------------------------------------------------------#
# Loop pres sitecode-habitat ----
#----------------------------------------------------------#

# Ensure 'input_data' is defined.

# Initialize Result Storage
# We create an empty tibble with the correct columns to start
paseky_results <- tibble(
  TYP_CHU = character(),
  SITECODE = character(),
  HABITAT_CODE = character(),
  ROZLOHA_PASEKY = numeric(),
  ROZLOHA_HOLINY = numeric(),
  POCET_SEGMENTU_PASEKY = integer()
)

# Initialize Progress Bar
pb <- progress::progress_bar$new(
  format = "  Zpracovávám [:bar] :percent | :current/:total | ETA: :eta", 
  total = nrow(input_data),
  clear = FALSE,
  width = 100
)

# The Loop
for(i in 1:nrow(input_data)) {
  
  pb$tick()
  
  tryCatch({
    withCallingHandlers({
      
      # 1. Run the calculation
      # Using columns 5 (hab_code) and 1 (sitecode)
      current_row <- paseky(
        hab_code = input_data[i, 5], 
        evl_site = input_data[i, 1], 
        typ_chu = tpchu
      )
      
      # 2. Bind to main results
      paseky_results <- bind_rows(paseky_results, current_row)
      
    }, message = function(m) {
      # Handle messages cleanly in PB
      txt <- trimws(m$message, which = "right")
      if(nchar(txt) > 0) pb$message(txt) 
      invokeRestart("muffleMessage")
    })
  }, error = function(e) {
    # Handle errors cleanly in PB
    pb$message(paste("!!! CHYBA [Row", i, "]:", e$message))
  })
}

#-------------------------------------------------------------------------#
# 4. SAVE OUTPUT
#-------------------------------------------------------------------------#

write.csv2(
  paseky_results, 
  file.path(data_folder, paste0("paseky_results_", format(Sys.Date(), "%Y%m%d"), ".csv")), 
  row.names = FALSE
)

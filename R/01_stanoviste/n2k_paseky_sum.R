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
        sitecode = as.character(sitecode),
        habitat = as.character(habitat),
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
  if(substr(hab_code, 1, 1) == 9 | substr(hab_code, 1, 1) == "L") {
    
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
# Vypocet GIS vrstvy ----
#----------------------------------------------------------#
paseky("L6.3", "5874", typ_chu = "MZCHU")

#----------------------------------------------------------#
# Vypocet sumarizace ----
#----------------------------------------------------------#
# Ensure 'sites_habitats' is defined. 
# Using a generic name here - replace with 'sites_habitats_mzchu_test' if needed
input_data <- sites_habitats_mzchu_test 

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
      # Using columns 5 (hab_code) and 1 (sitecode) as per your snippet
      current_row <- paseky(
        hab_code = input_data[i, 5], 
        evl_site = input_data[i, 1], 
        typ_chu = "MZCHU"
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
# Create directory if it doesn't exist
dir_create("Outputs/Data/stanoviste/paseky/MZCHU/")

write.csv2(
  paseky_results, 
  "Outputs/Data/stanoviste/paseky/MZCHU/paseky_results_20260127.csv", 
  row.names = FALSE
)

print("Hotovo!")

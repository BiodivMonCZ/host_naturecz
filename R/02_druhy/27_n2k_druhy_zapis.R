#----------------------------------------------------------#
# Priprava prostredi ----- 
#----------------------------------------------------------#

# 1. Definice klicove promenne pro urceni sloupcu
ncol_orig <- ncol(n2k_load) 

#----------------------------------------------------------#
# Druhovy seznam----- 
#----------------------------------------------------------#

#species_list <- unique(subset(n2k_load, SKUPINA == "Obojživelníci")$DRUH)
species_list <- n2k_load %>% 
  dplyr::filter(SKUPINA == "Ryby a mihule") %>% 
  dplyr::pull(DRUH) %>% 
  unique() %>% 
  as.character()
species_list <- "Pulsatilla patens"

# Spocitame celkovy pocet pro hlasky
N_species <- length(species_list)

#----------------------------------------------------------#
# 1. Uroven akce (Vypocet indikatoru) ----- 
#----------------------------------------------------------#
message(paste0("--- ZAČÍNÁM VÝPOČET FÁZE 1 (AKCE) PRO ", N_species, " DRUHŮ ---"))

n2k_druhy <- lapply(seq_along(species_list), function(i) {
  
  sp <- species_list[i]
  
  # Hlaska o postupu
  message(sprintf("[1/4 Akce] %s (%d/%d) - Zbývá: %d", sp, i, N_species, N_species - i))
  
  run_n2k_druhy(n2k_load, sp, sites_subjects, limity, current_year = 2024)
}) %>%
  dplyr::bind_rows() 

readr::write_csv(
  n2k_druhy,
  paste0("Data/Temp/n2k_druhy", ".csv")
)

#----------------------------------------------------------#
# 2. Porovnani s limity ----- 
#----------------------------------------------------------#
message(paste0("--- ZAČÍNÁM VÝPOČET FÁZE 2 (LIMITY) ---"))

n2k_druhy_lim <- lapply(seq_along(species_list), function(i) {
  
  sp <- species_list[i]
  
  # Hlaska o postupu
  message(sprintf("[2/4 Limity] %s (%d/%d) - Zbývá: %d", sp, i, N_species, N_species - i))
  
  data_subset <- n2k_druhy %>% dplyr::filter(DRUH == sp)
  
  if(nrow(data_subset) == 0) return(NULL)
  
  run_n2k_druhy_lim(data_subset, sp, sites_subjects, limity, current_year = 2024)
  
}) %>%
  dplyr::bind_rows()

readr::write_csv(
  n2k_druhy_lim,
  paste0("Data/Temp/n2k_druhy_lim", ".csv")
)

#----------------------------------------------------------#
# 3. Uroven lokality (Agregace a semafor) ----- 
#----------------------------------------------------------#
#--------------------------------------------------#
## Nacteni temp dat ----
#--------------------------------------------------#
n2k_druhy_lim <- 
  readr::read_csv(
    "Data/Temp/n2k_druhy_lim.csv"
  )

ncol_druhy_lim <- 
  ncol(
    n2k_druhy_lim
  )

message(paste0("--- ZAČÍNÁM VÝPOČET FÁZE 3 (LOKALITY) ---"))

n2k_druhy_lok <- lapply(seq_along(species_list), function(i) {
  
  sp <- species_list[i]
  
  # Hlaska o postupu
  message(sprintf("[3/4 Lokality] %s (%d/%d) - Zbývá: %d", sp, i, N_species, N_species - i))
  
  data_subset <- n2k_druhy_lim %>% dplyr::filter(DRUH == sp)
  
  if(nrow(data_subset) == 0) return(NULL)
  
  run_n2k_druhy_lok(data_subset, sp, sites_subjects, limity, current_year = 2024)
  
}) %>%
  dplyr::bind_rows() 

readr::write_csv(
  n2k_druhy_lok,
  paste0("Data/Temp/n2k_druhy_lok", ".csv")
)

#----------------------------------------------------------#
# 4. Uroven uzemi (Celkove hodnoceni) ----- 
#----------------------------------------------------------#
message(paste0("--- ZAČÍNÁM VÝPOČET FÁZE 4 (ÚZEMÍ) ---"))

n2k_druhy_uzemi <- lapply(seq_along(species_list), function(i) {
  
  sp <- species_list[i]
  
  # Hlaska o postupu
  message(sprintf("[4/4 Území] %s (%d/%d) - Zbývá: %d", sp, i, N_species, N_species - i))
  
  data_subset <- n2k_druhy_lok %>% dplyr::filter(DRUH == sp)
  
  if(nrow(data_subset) == 0) return(NULL)
  
  run_n2k_druhy_uzemi(
    data_subset, sp, sites_subjects, limity, biotop_evd, 
    current_year = 2024
  )
}) %>%
  dplyr::bind_rows()

readr::write_csv(
  n2k_druhy_uzemi,
  paste0("Data/Temp/n2k_druhy_uzemi", ".csv")
)

message("--- HOTOVO: Všechny výpočty dokončeny ---")


#----------------------------------------------------------#
# Zapis dat lok -----
#----------------------------------------------------------#

lok_export <-
  function() {
    
    n2k_druhy_lok_write <-
      n2k_druhy_lokeval %>%
      dplyr::left_join(
        ., 
        evl %>%
          sf::st_drop_geometry() %>%
          dplyr::select(
            SITECODE, 
            NAZEV
          ),
        by = c(
          "kod_chu" = "SITECODE"
        )
      ) %>%
      dplyr::left_join(
        ., 
        n2k_druhy_obdobi_lok,
        by = join_by(
          "kod_chu",
          "KOD_LOKAL",
          "POLE",
          "DRUH",
        )
      ) %>%
      dplyr::left_join(
        .,
        rp_code,
        by = join_by(
          "kod_chu"
        )
      ) %>%
      dplyr::left_join(
        .,
        n2k_oop,
        by = c("kod_chu" = "SITECODE")
      ) %>%
      dplyr::distinct()
    
    sep_isop <- ";"
    quote_env_isop <- FALSE
    encoding_isop <- "UTF-8"
    
    sep <- ","
    quote_env <- TRUE
    encoding <- "Windows-1250"
    
    write.table(
      n2k_druhy_lok_write,
      paste0(
        "Outputs/Data/druhy/",
        "n2k_druhy_lok",
        "_",
        current_year,
        "_",
        gsub(
          "-", 
          "", 
          Sys.Date()
          ),
        "_",
        encoding,
        ".csv"
      ),
      row.names = FALSE,
      sep = sep,
      quote = quote_env,
      fileEncoding = encoding
    )  
    
    write.table(
      n2k_druhy_lok_write,
      paste0(
        "Outputs/Data/druhy/",
        "n2k_druhy_lok",
        "_",
        current_year,
        "_",
        gsub(
          "-", 
          "", 
          Sys.Date()
          ),
        "_",
        encoding_isop,
        ".csv"
      ),
      row.names = FALSE,
      sep = sep_isop,
      quote = quote_env_isop,
      fileEncoding = encoding_isop
    )  
    
  }


#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
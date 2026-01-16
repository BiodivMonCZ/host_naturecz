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
species_list <- n2k_load %>% 
  dplyr::filter(SKUPINA == "Cévnaté rostliny") %>% 
  dplyr::pull(DRUH) %>% 
  unique() %>% 
  as.character()

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
# Zapis dat nalez -----
#----------------------------------------------------------#

nal_export <- function(
    n2k_load,
    species_list,
    sites_subjects,
    limity,
    evl,
    rp_code,
    n2k_oop,
    current_year = 2024
) {
  
  # Zajisteni existence slozky pro docasne soubory
  if (!dir.exists("Data/Temp/")) {
    dir.create("Data/Temp/", recursive = TRUE)
  }
  
  N_species <- length(species_list)
  
  #----------------------------------------------------------#
  # 1. Uroven akce (Vypocet indikatoru) ----- 
  #----------------------------------------------------------#
  message(paste0("--- ZACINAM VYPOCET FAZE 1 (AKCE) PRO ", N_species, " DRUHU ---"))
  
  n2k_druhy <- lapply(seq_along(species_list), function(i) {
    
    sp <- species_list[i]
    message(sprintf("[1/4 Akce] %s (%d/%d) - Zbyva: %d", sp, i, N_species, N_species - i))
    
    run_n2k_druhy(n2k_load, sp, sites_subjects, limity, current_year = current_year)
    
  }) %>%
    dplyr::bind_rows() 
  
  # Ulozeni mezivysledku 1
  message("--- UKLADAM MEZIVYSLEDEK 1 DO TEMP ---")
  readr::write_csv(
    n2k_druhy,
    paste0("Data/Temp/n2k_druhy", ".csv")
  )
  
  #----------------------------------------------------------#
  # 2. Porovnani s limity ----- 
  #----------------------------------------------------------#
  message(paste0("--- ZACINAM VYPOCET FAZE 2 (LIMITY) ---"))
  
  if (nrow(n2k_druhy) == 0) {
    warning("Faze 1 nevygenerovala zadna data. Faze 2 a export budou preskoceny.")
    return(NULL)
  }
  
  n2k_druhy_lim <- lapply(seq_along(species_list), function(i) {
    
    sp <- species_list[i]
    message(sprintf("[2/4 Limity] %s (%d/%d) - Zbyva: %d", sp, i, N_species, N_species - i))
    
    data_subset <- n2k_druhy %>% dplyr::filter(DRUH == sp)
    
    if(nrow(data_subset) == 0) return(NULL)
    
    run_n2k_druhy_lim(data_subset, sp, sites_subjects, limity, current_year = current_year)
    
  }) %>%
    dplyr::bind_rows()
  
  # Ulozeni mezivysledku 2 (Vstup pro lok_export)
  message("--- UKLADAM MEZIVYSLEDEK 2 DO TEMP ---")
  readr::write_csv(
    n2k_druhy_lim,
    paste0("Data/Temp/n2k_druhy_lim", ".csv")
  )
  
  #----------------------------------------------------------#
  # 3. Propojeni s metadaty a serazeni sloupcu ----- 
  #----------------------------------------------------------#
  
  message("--- PRIPRAVA DAT PRO EXPORT (NALEZY) ---")
  
  if (nrow(n2k_druhy_lim) == 0) {
    warning("Faze 2 nevygenerovala zadna data. Export se neprovede.")
    return(NULL)
  }
  
  n2k_druhy_lim_write <- n2k_druhy_lim %>%
    # Pripojeni informaci o EVL
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
    # Pripojeni kodu RP
    dplyr::left_join(
      .,
      rp_code,
      by = dplyr::join_by(
        "kod_chu"
      )
    ) %>%
    # Pripojeni informaci o OOP
    dplyr::left_join(
      .,
      n2k_oop,
      by = c("kod_chu" = "SITECODE")
    ) %>%
    dplyr::distinct() %>%
    # Logicke serazeni sloupcu (stejna logika jako u lok_export)
    dplyr::select(
      # 1. Identifikace
      ROK,
      kod_chu,
      NAZEV,          # Nazev EVL
      DRUH,
      SKUPINA,
      
      # 2. Lokalita
      KOD_LOKAL,
      LOKALITA,
      pracoviste,
      oop,
      
      # 3. Akce a Nalez
      IDX_ND_AKCE,
      ID_ND_NALEZ,    # Specificke pro nal_export
      DATUM,
      AUTOR,          # Specificke pro nal_export
      
      # 4. Definice Indikatoru
      ID_IND,
      JEDNOTKA,
      TYP_IND,
      IND_GRP,
      KLIC,
      UROVEN,
      
      # 5. Vysledky
      LIM_IND,
      HOD_IND,
      STAV_IND,
      
      # Zbytek
      dplyr::everything()
    )
  
  #----------------------------------------------------------#
  # 4. Export dat ----- 
  #----------------------------------------------------------#
  
  sep_isop <- ";"
  quote_env_isop <- FALSE
  encoding_isop <- "UTF-8"
  
  sep <- ","
  quote_env <- TRUE
  encoding <- "Windows-1250"
  
  date_stamp <- gsub("-", "", Sys.Date())
  file_base <- paste0("Outputs/Data/druhy/n2k_druhy_nal_", current_year, "_", date_stamp)
  
  message(paste0("--- EXPORTUJI SOUBORY DO: ", file_base, "... ---"))
  
  if (!dir.exists("Outputs/Data/druhy/")) {
    dir.create("Outputs/Data/druhy/", recursive = TRUE)
  }
  
  # Export 1: Windows-1250
  write.table(
    n2k_druhy_lim_write,
    paste0(file_base, "_", encoding, ".csv"),
    row.names = FALSE,
    sep = sep,
    quote = quote_env,
    fileEncoding = encoding
  )  
  
  # Export 2: UTF-8
  write.table(
    n2k_druhy_lim_write,
    paste0(file_base, "_", encoding_isop, ".csv"),
    row.names = FALSE,
    sep = sep_isop,
    quote = quote_env_isop,
    fileEncoding = encoding_isop
  )  
  
  message("--- HOTOVO (NAL_EXPORT) ---")
  
  return(n2k_druhy_lim_write)
  
}

nal_export(n2k_load, species_list, sites_subjects, limity, evl, rp_code, n2k_oop)

#----------------------------------------------------------#
# Zapis mapy indikatoru ----
#----------------------------------------------------------#
#source("R/0_Config/01_n2k_map_ind.R")
#map <- build_indicator_map(
#  n2k_druhy,
#  limity,
#  script = "R/2_druhy/21_n2k_druhy_akce.R",
#  out_file = "Outputs/indicator_map_n2k.csv"
#)
#----------------------------------------------------------#
# Zapis dat lok -----
#----------------------------------------------------------#

lok_export <- function(
    species_list,
    sites_subjects,
    limity,
    evl,
    n2k_druhy_obdobi_lok,
    rp_code,
    n2k_oop,
    current_year = 2024,
    input_path = "Data/Temp/n2k_druhy_lim.csv"
) {
  
  #--------------------------------------------------#
  # 1. Nacteni temp dat ----
  #--------------------------------------------------#
  if (!file.exists(input_path)) {
    stop(paste0("Input file not found: ", input_path))
  }
  
  n2k_druhy_lim <- readr::read_csv(input_path, show_col_types = FALSE)
  
  message("--- ZACINAM VYPOCET FAZE 3 (LOKALITY) ---")
  
  N_species <- length(species_list)
  
  #--------------------------------------------------#
  # 2. Vypocet - Agregace na uroven lokality ----
  #--------------------------------------------------#
  
  n2k_druhy_lok <- lapply(seq_along(species_list), function(i) {
    
    sp <- species_list[i]
    message(sprintf("[3/4 Lokality] %s (%d/%d) - Zbyva: %d", sp, i, N_species, N_species - i))
    
    data_subset <- n2k_druhy_lim %>% dplyr::filter(DRUH == sp)
    
    if(nrow(data_subset) == 0) return(NULL)
    
    # Volani vypocetni funkce pro lokalitu
    run_n2k_druhy_lok(data_subset, sp, sites_subjects, limity, current_year = current_year)
    
  }) %>%
    dplyr::bind_rows() 
  
  if (is.null(n2k_druhy_lok) || nrow(n2k_druhy_lok) == 0) {
    warning("Zadna data nebyla vygenerovana (n2k_druhy_lok je prazdne). Export se neprovede.")
    return(NULL)
  }
  
  # --- UKLADANI TEMP DAT ---
  # Ulozeni mezivysledku pro dalsi funkce
  message("--- UKLADAM MEZIVYSLEDEK DO TEMP ---")
  
  # Ujistime se, ze existuje slozka (i kdyz pro cteni existovala)
  if (!dir.exists("Data/Temp/")) {
    dir.create("Data/Temp/", recursive = TRUE)
  }
  
  readr::write_csv(
    n2k_druhy_lok,
    paste0("Data/Temp/n2k_druhy_lok", ".csv")
  )
  
  #--------------------------------------------------#
  # 3. Propojeni s metadaty a serazeni sloupcu ----
  #--------------------------------------------------#
  
  message("--- PRIPRAVA DAT PRO EXPORT ---")
  
  n2k_druhy_lok_write <- n2k_druhy_lok %>%
    # Pripojeni informaci o EVL
    dplyr::left_join(
      ., 
      evl %>%
        sf::st_drop_geometry() %>%
        dplyr::select(SITECODE, NAZEV),
      by = c("kod_chu" = "SITECODE")
    ) %>%
    # Pripojeni informaci o obdobich
    dplyr::left_join(
      ., 
      n2k_druhy_obdobi_lok,
      by = dplyr::join_by("kod_chu", "KOD_LOKAL", "POLE", "DRUH")
    ) %>%
    # Pripojeni kodu RP
    dplyr::left_join(
      .,
      rp_code,
      by = dplyr::join_by("kod_chu")
    ) %>%
    # Pripojeni informaci o OOP
    dplyr::left_join(
      .,
      n2k_oop,
      by = c("kod_chu" = "SITECODE")
    ) %>%
    dplyr::distinct() %>%
    # Logicke serazeni sloupcu
    dplyr::select(
      # 1. Identifikace (Kde, Kdo, Kdy)
      ROK,
      kod_chu,
      NAZEV,          # Nazev EVL
      DRUH,
      SKUPINA,
      
      # 2. Lokalita a Odpovednost
      KOD_LOKAL,
      NAZEV_LOK,
      POLE,
      pracoviste,
      oop,
      
      # 3. Obdobi a Akce
      HODNOCENE_OBDOBI_OD,
      HODNOCENE_OBDOBI_DO,
      ID_ND_AKCE,
      DATUM,
      CILMON,
      
      # 4. Definice Indikatoru
      ID_IND,
      JEDNOTKA,
      TYP_IND,
      IND_GRP,
      KLIC,
      UROVEN,
      
      # 5. Vysledky a Limity
      LIM_IND,
      LIM_INDLIST,
      HOD_IND,
      STAV_IND,
      
      # Zbytek
      dplyr::everything()
    )
  
  #--------------------------------------------------#
  # 4. Export dat ----
  #--------------------------------------------------#
  
  sep_isop <- ";"
  quote_env_isop <- FALSE
  encoding_isop <- "UTF-8"
  
  sep <- ","
  quote_env <- TRUE
  encoding <- "Windows-1250"
  
  date_stamp <- gsub("-", "", Sys.Date())
  file_base <- paste0("Outputs/Data/druhy/n2k_druhy_lok_", current_year, "_", date_stamp)
  
  message(paste0("--- EXPORTUJI SOUBORY DO: ", file_base, "... ---"))
  
  if (!dir.exists("Outputs/Data/druhy/")) {
    dir.create("Outputs/Data/druhy/", recursive = TRUE)
  }
  
  # Export 1: Windows-1250
  write.table(
    n2k_druhy_lok_write,
    paste0(file_base, "_", encoding, ".csv"),
    row.names = FALSE,
    sep = sep,
    quote = quote_env,
    fileEncoding = encoding
  )  
  
  # Export 2: UTF-8
  write.table(
    n2k_druhy_lok_write,
    paste0(file_base, "_", encoding_isop, ".csv"),
    row.names = FALSE,
    sep = sep_isop,
    quote = quote_env_isop,
    fileEncoding = encoding_isop
  )  
  
  message("--- HOTOVO ---")
  
  return(n2k_druhy_lok_write)
}

lok_export(species_list, sites_subjects, limity, evl, n2k_druhy_obdobi_lok, rp_code, n2k_oop)

chu_export <- function(
    n2k_druhy_uzemi,
    evl,
    rp_code,
    n2k_oop,
    indikatory_id,
    sites_subjects,
    current_year = 2024
) {
  
  message("--- ZACINAM PRIPRAVU DAT PRO EXPORT (CHU) ---")
  
  # Kontrola vstupu
  if (is.null(n2k_druhy_uzemi) || nrow(n2k_druhy_uzemi) == 0) {
    warning("Vstupni data (n2k_druhy_uzemi) jsou prazdna. Export se neprovede.")
    return(NULL)
  }
  
  #----------------------------------------------------------#
  # 1. Zpracovani a formatovani dat -----
  #----------------------------------------------------------#
  
  n2k_druhy_chu_write <- n2k_druhy_uzemi %>%
    # Pripojeni informaci o EVL (nazev)
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
    # Pripojeni kodu RP
    dplyr::left_join(
      .,
      rp_code,
      by = dplyr::join_by(
        "kod_chu"
      )
    ) %>%
    # Pripojeni informaci o OOP
    dplyr::left_join(
      ., 
      n2k_oop,
      by = c(
        "kod_chu" = "SITECODE"
      )
    ) %>%
    # Pridani konstantnich sloupcu
    dplyr::mutate(
      typ_predmetu_hodnoceni = "Druh",
      feature_code = NA,
      trend = "neznámý",
      datum_hodnoceni = Sys.Date()
    ) %>%
    # Filtrace pouze na CILOVY MONITORING CHU
    dplyr::filter(
      CILMON_CHU == 1
    ) %>%
    # Prejmenovani sloupcu pro vystup
    dplyr::rename(
      nazev_chu = NAZEV, 
      druh = DRUH, 
      hodnocene_obdobi_od = HODNOCENE_OBDOBI_OD, 
      hodnocene_obdobi_do = HODNOCENE_OBDOBI_DO, 
      parametr_nazev = ID_IND, 
      parametr_hodnota = HOD_IND, 
      parametr_limit = LIM_IND, 
      parametr_jednotka = JEDNOTKA, 
      stav = STAV_IND
    ) %>%
    # Vyber relevantnich sloupcu
    dplyr::select(
      typ_predmetu_hodnoceni, 
      kod_chu, 
      nazev_chu, 
      druh, 
      feature_code, 
      hodnocene_obdobi_od, 
      hodnocene_obdobi_do, 
      oop, 
      parametr_nazev, 
      parametr_hodnota, 
      parametr_limit, 
      parametr_jednotka, 
      stav, 
      trend, 
      datum_hodnoceni, 
      pracoviste, 
      ID_ND_AKCE
    ) %>%
    # Mapovani nazvu indikatoru dle ciselniku
    dplyr::left_join(
      ., 
      indikatory_id, 
      by = c("parametr_nazev" = "ind_r")
    ) %>%
    # Cisteni textu a nastaveni metodiky
    dplyr::mutate(
      parametr_nazev = ind_id,
      pracoviste = gsub(
        ",",
        "",
        pracoviste
      ),
      metodika = 15087
    ) %>%
    # Navazani pouze na predmety ochrany (sites_subjects)
    # Right join zajisti, ze ve vystupu budou vsechny predmety ochrany,
    # i ty, ktere nemaji data (budou mit NA)
    dplyr::right_join(
      .,
      sites_subjects %>%
        dplyr::select(
          site_code,
          nazev_lat
        ),
      by = c(
        "kod_chu" = "site_code",
        "druh" = "nazev_lat"
      )
    ) %>%
    # Odstraneni pomocnych sloupcu
    dplyr::select(
      -c(
        ind_id,
        ind_popis, 
        ID_ND_AKCE
      )
    ) %>%
    dplyr::distinct()
  
  #----------------------------------------------------------#
  # 2. Export dat -----
  #----------------------------------------------------------#
  
  # Nastaveni pro export
  sep_isop <- ";"
  quote_env_isop <- FALSE
  encoding_isop <- "UTF-8"
  
  sep <- ","
  quote_env <- TRUE
  encoding <- "Windows-1250"
  
  date_stamp <- gsub("-", "", Sys.Date())
  file_base <- paste0("Outputs/Data/druhy/n2k_druhy_chu_", current_year, "_", date_stamp)
  
  message(paste0("--- EXPORTUJI SOUBORY DO: ", file_base, "... ---"))
  
  # Kontrola adresare
  if (!dir.exists("Outputs/Data/druhy/")) {
    dir.create("Outputs/Data/druhy/", recursive = TRUE)
  }
  
  # Export 1: Windows-1250 (Excel)
  write.table(
    n2k_druhy_chu_write,
    paste0(file_base, "_", encoding, ".csv"),
    row.names = FALSE,
    sep = sep,
    quote = quote_env,
    fileEncoding = encoding
  )  
  
  # Export 2: UTF-8 (ISOP/Standard)
  write.table(
    n2k_druhy_chu_write,
    paste0(file_base, "_", encoding_isop, ".csv"),
    row.names = FALSE,
    sep = sep_isop,
    quote = quote_env_isop,
    fileEncoding = encoding_isop
  )  
  
  message("--- HOTOVO (CHU_EXPORT) ---")
  
  return(n2k_druhy_chu_write)
}

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
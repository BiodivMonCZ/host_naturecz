#----------------------------------------------------------#
# ZDROJOVA DATA A CISELNIKY - STANOVISTE -----
#----------------------------------------------------------#
# Vse, co potrebuje vetev hodnoceni stanovist (R/01_stanoviste,
# R/03_Stanoviste_analyza) a co nepotrebuji druhy.
#
# Vytvarene objekty: evl_lengths, sites_habitats, sites_subjects_mzchu,
# sites_habitats_mzchu, sites_habitats_mzchu_test, sdo_II_sites,
# cis_habitat, mzchu, biotop_zvld, n2k_union, minimisize,
# habitat_areas_2022, habitat_areas_a1, czechia, czechia_line,
# akt_okrsky, redlist_species, invasive_species, expansive_species.
#
# POZNAMKA k zarazeni: 'biotop_zvld' a 'n2k_union' se dnes nepouzivaji
# nikde, 'akt_okrsky' a exporty redlist/invaze/expanze maji chybejici
# zdrojove soubory. Jsou zde proto, ze je vetev druhu nepotrebuje - drive
# byly v obecnem configu a shazovaly cely beh jeste pred nactenim NDOP.
# Pokud nekam patri jinam, staci je presunout - na vetev druhu to nema vliv.

#----------------------------------------------------------#
# Zavislost na obecnem configu ----
#----------------------------------------------------------#
# Datovy skript stoji na objektech z 00_n2k_config.R (limity, ciselniky,
# vrstvy EVL/PO, slozka_lokal, pomocne funkce). Pokud config jeste nebezel,
# spusti se automaticky; uz nactena data se znovu necetou.

if (!exists("limity")) {
  source("R/00_config/00_n2k_config.R", encoding = "UTF-8")
}

#--------------------------------------------------#
### Delky EVL ----
# MAXIMÁLNÍ VZDÁLENOST MEZI 2 BODY PRO KAŽDOU EVL - LINESTRINGY BYLY PŘEVEDENY NA MULTIPOINT 
# PRO EVL S OBVODEM < 10 KM BYLY POUŽITY VŠECHNY BODY, PRO VĚTŠÍ EVL KAŽDÝ SEDMÝ
#--------------------------------------------------#
evl_lengths <- 
  readr::read_csv(
    "Data/Input/evl_max_dist.csv", 
    locale = readr::locale(encoding = "UTF-8")
  )

#--------------------------------------------------#
### Predmety ochrany EVL - stanoviste ----
#--------------------------------------------------#
# Odvozeno ze 'sites_subjects' nacteneho v obecnem configu.
sites_habitats <- sites_subjects %>%
  dplyr::filter(feature_type == "stanoviště")

#--------------------------------------------------#
### Seznam predmetu ochrany MZCHU ---- 
#--------------------------------------------------#
sites_subjects_mzchu <- openxlsx::read.xlsx(
  "Data/Input/DatabazePrO_2025.xlsx",
  sheet = 1
) %>%
  dplyr::rename(
    site_code = `kód`,
    site_name = `název`,
    site_type = `kategorie`,
    feature_type = `typ.předmětu.ochrany`,
    #sdf_code = `Kód.SDF`,
    feature_code = `kód.biotopu`,
    nazev_cz = `název.biotopu`,
    nazev_lat = `latinský.název`
  )

sites_habitats_mzchu <- sites_subjects_mzchu %>%
  dplyr::filter(feature_type == "ekosystém")

sites_habitats_mzchu_test <- sites_habitats_mzchu %>%
  dplyr::filter(site_code %in% c("5874", "681", "2213", "1183"))

#--------------------------------------------------#
### Seznam EVL SDO II ---- 
#--------------------------------------------------#
sdo_II_sites <- readr::read_csv2(
  "Data/Input/SDO_II_predmetolokality.csv", 
  locale = readr::locale(encoding = "Windows-1250")
) 

#--------------------------------------------------#
## Ciselniky - stanoviste ---- 
#--------------------------------------------------#
#------------------------------------------#
### Ciselnik kodu a nazvu typu prirodnich stanovist ---- 
#------------------------------------------#
cis_habitat <- 
  readr::read_csv2(
    "Data/Input/cis_habitat.csv", 
    locale = readr::locale(encoding = "Windows-1250")
  ) %>%
  dplyr::select(
    KOD_HABITAT, 
    NAZEV_HABITAT, 
    PRIORITA
  ) %>% 
  dplyr::mutate(
    KOD_HABITAT = dplyr::case_when(
      KOD_HABITAT == "91" ~ "91E0",
      KOD_HABITAT == 6210 & PRIORITA == "p" ~ "6210p",
      TRUE ~ KOD_HABITAT
    )
  ) %>%
  dplyr::select(
    KOD_HABITAT, 
    NAZEV_HABITAT
  )

#--------------------------------------------------#
### Vrstvy pouzivane jen u stanovist ----
#--------------------------------------------------#
mzchu  <- read_layer("Data/Input/MaloplZCHU.shp", getfeature_url_mzchu,  n2k = n2k_oop) %>%
  dplyr::rename(SITECODE = KOD)
biotop_zvld <- read_layer("Data/Input/BiotopZvld.shp", getfeature_url_biotopzvld)

#--------------------------------------------------#
### Spojení EVL a PO ----
#--------------------------------------------------#

n2k_union <- sf::st_join(evl, po)
#--------------------------------------------------#
### Ciselnik minimiarealu typu prirodnich stanovist ---- 
#--------------------------------------------------#
minimisize <- 
  readr::read_csv(
    "Data/Input/minimisize.csv", 
    locale = readr::locale(encoding = "Windows-1250")
  ) %>%
  dplyr::group_by(
    HABITAT
  ) %>%
  dplyr::reframe(
    MINIMISIZE = max(MINIMISIZE)/10000
  ) %>%
  dplyr::ungroup()

#--------------------------------------------------#
### Rozloha stanovišť v ČR v rámci AVMB2022 ----
#--------------------------------------------------#
habitat_areas_2022 <- 
  readr::read_csv(
    "Outputs/Data/stanoviste/celkova_rozloha/stanoviste_rozloha_cr_a1.csv", 
    locale = readr::locale(encoding = "Windows-1250")
  )

#--------------------------------------------------#
### Rozloha stanovišť v ČR v rámci VMB2----
#--------------------------------------------------#
habitat_areas_a1 <- 
  readr::read_csv(
    "Outputs/Data/stanoviste/celkova_rozloha/stanoviste_rozloha_cr_a1.csv", 
    locale = readr::locale(encoding = "Windows-1250")
  )

#--------------------------------------------------#
## Stažení hranice CR ---- 
#--------------------------------------------------#
czechia <- sf::st_read("Data/Input/HraniceCR.shp")
czechia_line <- sf::st_cast(czechia, "LINESTRING")

#--------------------------------------------------#
## Aktualizaceni okrsky mapovani biotopu ---- 
#--------------------------------------------------#
akt_okrsky <- sf::st_read("Data/Input/AktualizacniOkrsky.shp") %>%
  dplyr::rename(SITECODE = kod)

#------------------------------------------------------#
## RL druhy ----
# export obsahuje data o vyskytu citlivych druhu: 
# kompletni pouze pro overene uzivatele,
# bez vyskytu citlivych druhu na vyzadani na jonas.gaigr@aopk.gov.cz
#------------------------------------------------------#
# 1. Read the data with strict encoding and column specifications
redlist_species_raw <- read_csv(
  paste0(slozka_lokal, "export_redlist.csv"), 
  locale = locale(encoding = "UTF-8"), # Reverting to your original choice
  col_types = cols(
    .default = col_guess(),
    NEGATIVNI = col_character(), # Forces this to character so "ano"/"ne" works
    EVL = col_character()        # Ensures EVL is strictly text for substr()
  )
)

# 2. Apply your transformations
redlist_species <- redlist_species_raw %>%
  # filter(SKUPINA == "Cévnaté rostliny") %>%
  # filter(DRUH %in% invaz_list$TAXON) %>%
  mutate(
    DRUH = as.factor(DRUH),
    DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
    DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y'),
    YEAR = substring(DATE, 1, 4),
    # 1. Purge invalid bytes: This reads the string, and if it finds an invalid 
    # multibyte character anywhere, it silently drops it (sub = "")
    EVL_SAFE = iconv(as.character(EVL), from = "UTF-8", to = "UTF-8", sub = ""),
    
    # 2. Use stringr for extraction: It handles encodings much better than base R
    SITECODE = stringr::str_sub(EVL_SAFE, 1, 9)
  ) %>% 
  st_as_sf(coords = c("X", "Y"), crs = "+init=epsg:5514")

#------------------------------------------------------#
## Invazni nepuvodni druhy ----
# export obsahuje data o vyskytu citlivych druhu: 
# kompletni pouze pro overene uzivatele,
# bez vyskytu citlivych druhu na vyzadani na jonas.gaigr@aopk.gov.cz
#------------------------------------------------------#
# 1. Read the data with strict encoding and column specifications
invasive_species_raw <- read_csv(
  paste0(slozka_lokal, "export_invaze.csv"), 
  locale = locale(encoding = "UTF-8"), # Reverting to your original choice
  col_types = cols(
    .default = col_guess(),
    NEGATIVNI = col_character(), # Forces this to character so "ano"/"ne" works
    EVL = col_character()        # Ensures EVL is strictly text for substr()
  )
)

# 2. Apply your transformations
invasive_species <- invasive_species_raw %>%
  # filter(SKUPINA == "Cévnaté rostliny") %>%
  # filter(DRUH %in% invaz_list$TAXON) %>%
  mutate(
    DRUH = as.factor(DRUH),
    DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
    DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y'),
    YEAR = substring(DATE, 1, 4),
    # 1. Purge invalid bytes: This reads the string, and if it finds an invalid 
    # multibyte character anywhere, it silently drops it (sub = "")
    EVL_SAFE = iconv(as.character(EVL), from = "UTF-8", to = "UTF-8", sub = ""),
    
    # 2. Use stringr for extraction: It handles encodings much better than base R
    SITECODE = stringr::str_sub(EVL_SAFE, 1, 9)
  ) %>% 
  st_as_sf(coords = c("X", "Y"), crs = "+init=epsg:5514")

#------------------------------------------------------#
## Expanzivni druhy ----
# export obsahuje data o vyskytu citlivych druhu: 
# kompletni pouze pro overene uzivatele,
# bez vyskytu citlivych druhu na vyzadani na jonas.gaigr@aopk.gov.cz
#------------------------------------------------------#
# 1. Read the data with strict encoding and column specifications
expansive_species_raw <- read_csv(
  paste0(slozka_lokal, "export_expanze.csv"), 
  locale = locale(encoding = "UTF-8"), # Reverting to your original choice
  col_types = cols(
    .default = col_guess(),
    NEGATIVNI = col_character(), # Forces this to character so "ano"/"ne" works
    EVL = col_character()        # Ensures EVL is strictly text for substr()
  )
)

# 2. Apply your transformations
expansive_species <- expansive_species_raw %>%
  # filter(SKUPINA == "Cévnaté rostliny") %>%
  # filter(DRUH %in% invaz_list$TAXON) %>%
  mutate(
    DRUH = as.factor(DRUH),
    DATE = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
    DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y'),
    YEAR = substring(DATE, 1, 4),
    # 1. Purge invalid bytes: This reads the string, and if it finds an invalid 
    # multibyte character anywhere, it silently drops it (sub = "")
    EVL_SAFE = iconv(as.character(EVL), from = "UTF-8", to = "UTF-8", sub = ""),
    
    # 2. Use stringr for extraction: It handles encodings much better than base R
    SITECODE = stringr::str_sub(EVL_SAFE, 1, 9)
  ) %>% 
  st_as_sf(coords = c("X", "Y"), crs = "+init=epsg:5514")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

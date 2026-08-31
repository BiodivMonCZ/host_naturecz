#----------------------------------------------------------#
# OBECNY CONFIG - spolecny pro druhy i stanoviste -----
#----------------------------------------------------------#
# Nacita jen to, co potrebuji OBE vetve hodnoceni: knihovny, rok hodnoceni,
# limity, sdilene ciselniky, vrstvy EVL a PO a pomocne funkce.
#
# Data specificka pro jednu vetev jsou v samostatnych skriptech:
#   R/00_config/02_n2k_data_druhy.R      - zdrojova data a ciselniky druhu
#   R/00_config/03_n2k_data_stanoviste.R - zdrojova data a ciselniky stanovist
#
# Rozdeleni je vedeno podle SKUTECNEHO pouziti objektu ve skriptech
# R/02_druhy vs. R/01_stanoviste + R/03_Stanoviste_analyza.

#----------------------------------------------------------#
# Nacteni knihoven -----
#----------------------------------------------------------#
packages <- c(
  "tidyverse",
  "sf",
  "sp",
  "proj4",
  "openxlsx",
  "fuzzyjoin",
  "remotes",
  "ggplot2",
  "progress",
  "fs",
  # data.table se pouziva pouze pro agregaci STAV_IND v R/02_druhy/21_2_...R,
  # ktera je pri poctu skupin v radu statisicu v dplyr neunosne pomala
  # (pres 20 minut na jeden druh vs. ~2 s) - viz komentar u agg_stav_ind()
  "data.table"
)

# Standardni package
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# GitHub remotes
if (!require("rn2kcz", quietly = TRUE)) {
  remotes::install_github("jonasgaigr/rn2kcz", force = TRUE)
  library(rn2kcz)
}

#--------------------------------------------------#
# Rok hodnoceni ---- 
#--------------------------------------------------#
current_year <- as.numeric(format(Sys.Date(), "%Y")) - 1

#----------------------------------------------------------#
# Nacteni remote dat -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Limity hodnoceni stavu ---- 
#--------------------------------------------------#
#------------------------------------------#
### Limity - cévnaté rostliny ---- 
#------------------------------------------#
limity_cev <- readr::read_csv(
  "Data/Input/limity_cevky.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#------------------------------------------#
### Limity - ryby ---- 
#------------------------------------------#
limity_ryb <- readr::read_csv2(
  "Data/Input/limity_ryby.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#------------------------------------------#
### Limity - hlavní soubor ---- 
#------------------------------------------#
limity <- readr::read_csv(
  "Data/Input/limity_vse.csv", 
  locale = readr::locale(encoding = "Windows-1250")
) %>%
  dplyr::bind_rows(
    ., 
    limity_cev,
    limity_ryb
  ) %>%
  dplyr::group_by(
    DRUH, 
    ID_IND, 
    TYP_IND, 
    UROVEN
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    LIM_INDLIST = dplyr::case_when(
      TYP_IND == "max" ~ paste(
        "nejvýš", 
        LIM_IND, 
        JEDNOTKA
      ),
      TYP_IND == "min" ~ paste(
        "alespoň", 
        LIM_IND, 
        JEDNOTKA
      ),
      TYP_IND == "val" ~ paste(
        paste0(
          unique(LIM_IND), 
          collapse = ", "
        )
      ),
      TRUE ~ NA_character_)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(
    DRUH, 
    ID_IND
  ) %>%
  dplyr::mutate(
    LIM_INDLIST = toString(
      na.omit(
        unique(LIM_INDLIST)
      )
    )
  ) %>%
  dplyr::ungroup()

#--------------------------------------------------#
## Ciselniky - sdilene ---- 
#--------------------------------------------------#
#--------------------------------------------------#
### Seznam predmetu ochrany EVL ---- 
#--------------------------------------------------#
sites_subjects <- openxlsx::read.xlsx(
  "Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx",
  sheet = 1
) %>%
  dplyr::rename(
    site_code = `Kód.lokality`,
    site_name = `Název.lokality`,
    site_type = `Typ.lokality`,
    feature_type = `Typ.předmětu.ochrany`,
    sdf_code = `Kód.SDF`,
    feature_code = `Kód.ISOP`,
    nazev_cz = `Název.česky`,
    nazev_lat = `Název.latinsky.(druh)`
  )

#--------------------------------------------------#
### Ciselnik OOP ---- 
#--------------------------------------------------#
n2k_oop <- readr::read_csv2(
  "Data/Input/n2k_oop_25.csv", 
  locale = readr::locale(encoding = "Windows-1250")
) %>%
  mutate(oop = gsub(";", ",", oop)) %>%
  dplyr::rename(SITECODE = sitecode) %>%
  dplyr::select(SITECODE, oop)

#--------------------------------------------------#
### Ciselnik RP AOPK CR ---- 
#--------------------------------------------------#
rp_code <- readr::read_csv2(
  "Data/Input/n2k_rp_25.csv", 
  locale = readr::locale(encoding = "Windows-1250")
) %>%
  dplyr::rename(
    kod_chu = sitecode
  ) %>%
  dplyr::select(
    kod_chu, 
    pracoviste) %>%
  dplyr::mutate(
    pracoviste = gsub(",", 
                      "", 
                      pracoviste
    )
  )

#------------------------------------------#
### Ciselnik indikatoru hodnoceni stavu ---- 
#------------------------------------------#
indikatory_id <- readr::read_csv(
  "Data/Input/cis_indikatory_popis.csv",
  locale = readr::locale(encoding = "Windows-1250")
)

#--------------------------------------------------#
### Ciselnik periody hodnoceni stavu ---- 
#--------------------------------------------------#
cis_evd_perioda <- readr::read_csv(
  "Data/Input/cis_evd_perioda.csv", 
  locale = readr::locale(encoding = "Windows-1250")
) %>%
  dplyr::select(
    TAXON, 
    PERIODA
  )

#--------------------------------------------------#
### Ciselnik metodiky hodnoceni stavu ---- 
#--------------------------------------------------#
cis_metodika <- readr::read_csv(
  "Data/Input/cis_metodika.csv", 
  locale = readr::locale(encoding = "Windows-1250")
) %>%
  dplyr::select(
    druh, 
    metodika
  )

#--------------------------------------------------#
## Stazeni GIS vrstev AOPK CR ---- 
#--------------------------------------------------#

endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl <- "Opendata:Evropsky_vyznamne_lokality"
layer_name_po <- "Opendata:Ptaci_oblasti"
layer_name_mzchu <- "Opendata:Maloplosna_zvlaste_chranena_uzemi__MZCHU_"
layer_name_biotopzvld <- "Opendata:Biotop_zvlaste_chranenych_druhu_velkych_savcu"

getfeature_url_evl <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_evl
)
getfeature_url_po <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_po
)
getfeature_url_mzchu <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_mzchu
)
getfeature_url_biotopzvld <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_biotopzvld
)

#--------------------------------------------------#
### Vodstvo (CUZK) - ODSTRANENO 2026-08-28 ----
#--------------------------------------------------#
# Puvodne zde bylo bezpodminecne:
#
#   vodstvo <- sf::st_read("https://geoportal.cuzk.gov.cz/geoserver/hy-p/wfs?")
#
# Duvod odstraneni:
#   1. Endpoint uz WFS neposkytuje - na jakykoli dotaz (vcetne GetCapabilities)
#      vraci 302 na https://geoportal.cuzk.gov.cz/Dokumenty/Podminky.pdf
#      a nasledovani presmerovani skonci ve smycce. Volani tedy vzdy selze.
#   2. Objekt 'vodstvo' se nikde v repozitari nepouzival.
#   3. Volani nemelo lokalni fallback ani osetreni chyby, takze shazovalo
#      CELY config jeste pred nactenim dat z NDOP - kvuli tomu nesla spustit
#      kaskada hodnoceni (viz Metodiky/Obojzivelnici/harmonizace_registr.md).
#
# Az bude vodstvo potreba, doplnit stejnym vzorem jako ostatni vrstvy nize,
# tj. pres read_layer() s lokalnim souborem a WFS jen jako zalohou:
#
#   vodstvo <- read_layer("Data/Input/Vodstvo.shp", getfeature_url_vodstvo)
#
# Aktualni WFS adresu je nutne overit v katalogu sluzeb CUZK - stara adresa
# ani odhadovane varianty na services.cuzk.cz uz neodpovidaji.

#--------------------------------------------------#
### Funkce pro načtení vrstvy: nejprve lokálně, jinak z WFS ----
#--------------------------------------------------#

read_layer <- function(local_path, wfs_url, n2k = NULL) {
  if (file.exists(local_path)) {
    message("Reading local file: ", local_path)
    shp <- sf::st_read(local_path, options = "ENCODING=CP1250", quiet = TRUE)
  } else {
    message("Local file not found, downloading from WFS: ", wfs_url)
    shp <- sf::st_read(wfs_url, quiet = TRUE)
  }
  
  shp <- sf::st_transform(
    shp, 
    st_crs("+init=epsg:5514")
  )
  
  if (!is.null(n2k) & local_path != "Data/Input/MaloplZCHU.shp") {
    shp <- dplyr::left_join(shp, n2k, by = "SITECODE")
  }
  
  return(shp)
}

#--------------------------------------------------#
### Načtení vrstev ----
#--------------------------------------------------#

evl <- read_layer("Data/Input/EvVyzLok.shp", getfeature_url_evl, n2k = n2k_oop)
po  <- read_layer("Data/Input/PtaciObl.shp", getfeature_url_po,  n2k = n2k_oop)
#----------------------------------------------------------#
# Nacteni lokalnich dat -----
#----------------------------------------------------------#

#--------------------------------------------------#
## Cesta k lokalnim datum ---- 
#--------------------------------------------------#

slozka_lokal <- "C:/Users/jonas.gaigr/Documents/host_data/"

#----------------------------------------------------------#
# Vlastní funkce na úpravu dat ----
#----------------------------------------------------------#
#--------------------------------------------------#
## Zaokrouhlení na dve desetinna mista - vzdy dolu ----
#--------------------------------------------------#
safe_floor <- function(x, decimals = 2) {
  x_num <- round(as.numeric(x), 10)  # normalize precision
  factor <- 10^decimals
  floor(x_num * factor - 1e-9) / factor
}
#--------------------------------------------------#
## Prace s agregaci nalezu ----
#--------------------------------------------------#
safe_max <- function(x) {
  x <- x[!is.infinite(x)]
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}
safe_sum_num <- function(x) {
  # x muze obsahovat text; prevet robustně (carka -> tecka)
  num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
  sum(num, na.rm = TRUE)
}
to_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}
#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

#----------------------------------------------------------#
# Nacteni knihoven -----
#----------------------------------------------------------#
if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)}

if(!isTRUE(require(sf, quietly = TRUE))) {
  install.packages("sf", dependencies = TRUE); library(sf)
} else {
  require(sf)}

if(!isTRUE(require(sp, quietly = TRUE))) {
  install.packages("sp", dependencies = TRUE); library(sp)
} else {
  require(sp)}

if(!isTRUE(require(proj4, quietly = TRUE))) {
  install.packages("proj4", dependencies = TRUE); library(proj4)
} else {
  require(proj4)}

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

if(!isTRUE(require(fuzzyjoin, quietly = TRUE))) {
  install.packages("fuzzyjoin", dependencies = TRUE); library(fuzzyjoin)
} else {
  require(fuzzyjoin)}

if(!isTRUE(require(remotes, quietly = TRUE))) {
  install.packages("remotes", dependencies = TRUE); library(remotes)
} else {
  require(remotes)}

if(!isTRUE(require(rn2kcz, quietly = TRUE))) {
  remotes::install_github("jonasgaigr/rn2kcz", force = TRUE); library(rn2kcz)
} else {
  require(rn2kcz)}

#----------------------------------------------------------#
# Nacteni lokalnich dat -----
#----------------------------------------------------------#

#--------------------------------------------------#
## Cesta k lokalnim datum ---- 
#--------------------------------------------------#

slozka_lokal <- "C:/Users/jonas.gaigr/Documents/host_data/"

#------------------------------------------------------#
## Zdrojova data - export z NDOP ----
# export obsahuje data o vyskytu citlivych druhu: 
# kompletni pouze pro overene uzivatele,
# bez vyskytu citlivych druhu na vyzadani na jonas.gaigr@aopk.gov.cz
#------------------------------------------------------#
n2k_export <- readr::read_csv2(
  paste0(
    slozka_lokal,
    "evl_data_export_20250408.csv"
    ), 
  locale = readr::locale(encoding = "Windows-1250")
)

ncol_orig <- ncol(n2k_export)

#----------------------------------------------------------#
# Nacteni remote dat -----
#----------------------------------------------------------#

#--------------------------------------------------#
## Zdroj cileného monitoringu ---- 
#--------------------------------------------------#
CIS_CILMON <- readr::read_csv(
  "Data/Input/cil_mon_zdroj.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#--------------------------------------------------#
## Rok hodnoceni ---- 
#--------------------------------------------------#
current_year <- as.numeric(format(Sys.Date(), "%Y")) - 1

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
### Limity - hlavní soubor ---- 
#------------------------------------------#
limity <- readr::read_csv(
  "Data/Input/limity_vse.csv", 
  locale = readr::locale(encoding = "Windows-1250")
  ) %>%
  dplyr::bind_rows(
    ., 
    limity_cev
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
## Ciselnik poctu navazanych na relativni kategorii pocetnost ---- 
#--------------------------------------------------#
cis_pocet_kat <- readr::read_csv(
  "Data/Input/cis_pocet_kat.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#--------------------------------------------------#
## Ciselnik indikatoru hodnoceni stavu ---- 
#--------------------------------------------------#
indikatory_id <- readr::read_csv(
  "Data/Input/cis_indikatory_popis.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#--------------------------------------------------#
## Ciselnik periody hodnoceni stavu ---- 
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
## Ciselnik metodiky hodnoceni stavu ---- 
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
## Ciselnik kategorii delkovych struktur ryb a mihuli ---- 
#--------------------------------------------------#
cis_ryby_delky <- readr::read_csv(
  "Data/Input/cis_ryby_delky_strukt.csv", 
  locale = readr::locale(encoding = "Windows-1250")
)

#--------------------------------------------------#
## Ciselnik kodu a nazvu typu prirodnich stanovist ---- 
#--------------------------------------------------#
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
## Ciselnik minimiarealu typu prirodnich stanovist ---- 
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
## Seznam predmetu ochrany EVL ---- 
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
## Ciselnik OOP ---- 
#--------------------------------------------------#
n2k_oop <- readr::read_csv2(
  "Data/Input/n2k_oop_25.csv", 
  locale = readr::locale(encoding = "Windows-1250")
  ) %>%
  mutate(oop = gsub(";", ",", oop)) %>%
  dplyr::rename(SITECODE = sitecode) %>%
  dplyr::select(SITECODE, oop)

#--------------------------------------------------#
## Ciselnik RP AOPK CR ---- 
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

#--------------------------------------------------#
## Stazeni GIS vrstev AOPK CR ---- 
#--------------------------------------------------#
endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- base::paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl <- "Opendata:Evropsky_vyznamne_lokality"
layer_name_po <- "Opendata:Ptaci_oblasti"
layer_name_biotopzvld <- "Opendata:Biotop_zvlaste_chranenych_druhu_velkych_savcu"
getfeature_url_evl <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_evl
)
getfeature_url_po <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_po
)
getfeature_url_biotopzvld <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_biotopzvld
)

evl <- sf::st_read(getfeature_url_evl) %>%
  sf::st_transform(., st_crs("+init=epsg:5514")) %>%
  dplyr::left_join(., n2k_oop, by = "SITECODE") 
po <- sf::st_read(getfeature_url_po) %>%
  sf::st_transform(., st_crs("+init=epsg:5514")) %>%
  dplyr::left_join(., n2k_oop, by = "SITECODE") 
biotop_zvld <- sf::st_read(getfeature_url_biotopzvld) %>%
  sf::st_transform(., st_crs("+init=epsg:5514"))

n2k_union <- sf::st_join(
  evl, 
  po
  )

#--------------------------------------------------#
## Ciselnik biotopu EVD hmyzu ---- 
#--------------------------------------------------#
biotop_evd <- readr::read_csv(
  "Data/Input/biotopy_evd_hmyz.csv"
  )

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

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
## Ciselnik biotopu EVD hmyzu ---- 
#--------------------------------------------------#
biotop_evd <- readr::read_csv(
  "Data/Input/biotopy_evd_hmyz.csv"
)

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
## Stažení GIS vrstev AOPK ČR ---- 
#--------------------------------------------------#

endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl      <- "Opendata:Evropsky_vyznamne_lokality"
layer_name_po       <- "Opendata:Ptaci_oblasti"
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

#--------------------------------------------------#
## Funkce pro načtení vrstvy: nejprve lokálně, jinak z WFS ----
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
  
  if (!is.null(n2k)) {
    shp <- dplyr::left_join(shp, n2k, by = "SITECODE")
  }
  
  return(shp)
}

#--------------------------------------------------#
## Načtení vrstev ----
#--------------------------------------------------#

evl <- read_layer("Data/Input/EvVyzLok.shp", getfeature_url_evl, n2k = n2k_oop)
po  <- read_layer("Data/Input/PtaciObl.shp", getfeature_url_po,  n2k = n2k_oop)
biotop_zvld <- read_layer("Data/Input/BiotopZvld.shp", getfeature_url_biotopzvld)

#--------------------------------------------------#
## Spojení EVL a PO ----
#--------------------------------------------------#

n2k_union <- sf::st_join(evl, po)

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

n2k_load <- n2k_export %>%
  dplyr::rename(
    POLE = POLE_1_RAD
  ) %>% 
  dplyr::mutate(
    # Převedení DRUHu na kategorickou veličinu
    DRUH = as.factor(DRUH),
    # Převedení datumu do vhodného formátu
    DATUM = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    # Redukce data na den
    DEN = as.numeric(substring(DATUM_OD, 1, 2)),
    # Redukce data na měsíc
    MESIC = as.numeric(substring(DATUM_OD, 4, 5)),
    # Redukce data na rok
    ROK = as.numeric(
      substring(
        DATUM_OD, 
        7, 
        11
      )
    ),
    # Izolace kódu EVL
    kod_chu = substr(
      EVL, 
      1, 
      9
    ),
    # Izolace názvu lokality
    nazev_chu = substr(
      as.character(
        EVL
      ),
      12,
      nchar(
        as.character(
          EVL
        )
      )
    ),
    KOD_LOKRYB = readr::parse_character(
      stringr::str_extract(
        STRUKT_POZN,
        "(?<=<naz_tok>).*(?=</naz_tok>)"
      )
    ),
    KOD_LOKAL = dplyr::case_when(
      SKUPINA == "Letouni" ~ LOKALITA,
      SKUPINA == "Mechy" ~ LOKALITA,
      SKUPINA == "Motýli" ~ substring(
        KOD_LOKALITY, 
        1, 
        10
      ),
      SKUPINA == "Ryby a mihule" &
        is.na(KOD_LOKRYB) == FALSE 
      ~ KOD_LOKRYB,
      KOD_LOKALITY == "amp216" ~ "CZ0723412",
      KOD_LOKALITY == "amp222" ~ "CZ0724089_9",
      KOD_LOKALITY == "amp231" ~ "CZ0724089_19",
      KOD_LOKALITY == "amp185" ~ "CZ0623345",
      KOD_LOKALITY == "amp101" ~ "CZ0423006",
      KOD_LOKALITY == "amp71" ~ "CZ0323158",
      KOD_LOKALITY == "amp59" ~ "CZ0323144",
      KOD_LOKALITY == "amp15" ~ "CZ0213790",
      KOD_LOKALITY == "amp102" ~ "CZ0423215",
      KOD_LOKALITY == "amp254" ~ "CZ0813455",
      KOD_LOKALITY == "amp227" ~ "CZ0723410",
      KOD_LOKALITY == "amp336 (CZ_5)" ~ "CZ0714073_5",
      KOD_LOKALITY == "amp205 (CZ_3)" ~ "CZ0714073_3",
      KOD_LOKALITY == "amp337" ~ "CZ0713383",
      KOD_LOKALITY == "amp129" ~ "CZ0523011",
      KOD_LOKALITY == "amp334 (CZ_3)" ~ "CZ0523010_3",
      KOD_LOKALITY == "amp138 (CZ_2)" ~ "CZ0523010_2",
      KOD_LOKALITY == "amp335 (cz_1)" ~ "CZ0523010_1",
      KOD_LOKALITY == "amp102" ~ "CZ0423215",
      KOD_LOKALITY == "amp116" ~ "CZ0513249",
      KOD_LOKALITY == "amp101" ~ "CZ0423006",
      KOD_LOKALITY == "amp15" ~ "CZ0213790",
      KOD_LOKALITY == "amp30" ~ "CZ0213077",
      KOD_LOKALITY == "amp314 (cz_2)" ~ "CZ0213064_2",
      KOD_LOKALITY == "amp316 (cz_3)" ~ "CZ0213064_3",
      KOD_LOKALITY == "amp315 (cz_1)" ~ "CZ0213064_1",
      KOD_LOKALITY == "CZ0213008" ~ "CZ0213008_1",
      KOD_LOKALITY == "amp244" ~ "CZ0813457",
      KOD_LOKALITY == "amp207" ~ "CZ0713385",
      KOD_LOKALITY == "amp64" ~ "CZ0323143",
      KOD_LOKALITY == "amp24" ~ "CZ0213787",
      KOD_LOKALITY == "amp279" ~ "CZ0613335_03",
      KOD_LOKALITY == "amp110" ~ "CZ0513244",
      KOD_LOKALITY == "amp339" ~ "CZ0213066_2",
      KOD_LOKALITY == "amp340" ~ "CZ0213066_1",
      KOD_LOKALITY == "amp27" ~ "CZ0213058",
      KOD_LOKALITY == "amp226" ~ "CZ0724429_3",
      KOD_LOKALITY %in% c("amp211", "amp211" , "amp211", "amp107", "amp99",
                          "amp53", "amp252", "amp281", "amp280", "amp22",
                          "amp81", "amp25", "CZ0813450", "CZ0713397") ~ NA_character_,
      kod_chu == "CZ0623367" ~ "CZ0623367",
      is.na(KOD_LOKALITY) == TRUE ~ kod_chu,
      TRUE ~ NA_character_)
  ) %>%
  dplyr::mutate(
    KOD_LOKAL = dplyr::case_when(
      is.na(KOD_LOKAL) == TRUE ~ KOD_LOKALITY,
      TRUE ~ KOD_LOKAL
    )
  ) %>%
  dplyr::select(
    -KOD_LOKRYB
  )%>%
  dplyr::mutate(
    KOD_LOKAL = dplyr::case_when(
      is.na(KOD_LOKAL) == TRUE ~ LOKALITA,
      TRUE ~ KOD_LOKAL
    )
  ) %>%
  # identifikace dat cileneho monitoringu
  dplyr::mutate(
    CILMON = dplyr::case_when(
      ZDROJ %in% CIS_CILMON ~ 1,
      PROJEKT == "Monitoring druhů ČR" ~ 1,
      DRUH %in% c(
        "Carabus menetriesi pacholei", 
        "Bolbelasmus unicornis"
      ) 
      ~ 1,
      TRUE ~ 0)
  ) 

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#

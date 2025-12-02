# SET WD ----
setwd("~/N2K.CZ-main")
# LOAD PACKAGES ----
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

if(!isTRUE(require(matrixStats, quietly = TRUE))) {
  install.packages("matrixStats", dependencies = TRUE); library(matrixStats)
} else {
  require(matrixStats)}

if(!isTRUE(require(ggradar, quietly = TRUE))) {
  install.packages("ggradar", dependencies = TRUE); library(ggradar)
} else {
  require(ggradar)}

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

if(!isTRUE(require(lubridate, quietly = TRUE))) {
  install.packages("lubridate", dependencies = TRUE); library(lubridate)
} else {
  require(lubridate)}

if(!isTRUE(require(condformat, quietly = TRUE))) {
  install.packages("condformat", dependencies = TRUE); library(condformat)
} else {
  require(condformat)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

if(!isTRUE(require(raster, quietly = TRUE))) {
  install.packages("raster", dependencies = TRUE); library(raster)
} else {
  require(raster)}

if(!isTRUE(require(stars, quietly = TRUE))) {
  install.packages("stars", dependencies = TRUE); library(stars)
} else {
  require(stars)}

if(!isTRUE(require(ggsn, quietly = TRUE))) {
  install.packages("ggsn", dependencies = TRUE); library(ggsn)
} else {
  require(ggsn)}

if(!isTRUE(require(zoo, quietly = TRUE))) {
  install.packages("zoo", dependencies = TRUE); library(zoo)
} else {
  require(zoo)}

if(!isTRUE(require(rgdal, quietly = TRUE))) {
  install.packages("rgdal", dependencies = TRUE); library(rgdal)
} else {
  require(rgdal)}

if(!isTRUE(require(ggsn, quietly = TRUE))) {
  install.packages("ggsn", dependencies = TRUE); library(ggsn)
} else {
  require(ggsn)}

if(!isTRUE(require(grid, quietly = TRUE))) {
  install.packages("grid", dependencies = TRUE); library(grid)
} else {
  require(grid)}

if(!isTRUE(require(gridGraphics, quietly = TRUE))) {
  install.packages("gridGraphics", dependencies = TRUE); library(gridGraphics)
} else {
  require(gridGraphics)}

if(!isTRUE(require(DT, quietly = TRUE))) {
  install.packages("DT", dependencies = TRUE); library(DT)
} else {
  require(DT)}


# LOAD DATA ----
# VRSTVA EVL
evl <- st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp")
#evl <- sf::st_read("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/Evropsky_významné_lokality.geojson")
evl_sjtsk <- st_transform(evl, CRS("+init=epsg:5514"))
# HRANICE ČR
czechia <- st_read("HraniceCR.shp")
czechia_line <- st_cast(czechia, "LINESTRING")
# BIOGEOGRAFICKÉ ČLENĚNÍ ČR
#bioregs <- st_read("BiogeoRegions_CR.shp")
#bioregs <- st_transform(bioregs, CRS("+init=epsg:4326"))
# VMB - X a PB 2022
vmb_x_shp_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/X_Segment.shp")
vmb_x_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/X_biotop.dbf")
vmb_pb_shp_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/PB_Segment.shp")
vmb_pb_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/PB_BIOTOP.dbf")

vmb_x_22 <- vmb_x_shp_22 %>%
  dplyr::left_join(vmb_x_dbf_22, by = "SEGMENT_ID")

vmb_pb_22 <- vmb_pb_shp_22 %>%
  dplyr::left_join(vmb_pb_dbf_22, by = "SEGMENT_ID")

vmb_pb_x_22 <- dplyr::bind_rows(vmb_x_22, vmb_pb_22)

# VMB - MÁJOVÁ VRSTVA 2021
vmb_shp_sjtsk_21 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2021/20210610_Segment.shp")
vmb_hab_dbf_21 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2021/Biotop/HAB_BIOTOP.dbf")
vmb_shp_sjtsk_21 <- vmb_shp_sjtsk_21 %>%
  dplyr::left_join(vmb_hab_dbf_21, by = "SEGMENT_ID") %>%
  dplyr::mutate(FSB_EVAL = dplyr::case_when(STEJ_PR < 50 ~ "X",
                                            STEJ_PR >= 50 & STEJ_PR < 100 ~ "moz.",
                                            TRUE ~ FSB),
                HABITAT = dplyr::case_when(BIOTOP == "T3.3C" | 
                                      BIOTOP == "3.4A" |
                                      BIOTOP == "T3.4C" | 
                                      BIOTOP == "T3.5A" ~ "6210p",
                                    TRUE ~ HABITAT))
# VMB - ZÁKLADNÍ MAPOVÁNÍ
vmb_shp_sjtsk_orig <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/20060501_Segment.shp")
vmb_hab_dbf_orig <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/HAB20060501_BIOTOP.dbf")
vmb_shp_sjtsk_orig <- vmb_shp_sjtsk_orig %>%
  dplyr::left_join(vmb_hab_dbf_orig, by = "SEGMENT_ID") %>%
  dplyr::mutate(FSB_EVAL = dplyr::case_when(STEJ_PR < 50 ~ "X",
                                            STEJ_PR >= 50 & STEJ_PR < 100 ~ "moz.",
                                            TRUE ~ FSB),
                HABITAT = dplyr::case_when(BIOTOP == "T3.3C" |BIOTOP == "3.4A" |
                                      BIOTOP == "T3.4C" | 
                                      BIOTOP == "T3.5A" ~ "6210p",
                                    TRUE ~ HABITAT))

# SEZNAM EVL A PŘEDMĚTŮ OCHRANY
sites_subjects <- openxlsx::read.xlsx("https://webgis.nature.cz/publicdocs/opendata/natura2000/seznam_predmetolokalit_Natura2000_440_2021.xlsx")
sites_subjects <- sites_subjects %>%
  rename(Název.latinsky = "Název.latinsky.(druh)")
sites_habitats <- sites_subjects %>%
  filter(Typ.předmětu.ochrany == "stanoviště")
# ČÍSELNÍK HABITATŮ
habitats <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/habitats.csv", encoding = "UTF-8")
# SEZNAM MINIMIAREÁLŮ
minimisize <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/minimisize.csv", encoding = "UTF-8")
# rozloze stanoviště v ČR v rámci AVMB2022
habitat_areas_2022 <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/habitat_areas_2022.csv", encoding = "UTF-8")
# MAXIMÁLNÍ VZDÁLENOST MEZI 2 BODY PRO KAŽDOU EVL - LINESTRINGY BYLY PŘEVEDENY NA MULTIPOINT 
# PRO EVL S OBVODEM < 10 KM BYLY POUŽITY VŠECHNY BODY, PRO VĚTŠÍ EVL KAŽDÝ SEDMÝ
evl_lengths <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/evl_max_dist.csv", encoding = "UTF-8")
#evl_species <- read.csv("https://media.githubusercontent.com/media/jonasgaigr/N2K.CZ/main/cevnate_evl.csv", encoding = "UTF-8")
# REDLISTOVÉ, INVAZNÍ A EXPANZNÍ DRUHY
evl_species <- openxlsx::read.xlsx("S:/Vojík/export_nalezy_cev_rostliny_EVL_21062022.xlsx")
# STAŽENÍ PŘES GDRIVE "https://docs.google.com/spreadsheets/d/12n1eCbkw8ufsFoUnKvEMMg123bfMFOzI/edit?usp=sharing&ouid=102654749342263703541&rtpof=true&sd=true"
evl_expansive_species <- openxlsx::read.xlsx("S:/Vojík/export_nalezy_expanzivky_EVL_21062022.xlsx")

# EXPANZNÍ DRUHY
expansive_species <- evl_expansive_species %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  filter(DRUH != "Arrhenatherum elatius") %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

# DRUHY ČERVENÉHO SEZNAMU S ODFILTROVANÝMI C4 DRUHY
red_list_species <- evl_species %>%
  filter(is.na(Nepůvodní.druhy) == TRUE) %>%
  filter(is.na(Druhy.červeného.seznamu) == FALSE) %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

# INVAZNÍ DRUHY
invasive_species <- evl_species %>%
  filter(is.na(Nepůvodní.druhy) == FALSE) %>%
  filter(CXLOKAL_PRESNOST <= 50) %>%
  mutate(DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
         DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y')) %>%
  #filter(DATUM_DO >= "2013-01-01") %>%
  st_as_sf(., coords = c("CXLOKAL_X", "CXLOKAL_Y"), crs = "+init=epsg:5514")

# LIMITNÍ HODNOTY PARAMETRŮ HODNOCENÍ
hablimits <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/hablimits.csv", encoding = "UTF-8") %>%
  filter(REG != "alp" | is.na(REG) == TRUE)

# N2K.CZ FUNKCE ----
# FUNKCE USNADŇUJÍCÍ PRÁCI S PŘEDMĚTY OCHRANY
# SITECODE EVL PODLE PŘEDMĚTU OCHRANY
find_evl_SITECODE <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Název.česky == species) %>%
           dplyr::pull(Kód.lokality) %>%
           unique()
  )
}
# NÁZEV EVL PODLE PŘEDMĚTU OCHRANY
find_evl_NAZEV <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Název.česky == species) %>%
           dplyr::pull(Název.lokality) %>%
           unique()
  )
}
# POČET EVL PODLE PŘEDMĚTU OCHRANY
find_evl_NUMBER <- function(species) {
  return(nrow(subset(sites_subjects, sites_subjects$Název.česky == species)))
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Název.česky == species) %>%
           nrow()
  )
}
# SITECODE EVL PODLE NÁZVU
find_evl_NAME_TO_CODE <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Název.lokality == species) %>%
           dplyr::pull(Kód.lokality) %>%
           unique()
  )
}
# NÁZEV EVL PODLE KÓDU
find_evl_CODE_TO_NAME <- function(species) {
  return(sites_subjects %>%
           dplyr::filter(Typ.lokality == "EVL") %>%
           dplyr::filter(Kód.lokality == species) %>%
           dplyr::pull(Název.lokality) %>%
           unique()
  )
}
# NÁZVEV HABITATU PODLE KÓDU
find_habitat_NAME_CZ <-function(species) {
  return(habitats %>%
           dplyr::filter(HABITAT_CODE == species) %>%
           dplyr::pull(HABITAT_CZ) %>%
           unique()
  )
}
# KÓD HABITATU PODLE NÁZVU
find_habitat_CODE <-function(species) {
  return(habitats %>%
           dplyr::filter(HABITAT_CZ == species) %>%
           dplyr::pull(HABITAT_CODE) %>%
           unique()
  )
}
# MINIMIAREÁL HABITATU
find_habitat_MINIMISIZE <- function(species) {
  return(minimisize %>%
           dplyr::filter(HABITAT == species) %>%
           pull(MINIMISIZE) %>%
           unique()
  )
}
# ROZLOHA HABITATU 2022
find_habitat_AREA2022 <- function(species) {
  return(habitat_areas_2022 %>%
           dplyr::filter(HABITAT == species) %>%
           pull(TOTAL_AREA) %>%
           unique()
  )
}
# PRIORITA OCHRANY HABITATU
find_evl_PRIORITY <- function(species) {
  return(minimisize %>%
           dplyr::filter(HABITAT == species) %>%
           pull(PRIORITA) %>%
           unique()
  )
}
# LIMITNÍ HODNOTY PARAMETRŮ HODNOCENÍ HABITATU
find_habitat_LIMIT <- function(species) {
  return(hablimits %>%
           dplyr::filter(HABITAT_CODE == species)
  )
}
# SEZNAM PŘEDMĚTŮ OCHRANY EVL
find_evl_TARGETS <- function(species) {
  return(sites_subjects %>%
           filter(Typ.lokality == "EVL") %>%
           filter(Název.lokality == species) %>%
           pull(Název.česky)
  )
}

# VÝPOČET PASEK ----
paseky_evl <- function(hab_code, evl_site) {
  
  if(substr(hab_code, 1, 1) == 9) {
    vmb_target_sjtsk_update <- vmb_pb_x_22 %>%
      sf::st_intersection(filter(evl_sjtsk, SITECODE == evl_site)) %>%
      dplyr::mutate(AREA_real_update = units::drop_units(st_area(geometry))) %>%
      dplyr::mutate(PLO_BIO_M2_EVL_update = STEJ_PR/100*AREA_real_update) %>%
      dplyr::rename(FSB_update = FSB,
                    BIOTOP_update = BIOTOP,
                    STEJ_PR_update = STEJ_PR,
                    ROK_AKT_update = ROK_AKT.y)
    
    vmb_target_sjtsk_orig <- vmb_shp_sjtsk_orig %>%
      sf::st_intersection(filter(evl_sjtsk, SITECODE == evl_site)) %>%
      dplyr::filter(HABITAT == hab_code) %>%
      dplyr::mutate(AREA_real_orig = units::drop_units(st_area(geometry))) %>%
      dplyr::mutate(PLO_BIO_M2_EVL_orig = STEJ_PR/100*AREA_real_orig) %>%
      dplyr::rename(FSB_orig = FSB,
                    BIOTOP_orig = BIOTOP,
                    STEJ_PR_orig = STEJ_PR)
    
    vmb_target_sjtsk_intersection <- vmb_target_sjtsk_update %>%
      sf::st_intersection(., vmb_target_sjtsk_orig) %>%
      dplyr::mutate(PASEKA = dplyr::case_when(BIOTOP_update %in% c("LP", "X10") ~ 1,
                                              BIOTOP_update %in% c("X11", "X12A", "X12B") &
                                                ROK_AKT_update %in% c(2007:2012) ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::mutate(AREA_real_intersection = units::drop_units(st_area(geometry))) %>%
      dplyr::mutate(PLO_BIO_M2_EVL_intersection = AREA_real_intersection*STEJ_PR_orig/100*STEJ_PR_update/100) %>%
      dplyr::mutate(HOLINA = dplyr::case_when(PASEKA == 1 & PLO_BIO_M2_EVL_intersection > 10000 ~ 1,
                                              TRUE ~ 0))
    
    rozloha_paseky <- vmb_target_sjtsk_intersection %>%
      dplyr::filter(PASEKA == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL_intersection) %>%
      sum()/10000
    
    rozloha_holiny <- vmb_target_sjtsk_intersection %>%
      dplyr::filter(HOLINA == 1) %>%
      dplyr::pull(PLO_BIO_M2_EVL_intersection) %>%
      sum()/10000
    
    pocet_segmentu <- vmb_target_sjtsk_intersection %>%
      dplyr::filter(PASEKA == 1) %>%
      dplyr::pull(SEGMENT_ID) %>%
      unique() %>%
      length()
    
    result <- tibble(SITECODE = evl_site,
                     HABITAT_CODE = hab_code,
                     ROZLOHA_PASEKY = rozloha_paseky,
                     ROZLOHA_HOLINY = rozloha_holiny,
                     POCET_SEGMENTU_PASEKY = pocet_segmentu)
    
  } else {
    result <- tibble(SITECODE = evl_site,
                     HABITAT_CODE = hab_code,
                     ROZLOHA_PASEKY = NA,
                     ROZLOHA_HOLINY = NA,
                     POCET_SEGMENTU_PASEKY = NA)
  }
  
  result
  
}

# RESULTS ----
hu_paseky <- paseky_evl(sites_habitats[1,5], sites_habitats[1,1])
paseky_results <- matrix(NA, 1, ncol(hu_paseky)) %>% dplyr::as_tibble()
colnames(paseky_results) <- colnames(hu_paseky)
for(i in 1:nrow(sites_habitats)) {
  paseky_results <- dplyr::bind_rows(paseky_results,
                                     as.data.frame(paseky_evl(sites_habitats[i,5], sites_habitats[i,1])))
}
write.csv2(paseky_results, 
           "S:/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv", 
           row.names = FALSE)


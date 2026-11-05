#setwd("C:/Users/Viola Work/Desktop/AOPKpráce/Data/ARROW")
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

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

library(lifecycle)

install.packages("here")
library(here)

# LOAD DATA EVL ----

rn2kcz::load_n2k_sites() 

# LOAD DATA CHEMISTRY ARROW ----

#data1 <- read.csv("Fyz_chem_PMO_csv_2.csv")
data1 <- read.csv(here("data", "Fyz_chem_PMO_csv_2.csv"))
data1$povodi = "PMO"
#data2 <- read.csv("Fyz_chem_PLA_csv_2.csv")
data2 <- read.csv(here("data", "Fyz_chem_PLA_csv_2.csv"))
data2$povodi = "PLA"
data3 <- read.csv(here("data", "Fyz_chem_PVL_csv_2.csv"))
#data3<- read.csv("Fyz_chem_PVL_csv_2.csv")
data3$povodi = "PVL"

data = rbind(data1,data2,data3)

# LOAD DATA PROFILES ARROW----

#profily_shp <- sf::st_read("C:/Users/Viola Work/Desktop/AOPKpráce/Data/ARROW/Arrow_profily/Arrow_profily.shp")
profily_shp <- st_read(here("Data", "Arrow_profily", "Arrow_profily.shp"))
# LOAD DATA EVL SHAPEFILES ----
#evl_shp <- sf::st_read("C:/Users/Viola Work/Desktop/AOPKpráce/Data/ARROW/evl/evl.shp")
evl_shp <- st_read(here("Data", "evl", "evl.shp"))

allfish <- evl_shp %>%
  filter(SITECODE %in% (sites_subjects %>%
                          filter(nazev_lat %in%  c ("Cottus gobio", "Eudontomyzon mariane", "Gymnocephalus baloni","Zingel zingel
","Romanogobio albipinatus","Zingel streber","Salmo salar","Lampetra planeri","Romanogobio banaticus
","Pelecus cultratus","Rhodeus amarus","Misgurnus fossilis","Sabanejewia balcanica","Cobitis elongatoides
","Leuciscus aspius")) %>%
                          pull(site_code)))

#intersection for Arrow buffer

evl_profily_buff <- sf::st_intersection(st_buffer(allfish, 500), profily_shp)
#intersection for Arrow buffer
evl_profily <- sf::st_intersection(filter(evl_shp, SITECODE %in% allfish$SITECODE),profily_shp)

# CISLO == NAMERENA IMPORTOVANA HODNOTA CHEMIE VODY Z ARROW!

data$CISLO=as.numeric(data$CISLO)

# UROVNAT FORMAT ROK 

data$rok=substr(data$DATUM, nchar(data$DATUM) - 3, nchar(data$DATUM))

# Minimalni pocet mereni chemie za jeden rok v ARROW pro vypocet medianu

min_pocet <- 10

# Vypocet vysledky chemie v EVL:


result <- data %>%
  filter(ARROW_ID %in% evl_profily_buff$ARROW_ID) %>%
  group_by(ARROW_ID, rok, UKAZATEL_NM) %>%
  filter(sum(!is.na(CISLO)) > 0) %>%  # <-- exclude groups with all NA
  summarise(
    minDATA = min(CISLO, na.rm = TRUE),
    maxDATA = max(CISLO, na.rm = TRUE),
    medDATA = median(CISLO, na.rm = TRUE),
    sdDATA = sd(CISLO, na.rm = TRUE),
    sd_procento = sdDATA / medDATA * 100,
    pocet_mereni = sum(!is.na(CISLO)),
    valid = dplyr::case_when(
      pocet_mereni < min_pocet ~ 0,
      pocet_mereni >= min_pocet ~ 1,
      TRUE ~ 0
    ),
    .groups = "drop"
  ) %>%
  dplyr::filter(valid == 1)

# join arrow sample sites with evl by sitecode
vysledky_evl <- evl_profily %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(., result, by = join_by(ARROW_ID)) %>%
  dplyr::left_join(., sites_subjects, by = join_by("SITECODE" == "site_code"))%>%
  filter(is.na(UKAZATEL_NM) == FALSE)


missing_cislo_df <- data %>%
  mutate(row_number = row_number()) %>%
  filter(is.na(CISLO))

# View the result
print(missing_cislo_df)

# Arrow joined with evl subsapmple for fish only
vysledky_evl_allfish <- vysledky_evl %>%
  dplyr::filter(nazev_lat  %in%  c ("Cottus gobio", "Eudontomyzon mariane", "Gymnocephalus baloni","Zingel zingel
","Romanogobio albipinatus","Zingel streber","Salmo salar","Lampetra planeri","Romanogobio banaticus
","Pelecus cultratus","Rhodeus amarus","Misgurnus fossilis","Sabanejewia balcanica","Cobitis elongatoides
","Leuciscus aspius"))


vysledky_evl_allfish_sum <- vysledky_evl_allfish %>%
  dplyr::filter(nazev_lat %in% c(
    "Cottus gobio",
    "Eudontomyzon mariane",
    "Gymnocephalus baloni",
    "Zingel zingel",
    "Romanogobio albipinatus",
    "Zingel streber",
    "Salmo salar",
    "Lampetra planeri",
    "Romanogobio banaticus",
    "Pelecus cultratus",
    "Rhodeus amarus",
    "Misgurnus fossilis",
    "Sabanejewia balcanica",
    "Cobitis elongatoides",
    "Leuciscus aspius"
  ))

#limity_ryby = read.csv("Limity_ryby_Reshaped.csv", fileEncoding = "Windows-1250")

limity_ryby <- read.csv(here("Data", "Limity_ryby_Reshaped.csv"), fileEncoding = "Windows-1250")

metody_df <- data.frame(
  UKAZATEL_ID = c("CC0020", "CC0025", "CC0030", "CC0055", "CC0065", 
                  "CD0000", "CD0005", "BA0005", "BA0015", 
                  "BA0035",  "BA0055", "CA0000", 
                  "CA0020"),
  METODA = c("medi", "medi", "medi", "medi", "medi", 
             "medi", "medi", "range",  "range", 
             "medi", "medi", "mini", 
             "medi"),
  stringsAsFactors = FALSE
)
# perform a left join to add the METODA column

ukazatel_mapping <- data.frame(
  UKAZATEL_ID = c("CC0020", "CC0025", "CC0030", "CC0055", "CC0065", 
                  "CD0000", "CD0005", "BA0005", 
                  "BA0015", "BA0035", "BA0055", "CA0000", "CA0020"),
  UKAZATEL_NM = c("dusík amoniakální", "dusík dusitanový", "dusík dusičnanový",
                  "fosfor celkový", "fosfor fosforečnanový", "chloridy", "sírany",
                  "pH vody v laboratoři (25°C)", "konduktivita v laboratoři",
                  "teplota vody v terénu", "nerozpuštěné látky při 105 °C",
                  "kyslík rozpuštěný v terénu", "biochemická spotřeba kyslíku BSK-5"),
  stringsAsFactors = FALSE
)

ukazatel_mapping_unique <- ukazatel_mapping %>%
  distinct(UKAZATEL_NM, .keep_all = TRUE)

vysledky_evl_allfish_sum <- vysledky_evl_allfish_sum %>%
  left_join(ukazatel_mapping_unique, by = "UKAZATEL_NM")


vysledky_evl_allfish_sum <- vysledky_evl_allfish_sum %>%
  left_join(metody_df, by = "UKAZATEL_ID")

hodnocenichemie <- vysledky_evl_allfish_sum %>%
  left_join(limity_ryby, by = c("UKAZATEL_NM" = "UKAZATEL_NM", "nazev_lat" = "DRUH"))

hodnocenichemie <- hodnocenichemie %>%
  distinct(SITECODE, nazev_lat, rok, UKAZATEL_NM, .keep_all = TRUE)
#evaluate each site compared to limits

hodnocenichemie <- hodnocenichemie  %>% 
  mutate(
    STAV = case_when(
      METODA == "medi" & medDATA <= med ~ 0,
      METODA == "range" & minDATA >= min & maxDATA <= max ~ 0,
      METODA == "mini" & minDATA >= minDATA ~ 0,
      TRUE ~ 1
    )
  )

# sum  malus points 
hodnocenichemievysledek <- hodnocenichemie %>% 
  group_by(SITECODE, nazev_lat, rok) %>% 
  summarise(suma_nepriznivych = sum(STAV, na.rm = TRUE),
            pocet_par = length(unique(UKAZATEL_NM)),
            ukazatele = toString(unique(UKAZATEL_NM)))
#final evaluation ... criterion  SET TO 0.5
hodnocenichemievysledek <- hodnocenichemievysledek  %>%
  mutate(stav_EVL = ifelse ((suma_nepriznivych/ pocet_par) > 0.6 , "nepriznivy", "priznivy"))

write.csv(hodnocenichemievysledek,"Hodnocenichemie.csv", row.names = FALSE,fileEncoding ='Windows-1250' )

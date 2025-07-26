# LOAD DATA ----
results <- 
  readr::read_csv2(
  "Outputs/Data/results_habitats_A1_20250222.csv",
  locale = readr::locale(encoding = "Windows-1250")
  ) %>%
  dplyr::mutate(
    HABITAT_CODE = dplyr::case_when(
      HABITAT_CODE == "91" ~ "91E0",
      TRUE ~ HABITAT_CODE
      )
    ) %>%
  dplyr::mutate(
    ROZLOHA = replace_na(ROZLOHA, 0),
    #KVALITA = replace_na(KVALITA, 0),
    #KVALITA = round(KVALITA, 2),
    NAZEV = str_replace_all(
      NAZEV, 
      "–|—",
      "-"
      )
    ) %>%
  dplyr::distinct()

results_x <- 
  readr::read_csv2(
    "Outputs/Data/results_habitats_24_20250331.csv",
    locale = readr::locale(encoding = "Windows-1250")
    ) %>% 
  dplyr::mutate(
    HABITAT_CODE = dplyr::case_when(
      HABITAT_CODE == "91" ~ "91E0",
      TRUE ~ HABITAT_CODE
      )
    ) %>%
  dplyr::mutate(
    ROZLOHA = replace_na(ROZLOHA, 0),
    #KVALITA = replace_na(KVALITA, 0),
    #KVALITA = round(KVALITA, 2),
    NAZEV = str_replace_all(NAZEV, "–|—", "-")
    ) %>%
  dplyr::distinct()


limity_stan <- 
  readr::read_csv(
    "Data/Input/limity_stanoviste.csv",
    locale = readr::locale(encoding = "Windows-1250")
    ) %>% 
  dplyr::mutate(
    rowname = as.numeric(
      rownames(.)
      ),
    HABITAT_CODE = dplyr::case_when(
      HABITAT_CODE == "91" ~ "91E0",
      HABITAT_CODE == "9.10E+01" ~ "91E0",
      HABITAT_CODE == "9,10E+01" ~ "91E0",
      TRUE ~ HABITAT_CODE
      )
    ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    LIM_IND = dplyr::case_when(
      ID_IND == "ROZLOHA" ~ floor(LIM_IND * 100) / 100,
      ID_IND == "KVALITA" ~ ceiling(LIM_IND * 10) / 10
      ),
    ZDROJ = dplyr::case_when(
      ZDROJ == "AVMB2" ~ "VMB3",
      ZDROJ == "AVMB1" ~ "VMB2",
      ZDROJ == "AVMB" ~ "VMB2",
      grepl("SDF", ZDROJ) ~ "SDF",
      TRUE ~ ZDROJ
      )
    ) %>%
  dplyr::distinct()

kval <- 
  limity_stan %>%
  dplyr::filter(
    ID_IND == "KVALITA"
    ) %>%
  dplyr::group_by(
    SITECODE, 
    HABITAT_CODE
    ) %>%
  dplyr::arrange(
    rowname, 
    LIM_IND
    ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() 

rozl <- 
  limity_stan %>%
  dplyr::filter(
    ID_IND == "ROZLOHA"
    ) %>%
  dplyr::group_by(
    SITECODE, 
    HABITAT_CODE
    ) %>%
  dplyr::arrange(
    rowname,
    LIM_IND
    ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() 

#lim_new <- 
#  dplyr::bind_rows(
#    rozl, 
#    kval
#    )

#write.csv(
#  lim_new,
#  "limity_stanoviste_20250524.csv",
#  row.names = FALSE,
#  fileEncoding = "Windows-1250"
#  )

sdo_II_sites <- 
  readr::read_csv2(
  "Data/Input/SDO_II_predmetolokality.csv",
  locale = readr::locale(encoding = "Windows-1250")
  )

evl_sdo <- evl %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(sites_subjects, by = c("SITECODE" = "site_code")) %>%
  dplyr::left_join(rp_code, by = c("SITECODE" = "kod_chu")) %>%
  dplyr::mutate(SDO2 = dplyr::case_when(SITECODE %in% sdo_II_sites$sitecode ~ 1,
                                        TRUE ~ 0),
                KRAJE2024 = dplyr::case_when(grepl("Karlo", oop) == TRUE ~ 1,
                                             grepl("Libereckého", oop) == TRUE ~ 1,
                                             grepl("Plz", oop) == TRUE ~ 1,
                                             grepl("Král", oop) == TRUE ~ 1,
                                             grepl("Pardu", oop) == TRUE ~ 1,
                                             TRUE ~ 0)) %>%
  rowwise() %>%
  dplyr::mutate(rev = sum(SDO2, KRAJE2024))
#write.csv(evl_sdo,
#          "evl_sdo.csv",
#          row.names = FALSE,
#fileEncoding = "Windows-1250")


# TRANSFORM TO CHARACTER ---- 
results <- results %>%
  dplyr::mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
      )
    )

for(i in 4:ncol(results)) {
  results[,i] <- as.character(unlist(results[,i]))
}

results_x <- results_x %>%
  dplyr::mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )
for(i in 4:ncol(results_x)) {
  results_x[,i] <- as.character(unlist(results_x[,i]))
}


# TRANSPOSE TO LONG FORMAT ----
results_long_x <- results_x %>%
  dplyr::mutate(CELKOVE_HODNOCENI = NA) %>%
  dplyr::mutate(datum_hodnoceni_od = DATE_MIN,
                datum_hodnoceni_do = DATE_MAX) %>%
  #dplyr::rename(parametr_nazev = PAR_NAZEV,
  #              parametr_hodnota = PAR_HODNOTA) %>%
  dplyr::select(-c(DATE_MIN, DATE_MAX, DATE_MEAN, DATE_MEDIAN)) %>%
  tidyr::pivot_longer(cols = c(4:(ncol(.)-2)),
                      names_to = "parametr_nazev",
                      values_to = "parametr_hodnota") 

results_long <- results %>%
  dplyr::mutate(CELKOVE_HODNOCENI = NA) %>%
  dplyr::mutate(datum_hodnoceni_od = DATE_MIN,
                datum_hodnoceni_do = DATE_MAX) %>%
  #dplyr::rename(parametr_nazev = PAR_NAZEV,
  #              parametr_hodnota = PAR_HODNOTA) %>%
  dplyr::select(-c(DATE_MIN, DATE_MAX, DATE_MEAN, DATE_MEDIAN)) %>%
  tidyr::pivot_longer(cols = c(4:(ncol(.)-2)),
                      names_to = "parametr_nazev",
                      values_to = "parametr_hodnota") %>%
  dplyr::left_join(., cis_habitat, by = c("HABITAT_CODE" = "KOD_HABITAT")) %>%
  dplyr::left_join(., limity_stan, by = c("SITECODE" = "SITECODE", "HABITAT_CODE" = "HABITAT_CODE", "parametr_nazev" = "ID_IND" )) %>%
  #dplyr::left_join(., results_1vmb %>% dplyr::select(HABITAT_CODE, SITECODE, DATE_MIN_A1, DATE_MAX_A1)) %>%
  dplyr::left_join(., rp_code, by = c("SITECODE" = "kod_chu")) %>%
  dplyr::left_join(., indikatory_id, by = c("parametr_nazev" = "ind_r")) %>%
  dplyr::left_join(., n2k_oop, by = c("SITECODE" = "SITECODE")) %>%
  dplyr::left_join(., minimisize, by = c("HABITAT_CODE" = "HABITAT")) %>%
  #dplyr::group_by(HABITAT_CODE, SITECODE, parametr_nazev) %>%
  rowwise() %>%
  dplyr::mutate(ZDROJ = dplyr::case_when(HABITAT_CODE %in% c("91T0", "3140", "8310") ~ NA,
                                         ZDROJ == "EXPERT" ~ ZDROJ,
                                         ZDROJ == "VMB3" ~ ZDROJ,
                                         SITECODE %in% sdo_II_sites$sitecode |
                                           (grepl("Karlo", oop) == TRUE &
                                              grepl("Libereckého", oop) == TRUE &
                                              grepl("Plz", oop) == TRUE &
                                              grepl("Král", oop) == TRUE &
                                              grepl("Pardu", oop) == TRUE &
                                              grepl("Správa NP", oop) == FALSE &
                                              grepl("Správa KRNAP", oop) == FALSE) ~ ZDROJ,
                                         parametr_nazev == "KVALITA" &
                                           ZDROJ %in% c("SDF", "VMB2", "VMB3") &
                                           LIM_IND <= 2 ~ ZDROJ,
                                         parametr_nazev == "KVALITA" &
                                           (is.na(LIM_IND) == TRUE | LIM_IND == "NA") &
                                           parametr_hodnota %>% as.numeric() <= 2 ~ "VMB2",
                                         parametr_nazev == "KVALITA" &
                                           (is.na(LIM_IND) == TRUE | LIM_IND == "NA") &
                                           parametr_hodnota %>% as.numeric() > 2 ~ "MINIMI",
                                         parametr_nazev == "KVALITA" &
                                           parametr_hodnota %>% as.numeric() > 2 ~ "MINIMI",
                                         parametr_nazev == "ROZLOHA" &
                                           ZDROJ == "SDF" &
                                           LIM_IND >= MINIMISIZE ~ ZDROJ,
                                         parametr_nazev == "ROZLOHA" &
                                           ZDROJ == "SDF"  &
                                           LIM_IND < MINIMISIZE ~ "MINIMI",
                                         parametr_nazev == "ROZLOHA" &
                                           ZDROJ == "VMB2" &
                                           as.numeric(parametr_hodnota) >= MINIMISIZE ~ ZDROJ,
                                         parametr_nazev == "ROZLOHA" &
                                           ZDROJ == "VMB2" &
                                           as.numeric(parametr_hodnota) < MINIMISIZE ~ "MINIMI",
                                         parametr_nazev == "ROZLOHA" &
                                           as.numeric(parametr_hodnota) < MINIMISIZE ~ "MINIMI",
                                         (is.na(ZDROJ)  == TRUE | ZDROJ == "NA") &
                                           as.numeric(parametr_hodnota) == 0 ~ "MINIMI",
                                         (is.na(ZDROJ) == TRUE | ZDROJ == "NA") &
                                           as.numeric(parametr_hodnota) == NA ~ "MINIMI",
                                         (is.na(ZDROJ) == TRUE | ZDROJ == "NA") &
                                           parametr_hodnota == "NA" ~ "MINIMI",
                                         parametr_nazev == "KVALITA" &
                                           (is.na(LIM_IND) == TRUE | LIM_IND == "NA") ~ "MINIMI",
                                           TRUE ~ ZDROJ),
                LIM_IND = dplyr::case_when(HABITAT_CODE %in% c("91T0", "3140", "8310") ~ NA,
                                           parametr_nazev == "ROZLOHA" &
                                             SITECODE == "CZ0514672" ~ floor(as.numeric(LIM_IND) * 100) / 100,
                                           ZDROJ == "EXPERT" ~ LIM_IND,
                                           parametr_nazev == "ROZLOHA" &
                                             ZDROJ == "SDF" ~ floor(as.numeric(LIM_IND) * 100) / 100,
                                           parametr_nazev == "ROZLOHA" &
                                             ZDROJ == "VMB3" ~ LIM_IND,
                                           parametr_nazev == "ROZLOHA" &
                                             SITECODE %in% sdo_II_sites$sitecode |
                                             (grepl("Karlo", oop) == TRUE &
                                                grepl("Libereckého", oop) == TRUE &
                                                grepl("Plz", oop) == TRUE &
                                                grepl("Král", oop) == TRUE &
                                                grepl("Pardu", oop) == TRUE &
                                                grepl("Správa NP", oop) == FALSE &
                                                grepl("Správa KRNAP", oop) == FALSE) ~ LIM_IND,
                                           parametr_nazev == "ROZLOHA" &
                                             ZDROJ == "MINIMI" ~ MINIMISIZE,
                                           parametr_nazev == "ROZLOHA" &
                                             ZDROJ == "VMB2" ~ floor(as.numeric(parametr_hodnota) * 100) / 100,
                                           parametr_nazev == "KVALITA" &
                                             is.na(LIM_IND) == TRUE &
                                             parametr_hodnota <= 2 ~ ceiling(as.numeric(parametr_hodnota) * 10) / 10,
                                           parametr_nazev == "KVALITA" &
                                             ZDROJ == "MINIMI" ~ 2,
                                           parametr_nazev == "KVALITA" &
                                             (is.na(LIM_IND) == TRUE | LIM_IND == "NA") ~ 2,
                                           TRUE ~ LIM_IND)) %>%
  dplyr::mutate(stav = dplyr::case_when(is.na(LIM_IND) == TRUE ~ "nehodnocen",
                                        parametr_nazev == "KVALITA" & is.na(parametr_hodnota) ~ "špatný",
                                        parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota) < LIM_IND ~ "špatný",
                                        parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota) >= LIM_IND ~ "dobrý",
                                        parametr_nazev == "KVALITA" & as.numeric(parametr_hodnota) > LIM_IND ~ "špatný",
                                        parametr_nazev == "KVALITA" & as.numeric(parametr_hodnota) <= LIM_IND ~ "dobrý"),
                stav_toler = dplyr::case_when(is.na(LIM_IND) == TRUE ~ "nehodnocen",
                                              parametr_nazev == "KVALITA" & is.na(parametr_hodnota) ~ "špatný",
                                              parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota) == 0 ~ "špatný",
                                              parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota)*1.05 < LIM_IND ~ "špatný",
                                              parametr_nazev == "ROZLOHA" & as.numeric(parametr_hodnota)*1.05 >= LIM_IND ~ "dobrý",
                                              parametr_nazev == "KVALITA" & as.numeric(parametr_hodnota)*0.95 > LIM_IND ~ "špatný",
                                              parametr_nazev == "KVALITA" & as.numeric(parametr_hodnota)*0.95 <= LIM_IND ~ "dobrý"),
                parametr_jednotka = dplyr::case_when(parametr_nazev == "ROZLOHA" ~ "hektary", 
                                                     parametr_nazev == "KVALITA" ~ "kvalita", 
                                                     parametr_nazev == "TYPICKE_DRUHY" ~ "typické druhy",
                                                     parametr_nazev == "MINIMIAREAL" ~ "% rozlohy v celistvém uspořádání splňující hodnotu minimiareálu",
                                                     parametr_nazev == "MINIMIAREAL_JADRA" ~ "počet lokalit splňujících hodnotu minimiareálu",
                                                     parametr_nazev == "MINIMIAREAL_HODNOTA" ~ "hodnota minimiareálu",
                                                     parametr_nazev == "MOZAIKA_VNEJSI" ~ "% hranice s nepřírodními biotopy",
                                                     parametr_nazev == "MOZAIKA_VNITRNI" ~ "zastoupení nepřírodních biotopů v segmentech stanoviště",
                                                     parametr_nazev == "MOZAIKA_FIN" ~ "mozaikovitost",
                                                     parametr_nazev == "RED_LIST" ~ "druhy červeného seznamu",
                                                     parametr_nazev == "INVASIVE" ~ "% rozlohy zasažené invazními druhy",
                                                     parametr_nazev == "EXPANSIVE" ~ "% rozlohy zasažené expanzivními druhy",
                                                     parametr_nazev == "MRTVE_DREVO" ~ "mrtvé dřevo",
                                                     parametr_nazev == "KALAMITA_POLOM" ~ "kalamita/polom",
                                                     parametr_nazev == "RED_LIST_SPECIES" ~ "seznam druhů červeného seznamu",
                                                     parametr_nazev == "INVASIVE_LIST" ~ "seznam invazních druhů",
                                                     parametr_nazev == "EXPANSIVE_LIST" ~ "seznam expanzivních druhů")) %>%
  dplyr::group_by(SITECODE, HABITAT_CODE) %>%
  dplyr::mutate(stav_count = dplyr::case_when(stav == "dobrý" ~ 1),
                stav_count_toler = dplyr::case_when(stav_toler == "dobrý" ~ 1),
                glob_stav = case_when(HABITAT_CODE %in% c("91T0", "3140", "8310") ~ "nehodnocen",
                                      sum(stav_count, na.rm = TRUE) == 0 ~ "špatný",
                                      sum(stav_count, na.rm = TRUE) == 1 ~ "zhoršený",
                                      sum(stav_count, na.rm = TRUE) == 2 ~ "dobrý"),
                glob_stav_toler = case_when(HABITAT_CODE %in% c("91T0", "3140", "8310") ~ "nehodnocen",
                                            sum(stav_count_toler, na.rm = TRUE) == 0 ~ "špatný",
                                            sum(stav_count_toler, na.rm = TRUE) == 1 ~ "zhoršený",
                                            sum(stav_count_toler, na.rm = TRUE) == 2 ~ "dobrý"),
                stav = dplyr::case_when(parametr_nazev == "CELKOVE_HODNOCENI" ~ unique(glob_stav),
                                        TRUE ~ stav)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(#parametr_nazev = par_nazev,
                typ_predmetu_hodnoceni = "Stanoviště",
                trend = "neznámý",
                datum_hodnoceni = paste0(Sys.Date())) %>%
  dplyr::rename(kod_chu = SITECODE, 
                nazev_chu = NAZEV, 
                druh = NAZEV_HABITAT, 
                feature_code = HABITAT_CODE, 
                hodnocene_obdobi_od = datum_hodnoceni_od, 
                hodnocene_obdobi_do = datum_hodnoceni_do,
                parametr_limit = LIM_IND,
                poznamka = ZDROJ
                ) %>%
    dplyr::select(
      typ_predmetu_hodnoceni, kod_chu, nazev_chu, 
      druh, feature_code, 
      hodnocene_obdobi_od, hodnocene_obdobi_do, 
      parametr_nazev, parametr_hodnota, parametr_limit, parametr_jednotka, 
      stav, trend, datum_hodnoceni, oop, pracoviste, poznamka
      ) %>%
  #dplyr::left_join(., indikatory[,c(1:2)], by = c("par_nazev" = "ID")) %>%
  dplyr::mutate(SDO2 = dplyr::case_when(kod_chu %in% sdo_II_sites$sitecode ~ 1,
                                        TRUE ~ 0),
                KRAJE2024 = dplyr::case_when(grepl("Správa NP", oop) == TRUE ~ 0,
                                             grepl("Správa KRNAP", oop) == TRUE ~ 0,
                                             grepl("Karlo", oop) == TRUE ~ 1,
                                             grepl("Libereckého", oop) == TRUE ~ 1,
                                             grepl("Plz", oop) == TRUE ~ 1,
                                             grepl("Král", oop) == TRUE ~ 1,
                                             grepl("Pardu", oop) == TRUE ~ 1,
                                             TRUE ~ 0)) %>%
  dplyr::mutate(
    nazev_chu = str_replace_all(nazev_chu, "–|—", "-")
    ) %>%
  dplyr::distinct()

results_comp <- results_long %>%
  dplyr::left_join(
    .,
    results_long_x,
    by = c(
      "kod_chu" = "SITECODE",
      "feature_code" = "HABITAT_CODE",
      "parametr_nazev"
      )
    ) %>%
  dplyr::rowwise() %>%
  mutate(parametr_hodnota.xnum = as.numeric(parametr_hodnota.x),
         parametr_hodnota.ynum = as.numeric(parametr_hodnota.y)) %>%
  dplyr::mutate(trend = dplyr::case_when(parametr_hodnota.xnum == parametr_hodnota.ynum ~ "stabilní",
                                         parametr_hodnota.xnum <= parametr_hodnota.ynum*1.05 &
                                           parametr_hodnota.xnum >= parametr_hodnota.ynum*0.95 ~ "stabilní",
                                         parametr_nazev == "KVALITA" & 
                                           parametr_hodnota.xnum > parametr_hodnota.ynum*1.05 ~ "klesající",
                                         parametr_nazev == "KVALITA" & 
                                           parametr_hodnota.xnum < parametr_hodnota.ynum*0.95 ~ "vzrůstající",
                                         parametr_nazev == "ROZLOHA" & 
                                           parametr_hodnota.xnum < parametr_hodnota.ynum*0.95 ~ "klesající",
                                         parametr_nazev == "ROZLOHA" & 
                                           parametr_hodnota.xnum > parametr_hodnota.ynum*1.05 ~ "vzrůstající"))

nerealne <- results_long %>%
  dplyr::filter(parametr_nazev == "ROZLOHA") %>%
  dplyr::group_by(kod_chu) %>%
  dplyr::reframe(SUMA_LIMIT = sum(parametr_limit, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(., evl %>% 
                     sf::st_drop_geometry() %>% 
                     dplyr::mutate(rozloha_evl = SHAPE.AREA/10000) %>%
                     dplyr::select(SITECODE, NAZEV, rozloha_evl, oop),
                   by = c("kod_chu" = "SITECODE")) %>%
  dplyr::mutate(NEREALNE_CILE = dplyr::case_when(SUMA_LIMIT > rozloha_evl ~ 1,
                                                 TRUE ~ 0)#,
                #praha = dplyr::case_when(grepl("Prahy", oop) == TRUE ~ 1,
                #                         TRUE ~ 0),
                #ustecky = dplyr::case_when(grepl("Ústeck", oop) == TRUE ~ 1,
                #                           TRUE ~ 0),
                #jihocesky = dplyr::case_when(grepl("Jihočes", oop) == TRUE ~ 1,
                #                             TRUE ~ 0),
                #zlinsky = dplyr::case_when(grepl("Zlín", oop) == TRUE ~ 1,
                #                           TRUE ~ 0),
                #jihomoravsky = dplyr::case_when(grepl("Jihomoravsk", oop) == TRUE ~ 1,
                #                                TRUE ~ 0),
                #moravskoszlezky = dplyr::case_when(grepl("Moravskosz", oop) == TRUE ~ 1,
                #                                   TRUE ~ 0),
                #olomoucky = dplyr::case_when(grepl("Olomou", oop) == TRUE ~ 1,
                #                             TRUE ~ 0),
                #stredocesky = dplyr::case_when(grepl("Středočes", oop) == TRUE ~ 1,
                #                               TRUE ~ 0),
                #vysocina = dplyr::case_when(grepl("Vysoč", oop) == TRUE ~ 1,
                #                           TRUE ~ 0)
                ) %>%
  arrange(-NEREALNE_CILE)

write.csv(results_long,
          paste0("Outputs/Data/stanoviste_20250724.csv"),
          row.names = FALSE,
          fileEncoding = "Windows-1250") 


# Set chunk size
chunk_size <- 10000

# Calculate the number of chunks
num_chunks <- ceiling(nrow(results_long) / chunk_size)

# Loop to split and export data
for (i in seq_len(num_chunks)) {
  start_row <- ((i - 1) * chunk_size) + 1
  end_row <- min(i * chunk_size, nrow(results_long))
  
  chunk <- results_long[start_row:end_row, ]
  
  file_name <- paste0("C:/Users/jonas.gaigr/Documents/state_results/results_long_20250225_UTF_part", i, ".csv")
  
  write.csv2(chunk, file_name, row.names = FALSE, fileEncoding = "UTF-8", sep = ";", quote = FALSE)
  
}

cat("Export complete: ", num_chunks, "files created.")

results_kvk <- results_long %>%
  filter(grepl("Karl", oop)) %>%
  filter(NAZEV != "Hradiště") %>%
  filter(parametr_nazev %in% (limity %>% filter(DRUH == "stanoviste") %>% pull(ID_IND)) | parametr_nazev == "CELKOVE_HODNOCENI") %>%
  distinct()

write.csv(results_kvk,
          paste0("C:/Users/jonas.gaigr/Documents/state_results/stanoviste_kvk.csv"),
          row.names = FALSE,
          fileEncoding = "Windows-1250")

kuk <- results_long %>%
  mutate(kuk = parametr_nazev == "ROZLOHA" & parametr_hodnota < LIM_IND)

# sitmap ----
sitmap_geo <- sf::st_read("https://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?request=GetFeature&service=WFS&typeName=Opendata:Mapovaci_sit_-_deleni_1.radu")
rp_geo <- sf::st_read("https://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?request=GetFeature&service=WFS&typeName=Opendata:Uzemni_obvody_regionalnich_pracovist")
inter_rp <- rp_geo %>%
  sf::st_intersection(., sitmap_geo)
intersect_rp <- inter_rp %>%
  mutate(ROZLOHA = st_area(SHAPE) %>% units::drop_units()) %>%
  group_by(POLE) %>%
  arrange(-ROZLOHA) %>%
  slice(1) %>%
  ungroup() %>%
  st_drop_geometry()

write.csv(intersect_rp,
           "intersect_rp.csv",
           fileEncoding = "Windows-1250")

kukmal <- limity_stan %>%
  filter(SITECODE %in% sdo_II_sites$sitecode) 

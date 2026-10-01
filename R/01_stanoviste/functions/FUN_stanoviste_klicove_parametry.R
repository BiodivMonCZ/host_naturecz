# Stanoviste - klicove parametry
#
# Vstupy:
#   hab_code      - kod stanoviste (napr. "6210")
#   site_code     - SITECODE hodnocene site
#   vmb           - aktualni VMB jako sf objekt
#   site          - vrstva SITE jako sf objekt
#   paseky        - tabulka vypoctenych pasek
#   habitat_areas - tabulka celkovych ploch stanovist v CR
#
# Vystup:
#   tibble s jednim radkem pro kombinaci site x habitat

stanoviste_klic <- function(
    hab_code,
    site_code,
    vmb,
    site,
    paseky,
    habitat_areas
) {
  
  # site -----------------------------------------------------------------------
  
  site_target <- site |>
    dplyr::filter(SITECODE == site_code)
  
  site_name <- site_target |>
    sf::st_drop_geometry() |>
    dplyr::pull(NAZEV) |>
    base::unique()
  
  if (base::length(site_name) == 0) {
    site_name <- NA_character_
  } else {
    site_name <- site_name[[1]]
  }
  
  # Vyber kombinace site x stanoviste ------------------------------------------
  
  vmb_target <- vmb |>
    sf::st_intersection(site_target) |>
    dplyr::filter(HABITAT == hab_code) |>
    sf::st_make_valid() |>
    dplyr::filter(
      sf::st_geometry_type(geometry) != "POINT",
      sf::st_geometry_type(geometry) != "MULTIPOINT",
      sf::st_geometry_type(geometry) != "LINESTRING",
      sf::st_geometry_type(geometry) != "MULTILINESTRING"
    ) |>
    dplyr::mutate(
      AREA_real = units::drop_units(sf::st_area(geometry)),
      plo_bio_m2_site = STEJ_PR / 100 * AREA_real
    ) |>
    dplyr::filter(AREA_real > 0)
  
  # Plocha stanoviste vcetne pasek -------------------------------------------
  
  area_paseky_ha <- paseky |>
    dplyr::filter(
      SITECODE == site_code,
      HABITAT_CODE == hab_code
    ) |>
    dplyr::pull(ROZLOHA_PASEKY)
  
  if (base::length(area_paseky_ha) == 0) {
    area_paseky_ha <- NA_real_
  }
  
  sum_plo_bio_m2 <- base::sum(
    base::sum(vmb_target$plo_bio_m2_site, na.rm = TRUE),
    area_paseky_ha * 10000,
    na.rm = TRUE
  )
  
  target_area_ha <- sum_plo_bio_m2 / 10000
  
  # Degradovane a nereprezentativni plochy -----------------------------------
  
  area_w_ha <- vmb_target |>
    sf::st_drop_geometry() |>
    dplyr::filter(DG == "W" | RB == "W") |>
    dplyr::pull(plo_bio_m2_site) |>
    stats::na.omit() |>
    base::sum()
  
  area_w_ha <- area_w_ha / 10000
  
  area_w_perc <- area_w_ha / target_area_ha * 100
  
  area_degrad_ha <- base::sum(
    area_w_ha,
    area_paseky_ha,
    na.rm = TRUE
  )
  
  area_degrad_perc <- area_degrad_ha / target_area_ha * 100
  area_paseky_perc <- area_paseky_ha / target_area_ha * 100
  
  # Kvalitativni parametry -----------------------------------------------------
  
  vmb_qual <- vmb_target |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      TYP_DRUHY_SEG = dplyr::case_when(
        DG == "W" ~ 0,
        RB == "W" ~ 0,
        base::is.na(TD) ~ 0,
        TD == "N" ~ 0,
        TD == "MP" ~ 1,
        TD == "P" ~ 2
      ),
      REPREZENTAVITA_SEG = dplyr::case_when(
        DG == "W" ~ 0,
        RB == "W" ~ 0,
        RB == "F" ~ 0,
        RB == "P" ~ 1,
        RB == "V" ~ 2
      ),
      REPRESENTAVITY_SEG = dplyr::case_when(
        DG == "W" ~ "D",
        RB == "V" & TD == "P" ~ "A",
        RB == "V" & TD == "MP" ~ "B",
        RB == "V" & TD == "N" ~ "C",
        RB == "V" & base::is.na(TD) ~ "A",
        RB == "P" & TD == "P" ~ "B",
        RB == "P" & TD == "MP" ~ "C",
        RB == "P" & TD == "N" ~ "C",
        RB == "P" & base::is.na(TD) ~ "B",
        RB == "F" & TD == "P" ~ "C",
        RB == "F" & TD == "MP" ~ "C",
        RB == "F" & TD == "N" ~ "C",
        RB == "F" & base::is.na(TD) ~ "C",
        RB == "W" & TD == "P" ~ "D",
        RB == "W" & TD == "MP" ~ "D",
        RB == "W" & TD == "N" ~ "D",
        RB == "W" & base::is.na(TD) ~ "D",
        base::is.na(RB) & TD == "P" ~ "B",
        base::is.na(RB) & TD == "MP" ~ "C",
        base::is.na(RB) & TD == "N" ~ "C",
        base::is.na(RB) & base::is.na(TD) ~ "C"
      ),
      CONSERVATION_SEG = dplyr::case_when(
        SF == "P" ~ "A",
        SF == "MP" ~ "B",
        SF == "N" ~ "C",
        RB == "W" ~ "C",
        base::is.na(SF) & DG == 0 ~ "A",
        base::is.na(SF) & DG == 1 ~ "A",
        base::is.na(SF) & DG == 2 ~ "B",
        base::is.na(SF) & DG == 3 ~ "C",
        base::is.na(SF) & base::is.na(DG) ~ "B"
      ),
      REPRE_SDF_SEG = dplyr::case_when(
        REPRESENTAVITY_SEG == "D" ~ 0,
        REPRESENTAVITY_SEG == "C" ~ 1,
        REPRESENTAVITY_SEG == "B" ~ 2,
        REPRESENTAVITY_SEG == "A" ~ 3
      ),
      CON_SEG = dplyr::case_when(
        CONSERVATION_SEG == "C" ~ 0,
        CONSERVATION_SEG == "B" ~ 1,
        CONSERVATION_SEG == "A" ~ 2
      ),
      DEGREEOFCONS_SEG = dplyr::case_when(
        DG == "W" ~ 0,
        RB == "W" ~ 0,
        SF == "N" ~ 0,
        SF == "MP" ~ 100,
        SF == "P" ~ 100
      ),
      KVALITA_SEG = dplyr::case_when(
        DG == "W" ~ 0,
        RB == "W" ~ 0,
        base::is.na(KVALITA) ~ 0,
        KVALITA == 0 ~ 0,
        KVALITA == 1 ~ 3,
        KVALITA == 2 ~ 2,
        KVALITA == 3 ~ 1,
        KVALITA == 4 ~ 0
      ),
      MRTVE_DREVO_SEG = dplyr::case_when(
        base::substr(hab_code, 1, 1) != 9 ~ NA_real_,
        DG == "W" ~ 0,
        RB == "W" ~ 0,
        MD == 0 ~ 0,
        MD == 1 ~ 1,
        MD == 2 ~ 2,
        MD == 3 ~ 0,
        MD == 4 ~ 0
      ),
      KALAMITA_SEG = dplyr::case_when(
        base::substr(hab_code, 1, 1) != 9 ~ NA_real_,
        DG == "W" ~ 0,
        RB == "W" ~ 0,
        MD == 0 ~ 0,
        MD == 1 ~ 0,
        MD == 2 ~ 0,
        MD == 3 ~ 2,
        MD == 4 ~ 2
      ),
      
      # Vazeny prispevek segmentu.
      TD_SEG = TYP_DRUHY_SEG * plo_bio_m2_site / sum_plo_bio_m2,
      RB_SEG = REPREZENTAVITA_SEG * plo_bio_m2_site / sum_plo_bio_m2,
      RB_SDF_SEG = REPRE_SDF_SEG * plo_bio_m2_site / sum_plo_bio_m2,
      CS_SEG = CON_SEG * plo_bio_m2_site / sum_plo_bio_m2,
      DC_SEG = DEGREEOFCONS_SEG * plo_bio_m2_site / sum_plo_bio_m2,
      CN_SEG = CON_SEG * plo_bio_m2_site / sum_plo_bio_m2,
      QUAL_SEG = KVALITA_SEG * plo_bio_m2_site / sum_plo_bio_m2,
      MD_SEG = if (base::substr(hab_code, 1, 1) != "9") {
        NA_real_
      } else {
        MRTVE_DREVO_SEG * plo_bio_m2_site / sum_plo_bio_m2
      },
      KAL_SEG = if (base::substr(hab_code, 1, 1) != "9") {
        NA_real_
      } else {
        KALAMITA_SEG * plo_bio_m2_site / sum_plo_bio_m2
      }
    ) |>
    dplyr::mutate(
      TD_FIN = 3 - base::sum(TD_SEG, na.rm = TRUE),
      RB_FIN = base::sum(RB_SEG, na.rm = TRUE),
      RB_SDF_FIN = 4 - base::sum(RB_SDF_SEG, na.rm = TRUE),
      RB_SDF_FIN_KAT = dplyr::case_when(
        RB_SDF_FIN < 1.5 ~ "A",
        RB_SDF_FIN >= 1.5 & RB_SDF_FIN < 2.5 ~ "B",
        RB_SDF_FIN >= 2.5 & RB_SDF_FIN < 3.5 ~ "C",
        RB_SDF_FIN >= 3.5 ~ "D",
        TRUE ~ NA_character_
      ),
      DC_FIN = base::sum(DC_SEG, na.rm = TRUE),
      CN_FIN = 3 - base::sum(CN_SEG, na.rm = TRUE),
      MD_FIN = dplyr::case_when(
        base::substr(hab_code, 1, 1) != 9 ~ NA_real_,
        TRUE ~ base::sum(MD_SEG, na.rm = TRUE)
      ),
      KP_FIN = dplyr::case_when(
        base::substr(hab_code, 1, 1) != 9 ~ NA_real_,
        TRUE ~ base::sum(KAL_SEG, na.rm = TRUE)
      ),
      QUALITY = 4 - base::sum(QUAL_SEG, na.rm = TRUE)
    ) |>
    dplyr::distinct()
  
  # Plocha --------------------------------------------------------------------
  
  site_area_m2 <- site_target |>
    sf::st_drop_geometry() |>
    dplyr::pull(SHAPE_AREA) |>
    base::unique()
  
  area_site_perc <- base::unique(
    target_area_ha / (site_area_m2 / 10000) * 100
  )
  
  habitat_area_m2 <- habitat_areas |>
    dplyr::filter(HABITAT == hab_code) |>
    dplyr::pull(TOTAL_AREA_ALL)
  
  area_relative_perc <- (target_area_ha / (habitat_area_m2 / 10000)) * 100
  
  area_good_ha <- vmb_target |>
    dplyr::filter(SF == "P" | SF == "MP") |>
    dplyr::pull(plo_bio_m2_site) |>
    base::sum()
  
  area_good_ha <- area_good_ha / 10000
  
  if (base::nrow(vmb_target) == 0 & base::is.na(target_area_ha)) {
    target_area_ha <- 0
    area_w_ha <- 0
    area_w_perc <- 0
    area_site_perc <- 0
    area_good_ha <- 0
  }
  
  # Datum ---------------------------------------------------------------------
  
  vmb_target_date <- vmb_target |>
    dplyr::pull(DATUM)
  
  min_date <- vmb_target_date |>
    base::min() |>
    base::unique()
  
  max_date <- vmb_target_date |>
    base::max() |>
    base::unique()
  
  mean_date <- base::mean(vmb_target_date)
  median_date <- stats::median(vmb_target_date)
  
  # Podil segmentu v aktualizacnich obdobich ---------------------------------
  
  perc_seg_0 <- vmb_target |>
    sf::st_drop_geometry() |>
    dplyr::filter(ROK_AKT.y == 0) |>
    dplyr::pull(plo_bio_m2_site) |>
    base::sum()
  
  perc_seg_0 <- perc_seg_0 / target_area_ha / 100
  
  perc_seg_1 <- vmb_target |>
    sf::st_drop_geometry() |>
    dplyr::filter(ROK_AKT.y > 0 & ROK_AKT.y <= 2012) |>
    dplyr::pull(plo_bio_m2_site) |>
    base::sum()
  
  perc_seg_1 <- perc_seg_1 / target_area_ha / 100
  
  # Hodnota 2025 je zamerne zachovana z puvodniho skriptu.
  # Dynamizaci roku je vhodne resit jako samostatnou metodickou zmenu.
  perc_seg_2 <- vmb_target |>
    sf::st_drop_geometry() |>
    dplyr::filter(ROK_AKT.y > 2012 & ROK_AKT.y <= 2025) |>
    dplyr::pull(plo_bio_m2_site) |>
    base::sum()
  
  perc_seg_2 <- perc_seg_2 / target_area_ha / 100
  
  if (base::nrow(vmb_target) == 0) {
    perc_seg_0 <- NA_real_
    perc_seg_1 <- NA_real_
    perc_seg_2 <- NA_real_
  }
  
  # Vystup --------------------------------------------------------------------
  
  if (target_area_ha > 0 & !base::is.na(target_area_ha)) {
    
    result <- vmb_qual |>
      dplyr::reframe(
        SITECODE = site_code,
        NAZEV = site_name,
        HABITAT_CODE = hab_code,
        ROZLOHA = target_area_ha,
        KVALITA = base::unique(QUALITY)[1],
        TYPICKE_DRUHY = base::unique(TD_FIN)[1],
        REPRE = base::unique(RB_FIN)[1],
        REPRE_SDF = base::unique(RB_SDF_FIN)[1],
        CONSERVATION = base::unique(CN_FIN)[1],
        DEGREE_OF_CONSERVATION = base::unique(DC_FIN)[1],
        MRTVE_DREVO = base::unique(MD_FIN)[1],
        KALAMITA_POLOM = base::unique(KP_FIN)[1],
        RELATIVE_AREA_PERC = area_relative_perc,
        SITE_AREA_PERC = area_site_perc,
        GOOD_DOC_AREA_HA = area_good_ha,
        W_AREA_HA = area_w_ha,
        W_AREA_PERC = area_w_perc,
        PASEKY_AREA_HA = area_paseky_ha,
        PASEKY_AREA_PERC = area_paseky_perc,
        DEGRAD_AREA_HA = area_degrad_ha,
        DEGRAD_AREA_PERC = area_degrad_perc,
        PERC_0 = perc_seg_0,
        PERC_1 = perc_seg_1,
        PERC_2 = perc_seg_2,
        DATE_MIN = min_date,
        DATE_MAX = max_date,
        DATE_MEAN = mean_date,
        DATE_MEDIAN = median_date
      ) |>
      dplyr::distinct()
    
  } else {
    
    result <- dplyr::tibble(
      SITECODE = site_code,
      NAZEV = site_name,
      HABITAT_CODE = hab_code,
      ROZLOHA = target_area_ha,
      KVALITA = NA_real_,
      TYPICKE_DRUHY = NA_real_,
      REPRE = NA_real_,
      REPRE_SDF = NA_real_,
      CONSERVATION = NA_real_,
      DEGREE_OF_CONSERVATION = NA_real_,
      MRTVE_DREVO = NA_real_,
      KALAMITA_POLOM = NA_real_,
      RELATIVE_AREA_PERC = NA_real_,
      SITE_AREA_PERC = NA_real_,
      GOOD_DOC_AREA_HA = NA_real_,
      W_AREA_HA = area_w_ha,
      W_AREA_PERC = area_w_perc,
      PASEKY_AREA_HA = area_paseky_ha,
      PASEKY_AREA_PERC = area_paseky_perc,
      DEGRAD_AREA_HA = area_degrad_ha,
      DEGRAD_AREA_PERC = area_degrad_perc,
      PERC_0 = NA_real_,
      PERC_1 = NA_real_,
      PERC_2 = NA_real_,
      DATE_MIN = base::as.Date(NA),
      DATE_MAX = base::as.Date(NA),
      DATE_MEAN = base::as.Date(NA),
      DATE_MEDIAN = base::as.Date(NA)
    )
  }
  
  result |>
    dplyr::distinct()
}

# Stanoviste - druhove parametry
#
# Vstupy:
#   hab_code          - kod stanoviste (napr. "6210")
#   site_code         - SITECODE hodnocene site
#   vmb               - aktualni VMB jako sf objekt
#   site              - vrstva hodnocenych uzemi jako sf objekt
#   paseky            - tabulka vypoctenych pasek
#   red_list_species  - prostorova data druhu cerveneho seznamu
#   invasive_species  - prostorova data invaznich druhu
#   expansive_species - prostorova data expanznich druhu
#   habitat_col       - sloupec ve VMB obsahujici kod stanoviste
#
# Vystup:
#   tibble s jednim radkem pro kombinaci site x habitat

stanoviste_druhy <- function(
    hab_code,
    site_code,
    vmb,
    site,
    paseky,
    red_list_species,
    invasive_species,
    expansive_species,
    habitat_col = "HABITAT"
) {
  
  # Site -----------------------------------------------------------------------
  
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
    dplyr::filter(.data[[habitat_col]] == hab_code) |>
    sf::st_make_valid() |>
    dplyr::filter(
      sf::st_geometry_type(geometry) != "POINT",
      sf::st_geometry_type(geometry) != "MULTIPOINT",
      sf::st_geometry_type(geometry) != "LINESTRING",
      sf::st_geometry_type(geometry) != "MULTILINESTRING"
    ) |>
    dplyr::mutate(
      AREA_real = units::drop_units(sf::st_area(geometry)),
      plo_bio_m2_site = STEJ_PR / 100 * AREA_real,
      VMB_OBJECTID = OBJECTID
    ) |>
    dplyr::filter(AREA_real > 0)
  
  # Celkova plocha stanoviste vcetne pasek -------------------------------------
  
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
  
  # Pokud stanoviste nema plochu, druhove parametry nelze hodnotit ------------
  
  if (base::is.na(target_area_ha) || target_area_ha <= 0) {
    return(
      dplyr::tibble(
        SITECODE = site_code,
        NAZEV = site_name,
        HABITAT_CODE = hab_code,
        ROZLOHA = target_area_ha,
        RED_LIST = NA_real_,
        INVASIVE = NA_real_,
        EXPANSIVE = NA_real_,
        RED_LIST_SPECIES = NA_character_,
        INVASIVE_LIST = NA_character_,
        EXPANSIVE_LIST = NA_character_
      )
    )
  }
  
  # Red list species -----------------------------------------------------------
  
  vmb_spat <- vmb_target |>
    dplyr::filter(FSB_EVAL != "X")
  
  redlist_list <- red_list_species |>
    sf::st_filter(vmb_target) |>
    dplyr::pull(DRUH) |>
    base::unique()
  
  redlist <- base::length(redlist_list) / base::log(sum_plo_bio_m2) * 4
  
  if (redlist > 10) {
    redlist <- 10
  }
  
  if (base::length(redlist_list) == 0) {
    redlist_list <- NA_character_
  }
  
  if (base::nrow(vmb_spat) == 0) {
    redlist_list <- NA_character_
    redlist <- NA_real_
  }
  
  # Invazni druhy --------------------------------------------------------------
  
  invasive_target <- invasive_species
  
  # Arrhenatherum elatius se u 6510 / T1.1 nepovazuje za invazni druh.
  if (base::as.character(hab_code) %in% c("6510", "T1.1")) {
    invasive_target <- invasive_target |>
      dplyr::filter(DRUH != "Arrhenatherum elatius")
  }
  
  invaders_all <- sf::st_intersection(invasive_target, vmb_target) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(VMB_OBJECTID, DRUH) |>
    dplyr::slice(base::which.max(DATUM_OD)) |>
    dplyr::filter(NEGATIVNI == 0) |>
    dplyr::ungroup()
  
  invaders_calc <- invaders_all |>
    dplyr::group_by(VMB_OBJECTID) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  invaders_list <- invaders_all |>
    dplyr::pull(DRUH) |>
    base::unique()
  
  invaders <- base::sum(
    invaders_calc$plo_bio_m2_site,
    na.rm = TRUE
  ) / sum_plo_bio_m2 * 100
  
  if (
    base::length(invaders_list) == 0 ||
    base::nrow(vmb_target) == 0
  ) {
    invaders_list <- NA_character_
  }
  
  # Expanzni druhy -------------------------------------------------------------
  
  expanders_all <- expansive_species |>
    dplyr::filter(POKRYVN %in% c("3", "4", "5")) |>
    sf::st_intersection(vmb_target) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(VMB_OBJECTID, DRUH) |>
    dplyr::slice(base::which.max(DATUM_OD)) |>
    dplyr::filter(NEGATIVNI == 0) |>
    dplyr::ungroup()
  
  expanders_calc <- expanders_all |>
    dplyr::group_by(VMB_OBJECTID) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  expanders_list <- expanders_all |>
    dplyr::pull(DRUH) |>
    base::unique()
  
  expanders <- base::sum(
    expanders_calc$plo_bio_m2_site,
    na.rm = TRUE
  ) / sum_plo_bio_m2 * 100
  
  if (
    base::length(expanders_list) == 0 &&
    base::nrow(vmb_target) == 0
  ) {
    expanders <- NA_real_
    expanders_list <- NA_character_
  } else if (
    base::length(expanders_list) == 0 &&
    base::nrow(vmb_target) > 0
  ) {
    expanders <- 0
    expanders_list <- NA_character_
  }
  
  # Vystup ---------------------------------------------------------------------
  
  dplyr::tibble(
    SITECODE = site_code,
    NAZEV = site_name,
    HABITAT_CODE = hab_code,
    ROZLOHA = target_area_ha,
    RED_LIST = redlist,
    INVASIVE = invaders,
    EXPANSIVE = expanders,
    RED_LIST_SPECIES = if (
      base::all(base::is.na(redlist_list))
    ) NA_character_ else base::paste(redlist_list, collapse = ", "),
    INVASIVE_LIST = if (
      base::all(base::is.na(invaders_list))
    ) NA_character_ else base::paste(invaders_list, collapse = ", "),
    EXPANSIVE_LIST = if (
      base::all(base::is.na(expanders_list))
    ) NA_character_ else base::paste(expanders_list, collapse = ", ")
  ) |>
    dplyr::distinct()
}

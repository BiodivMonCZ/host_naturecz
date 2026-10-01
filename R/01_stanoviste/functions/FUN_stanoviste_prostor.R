# Stanoviste - prostorove parametry
#
# Vstupy:
#   hab_code      - kod stanoviste (napr. "6210")
#   site_code     - SITECODE hodnocene site
#   vmb           - aktualni VMB jako sf objekt
#   site          - vrstva SITE jako sf objekt
#   paseky        - tabulka vypoctenych pasek
#   minimisize    - tabulka limitu minimiarealu; MINIMISIZE je v m2
#   czechia_line  - hranice CR jako liniovy sf/sfc objekt
#   habitat_col   - sloupec ve VMB obsahujici kod stanoviste
#
# Vystup:
#   tibble s jednim radkem pro kombinaci site x habitat

stanoviste_prostor <- function(
    hab_code,
    site_code,
    vmb,
    site,
    paseky,
    minimisize,
    czechia_line,
    habitat_col = "HABITAT"
) {
  
  # Zakladni kontroly ----------------------------------------------------------
  
  if (!base::inherits(vmb, "sf")) {
    base::stop("`vmb` musi byt sf objekt.")
  }
  
  if (!base::inherits(site, "sf")) {
    base::stop("`site` musi byt sf objekt.")
  }
  
  if (!base::inherits(czechia_line, "sf") && !base::inherits(czechia_line, "sfc")) {
    base::stop("`czechia_line` musi byt sf nebo sfc objekt.")
  }
  
  if (sf::st_is_longlat(vmb)) {
    base::stop("`vmb` musi byt v projektovanem CRS; vzdalenosti se pocitaji v metrech.")
  }
  
  crs_units <- sf::st_crs(vmb)$units_gdal
  
  if (
    !base::is.null(crs_units) &&
    !base::tolower(crs_units) %in% base::c("metre", "meter", "m")
  ) {
    base::stop("CRS objektu `vmb` musi pouzivat metry.")
  }
  
  required_vmb_cols <- base::c(
    habitat_col,
    "STEJ_PR",
    "DG",
    "RB",
    "BIOTOP",
    "FSB_EVAL",
    "BIOTOP_SEZ"
  )
  
  missing_vmb_cols <- base::setdiff(required_vmb_cols, base::names(vmb))
  
  if (base::length(missing_vmb_cols) > 0) {
    base::stop(
      "Ve `vmb` chybi sloupce: ",
      base::paste(missing_vmb_cols, collapse = ", ")
    )
  }
  
  # Site ----------------------------------------------------------------------
  
  site_target <- site |>
    dplyr::filter(SITECODE == site_code)
  
  if (base::nrow(site_target) == 0) {
    base::stop("`site_code` nebyl nalezen ve vrstve `site`: ", site_code)
  }
  
  if (!base::isTRUE(sf::st_crs(site_target) == sf::st_crs(vmb))) {
    site_target <- sf::st_transform(site_target, sf::st_crs(vmb))
  }
  
  site_name <- site_target |>
    sf::st_drop_geometry() |>
    dplyr::pull(NAZEV) |>
    base::unique()
  
  if (base::length(site_name) == 0) {
    site_name <- NA_character_
  } else {
    site_name <- site_name[[1]]
  }
  
  site_geom <- site_target |>
    sf::st_geometry() |>
    sf::st_union()
  
  # Vyber kombinace site x stanoviste -----------------------------------------
  
  vmb_target <- vmb |>
    dplyr::filter(.data[[habitat_col]] == hab_code) |>
    sf::st_intersection(site_geom) |>
    sf::st_make_valid() |>
    sf::st_collection_extract("POLYGON", warn = FALSE) |>
    dplyr::mutate(
      area_real = units::drop_units(sf::st_area(geometry)),
      plo_bio_m2_site = STEJ_PR / 100 * area_real
    ) |>
    dplyr::filter(area_real > 0)
  
  # Celkova plocha stanoviste vcetne pasek -----------------------------------
  
  area_paseky_values <- paseky |>
    dplyr::filter(
      SITECODE == site_code,
      HABITAT_CODE == hab_code
    ) |>
    dplyr::pull(ROZLOHA_PASEKY)
  
  if (base::length(area_paseky_values) == 0) {
    area_paseky_ha <- NA_real_
    area_paseky_sum_ha <- 0
  } else {
    area_paseky_ha <- base::sum(area_paseky_values, na.rm = TRUE)
    area_paseky_sum_ha <- area_paseky_ha
  }
  
  sum_plo_bio_m2 <- base::sum(
    base::sum(vmb_target$plo_bio_m2_site, na.rm = TRUE),
    area_paseky_sum_ha * 10000,
    na.rm = TRUE
  )
  
  target_area_ha <- sum_plo_bio_m2 / 10000
  
  # Hodnota minimiarealu -------------------------------------------------------
  #
  # Pro nektere habitaty ma konkretni BIOTOP vlastni limit. Pokud je habitat
  # v site zastoupen vice BIOTOP kategoriemi, pouzije se nejvyssi relevantni
  # hodnota. V6 a M4.3 maji podle metodiky limit 0.05 ha = 500 m2.
  
  minimi_biotop_m2 <- base::c(
    "V6" = 500,
    "M2.2" = 1000,
    "M3" = 2000,
    "M2.1" = 7000,
    "M2.3" = 7000,
    "M4.2" = 300,
    "M4.3" = 500
  )
  
  present_biotops <- vmb_target |>
    sf::st_drop_geometry() |>
    dplyr::pull(BIOTOP) |>
    base::as.character() |>
    stats::na.omit() |>
    base::unique()
  
  base_minimi_values <- minimisize |>
    dplyr::filter(HABITAT == hab_code) |>
    dplyr::pull(MINIMISIZE) |>
    base::as.numeric()
  
  if (
    base::length(base_minimi_values) == 0 ||
    base::all(base::is.na(base_minimi_values))
  ) {
    base_minimi_m2 <- NA_real_
  } else {
    base_minimi_m2 <- base::max(base_minimi_values, na.rm = TRUE)
  }
  
  if (base::length(present_biotops) > 0) {
    
    minimi_per_biotop_m2 <- base::vapply(
      present_biotops,
      FUN = function(biotop) {
        
        # Metodicke vyjimky maji prednost pred obecnou habitatovou hodnotou.
        if (biotop %in% base::names(minimi_biotop_m2)) {
          return(base::unname(minimi_biotop_m2[[biotop]]))
        }
        
        # Pokud tabulka obsahuje BIOTOP, pouzij jeho konkretni hodnotu.
        if ("BIOTOP" %in% base::names(minimisize)) {
          biotop_values <- minimisize |>
            dplyr::filter(
              HABITAT == hab_code,
              BIOTOP == biotop
            ) |>
            dplyr::pull(MINIMISIZE) |>
            base::as.numeric()
          
          if (
            base::length(biotop_values) > 0 &&
            !base::all(base::is.na(biotop_values))
          ) {
            return(base::max(biotop_values, na.rm = TRUE))
          }
        }
        
        base_minimi_m2
      },
      FUN.VALUE = base::numeric(1)
    )
    
    if (base::all(base::is.na(minimi_per_biotop_m2))) {
      minimi_m2 <- NA_real_
    } else {
      minimi_m2 <- base::max(minimi_per_biotop_m2, na.rm = TRUE)
    }
    
  } else {
    minimi_m2 <- base_minimi_m2
  }
  
  minimi_value_ha <- minimi_m2 / 10000
  
  # Minimiareál ---------------------------------------------------------------
  #
  # Do citatele nevstupuji degradovane segmenty W ani paseky. Jmenovatelem je
  # ale cela plocha stanoviste vcetne W a pasek.
  #
  # Propojeni segmentu se urcuje skutecnou vzdalenosti <= 50 m. Nepouziva se
  # 50m buffer na kazdy segment, ktery by spojil i segmenty vzdalene az 100 m.
  
  minimi_segments <- vmb_target |>
    dplyr::filter(
      (base::is.na(DG) | DG != "W") &
        (base::is.na(RB) | RB != "W")
    )
  
  if (
    base::nrow(minimi_segments) > 0 &&
    !base::is.na(minimi_m2) &&
    sum_plo_bio_m2 > 0
  ) {
    
    neighbours <- sf::st_is_within_distance(
      minimi_segments,
      minimi_segments,
      dist = 50
    )
    
    component_id <- base::integer(base::nrow(minimi_segments))
    component_no <- 0L
    
    for (i in base::seq_len(base::nrow(minimi_segments))) {
      if (component_id[[i]] != 0L) {
        next
      }
      
      component_no <- component_no + 1L
      queue <- i
      component_id[[i]] <- component_no
      
      while (base::length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]
        
        current_neighbours <- neighbours[[current]]
        new_neighbours <- current_neighbours[component_id[current_neighbours] == 0L]
        
        if (base::length(new_neighbours) > 0) {
          component_id[new_neighbours] <- component_no
          queue <- base::c(queue, new_neighbours)
        }
      }
    }
    
    minimi_groups <- minimi_segments |>
      dplyr::mutate(minimi_group = component_id) |>
      sf::st_drop_geometry() |>
      dplyr::group_by(minimi_group) |>
      dplyr::summarise(
        group_area_m2 = base::sum(plo_bio_m2_site, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(meets_minimi = group_area_m2 >= minimi_m2)
    
    minimi_area_m2 <- minimi_groups |>
      dplyr::filter(meets_minimi) |>
      dplyr::pull(group_area_m2) |>
      base::sum(na.rm = TRUE)
    
    minimi_jadra <- minimi_groups |>
      dplyr::filter(meets_minimi) |>
      base::nrow()
    
    minimi_perc <- minimi_area_m2 / sum_plo_bio_m2 * 100
    minimi_perc <- base::min(base::max(minimi_perc, 0), 100)
    
  } else if (
    base::nrow(minimi_segments) == 0 &&
    !base::is.na(minimi_m2) &&
    sum_plo_bio_m2 > 0
  ) {
    
    minimi_perc <- 0
    minimi_jadra <- 0L
    
  } else {
    
    minimi_perc <- NA_real_
    minimi_jadra <- NA_integer_
  }
  
  # Mozaika -------------------------------------------------------------------
  
  vmb_spat <- vmb_target |>
    dplyr::filter(!base::is.na(FSB_EVAL), FSB_EVAL != "X")
  
  if (sum_plo_bio_m2 > 0) {
    perc_spat <- base::sum(vmb_spat$plo_bio_m2_site, na.rm = TRUE) /
      sum_plo_bio_m2 * 100
  } else {
    perc_spat <- NA_real_
  }
  
  # Vnitrni mozaika -----------------------------------------------------------
  #
  # Metodika: 100 - podil plochy stanoviste zastoupene v mozaikach s X.
  # Do jmenovatele vstupuji take W a paseky.
  
  if (sum_plo_bio_m2 > 0) {
    
    x_area_m2 <- vmb_target |>
      sf::st_drop_geometry() |>
      dplyr::filter(
        !base::is.na(BIOTOP_SEZ),
        base::grepl("X", BIOTOP_SEZ, ignore.case = TRUE)
      ) |>
      dplyr::pull(plo_bio_m2_site) |>
      base::sum(na.rm = TRUE)
    
    mozaika_x_perc <- x_area_m2 / sum_plo_bio_m2 * 100
    mozaika_vnitrni <- 100 - mozaika_x_perc
    mozaika_vnitrni <- base::min(base::max(mozaika_vnitrni, 0), 100)
    
  } else {
    mozaika_vnitrni <- NA_real_
  }
  
  # Vnejsi mozaika ------------------------------------------------------------
  #
  # Pomer delky hranice s ostatnimi prirodnimi biotopy k celkove zname delce
  # hranice. Cast hranice shodna se statni hranici se nevklada jako
  # "neprirodni"; chybejici zahranicni podil je dopocten z pomeru dostupneho
  # na ceske strane.
  
  if (base::nrow(vmb_spat) > 0) {
    
    target_mosaic_geom <- vmb_spat |>
      sf::st_geometry() |>
      sf::st_union() |>
      sf::st_make_valid()
    
    target_boundary <- sf::st_boundary(target_mosaic_geom)
    
    site_buffer <- sf::st_buffer(
      site_geom,
      dist = 500
    )
    
    vmb_natural <- vmb |>
      sf::st_filter(site_buffer) |>
      dplyr::filter(
        !base::is.na(FSB_EVAL),
        !FSB_EVAL %in% base::c("X", "-", "-1"),
        base::is.na(.data[[habitat_col]]) | .data[[habitat_col]] != hab_code
      ) |>
      sf::st_make_valid() |>
      sf::st_collection_extract("POLYGON", warn = FALSE)
    
    border_all <- target_boundary |>
      sf::st_length() |>
      units::drop_units() |>
      base::sum(na.rm = TRUE)
    
    czechia_line_target <- czechia_line
    
    if (!base::isTRUE(sf::st_crs(czechia_line_target) == sf::st_crs(vmb_target))) {
      czechia_line_target <- sf::st_transform(
        czechia_line_target,
        sf::st_crs(vmb_target)
      )
    }
    
    if (base::inherits(czechia_line_target, "sf")) {
      czechia_line_geom <- sf::st_geometry(czechia_line_target)
    } else {
      czechia_line_geom <- czechia_line_target
    }
    
    czechia_line_geom <- sf::st_union(czechia_line_geom)
    
    border_hsl <- sf::st_intersection(
      target_boundary,
      czechia_line_geom
    ) |>
      sf::st_length() |>
      units::drop_units() |>
      base::sum(na.rm = TRUE)
    
    known_border <- base::max(border_all - border_hsl, 0)
    
    if (border_all > 0) {
      mozaika_border_fill <- known_border / border_all
    } else {
      mozaika_border_fill <- NA_real_
    }
    
    if (base::nrow(vmb_natural) > 0 && known_border > 0) {
      
      natural_geom <- vmb_natural |>
        sf::st_geometry() |>
        sf::st_union() |>
        sf::st_make_valid()
      
      border_nat <- sf::st_intersection(
        target_boundary,
        natural_geom
      ) |>
        sf::st_length() |>
        units::drop_units() |>
        base::sum(na.rm = TRUE)
      
      mozaika_vnejsi <- border_nat / known_border * 100
      mozaika_vnejsi <- base::min(base::max(mozaika_vnejsi, 0), 100)
      
    } else if (known_border > 0) {
      mozaika_vnejsi <- 0
    } else {
      mozaika_vnejsi <- NA_real_
    }
    
  } else {
    mozaika_vnejsi <- NA_real_
    mozaika_border_fill <- NA_real_
  }
  
  # Finalni mozaika -----------------------------------------------------------
  
  if (base::is.na(perc_spat)) {
    mozaika_fin <- NA_real_
  } else if (perc_spat >= 25) {
    mozaika_fin <- mozaika_vnejsi
  } else {
    mozaika_fin <- mozaika_vnitrni
  }
  
  # Vystup --------------------------------------------------------------------
  
  dplyr::tibble(
    SITECODE = site_code,
    NAZEV = site_name,
    HABITAT_CODE = hab_code,
    ROZLOHA = target_area_ha,
    MINIMIAREAL = minimi_perc,
    MINIMIAREAL_JADRA = minimi_jadra,
    MINIMIAREAL_HODNOTA = minimi_value_ha,
    MOZAIKA_VNEJSI = mozaika_vnejsi,
    MOZAIKA_VNITRNI = mozaika_vnitrni,
    MOZAIKA_FIN = mozaika_fin,
    VYPLNENOST_MOZAIKA = mozaika_border_fill,
    PERC_SPAT = perc_spat
  ) |>
    dplyr::distinct()
}

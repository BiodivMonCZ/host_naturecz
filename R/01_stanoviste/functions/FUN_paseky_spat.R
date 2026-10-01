# FUN_paseky_spat.R
#
# Paseky - prostorovy vypocet pro jednu kandidatni dvojici VMB.
#
# REGION_ID se vztahuji k novejsi vrstve dane dvojice. Starsi vrstva se
# k nim nepripojuje podle REGION_ID; vazba mezi starsim a novejsim mapovanim
# se urci az skutecnym prostorovym prunikem.
#
# U Natura habitatovych kodu (napr. 91D0) se ve starsim mapovani pouziji
# pouze lesni biotopy L*. Tim se reprodukuje puvodni workflow, ve kterem se
# paseky pocitaly z ceskych lesnich biotopu a nelesni biotopy (napr. R3.2)
# do vypoctu nevstupovaly.
#
# Vystup:
#   sf s jednotlivymi pruniky starsiho a novejsiho mapovani.

paseky_spat <- function(
    hab_code,
    site_code,
    site,
    vmb_old,
    vmb_new,
    region_ids,
    pair,
    habitat_col = "HABITAT",
    biotop_col = "BIOTOP",
    share_col = "STEJ_PR",
    segment_id_col = "SEGMENT_ID",
    region_id_col = "REGION_ID",
    date_col = "DATUM",
    update_year_col = "ROK_AKT"
) {
  
  region_ids <- base::unique(
    base::as.character(
      region_ids
    )
  )
  
  region_ids <- region_ids[
    !base::is.na(region_ids)
  ]
  
  if (base::length(region_ids) == 0) {
    return(NULL)
  }
  
  if (
    !base::substr(
      base::as.character(hab_code),
      1,
      1
    ) %in% base::c(
      "9",
      "L"
    )
  ) {
    return(NULL)
  }
  
  if (!"SITECODE" %in% base::names(site)) {
    base::stop(
      "Vrstva `site` neobsahuje sloupec `SITECODE`."
    )
  }
  
  required_old <- base::c(
    habitat_col,
    biotop_col,
    share_col,
    segment_id_col,
    date_col
  )
  
  required_new <- base::c(
    biotop_col,
    share_col,
    segment_id_col,
    region_id_col,
    date_col,
    update_year_col
  )
  
  missing_old <- required_old[
    !required_old %in% base::names(vmb_old)
  ]
  
  missing_new <- required_new[
    !required_new %in% base::names(vmb_new)
  ]
  
  if (base::length(missing_old) > 0) {
    base::stop(
      "V `vmb_old` chybi sloupce: ",
      base::paste(
        missing_old,
        collapse = ", "
      )
    )
  }
  
  if (base::length(missing_new) > 0) {
    base::stop(
      "V `vmb_new` chybi sloupce: ",
      base::paste(
        missing_new,
        collapse = ", "
      )
    )
  }
  
  site_target <- site |>
    dplyr::filter(
      SITECODE == site_code
    )
  
  if (base::nrow(site_target) == 0) {
    base::stop(
      "`site_code` nebyl nalezen ve vrstve `site`: ",
      site_code
    )
  }
  
  site_target <- sf::st_make_valid(
    site_target
  )
  
  if (
    !base::isTRUE(
      sf::st_crs(vmb_old) ==
      sf::st_crs(site_target)
    )
  ) {
    vmb_old <- sf::st_transform(
      vmb_old,
      sf::st_crs(site_target)
    )
  }
  
  if (
    !base::isTRUE(
      sf::st_crs(vmb_new) ==
      sf::st_crs(site_target)
    )
  ) {
    vmb_new <- sf::st_transform(
      vmb_new,
      sf::st_crs(site_target)
    )
  }
  
  site_geom <- site_target |>
    sf::st_geometry() |>
    sf::st_union()
  
  # ---------------------------------------------------------------------------
  # Starsi mapovani - pouze cilovy LESNI biotop
  # ---------------------------------------------------------------------------
  #
  # Pro Natura habitatovy kod:
  #   HABITAT == hab_code + pouze BIOTOP zacinajici "L"
  #
  # Pro cesky kod biotopu:
  #   BIOTOP == hab_code
  #
  # Druha podminka zachovava zpetnou kompatibilitu s primym vypoctem nad
  # ceskym biotopovym kodem.
  
  old_target <- vmb_old |>
    dplyr::filter(
      (
        base::as.character(
          .data[[habitat_col]]
        ) == base::as.character(hab_code) &
          !base::is.na(
            .data[[biotop_col]]
          ) &
          base::substr(
            base::as.character(
              .data[[biotop_col]]
            ),
            1,
            1
          ) == "L"
      ) |
        base::as.character(
          .data[[biotop_col]]
        ) == base::as.character(hab_code)
    )
  
  if (base::nrow(old_target) == 0) {
    return(NULL)
  }
  
  old_target <- sf::st_filter(
    old_target,
    site_geom,
    .predicate = sf::st_intersects
  )
  
  if (base::nrow(old_target) == 0) {
    return(NULL)
  }
  
  old_target <- sf::st_intersection(
    old_target,
    site_geom
  ) |>
    sf::st_make_valid() |>
    dplyr::filter(
      base::as.character(
        sf::st_geometry_type(geometry)
      ) %in% base::c(
        "POLYGON",
        "MULTIPOLYGON"
      )
    ) |>
    dplyr::transmute(
      SEGMENT_ID_OLD = base::as.character(
        .data[[segment_id_col]]
      ),
      BIOTOP_ORIG = base::as.character(
        .data[[biotop_col]]
      ),
      STEJ_PR_ORIG = base::as.numeric(
        .data[[share_col]]
      ),
      DATUM_OLD = base::as.Date(
        .data[[date_col]]
      ),
      geometry
    )
  
  if (base::nrow(old_target) == 0) {
    return(NULL)
  }
  
  # ---------------------------------------------------------------------------
  # Novejsi mapovani - REGION_ID patri novejsi vrstve
  # ---------------------------------------------------------------------------
  
  new_target <- vmb_new |>
    dplyr::filter(
      base::as.character(
        .data[[region_id_col]]
      ) %in% region_ids
    )
  
  if (base::nrow(new_target) == 0) {
    return(NULL)
  }
  
  new_target <- sf::st_filter(
    new_target,
    site_geom,
    .predicate = sf::st_intersects
  )
  
  if (base::nrow(new_target) == 0) {
    return(NULL)
  }
  
  new_target <- sf::st_intersection(
    new_target,
    site_geom
  ) |>
    sf::st_make_valid() |>
    dplyr::filter(
      base::as.character(
        sf::st_geometry_type(geometry)
      ) %in% base::c(
        "POLYGON",
        "MULTIPOLYGON"
      )
    ) |>
    dplyr::transmute(
      SEGMENT_ID_NEW = base::as.character(
        .data[[segment_id_col]]
      ),
      REGION_ID = base::as.character(
        .data[[region_id_col]]
      ),
      BIOTOP_UPDATE = base::as.character(
        .data[[biotop_col]]
      ),
      STEJ_PR_UPDATE = base::as.numeric(
        .data[[share_col]]
      ),
      DATUM_NEW = base::as.Date(
        .data[[date_col]]
      ),
      ROK_AKT_UPDATE = base::as.integer(
        .data[[update_year_col]]
      ),
      geometry
    )
  
  if (base::nrow(new_target) == 0) {
    return(NULL)
  }
  
  # ---------------------------------------------------------------------------
  # Predvyber prekryvajicich se segmentu
  # ---------------------------------------------------------------------------
  
  hit_list <- sf::st_intersects(
    new_target,
    old_target
  )
  
  if (
    !base::any(
      base::lengths(
        hit_list
      ) > 0
    )
  ) {
    return(NULL)
  }
  
  new_sub <- new_target[
    base::lengths(hit_list) > 0,
  ]
  
  old_sub <- old_target[
    base::sort(
      base::unique(
        base::unlist(
          hit_list
        )
      )
    ),
  ]
  
  # ---------------------------------------------------------------------------
  # Finalni prunik
  # ---------------------------------------------------------------------------
  
  result <- sf::st_intersection(
    new_sub,
    old_sub
  ) |>
    sf::st_make_valid() |>
    dplyr::filter(
      base::as.character(
        sf::st_geometry_type(geometry)
      ) %in% base::c(
        "POLYGON",
        "MULTIPOLYGON"
      )
    ) |>
    dplyr::mutate(
      SITECODE = base::as.character(
        site_code
      ),
      HABITAT_CODE = base::as.character(
        hab_code
      ),
      PAIR = base::as.character(
        pair
      ),
      
      PASEKA = dplyr::case_when(
        BIOTOP_UPDATE %in% base::c(
          "LP",
          "X10"
        ) ~ 1L,
        BIOTOP_UPDATE %in% base::c(
          "X11",
          "X12A",
          "X12B"
        ) &
          ROK_AKT_UPDATE %in% 2007:2012 ~ 1L,
        TRUE ~ 0L
      ),
      
      AREA_INTERSECTION_M2 = units::drop_units(
        sf::st_area(geometry)
      ),
      
      PLO_BIO_M2_INTERSECTION =
        AREA_INTERSECTION_M2 *
        STEJ_PR_ORIG / 100 *
        STEJ_PR_UPDATE / 100,
      
      HOLINA = dplyr::case_when(
        PASEKA == 1L &
          PLO_BIO_M2_INTERSECTION > 10000 ~ 1L,
        TRUE ~ 0L
      )
    ) |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      PAIR,
      REGION_ID,
      DATUM_NEW,
      DATUM_OLD,
      SEGMENT_ID_OLD,
      SEGMENT_ID_NEW,
      BIOTOP_ORIG,
      BIOTOP_UPDATE,
      ROK_AKT_UPDATE,
      PASEKA,
      HOLINA,
      PLO_BIO_M2_INTERSECTION,
      geometry
    )
  
  if (base::nrow(result) == 0) {
    return(NULL)
  }
  
  result
}
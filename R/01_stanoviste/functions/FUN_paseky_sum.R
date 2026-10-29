# Paseky - sumarizace pro jednu kombinaci site x habitat.
#
# POCET_SEGMENTU_PASEKY se pocita z novejsi/aktualizacni vrstvy
# (SEGMENT_ID_NEW), nikoliv ze starsi vrstvy (SEGMENT_ID_OLD).

paseky_sum <- function(
    hab_code,
    site_code,
    paseky_detail
) {
  
  hab_code <- base::as.character(
    hab_code
  )
  
  site_code <- base::as.character(
    site_code
  )
  
  
  if (
    !base::substr(
      hab_code,
      1,
      1
    ) %in% base::c(
      "9",
      "L"
    )
  ) {
    return(
      tibble::tibble(
        SITECODE = site_code,
        HABITAT_CODE = hab_code,
        ROZLOHA_PASEKY = NA_real_,
        ROZLOHA_HOLINY = NA_real_,
        POCET_SEGMENTU_PASEKY = NA_integer_
      )
    )
  }
  
  
  if (
    base::is.null(paseky_detail) ||
    base::nrow(paseky_detail) == 0
  ) {
    return(
      tibble::tibble(
        SITECODE = site_code,
        HABITAT_CODE = hab_code,
        ROZLOHA_PASEKY = 0,
        ROZLOHA_HOLINY = 0,
        POCET_SEGMENTU_PASEKY = 0L
      )
    )
  }
  
  
  target <- if (
    base::inherits(
      paseky_detail,
      "sf"
    )
  ) {
    sf::st_drop_geometry(
      paseky_detail
    )
  } else {
    paseky_detail
  }
  
  
  required_cols <- base::c(
    "SITECODE",
    "HABITAT_CODE",
    "PASEKA",
    "HOLINA",
    "PLO_BIO_M2_INTERSECTION",
    "SEGMENT_ID_NEW"
  )
  
  missing_cols <- base::setdiff(
    required_cols,
    base::names(target)
  )
  
  if (base::length(missing_cols) > 0) {
    base::stop(
      "paseky_sum(): v `paseky_detail` chybi sloupce: ",
      base::paste(
        missing_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  target <- target |>
    dplyr::filter(
      base::as.character(SITECODE) == site_code,
      base::as.character(HABITAT_CODE) == hab_code
    )
  
  
  rozloha_paseky <- target |>
    dplyr::filter(
      PASEKA == 1L
    ) |>
    dplyr::pull(
      PLO_BIO_M2_INTERSECTION
    ) |>
    base::sum(
      na.rm = TRUE
    )
  
  rozloha_holiny <- target |>
    dplyr::filter(
      HOLINA == 1L
    ) |>
    dplyr::pull(
      PLO_BIO_M2_INTERSECTION
    ) |>
    base::sum(
      na.rm = TRUE
    )
  
  # DULEZITA OPRAVA:
  # pocitame unikatni segmenty NOVEJSI vrstvy.
  pocet_segmentu <- target |>
    dplyr::filter(
      PASEKA == 1L
    ) |>
    dplyr::pull(
      SEGMENT_ID_NEW
    ) |>
    dplyr::n_distinct(
      na.rm = TRUE
    )
  
  
  tibble::tibble(
    SITECODE = site_code,
    HABITAT_CODE = hab_code,
    ROZLOHA_PASEKY = base::as.numeric(
      rozloha_paseky / 10000
    ),
    ROZLOHA_HOLINY = base::as.numeric(
      rozloha_holiny / 10000
    ),
    POCET_SEGMENTU_PASEKY = base::as.integer(
      pocet_segmentu
    )
  )
}

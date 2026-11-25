# FUN_paseky_select_pairs.R
#
# Paseky - priprava kandidatnich dvojic VMB.
#
# Tato funkce pouze pripravi mozne dvojice:
#   - REGION_ID z VMB0 -> VMB2_VMB0
#   - REGION_ID z VMB2 -> VMB1_VMB2
#   - REGION_ID z VMB0 -> VMB1_VMB0
#
# Skutecna chronologie DATUM_NEW > DATUM_OLD a definitivni vyber paru se
# provedou az ve stanoviste_paseky(), po realnem prostorovem pruniku.
#
# Priorita:
#   1. VMB2 -> VMB0
#   2. VMB1 -> VMB2
#   3. VMB1 -> VMB0
#
# Vystup:
#   kandidatni radky REGION_ID x PAIR. Sloupce DATUM_OLD / DATUM_NEW jsou
#   zachovany jako NA pouze kvuli kompatibilite s existujicim rozhranim
#   stanoviste_paseky(); skutecna data se odvozuji az z prostoroveho pruniku.

paseky_select_pairs <- function(
    vmb1_meta,
    vmb2_meta,
    vmb0_meta,
    region_id_col = "REGION_ID",
    date_col = "DATUM"
) {
  
  # `vmb1_meta` a `date_col` zustavaji v signatuře kvuli zpetne kompatibilite.
  # Pro pripravu kandidatu je rozhodujici pouze REGION_ID novejsi vrstvy.
  
  get_regions <- function(
    x,
    version
  ) {
    
    if (base::inherits(x, "sf")) {
      x <- sf::st_drop_geometry(x)
    }
    
    if (!region_id_col %in% base::names(x)) {
      base::stop(
        "paseky_select_pairs(): v metadatech ",
        version,
        " chybi sloupec `",
        region_id_col,
        "`.",
        call. = FALSE
      )
    }
    
    x |>
      dplyr::transmute(
        REGION_ID = base::as.character(
          .data[[region_id_col]]
        )
      ) |>
      dplyr::filter(
        !base::is.na(REGION_ID),
        base::nzchar(REGION_ID)
      ) |>
      dplyr::distinct()
  }
  
  
  regions_vmb2 <- get_regions(
    vmb2_meta,
    "VMB2"
  )
  
  regions_vmb0 <- get_regions(
    vmb0_meta,
    "VMB0"
  )
  
  
  candidates <- dplyr::bind_rows(
    
    regions_vmb0 |>
      dplyr::mutate(
        PAIR = "VMB2_VMB0",
        DATUM_OLD = base::as.Date(NA),
        DATUM_NEW = base::as.Date(NA),
        PAIR_PRIORITY = 1L
      ),
    
    regions_vmb2 |>
      dplyr::mutate(
        PAIR = "VMB1_VMB2",
        DATUM_OLD = base::as.Date(NA),
        DATUM_NEW = base::as.Date(NA),
        PAIR_PRIORITY = 2L
      ),
    
    regions_vmb0 |>
      dplyr::mutate(
        PAIR = "VMB1_VMB0",
        DATUM_OLD = base::as.Date(NA),
        DATUM_NEW = base::as.Date(NA),
        PAIR_PRIORITY = 3L
      )
  ) |>
    dplyr::arrange(
      REGION_ID,
      PAIR_PRIORITY
    )
  
  candidates
}
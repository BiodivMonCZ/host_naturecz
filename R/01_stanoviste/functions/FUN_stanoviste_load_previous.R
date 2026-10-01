# FUN_stanoviste_load_previous.R
#
# Nacteni historickeho wide outputu stareho i noveho workflow.
#
# Podporuje:
#   - lokalni CSV vytvorene pomoci write.csv2()/stanoviste_export()
#   - raw GitHub URL
#   - beznou GitHub "blob" URL
#
# Vystup:
#   tibble s jednim radkem pro SITECODE x HABITAT_CODE.

stanoviste_load_previous <- function(
    source,
    encoding = "Windows-1250"
) {
  
  if (
    base::length(source) != 1 ||
    base::is.na(source) ||
    !base::nzchar(base::as.character(source))
  ) {
    base::stop(
      "stanoviste_load_previous(): `source` musi byt jedna ne-prazdna cesta nebo URL.",
      call. = FALSE
    )
  }
  
  source <- base::as.character(source)
  
  # GitHub blob URL -> raw URL
  source_read <- base::sub(
    "^https://github\\.com/([^/]+)/([^/]+)/blob/([^/]+)/(.+)$",
    "https://raw.githubusercontent.com/\\1/\\2/\\3/\\4",
    source
  )
  
  previous <- readr::read_csv2(
    source_read,
    locale = readr::locale(
      encoding = encoding
    ),
    show_col_types = FALSE,
    progress = FALSE
  )
  
  required_cols <- base::c(
    "SITECODE",
    "HABITAT_CODE",
    "ROZLOHA",
    "KVALITA"
  )
  
  missing_cols <- base::setdiff(
    required_cols,
    base::names(previous)
  )
  
  if (base::length(missing_cols) > 0) {
    base::stop(
      "stanoviste_load_previous(): historicky soubor nema povinne sloupce: ",
      base::paste(
        missing_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  previous <- previous |>
    dplyr::mutate(
      SITECODE = base::as.character(SITECODE),
      HABITAT_CODE = base::as.character(HABITAT_CODE),
      HABITAT_CODE = dplyr::case_when(
        HABITAT_CODE == "91" ~ "91E0",
        HABITAT_CODE == "9.10E+01" ~ "91E0",
        HABITAT_CODE == "9,10E+01" ~ "91E0",
        TRUE ~ HABITAT_CODE
      ),
      # Zachovani puvodni logiky n2k_stanoviste_srovnani.R
      ROZLOHA = tidyr::replace_na(
        base::as.numeric(ROZLOHA),
        0
      )
    )
  
  if ("NAZEV" %in% base::names(previous)) {
    previous <- previous |>
      dplyr::mutate(
        NAZEV = stringr::str_replace_all(
          NAZEV,
          "–|—",
          "-"
        )
      )
  }
  
  date_cols <- base::intersect(
    base::c(
      "DATE_MIN",
      "DATE_MAX",
      "DATE_MEAN",
      "DATE_MEDIAN"
    ),
    base::names(previous)
  )
  
  if (base::length(date_cols) > 0) {
    previous <- previous |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(date_cols),
          ~ base::as.Date(.x)
        )
      )
  }
  
  previous <- previous |>
    dplyr::distinct()
  
  duplicate_keys <- previous |>
    dplyr::count(
      SITECODE,
      HABITAT_CODE,
      name = "n"
    ) |>
    dplyr::filter(
      n > 1
    )
  
  if (base::nrow(duplicate_keys) > 0) {
    base::stop(
      "stanoviste_load_previous(): historicky soubor obsahuje vice ruznych radku ",
      "pro stejnou kombinaci SITECODE x HABITAT_CODE.",
      call. = FALSE
    )
  }
  
  previous
}

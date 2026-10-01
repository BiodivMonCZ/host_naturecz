# paseky_export.R
#
# Zapis vysledku samostatneho workflow pasek.
#
# Vytvari:
#   - datumovou archivni kopii,
#   - stabilni paseky_results_latest.csv pro hlavni workflow,
#   - log batch vypoctu,
#   - tabulku vybranych VMB paru podle REGION_ID.
#
# Export se standardne neprovede, pokud batch obsahuje chyby.

paseky_export <- function(
    batch_result,
    output_dir,
    calculation_date = base::Sys.Date(),
    overwrite_archive = FALSE,
    allow_partial = FALSE
) {

  if (!base::is.list(batch_result)) {
    base::stop(
      "paseky_export(): `batch_result` musi byt list z paseky_batch().",
      call. = FALSE
    )
  }

  required_items <- base::c(
    "results",
    "log",
    "selected_pairs"
  )

  missing_items <- base::setdiff(
    required_items,
    base::names(batch_result)
  )

  if (base::length(missing_items) > 0) {
    base::stop(
      "paseky_export(): v `batch_result` chybi: ",
      base::paste(
        missing_items,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  results <- batch_result$results
  log_tbl <- batch_result$log
  selected_pairs <- batch_result$selected_pairs

  if (!base::is.data.frame(results)) {
    base::stop(
      "paseky_export(): `batch_result$results` neni tabulka.",
      call. = FALSE
    )
  }

  n_error <- if (
    base::is.data.frame(log_tbl) &&
    "status" %in% base::names(log_tbl)
  ) {
    base::sum(
      log_tbl$status == "error",
      na.rm = TRUE
    )
  } else {
    0L
  }

  if (
    n_error > 0 &&
    !base::isTRUE(allow_partial)
  ) {
    base::stop(
      "paseky_export(): batch obsahuje ",
      n_error,
      " chyb. Export neuplne tabulky je zablokovan.",
      call. = FALSE
    )
  }

  required_result_cols <- base::c(
    "SITECODE",
    "HABITAT_CODE",
    "ROZLOHA_PASEKY",
    "ROZLOHA_HOLINY",
    "POCET_SEGMENTU_PASEKY"
  )

  missing_result_cols <- base::setdiff(
    required_result_cols,
    base::names(results)
  )

  if (base::length(missing_result_cols) > 0) {
    base::stop(
      "paseky_export(): ve vysledcich chybi: ",
      base::paste(
        missing_result_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  results_export <- results |>
    dplyr::select(
      dplyr::any_of(".target_id"),
      dplyr::all_of(
        required_result_cols
      )
    ) |>
    dplyr::mutate(
      TYP_CHU = "EVL",
      DATUM_VYPOCTU = base::as.Date(
        calculation_date
      )
    ) |>
    dplyr::relocate(
      TYP_CHU,
      SITECODE,
      HABITAT_CODE
    ) |>
    dplyr::select(
      -dplyr::any_of(".target_id")
    )

  duplicates <- results_export |>
    dplyr::count(
      SITECODE,
      HABITAT_CODE,
      name = "n"
    ) |>
    dplyr::filter(
      n > 1
    )

  if (base::nrow(duplicates) > 0) {
    base::stop(
      "paseky_export(): vysledky obsahuji duplicitni site x habitat.",
      call. = FALSE
    )
  }

  base::dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  date_tag <- base::format(
    base::as.Date(
      calculation_date
    ),
    "%Y%m%d"
  )

  archive_path <- base::file.path(
    output_dir,
    base::paste0(
      "paseky_results_",
      date_tag,
      ".csv"
    )
  )

  latest_path <- base::file.path(
    output_dir,
    "paseky_results_latest.csv"
  )

  log_path <- base::file.path(
    output_dir,
    base::paste0(
      "paseky_log_",
      date_tag,
      ".csv"
    )
  )

  pairs_path <- base::file.path(
    output_dir,
    base::paste0(
      "paseky_selected_pairs_",
      date_tag,
      ".csv"
    )
  )

  if (
    base::file.exists(archive_path) &&
    !base::isTRUE(overwrite_archive)
  ) {
    base::stop(
      "paseky_export(): archivni soubor uz existuje: ",
      archive_path,
      ". Pouzij `overwrite_archive = TRUE`, pokud jej chces nahradit.",
      call. = FALSE
    )
  }

  # Archiv
  readr::write_csv2(
    results_export,
    archive_path,
    na = ""
  )

  # Latest zapisujeme pres docasny soubor, aby po prerusenem zapisu
  # nezustal poskozeny produkcni vstup.
  latest_tmp <- base::paste0(
    latest_path,
    ".tmp"
  )

  readr::write_csv2(
    results_export,
    latest_tmp,
    na = ""
  )

  if (base::file.exists(latest_path)) {
    ok_remove <- base::file.remove(
      latest_path
    )

    if (!base::isTRUE(ok_remove)) {
      base::stop(
        "paseky_export(): nelze nahradit existujici latest soubor: ",
        latest_path,
        call. = FALSE
      )
    }
  }

  ok_rename <- base::file.rename(
    latest_tmp,
    latest_path
  )

  if (!base::isTRUE(ok_rename)) {
    base::stop(
      "paseky_export(): nelze prejmenovat docasny latest soubor.",
      call. = FALSE
    )
  }

  if (base::is.data.frame(log_tbl)) {
    readr::write_csv2(
      log_tbl,
      log_path,
      na = ""
    )
  }

  if (base::is.data.frame(selected_pairs)) {
    readr::write_csv2(
      selected_pairs,
      pairs_path,
      na = ""
    )
  }

  tibble::tibble(
    type = base::c(
      "archive",
      "latest",
      "log",
      "selected_pairs"
    ),
    path = base::c(
      archive_path,
      latest_path,
      log_path,
      pairs_path
    )
  )
}

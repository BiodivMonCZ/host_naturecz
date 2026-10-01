# TEST_stanoviste_trend_regression.R
#
# Izolovany regresni test trendove vetve bez GIS vstupu.
#
# Porovnava historicka raw obdobi:
#   previous: results_habitats_A1_20250222.csv
#   current:  results_habitats_24_20250806.csv
#
# Defaultne testuje EVL Hradiste (CZ0414127).
# Pro cely dataset nastav site_code <- NULL.
#
# Vystupy:
#   trend_detail.csv
#   n2k_new.csv
#   comparison_old_vs_new.csv
#   summary.csv

# =============================================================================
# 0. Nastaveni
# =============================================================================

site_code <- "CZ0414127"

previous_file <- base::file.path(
  "Outputs", "Data", "stanoviste",
  "results_habitats_A1_20250222.csv"
)

current_file <- base::file.path(
  "Outputs", "Data", "stanoviste",
  "results_habitats_24_20250806.csv"
)

old_export_files <- base::file.path(
  "Outputs", "Data", "stanoviste",
  base::paste0(
    "n2k_stanoviste_2025_20260220_UTF-8_part",
    1:3,
    ".csv"
  )
)

out_dir <- base::file.path(
  "Outputs", "Data", "stanoviste",
  "trend_regression_test"
)

base::dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# =============================================================================
# 1. Funkce
# =============================================================================

base::source(
  "R/01_stanoviste/functions/FUN_stanoviste_hodnoceni.R"
)

base::source(
  "R/01_stanoviste/functions/FUN_stanoviste_trend.R"
)

base::source(
  "R/01_stanoviste/io/stanoviste_export.R"
)

# =============================================================================
# 2. Minimalni vstupy pro hodnoceni a export
# =============================================================================

limits <- readr::read_csv(
  "Data/Input/limity_stanoviste.csv",
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
)

minimisize <- readr::read_csv(
  "Data/Input/minimisize.csv",
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
) |>
  dplyr::group_by(
    HABITAT
  ) |>
  dplyr::reframe(
    MINIMISIZE = base::max(
      MINIMISIZE,
      na.rm = TRUE
    ) / 10000
  ) |>
  dplyr::ungroup()

n2k_oop <- readr::read_csv2(
  "Data/Input/n2k_oop_25.csv",
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
) |>
  dplyr::transmute(
    SITECODE = base::as.character(
      sitecode
    ),
    oop = base::gsub(
      ";",
      ",",
      oop
    )
  )

rp_code <- readr::read_csv2(
  "Data/Input/n2k_rp_25.csv",
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
) |>
  dplyr::transmute(
    SITECODE = base::as.character(
      sitecode
    ),
    pracoviste = base::gsub(
      ",",
      "",
      pracoviste
    )
  )

site_context <- n2k_oop |>
  dplyr::full_join(
    rp_code,
    by = "SITECODE"
  ) |>
  dplyr::distinct(
    SITECODE,
    .keep_all = TRUE
  )

sdo_ii_sites <- readr::read_csv2(
  "Data/Input/SDO_II_predmetolokality.csv",
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
)

indicator_lookup <- readr::read_csv(
  "Data/Input/cis_indikatory_popis.csv",
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
)

habitat_lookup <- readr::read_csv2(
  "Data/Input/cis_habitat.csv",
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    KOD_HABITAT = base::as.character(
      KOD_HABITAT
    ),
    KOD_HABITAT = dplyr::case_when(
      KOD_HABITAT == "91" ~ "91E0",
      KOD_HABITAT == "6210" &
        PRIORITA == "p" ~ "6210p",
      TRUE ~ KOD_HABITAT
    )
  ) |>
  dplyr::select(
    KOD_HABITAT,
    NAZEV_HABITAT
  ) |>
  dplyr::distinct()

# =============================================================================
# 3. Historicke raw vysledky
# =============================================================================

previous_raw <- readr::read_csv2(
  previous_file,
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
)

current_raw <- readr::read_csv2(
  current_file,
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  show_col_types = FALSE
)

if (!base::is.null(site_code)) {
  
  previous_raw <- previous_raw |>
    dplyr::filter(
      SITECODE == site_code
    )
  
  current_raw <- current_raw |>
    dplyr::filter(
      SITECODE == site_code
    )
}

base::stopifnot(
  base::nrow(previous_raw) > 0,
  base::nrow(current_raw) > 0
)

# =============================================================================
# 4. Hodnoceni obou obdobi stejnou aktualni funkci
# =============================================================================

previous_eval <- stanoviste_hodnoceni(
  results = previous_raw,
  limits = limits,
  minimisize = minimisize,
  site_context = site_context,
  sdo_ii_sites = sdo_ii_sites
)

current_eval <- stanoviste_hodnoceni(
  results = current_raw,
  limits = limits,
  minimisize = minimisize,
  site_context = site_context,
  sdo_ii_sites = sdo_ii_sites
)

# =============================================================================
# 5. Trend
# =============================================================================

trend_test <- stanoviste_trend(
  current = current_eval,
  previous = previous_eval,
  tolerance = 0.05,
  return_detail = TRUE
)

final_results <- trend_test$result

readr::write_csv2(
  trend_test$detail,
  base::file.path(
    out_dir,
    "trend_detail.csv"
  ),
  na = "NA"
)

# =============================================================================
# 6. Systemovy export noveho workflow
# =============================================================================

stanoviste_export(
  raw_results = current_raw,
  evaluated_results = current_eval,
  final_results = final_results,
  batch_log = NULL,
  indicator_lookup = indicator_lookup,
  habitat_lookup = habitat_lookup,
  site_context = site_context,
  output_dir = out_dir,
  period_id = "trend_test",
  assessment_year = 2025L,
  date_stamp = base::as.Date(
    "2026-02-20"
  ),
  write_raw = FALSE,
  write_raw_long = FALSE,
  write_evaluated = FALSE,
  write_final = FALSE,
  write_final_long = FALSE,
  write_batch_log = FALSE,
  write_legacy_system = TRUE,
  write_legacy_xlsx = FALSE,
  overwrite = TRUE
)

new_export_path <- base::file.path(
  out_dir,
  "n2k_stanoviste_2025_20260220_Windows-1250.csv"
)

new_export <- readr::read_csv(
  new_export_path,
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  col_types = readr::cols(
    .default = readr::col_character()
  ),
  show_col_types = FALSE
)

readr::write_csv2(
  new_export,
  base::file.path(
    out_dir,
    "n2k_new.csv"
  ),
  na = "NA"
)

# =============================================================================
# 7. Referencni export stareho workflow
# =============================================================================

old_export <- base::lapply(
  old_export_files,
  FUN = function(path) {
    readr::read_csv2(
      path,
      locale = readr::locale(
        encoding = "UTF-8"
      ),
      col_types = readr::cols(
        .default = readr::col_character()
      ),
      show_col_types = FALSE
    )
  }
) |>
  dplyr::bind_rows()

if (!base::is.null(site_code)) {
  old_export <- old_export |>
    dplyr::filter(
      kod_chu == site_code
    )
}

# =============================================================================
# 8. Porovnani old vs new
# =============================================================================

compare_cols <- base::c(
  "kod_chu",
  "feature_code",
  "parametr_nazev"
)

old_cmp <- old_export |>
  dplyr::transmute(
    kod_chu = base::as.character(
      kod_chu
    ),
    feature_code = base::as.character(
      feature_code
    ),
    parametr_nazev = base::as.character(
      parametr_nazev
    ),
    old_hodnota = base::as.character(
      parametr_hodnota
    ),
    old_limit = base::as.character(
      parametr_limit
    ),
    old_stav = base::as.character(
      stav
    ),
    old_trend = base::as.character(
      trend
    )
  )

new_cmp <- new_export |>
  dplyr::transmute(
    kod_chu = base::as.character(
      kod_chu
    ),
    feature_code = base::as.character(
      feature_code
    ),
    parametr_nazev = base::as.character(
      parametr_nazev
    ),
    new_hodnota = base::as.character(
      parametr_hodnota
    ),
    new_limit = base::as.character(
      parametr_limit
    ),
    new_stav = base::as.character(
      stav
    ),
    new_trend = base::as.character(
      trend
    )
  )

comparison <- old_cmp |>
  dplyr::full_join(
    new_cmp,
    by = compare_cols
  ) |>
  dplyr::mutate(
    same_stav = dplyr::coalesce(
      old_stav == new_stav,
      base::is.na(old_stav) &
        base::is.na(new_stav)
    ),
    same_trend = dplyr::coalesce(
      old_trend == new_trend,
      base::is.na(old_trend) &
        base::is.na(new_trend)
    )
  ) |>
  dplyr::arrange(
    feature_code,
    parametr_nazev
  )

readr::write_csv2(
  comparison,
  base::file.path(
    out_dir,
    "comparison_old_vs_new.csv"
  ),
  na = "NA"
)

summary <- tibble::tibble(
  metric = base::c(
    "old_rows",
    "new_rows",
    "same_stav",
    "different_stav",
    "same_trend",
    "different_trend"
  ),
  value = base::c(
    base::nrow(old_cmp),
    base::nrow(new_cmp),
    base::sum(
      comparison$same_stav,
      na.rm = TRUE
    ),
    base::sum(
      !comparison$same_stav,
      na.rm = TRUE
    ),
    base::sum(
      comparison$same_trend,
      na.rm = TRUE
    ),
    base::sum(
      !comparison$same_trend,
      na.rm = TRUE
    )
  )
)

readr::write_csv2(
  summary,
  base::file.path(
    out_dir,
    "summary.csv"
  )
)

base::print(
  summary,
  n = Inf
)

base::message(
  "\nRozdily trendu:"
)

base::print(
  comparison |>
    dplyr::filter(
      !same_trend
    ) |>
    dplyr::select(
      kod_chu,
      feature_code,
      parametr_nazev,
      old_trend,
      new_trend
    ),
  n = Inf
)

base::message(
  "\nHotovo. Vystupy jsou v: ",
  out_dir
)

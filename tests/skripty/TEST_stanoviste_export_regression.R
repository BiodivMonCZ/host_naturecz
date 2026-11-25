# TEST_stanoviste_export_regression.R
#
# Izolovany regresni test systemoveho N2K exportu bez GIS vstupu.
#
# Overuje:
#   1) export presne 11 systemovych indikatoru,
#   2) pomocne indikatory maji stav = 8 (nehodnocen),
#   3) pokud trend nebyl pocitan, zustava trend = NA,
#   4) pomocne raw parametry bez systemoveho ind_id se do N2K CSV nedostanou.
#
# Test nepouziva produkcni data a zapisuje pouze do docasneho adresare.

# =============================================================================
# 0. Funkce
# =============================================================================

base::source(
  "R/01_stanoviste/io/stanoviste_export.R"
)

# =============================================================================
# 1. Minimalni synteticky raw vysledek
# =============================================================================

raw_results <- tibble::tibble(
  SITECODE = "CZ_TEST",
  NAZEV = "Testovaci EVL",
  HABITAT_CODE = "9130",
  ROZLOHA = 100,
  KVALITA = 2.1,
  TYPICKE_DRUHY = 2.3,
  MRTVE_DREVO = 1.2,
  KALAMITA_POLOM = 0.4,
  RED_LIST = 7,
  INVASIVE = 12,
  EXPANSIVE = 18,
  MINIMIAREAL = 85,
  MINIMIAREAL_JADRA = 3,
  MINIMIAREAL_HODNOTA = 40,
  MOZAIKA_VNEJSI = 25,
  MOZAIKA_VNITRNI = 15,
  MOZAIKA_FIN = 25,
  DATE_MIN = base::as.Date("2020-01-01"),
  DATE_MAX = base::as.Date("2025-12-31")
)

# =============================================================================
# 2. Minimalni vyhodnoceni
# =============================================================================

evaluated_results <- raw_results |>
  dplyr::mutate(
    ROZLOHA_LIMIT = 90,
    ROZLOHA_ZDROJ = "TEST",
    ROZLOHA_STAV = "dobrý",
    KVALITA_LIMIT = 2,
    KVALITA_ZDROJ = "TEST",
    KVALITA_STAV = "zhoršený",
    CELKOVE_HODNOCENI = "zhoršený"
  )

# Trend zamerne neni pocitan.
final_results <- evaluated_results

# =============================================================================
# 3. Minimalni ciselniky
# =============================================================================

indicator_lookup <- tibble::tribble(
  ~ind_r,                 ~ind_popis,                    ~ind_id,
  "CELKOVE_HODNOCENI",    "celkové hodnocení",          10,
  "ROZLOHA",              "rozloha",                    128,
  "KVALITA",              "kvalita",                    129,
  "TYPICKE_DRUHY",        "typické druhy",              132,
  "MINIMIAREAL",          "minimiareál",                133,
  "MOZAIKA_VNITRNI",      "mozaikovitost",              134,
  "MOZAIKA_VNEJSI",       "okrajový efekt",             135,
  "RED_LIST",             "druhy červeného seznamu",    136,
  "INVASIVE",             "přítomost invazních druhů",  137,
  "EXPANSIVE",            "přítomost expanzivních druhů", 138,
  "MRTVE_DREVO",          "mrtvé dřevo",                162,
  "MINIMIAREAL_JADRA",    "minimiareál (počet jader)",  NA_real_,
  "MINIMIAREAL_HODNOTA",  "minimiareál (hodnota)",      NA_real_,
  "MOZAIKA_FIN",          "mozaika final",              NA_real_,
  "KALAMITA_POLOM",       "mrtvé dřevo (potenciál)",    NA_real_
)

habitat_lookup <- tibble::tibble(
  KOD_HABITAT = "9130",
  NAZEV_HABITAT = "Bučiny asociace Asperulo-Fagetum"
)

site_context <- tibble::tibble(
  SITECODE = "CZ_TEST",
  oop = "TEST OOP",
  pracoviste = "TEST pracoviste"
)

# =============================================================================
# 4. Export do docasneho adresare
# =============================================================================

out_dir <- base::file.path(
  base::tempdir(),
  "stanoviste_export_regression"
)

if (base::dir.exists(out_dir)) {
  base::unlink(
    out_dir,
    recursive = TRUE,
    force = TRUE
  )
}

base::dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

stanoviste_export(
  raw_results = raw_results,
  evaluated_results = evaluated_results,
  final_results = final_results,
  batch_log = NULL,
  indicator_lookup = indicator_lookup,
  habitat_lookup = habitat_lookup,
  site_context = site_context,
  output_dir = out_dir,
  period_id = "test",
  assessment_year = 2026L,
  date_stamp = base::as.Date("2026-09-24"),
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

system_path <- base::file.path(
  out_dir,
  "n2k_stanoviste_2026_20260924_Windows-1250.csv"
)

if (!base::file.exists(system_path)) {
  base::stop(
    "TEST FAILED: systemovy N2K export nebyl vytvoren.",
    call. = FALSE
  )
}

system_export <- readr::read_csv(
  system_path,
  locale = readr::locale(
    encoding = "Windows-1250"
  ),
  col_types = readr::cols(
    .default = readr::col_character()
  ),
  show_col_types = FALSE
)

# =============================================================================
# 5. Kontroly
# =============================================================================

expected_codes <- base::c(
  "10",
  "128",
  "129",
  "132",
  "133",
  "134",
  "135",
  "136",
  "137",
  "138",
  "162"
)

actual_codes <- base::sort(
  base::unique(
    system_export$parametr_nazev
  )
)

expected_codes_sorted <- base::sort(
  expected_codes
)

if (!base::identical(
  actual_codes,
  expected_codes_sorted
)) {
  base::stop(
    "TEST FAILED: sada systemovych indikatoru nesouhlasi.\n",
    "Ocekavano: ",
    base::paste(expected_codes_sorted, collapse = ", "),
    "\nNalezeno: ",
    base::paste(actual_codes, collapse = ", "),
    call. = FALSE
  )
}

if (base::nrow(system_export) != 11L) {
  base::stop(
    "TEST FAILED: ocekavano 11 radku, nalezeno ",
    base::nrow(system_export),
    ".",
    call. = FALSE
  )
}

auxiliary_codes <- base::c(
  "132",
  "133",
  "134",
  "135",
  "136",
  "137",
  "138",
  "162"
)

auxiliary_rows <- system_export |>
  dplyr::filter(
    parametr_nazev %in% auxiliary_codes
  )

if (
  base::nrow(auxiliary_rows) !=
    base::length(auxiliary_codes) ||
  !base::all(auxiliary_rows$stav == "8")
) {
  base::stop(
    "TEST FAILED: pomocne indikatory nemaji vsechny stav = 8.",
    call. = FALSE
  )
}

if (!base::all(base::is.na(system_export$trend))) {
  base::stop(
    "TEST FAILED: pri neprovedenem trendu musi byt vsechny hodnoty trend = NA.",
    call. = FALSE
  )
}

forbidden_codes_present <- system_export |>
  dplyr::filter(
    parametr_nazev %in% base::c(
      "MINIMIAREAL_JADRA",
      "MINIMIAREAL_HODNOTA",
      "MOZAIKA_FIN",
      "KALAMITA_POLOM"
    )
  )

if (base::nrow(forbidden_codes_present) > 0) {
  base::stop(
    "TEST FAILED: do systemoveho exportu pronikly parametry bez ind_id.",
    call. = FALSE
  )
}

key_states <- system_export |>
  dplyr::filter(
    parametr_nazev %in% base::c(
      "10",
      "128",
      "129"
    )
  ) |>
  dplyr::select(
    parametr_nazev,
    stav
  ) |>
  dplyr::arrange(
    parametr_nazev
  )

expected_key_states <- tibble::tribble(
  ~parametr_nazev, ~stav,
  "10",            "12",
  "128",           "11",
  "129",           "12"
) |>
  dplyr::arrange(
    parametr_nazev
  )

if (!base::identical(
  key_states,
  expected_key_states
)) {
  base::stop(
    "TEST FAILED: stav klicovych indikatoru 10/128/129 nesouhlasi.",
    call. = FALSE
  )
}

# =============================================================================
# 6. Vysledek
# =============================================================================

base::message(
  "\nTEST PASSED: stanoviste_export()"
)

base::message(
  "  - 11/11 systemovych indikatoru pritomno"
)

base::message(
  "  - pomocne indikatory 132-138 a 162 maji stav = 8"
)

base::message(
  "  - nepocitany trend zustava NA"
)

base::message(
  "  - parametry bez ind_id nejsou v systemovem N2K CSV"
)

base::print(
  system_export |>
    dplyr::select(
      feature_code,
      parametr_nazev,
      stav,
      trend
    ) |>
    dplyr::arrange(
      base::as.numeric(
        parametr_nazev
      )
    ),
  n = base::Inf
)

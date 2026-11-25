# FUN_stanoviste_trend.R
#
# Porovnani dvou VYHODNOCENYCH obdobi.
#
# Funkce je agnosticka vuci puvodu dat:
#   - obe obdobi mohou byt z noveho workflow,
#   - previous muze byt stary output, pokud byl nejdrive znovu vyhodnocen
#     pomoci stanoviste_hodnoceni().
#
# Zachovana logika n2k_stanoviste_srovnani.R:
#
# ROZLOHA:
#   +/- 5 % = stabilni
#   pokles > 5 % = zhorsujici se
#   rust   > 5 % = zlepsujici se
#
# KVALITA:
#   +/- 5 % = stabilni
#   rust   > 5 % = zhorsujici se
#   pokles > 5 % = zlepsujici se
#
# CELKOVE_HODNOCENI:
#   puvodni systemovy export trend celkoveho hodnoceni nevyplnoval,
#   proto zde zustava vzdy NA.
#
# U ostatnich indikatoru puvodni skript neurcoval smer zmeny.
# Shoda (resp. numericka zmena v toleranci) je proto "stabilni".
# Jina zmena, chybejici hodnota nebo neurcitelny smer zustava NA,
# stejne jako v puvodnim n2k_stanoviste_srovnani.R.

stanoviste_trend <- function(
    current,
    previous,
    tolerance = 0.05,
    indicators = base::c(
      "ROZLOHA",
      "KVALITA",
      "TYPICKE_DRUHY",
      "MINIMIAREAL",
      "MINIMIAREAL_JADRA",
      "MINIMIAREAL_HODNOTA",
      "MOZAIKA_VNEJSI",
      "MOZAIKA_VNITRNI",
      "MOZAIKA_FIN",
      "RED_LIST",
      "INVASIVE",
      "EXPANSIVE",
      "MRTVE_DREVO",
      "KALAMITA_POLOM",
      "RED_LIST_SPECIES",
      "INVASIVE_LIST",
      "EXPANSIVE_LIST",
      "CELKOVE_HODNOCENI"
    ),
    return_detail = FALSE
) {
  
  if (!base::is.data.frame(current)) {
    base::stop(
      "stanoviste_trend(): `current` musi byt data.frame/tibble.",
      call. = FALSE
    )
  }
  
  if (!base::is.data.frame(previous)) {
    base::stop(
      "stanoviste_trend(): `previous` musi byt data.frame/tibble.",
      call. = FALSE
    )
  }
  
  if (
    base::length(tolerance) != 1 ||
    base::is.na(tolerance) ||
    !base::is.numeric(tolerance) ||
    tolerance < 0
  ) {
    base::stop(
      "stanoviste_trend(): `tolerance` musi byt jedno nezaporne cislo.",
      call. = FALSE
    )
  }
  
  key_cols <- base::c(
    "SITECODE",
    "HABITAT_CODE"
  )
  
  check_input <- function(
    x,
    object_name
  ) {
    
    missing_keys <- base::setdiff(
      key_cols,
      base::names(x)
    )
    
    if (base::length(missing_keys) > 0) {
      base::stop(
        "stanoviste_trend(): v `",
        object_name,
        "` chybi klicove sloupce: ",
        base::paste(
          missing_keys,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    duplicate_keys <- x |>
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
        "stanoviste_trend(): `",
        object_name,
        "` nema unikatni SITECODE x HABITAT_CODE.",
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  check_input(
    current,
    "current"
  )
  
  check_input(
    previous,
    "previous"
  )
  
  normalize_keys <- function(x) {
    
    x |>
      dplyr::mutate(
        SITECODE = base::as.character(
          SITECODE
        ),
        HABITAT_CODE = base::as.character(
          HABITAT_CODE
        ),
        HABITAT_CODE = dplyr::case_when(
          HABITAT_CODE == "91" ~ "91E0",
          HABITAT_CODE == "9.10E+01" ~ "91E0",
          HABITAT_CODE == "9,10E+01" ~ "91E0",
          TRUE ~ HABITAT_CODE
        )
      )
  }
  
  current <- normalize_keys(
    current
  )
  
  previous <- normalize_keys(
    previous
  )
  
  indicators_common <- indicators[
    indicators %in% base::names(current) &
      indicators %in% base::names(previous)
  ]
  
  if (base::length(indicators_common) == 0) {
    base::stop(
      "stanoviste_trend(): current a previous nemaji zadny spolecny sledovany indikator.",
      call. = FALSE
    )
  }
  
  current_long <- current |>
    dplyr::select(
      dplyr::all_of(key_cols),
      dplyr::all_of(indicators_common)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        indicators_common
      ),
      names_to = "parametr_nazev",
      values_to = "current_value",
      values_transform = base::list(
        current_value = base::as.character
      )
    )
  
  previous_long <- previous |>
    dplyr::select(
      dplyr::all_of(key_cols),
      dplyr::all_of(indicators_common)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        indicators_common
      ),
      names_to = "parametr_nazev",
      values_to = "previous_value",
      values_transform = base::list(
        previous_value = base::as.character
      )
    )
  
  detail <- current_long |>
    dplyr::full_join(
      previous_long,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE",
        "parametr_nazev"
      )
    ) |>
    dplyr::mutate(
      current_num = base::suppressWarnings(
        base::as.numeric(
          current_value
        )
      ),
      previous_num = base::suppressWarnings(
        base::as.numeric(
          previous_value
        )
      ),
      trend = dplyr::case_when(
        
        base::is.na(current_value) |
          base::is.na(previous_value) ~
          NA_character_,
        
        # Stary systemovy export trend CELKOVE_HODNOCENI nevyplnoval.
        # Kontrola musi byt pred testem shody, jinak by shodna kategorie
        # dostala chybne trend = "stabilní".
        parametr_nazev ==
          "CELKOVE_HODNOCENI" ~
          NA_character_,
        
        current_value ==
          previous_value ~
          "stabilní",
        
        !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num <=
          previous_num *
          (1 + tolerance) &
          current_num >=
          previous_num *
          (1 - tolerance) ~
          "stabilní",
        
        parametr_nazev == "KVALITA" &
          !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num >
          previous_num *
          (1 + tolerance) ~
          "zhoršující se",
        
        parametr_nazev == "KVALITA" &
          !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num <
          previous_num *
          (1 - tolerance) ~
          "zlepšující se",
        
        parametr_nazev == "ROZLOHA" &
          !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num <
          previous_num *
          (1 - tolerance) ~
          "zhoršující se",
        
        parametr_nazev == "ROZLOHA" &
          !base::is.na(current_num) &
          !base::is.na(previous_num) &
          current_num >
          previous_num *
          (1 + tolerance) ~
          "zlepšující se",
        
        TRUE ~
          NA_character_
      )
    )
  
  trend_wide <- detail |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      parametr_nazev,
      trend
    ) |>
    tidyr::pivot_wider(
      names_from = parametr_nazev,
      values_from = trend,
      names_prefix = "TREND_"
    )
  
  result <- current |>
    dplyr::left_join(
      trend_wide,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE"
      )
    )
  
  if (base::isTRUE(return_detail)) {
    return(
      base::list(
        result = result,
        detail = detail |>
          dplyr::select(
            SITECODE,
            HABITAT_CODE,
            parametr_nazev,
            previous_value,
            current_value,
            trend
          )
      )
    )
  }
  
  result
}

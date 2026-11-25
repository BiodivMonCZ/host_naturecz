# stanoviste_input_validator.R
#
# Validace vstupu hlavniho workflow hodnoceni stanovist.
#
# Pri uspechu vraci invisible(TRUE).

stanoviste_validate_inputs <- function(
    hab_code,
    site_code,
    data,
    tables
) {
  
  # ---------------------------------------------------------------------------
  # Pomocne validacni funkce
  # ---------------------------------------------------------------------------
  
  require_sf <- function(
    x,
    object_name
  ) {
    
    if (!base::inherits(x, "sf")) {
      base::stop(
        "stanoviste_validate_inputs(): `",
        object_name,
        "` musi byt sf objekt.",
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  
  require_table <- function(
    x,
    object_name
  ) {
    
    if (!base::is.data.frame(x)) {
      base::stop(
        "stanoviste_validate_inputs(): `",
        object_name,
        "` musi byt data.frame/tibble nebo sf.",
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  
  require_cols <- function(
    x,
    cols,
    object_name
  ) {
    
    missing_cols <- base::setdiff(
      cols,
      base::names(x)
    )
    
    if (base::length(missing_cols) > 0) {
      base::stop(
        "stanoviste_validate_inputs(): v `",
        object_name,
        "` chybi sloupce: ",
        base::paste(
          missing_cols,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  # ---------------------------------------------------------------------------
  # 1. Zakladni argumenty
  # ---------------------------------------------------------------------------
  
  if (
    base::length(hab_code) != 1 ||
    base::is.na(hab_code) ||
    !base::nzchar(
      base::as.character(hab_code)
    )
  ) {
    base::stop(
      "stanoviste_validate_inputs(): `hab_code` musi byt jedna neprazdna hodnota.",
      call. = FALSE
    )
  }
  
  if (
    base::length(site_code) != 1 ||
    base::is.na(site_code) ||
    !base::nzchar(
      base::as.character(site_code)
    )
  ) {
    base::stop(
      "stanoviste_validate_inputs(): `site_code` musi byt jedna neprazdna hodnota.",
      call. = FALSE
    )
  }
  
  hab_code <- base::as.character(
    hab_code
  )
  
  site_code <- base::as.character(
    site_code
  )
  
  if (!base::is.list(data)) {
    base::stop(
      "stanoviste_validate_inputs(): `data` musi byt pojmenovany list.",
      call. = FALSE
    )
  }
  
  if (!base::is.list(tables)) {
    base::stop(
      "stanoviste_validate_inputs(): `tables` musi byt pojmenovany list.",
      call. = FALSE
    )
  }
  
  if (base::is.null(base::names(data))) {
    base::stop(
      "stanoviste_validate_inputs(): `data` musi byt pojmenovany list.",
      call. = FALSE
    )
  }
  
  if (base::is.null(base::names(tables))) {
    base::stop(
      "stanoviste_validate_inputs(): `tables` musi byt pojmenovany list.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 2. Dostupnost potrebnych funkci
  # ---------------------------------------------------------------------------
  
  required_functions <- base::c(
    "stanoviste_klic",
    "stanoviste_druhy",
    "stanoviste_prostor"
  )
  
  missing_functions <- required_functions[
    !base::vapply(
      required_functions,
      FUN = function(x) {
        base::exists(
          x,
          mode = "function",
          inherits = TRUE
        )
      },
      FUN.VALUE = base::logical(1)
    )
  ]
  
  if (base::length(missing_functions) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): nejsou nacteny potrebne funkce: ",
      base::paste(
        missing_functions,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 3. Struktura data a tables
  # ---------------------------------------------------------------------------
  
  required_data <- base::c(
    "site",
    "vmb",
    "czechia_line"
  )
  
  required_tables <- base::c(
    "habitat_areas",
    "minimisize",
    "red_list_species",
    "invasive_species",
    "expansive_species",
    "paseky"
  )
  
  missing_data <- base::setdiff(
    required_data,
    base::names(data)
  )
  
  missing_tables <- base::setdiff(
    required_tables,
    base::names(tables)
  )
  
  if (base::length(missing_data) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): v `data` chybi: ",
      base::paste(
        missing_data,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (base::length(missing_tables) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): v `tables` chybi: ",
      base::paste(
        missing_tables,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  null_data <- required_data[
    base::vapply(
      data[required_data],
      base::is.null,
      FUN.VALUE = base::logical(1)
    )
  ]
  
  null_tables <- required_tables[
    base::vapply(
      tables[required_tables],
      base::is.null,
      FUN.VALUE = base::logical(1)
    )
  ]
  
  if (base::length(null_data) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): tyto polozky v `data` jsou NULL: ",
      base::paste(
        null_data,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (base::length(null_tables) > 0) {
    base::stop(
      "stanoviste_validate_inputs(): tyto polozky v `tables` jsou NULL: ",
      base::paste(
        null_tables,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4. Prostorove vstupy
  # ---------------------------------------------------------------------------
  
  require_sf(
    data$site,
    "data$site"
  )
  
  require_sf(
    data$vmb,
    "data$vmb"
  )
  
  if (
    !base::inherits(
      data$czechia_line,
      "sf"
    ) &&
    !base::inherits(
      data$czechia_line,
      "sfc"
    )
  ) {
    base::stop(
      "stanoviste_validate_inputs(): `data$czechia_line` musi byt sf nebo sfc objekt.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 5. Site
  # ---------------------------------------------------------------------------
  
  require_cols(
    data$site,
    base::c(
      "SITECODE",
      "NAZEV",
      "SHAPE_AREA"
    ),
    "data$site"
  )
  
  if (
    !site_code %in%
    base::as.character(
      data$site$SITECODE
    )
  ) {
    base::stop(
      "stanoviste_validate_inputs(): site `",
      site_code,
      "` nebyla nalezena v `data$site`.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 6. Aktualni VMB - KLIC / DRUHY / PROSTOR
  # ---------------------------------------------------------------------------
  
  require_cols(
    data$vmb,
    base::c(
      "OBJECTID",
      "HABITAT",
      "BIOTOP",
      "STEJ_PR",
      "DG",
      "RB",
      "TD",
      "SF",
      "KVALITA",
      "MD",
      "DATUM",
      "ROK_AKT.y",
      "FSB_EVAL",
      "BIOTOP_SEZ"
    ),
    "data$vmb"
  )
  
  # ---------------------------------------------------------------------------
  # 7. Lookup tabulky
  # ---------------------------------------------------------------------------
  
  require_table(
    tables$habitat_areas,
    "tables$habitat_areas"
  )
  
  require_cols(
    tables$habitat_areas,
    base::c(
      "HABITAT",
      "TOTAL_AREA_ALL"
    ),
    "tables$habitat_areas"
  )
  
  require_table(
    tables$minimisize,
    "tables$minimisize"
  )
  
  require_cols(
    tables$minimisize,
    base::c(
      "HABITAT",
      "MINIMISIZE"
    ),
    "tables$minimisize"
  )
  
  # ---------------------------------------------------------------------------
  # 8. Druhova prostorova data
  # ---------------------------------------------------------------------------
  
  require_sf(
    tables$red_list_species,
    "tables$red_list_species"
  )
  
  require_sf(
    tables$invasive_species,
    "tables$invasive_species"
  )
  
  require_sf(
    tables$expansive_species,
    "tables$expansive_species"
  )
  
  require_cols(
    tables$red_list_species,
    "DRUH",
    "tables$red_list_species"
  )
  
  require_cols(
    tables$invasive_species,
    base::c(
      "DRUH",
      "DATUM_OD",
      "NEGATIVNI"
    ),
    "tables$invasive_species"
  )
  
  require_cols(
    tables$expansive_species,
    base::c(
      "DRUH",
      "DATUM_OD",
      "NEGATIVNI",
      "POKRYVN"
    ),
    "tables$expansive_species"
  )
  
  # ---------------------------------------------------------------------------
  # 9. Hotova tabulka pasek
  # ---------------------------------------------------------------------------
  
  require_table(
    tables$paseky,
    "tables$paseky"
  )
  
  require_cols(
    tables$paseky,
    base::c(
      "SITECODE",
      "HABITAT_CODE",
      "ROZLOHA_PASEKY",
      "ROZLOHA_HOLINY",
      "POCET_SEGMENTU_PASEKY"
    ),
    "tables$paseky"
  )
  
  paseky_target <- tables$paseky |>
    dplyr::filter(
      base::as.character(SITECODE) == site_code,
      base::as.character(HABITAT_CODE) == hab_code
    )
  
  if (base::nrow(paseky_target) == 0) {
    base::stop(
      "stanoviste_validate_inputs(): v `tables$paseky` chybi kombinace ",
      site_code,
      " x ",
      hab_code,
      ". Tabulka pasek je pravdepodobne neaktualni nebo nekompletni.",
      call. = FALSE
    )
  }
  
  if (base::nrow(paseky_target) > 1) {
    base::stop(
      "stanoviste_validate_inputs(): v `tables$paseky` je kombinace ",
      site_code,
      " x ",
      hab_code,
      " vicekrat.",
      call. = FALSE
    )
  }
  
  base::invisible(TRUE)
}

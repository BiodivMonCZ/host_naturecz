# stanoviste_load_inputs.R
#
# Vstupni adapter mezi existujici config vrstvou repozitare (R/00_config)
# a hlavnim workflow hodnoceni stanovist.
#
# Paseky se v hlavnim workflow NEPOCITAJI. Vstupuji jako hotova tabulka
# z Outputs/Data/stanoviste/paseky/paseky_results_latest.csv.
# Pokud stabilni latest soubor jeste neexistuje, lze docasne pouzit legacy
# tabulku vracenou pres load_vmb(vmb_x = 0).
#
# Starsi verze VMB (VMB1, VMB2) patri pouze do samostatneho workflow
# pro aktualizaci pasek a zde se vubec nenacitaji.
#
# Vystup:
#   list(
#     data = ...,
#     tables = ...,
#     manifest = ...,
#     config_env = ...   # pouze pri keep_config_env = TRUE
#   )

load_stanoviste_inputs <- function(
    repo_root = ".",
    typ_chu = "evl",
    paseky_file = NULL,
    config_env = NULL,
    keep_config_env = FALSE
) {

  repo_root <- base::normalizePath(
    repo_root,
    winslash = "/",
    mustWork = TRUE
  )

  old_wd <- base::getwd()

  base::on.exit(
    base::setwd(old_wd),
    add = TRUE
  )

  # Configy i load_vmb() pouzivaji relativni cesty vuci koreni repozitare.
  base::setwd(repo_root)

  typ_chu <- stanoviste_normalize_typ_chu(
    typ_chu
  )

  chu_spec <- stanoviste_get_chu_spec(
    typ_chu
  )

  # ---------------------------------------------------------------------------
  # 1. Source existujici config vrstvy
  # ---------------------------------------------------------------------------

  if (base::is.null(config_env)) {

    config_env <- base::new.env(
      parent = base::globalenv()
    )

    config_files <- base::c(
      "R/00_config/00_n2k_config.R",
      "R/00_config/02_n2k_data_druhy.R",
      "R/00_config/03_n2k_data_stanoviste.R",
      "R/00_config/load_vmb.R"
    )

    missing_config_files <- config_files[
      !base::file.exists(config_files)
    ]

    if (base::length(missing_config_files) > 0) {
      base::stop(
        "load_stanoviste_inputs(): chybi config soubory: ",
        base::paste(
          missing_config_files,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    base::invisible(
      base::lapply(
        config_files,
        FUN = function(file) {

          base::message(
            "Sourcing: ",
            file
          )

          base::sys.source(
            file,
            envir = config_env
          )
        }
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Pomocne funkce
  # ---------------------------------------------------------------------------

  resolve_object <- function(
      candidates,
      label
  ) {

    found <- candidates[
      base::vapply(
        candidates,
        FUN = function(x) {
          base::exists(
            x,
            envir = config_env,
            inherits = FALSE
          )
        },
        FUN.VALUE = base::logical(1)
      )
    ]

    if (base::length(found) == 0) {

      available <- base::ls(
        config_env,
        all.names = TRUE
      )

      base::stop(
        "load_stanoviste_inputs(): po nacteni configu se nepodarilo najit `",
        label,
        "`. Hledane nazvy: ",
        base::paste(
          candidates,
          collapse = ", "
        ),
        ".\nDostupne objekty v config prostredi:\n",
        base::paste(
          available,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    selected <- found[[1]]

    base::list(
      name = selected,
      value = base::get(
        selected,
        envir = config_env,
        inherits = FALSE
      )
    )
  }


  add_alias <- function(
      x,
      target,
      candidates,
      object_name,
      required = TRUE
  ) {

    if (target %in% base::names(x)) {
      return(x)
    }

    source <- candidates[
      candidates %in% base::names(x)
    ]

    if (base::length(source) == 0) {

      if (base::isTRUE(required)) {
        base::stop(
          "load_stanoviste_inputs(): v `",
          object_name,
          "` nelze vytvorit sloupec `",
          target,
          "`. Hledany alternativy: ",
          base::paste(
            candidates,
            collapse = ", "
          ),
          ".",
          call. = FALSE
        )
      }

      return(x)
    }

    x[[target]] <- x[[source[[1]]]]

    x
  }


  normalize_current_vmb <- function(x) {

    if (!base::inherits(x, "sf")) {
      base::stop(
        "load_stanoviste_inputs(): aktualni VMB musi byt sf objekt.",
        call. = FALSE
      )
    }

    # Po joinu v load_vmb() mohou zustat varianty OBJECTID.x / OBJECTID.y.
    x <- add_alias(
      x = x,
      target = "OBJECTID",
      candidates = base::c(
        "OBJECTID.y",
        "OBJECTID_Y",
        "OBJECTID.x",
        "OBJECTID_X",
        "OBJECTID_1"
      ),
      object_name = "aktualni VMB"
    )

    if ("HABITAT" %in% base::names(x)) {
      x$HABITAT <- base::as.character(
        x$HABITAT
      )
    }

    if ("BIOTOP" %in% base::names(x)) {
      x$BIOTOP <- base::as.character(
        x$BIOTOP
      )
    }

    if (
      "DATUM" %in% base::names(x) &&
      !base::inherits(
        x$DATUM,
        "Date"
      )
    ) {
      x$DATUM <- base::as.Date(
        x$DATUM
      )
    }

    x
  }


  normalize_paseky <- function(x) {

    if (!base::is.data.frame(x)) {
      base::stop(
        "load_stanoviste_inputs(): `paseky` z load_vmb(vmb_x = 0) ",
        "musi byt data.frame/tibble.",
        call. = FALSE
      )
    }

    # Aktualni vystup stareho pasekoveho workflow tyto nazvy uz pouziva.
    # Aliasovani je pouze pojistka pro starsi exporty.
    x <- add_alias(
      x = x,
      target = "SITECODE",
      candidates = base::c(
        "sitecode",
        "SITE_CODE"
      ),
      object_name = "paseky"
    )

    x <- add_alias(
      x = x,
      target = "HABITAT_CODE",
      candidates = base::c(
        "habitat_code",
        "HABITAT",
        "habitat"
      ),
      object_name = "paseky"
    )

    x <- add_alias(
      x = x,
      target = "ROZLOHA_PASEKY",
      candidates = base::c(
        "rozloha_paseky"
      ),
      object_name = "paseky"
    )

    x <- add_alias(
      x = x,
      target = "ROZLOHA_HOLINY",
      candidates = base::c(
        "rozloha_holiny"
      ),
      object_name = "paseky"
    )

    x <- add_alias(
      x = x,
      target = "POCET_SEGMENTU_PASEKY",
      candidates = base::c(
        "pocet_segmentu_paseky"
      ),
      object_name = "paseky"
    )

    x |>
      dplyr::mutate(
        SITECODE = base::as.character(
          SITECODE
        ),
        HABITAT_CODE = base::as.character(
          HABITAT_CODE
        ),
        ROZLOHA_PASEKY = base::as.numeric(
          ROZLOHA_PASEKY
        ),
        ROZLOHA_HOLINY = base::as.numeric(
          ROZLOHA_HOLINY
        ),
        POCET_SEGMENTU_PASEKY = base::as.numeric(
          POCET_SEGMENTU_PASEKY
        )
      )
  }

  # ---------------------------------------------------------------------------
  # 3. Objekty z existujicich configu
  # ---------------------------------------------------------------------------

  site_res <- resolve_object(
    candidates = chu_spec$site_candidates,
    label = base::paste0(
      "vrstva ",
      chu_spec$label
    )
  )

  # Novy workflow potrebuje puvodni limity v m2, vcetne vazby HABITAT-BIOTOP.
  # Objekt "minimisize" z configu je agregovany podle HABITAT a prepocteny na ha.
  minimisize_source <- base::file.path(
    "Data", "Input", "minimisize.csv"
  )

  minimisize_raw <- readr::read_csv(
    minimisize_source,
    locale = readr::locale(encoding = "Windows-1250"),
    col_types = readr::cols(
      HABITAT = readr::col_character(),
      BIOTOP = readr::col_character(),
      MINIMISIZE = readr::col_double(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  habitat_areas_res <- resolve_object(
    candidates = base::c(
      "habitat_areas_2022",
      "habitat_areas_a1"
    ),
    label = "tabulka celkovych ploch habitatu"
  )

  red_list_res <- resolve_object(
    candidates = "redlist_species",
    label = "data druhu cerveneho seznamu"
  )

  invasive_res <- resolve_object(
    candidates = "invasive_species",
    label = "data invaznich druhu"
  )

  expansive_res <- resolve_object(
    candidates = "expansive_species",
    label = "data expanznich druhu"
  )

  site_raw <- site_res$value

  # Hranice CR
  if (
    base::exists(
      "czechia_line",
      envir = config_env,
      inherits = FALSE
    )
  ) {

    czechia_line_name <- "czechia_line"

    czechia_line <- base::get(
      czechia_line_name,
      envir = config_env,
      inherits = FALSE
    )

  } else if (
    base::exists(
      "czechia",
      envir = config_env,
      inherits = FALSE
    )
  ) {

    czechia_line_name <- "czechia -> st_boundary(st_union())"

    czechia <- base::get(
      "czechia",
      envir = config_env,
      inherits = FALSE
    )

    czechia_line <- czechia |>
      sf::st_geometry() |>
      sf::st_union() |>
      sf::st_boundary()

  } else {

    base::stop(
      "load_stanoviste_inputs(): configy nevytvorily `czechia_line` ani `czechia`.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Aktualni VMB + hotova tabulka pasek
  # ---------------------------------------------------------------------------

  if (
    !base::exists(
      "load_vmb",
      envir = config_env,
      mode = "function",
      inherits = FALSE
    )
  ) {
    base::stop(
      "load_stanoviste_inputs(): po source `load_vmb.R` neni dostupna ",
      "funkce `load_vmb()`.",
      call. = FALSE
    )
  }

  load_vmb_fun <- base::get(
    "load_vmb",
    envir = config_env,
    inherits = FALSE
  )

  base::message(
    "Nacitam aktualni VMB pres load_vmb(vmb_x = 0)..."
  )

  vmb0 <- load_vmb_fun(
    vmb_x = 0
  )

  required_vmb0 <- "vmb_shp_sjtsk_akt"

  missing_vmb0 <- base::setdiff(
    required_vmb0,
    base::names(vmb0)
  )

  if (base::length(missing_vmb0) > 0) {
    base::stop(
      "load_stanoviste_inputs(): load_vmb(vmb_x = 0) nevratil: ",
      base::paste(
        missing_vmb0,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  vmb_current <- normalize_current_vmb(
    vmb0$vmb_shp_sjtsk_akt
  )

  site <- stanoviste_normalize_site(
    site = site_raw,
    typ_chu = typ_chu,
    target_crs = vmb_current
  )

  if (base::is.null(paseky_file)) {
    paseky_file <- base::file.path(
      repo_root,
      "Outputs",
      "Data",
      "stanoviste",
      typ_chu,
      "paseky",
      "paseky_results_latest.csv"
    )
  }

  legacy_latest <- base::file.path(
    repo_root,
    "Outputs",
    "Data",
    "stanoviste",
    "paseky",
    "paseky_results_latest.csv"
  )

  if (base::file.exists(paseky_file)) {

    base::message(
      "Nacitam paseky z: ",
      paseky_file
    )

    paseky_raw <- readr::read_csv2(
      paseky_file,
      show_col_types = FALSE
    )

    paseky_source <- paseky_file

  } else if (
    typ_chu == "evl" &&
    base::file.exists(legacy_latest)
  ) {

    base::warning(
      "load_stanoviste_inputs(): pouziva se puvodni umisteni EVL pasek: ",
      legacy_latest,
      call. = FALSE
    )

    paseky_raw <- readr::read_csv2(
      legacy_latest,
      show_col_types = FALSE
    )

    paseky_source <- legacy_latest

  } else if (
    typ_chu == "evl" &&
    "paseky" %in% base::names(vmb0) &&
    !base::is.null(vmb0$paseky)
  ) {

    base::warning(
      "load_stanoviste_inputs(): `paseky_results_latest.csv` nebyl nalezen. ",
      "Pouziva se legacy tabulka `load_vmb(vmb_x = 0)$paseky`.",
      call. = FALSE
    )

    paseky_raw <- vmb0$paseky
    paseky_source <- "load_vmb(vmb_x = 0)$paseky [legacy fallback]"

  } else {

    base::stop(
      "load_stanoviste_inputs(): nebyla nalezena hotova tabulka pasek pro ",
      "`typ_chu = ",
      typ_chu,
      "`: ",
      paseky_file,
      call. = FALSE
    )
  }

  paseky <- normalize_paseky(
    paseky_raw
  )

  if ("TYP_CHU" %in% base::names(paseky)) {

    paseky_types <- paseky$TYP_CHU |>
      base::as.character() |>
      base::toupper() |>
      stats::na.omit() |>
      base::unique()

    if (
      base::length(paseky_types) > 0 &&
      !base::all(
        paseky_types == base::toupper(
          typ_chu
        )
      )
    ) {
      base::stop(
        "load_stanoviste_inputs(): tabulka pasek patri k jinemu `typ_chu`.",
        call. = FALSE
      )
    }
  }

  duplicate_paseky <- paseky |>
    dplyr::count(
      SITECODE,
      HABITAT_CODE,
      name = "n"
    ) |>
    dplyr::filter(
      n > 1
    )

  if (base::nrow(duplicate_paseky) > 0) {
    base::stop(
      "load_stanoviste_inputs(): tabulka `paseky` obsahuje duplicitni ",
      "kombinace SITECODE x HABITAT_CODE.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 5. Vystupni struktura pro hlavni workflow
  # ---------------------------------------------------------------------------

  data <- base::list(
    typ_chu = typ_chu,
    habitat_col = chu_spec$habitat_col,
    site = site,
    vmb = vmb_current,
    czechia_line = czechia_line
  )

  tables <- base::list(
    habitat_areas = habitat_areas_res$value,
    minimisize = minimisize_raw,
    red_list_species = red_list_res$value,
    invasive_species = invasive_res$value,
    expansive_species = expansive_res$value,
    paseky = paseky
  )

  manifest <- dplyr::tibble(
    target = base::c(
      "data$site",
      "data$vmb",
      "data$czechia_line",
      "tables$habitat_areas",
      "tables$minimisize",
      "tables$red_list_species",
      "tables$invasive_species",
      "tables$expansive_species",
      "tables$paseky"
    ),
    source = base::c(
      base::paste0(
        "R/00_config: ",
        site_res$name
      ),
      "load_vmb(vmb_x = 0)$vmb_shp_sjtsk_akt",
      base::paste0(
        "R/00_config: ",
        czechia_line_name
      ),
      base::paste0(
        "R/00_config: ",
        habitat_areas_res$name
      ),
      minimisize_source,
      base::paste0(
        "R/00_config: ",
        red_list_res$name
      ),
      base::paste0(
        "R/00_config: ",
        invasive_res$name
      ),
      base::paste0(
        "R/00_config: ",
        expansive_res$name
      ),
      paseky_source
    )
  )

  result <- base::list(
    typ_chu = typ_chu,
    data = data,
    tables = tables,
    manifest = manifest
  )

  if (base::isTRUE(keep_config_env)) {
    result$config_env <- config_env
  }

  result
}

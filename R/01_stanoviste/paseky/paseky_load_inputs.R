# paseky_load_inputs.R
#
# Nacteni vstupu pro samostatny workflow pasek.
#
# Workflow zamerne pouziva vsechny tri generace VMB, ale pouze zde:
#   VMB1 = puvodni mapovani
#   VMB2 = Aktualizace 1
#   VMB0 = aktualni VMB
#
# Hlavni workflow hodnoceni stanovist tyto starsi vrstvy nenacita.
#
# Vystup:
#   list(
#     data = ...,
#     targets = ...,
#     manifest = ...,
#     config_env = ...   # volitelne
#   )

load_paseky_inputs <- function(
    repo_root = ".",
    targets = NULL,
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

  base::setwd(repo_root)

  # ---------------------------------------------------------------------------
  # 1. Config
  # ---------------------------------------------------------------------------

  if (base::is.null(config_env)) {

    config_env <- base::new.env(
      parent = base::globalenv()
    )

    config_files <- base::c(
      "R/00_config/00_n2k_config.R",
      "R/00_config/03_n2k_data_stanoviste.R",
      "R/00_config/load_vmb.R"
    )

    missing_files <- config_files[
      !base::file.exists(config_files)
    ]

    if (base::length(missing_files) > 0) {
      base::stop(
        "load_paseky_inputs(): chybi soubory: ",
        base::paste(
          missing_files,
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
      base::stop(
        "load_paseky_inputs(): nepodarilo se najit `",
        label,
        "`. Hledane objekty: ",
        base::paste(
          candidates,
          collapse = ", "
        ),
        ".",
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
          "load_paseky_inputs(): v `",
          object_name,
          "` nelze vytvorit `",
          target,
          "`. Alternativy: ",
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


  normalize_base <- function(
      x,
      object_name
  ) {

    if (!base::inherits(x, "sf")) {
      base::stop(
        "load_paseky_inputs(): `",
        object_name,
        "` musi byt sf.",
        call. = FALSE
      )
    }

    x <- add_alias(
      x,
      "REGION_ID",
      base::c(
        "REGION_ID.x",
        "REGION_ID_X",
        "REGION_ID.y",
        "REGION_ID_Y"
      ),
      object_name
    )

    x <- add_alias(
      x,
      "DATUM",
      base::c(
        "DATUM.x",
        "DATUM_X",
        "DATUM.y",
        "DATUM_Y"
      ),
      object_name
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


  normalize_update <- function(
      x,
      object_name
  ) {

    if (!base::inherits(x, "sf")) {
      base::stop(
        "load_paseky_inputs(): `",
        object_name,
        "` musi byt sf.",
        call. = FALSE
      )
    }

    # Pro zachovani stare pasekove logiky preferujeme atributy .x,
    # zejmena ROK_AKT.x, ktery pouzival puvodni n2k_paseky_spat.R.
    x <- add_alias(
      x,
      "REGION_ID",
      base::c(
        "REGION_ID.x",
        "REGION_ID_X",
        "REGION_ID.y",
        "REGION_ID_Y"
      ),
      object_name
    )

    x <- add_alias(
      x,
      "DATUM",
      base::c(
        "DATUM.x",
        "DATUM_X",
        "DATUM.y",
        "DATUM_Y"
      ),
      object_name
    )

    x <- add_alias(
      x,
      "ROK_AKT",
      base::c(
        "ROK_AKT.x",
        "ROK_AKT_X",
        "ROK_AKT.y",
        "ROK_AKT_Y"
      ),
      object_name
    )

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


  make_meta <- function(
      x,
      object_name
  ) {

    missing <- base::setdiff(
      base::c(
        "REGION_ID",
        "DATUM"
      ),
      base::names(x)
    )

    if (base::length(missing) > 0) {
      base::stop(
        "load_paseky_inputs(): z `",
        object_name,
        "` nelze vytvorit metadata; chybi: ",
        base::paste(
          missing,
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }

    x |>
      sf::st_drop_geometry() |>
      dplyr::transmute(
        REGION_ID = base::as.character(
          REGION_ID
        ),
        DATUM = base::as.Date(
          DATUM
        )
      ) |>
      dplyr::filter(
        !base::is.na(REGION_ID)
      ) |>
      dplyr::distinct()
  }


  normalize_targets <- function(x) {

    if (!base::is.data.frame(x)) {
      base::stop(
        "load_paseky_inputs(): `targets` musi byt data.frame/tibble.",
        call. = FALSE
      )
    }

    site_candidates <- base::c(
      "SITECODE",
      "site_code",
      "sitecode",
      "SITE_CODE",
      "kod_chu"
    )

    habitat_candidates <- base::c(
      "HABITAT_CODE",
      "habitat_code",
      "HABITAT",
      "habitat",
      "sdf_code"
    )

    site_col <- site_candidates[
      site_candidates %in% base::names(x)
    ][1]

    habitat_col <- habitat_candidates[
      habitat_candidates %in% base::names(x)
    ][1]

    if (
      base::length(site_col) == 0 ||
      base::is.na(site_col)
    ) {
      base::stop(
        "load_paseky_inputs(): v target tabulce nebyl nalezen sloupec ",
        "site. Dostupne sloupce: ",
        base::paste(
          base::names(x),
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    if (
      base::length(habitat_col) == 0 ||
      base::is.na(habitat_col)
    ) {
      base::stop(
        "load_paseky_inputs(): v target tabulce nebyl nalezen sloupec ",
        "habitatu. Dostupne sloupce: ",
        base::paste(
          base::names(x),
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    x |>
      dplyr::transmute(
        SITECODE = base::as.character(
          .data[[site_col]]
        ),
        HABITAT_CODE = base::as.character(
          .data[[habitat_col]]
        )
      ) |>
      dplyr::filter(
        !base::is.na(SITECODE),
        !base::is.na(HABITAT_CODE),
        base::nzchar(SITECODE),
        base::nzchar(HABITAT_CODE)
      ) |>
      dplyr::distinct()
  }

  # ---------------------------------------------------------------------------
  # 3. Site + targets
  # ---------------------------------------------------------------------------

  site_res <- resolve_object(
    base::c(
      "evl",
      "evl_sjtsk"
    ),
    "vrstva EVL"
  )

  site <- site_res$value

  if (!base::inherits(site, "sf")) {
    base::stop(
      "load_paseky_inputs(): vrstva EVL musi byt sf.",
      call. = FALSE
    )
  }

  if (base::is.null(targets)) {

    targets_res <- resolve_object(
      base::c(
        "sites_habitats",
        "sites_habitats_evl"
      ),
      "kombinace site x habitat"
    )

    targets_source_name <- targets_res$name
    targets <- targets_res$value

  } else {

    targets_source_name <- "argument targets"
  }

  targets <- normalize_targets(
    targets
  )

  # ---------------------------------------------------------------------------
  # 4. VMB1 / VMB2 / VMB0
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
      "load_paseky_inputs(): neni dostupna funkce `load_vmb()`.",
      call. = FALSE
    )
  }

  load_vmb_fun <- base::get(
    "load_vmb",
    envir = config_env,
    inherits = FALSE
  )

  base::message(
    "Nacitam VMB1..."
  )
  vmb1 <- load_vmb_fun(
    vmb_x = 1
  )

  base::message(
    "Nacitam VMB2..."
  )
  vmb2 <- load_vmb_fun(
    vmb_x = 2
  )

  base::message(
    "Nacitam VMB0..."
  )
  vmb0 <- load_vmb_fun(
    vmb_x = 0
  )

  required_vmb1 <- "vmb_shp_sjtsk_orig"

  required_vmb2 <- base::c(
    "vmb_shp_sjtsk_a1",
    "vmb_pb_x_a1"
  )

  required_vmb0 <- base::c(
    "vmb_shp_sjtsk_akt",
    "vmb_pb_x_akt"
  )

  check_vmb <- function(
      x,
      required,
      label
  ) {

    missing <- base::setdiff(
      required,
      base::names(x)
    )

    if (base::length(missing) > 0) {
      base::stop(
        "load_paseky_inputs(): ",
        label,
        " nevratil: ",
        base::paste(
          missing,
          collapse = ", "
        ),
        ".",
        call. = FALSE
      )
    }
  }

  check_vmb(
    vmb1,
    required_vmb1,
    "load_vmb(vmb_x = 1)"
  )

  check_vmb(
    vmb2,
    required_vmb2,
    "load_vmb(vmb_x = 2)"
  )

  check_vmb(
    vmb0,
    required_vmb0,
    "load_vmb(vmb_x = 0)"
  )

  vmb1_base <- normalize_base(
    vmb1$vmb_shp_sjtsk_orig,
    "VMB1 base"
  )

  vmb2_base <- normalize_base(
    vmb2$vmb_shp_sjtsk_a1,
    "VMB2 base"
  )

  vmb0_base <- normalize_base(
    vmb0$vmb_shp_sjtsk_akt,
    "VMB0 base"
  )

  vmb2_update <- normalize_update(
    vmb2$vmb_pb_x_a1,
    "VMB2 update"
  )

  vmb0_update <- normalize_update(
    vmb0$vmb_pb_x_akt,
    "VMB0 update"
  )

  # Metadata pro vyber dvojice se odvozuji z kompletnich base vrstev.
  vmb1_meta <- make_meta(
    vmb1_base,
    "VMB1 base"
  )

  vmb2_meta <- make_meta(
    vmb2_base,
    "VMB2 base"
  )

  vmb0_meta <- make_meta(
    vmb0_base,
    "VMB0 base"
  )

  # ---------------------------------------------------------------------------
  # 5. Vystup
  # ---------------------------------------------------------------------------

  data <- base::list(
    site = site,
    vmb1_base = vmb1_base,
    vmb2_base = vmb2_base,
    vmb2_update = vmb2_update,
    vmb0_update = vmb0_update,
    vmb1_meta = vmb1_meta,
    vmb2_meta = vmb2_meta,
    vmb0_meta = vmb0_meta
  )

  manifest <- dplyr::tibble(
    target = base::c(
      "data$site",
      "targets",
      "data$vmb1_base",
      "data$vmb2_base",
      "data$vmb2_update",
      "data$vmb0_update",
      "data$vmb1_meta",
      "data$vmb2_meta",
      "data$vmb0_meta"
    ),
    source = base::c(
      base::paste0(
        "R/00_config: ",
        site_res$name
      ),
      targets_source_name,
      "load_vmb(vmb_x = 1)$vmb_shp_sjtsk_orig",
      "load_vmb(vmb_x = 2)$vmb_shp_sjtsk_a1",
      "load_vmb(vmb_x = 2)$vmb_pb_x_a1",
      "load_vmb(vmb_x = 0)$vmb_pb_x_akt",
      "VMB1 base: REGION_ID + DATUM",
      "VMB2 base: REGION_ID + DATUM",
      "VMB0 base: REGION_ID + DATUM"
    )
  )

  result <- base::list(
    data = data,
    targets = targets,
    manifest = manifest
  )

  if (base::isTRUE(keep_config_env)) {
    result$config_env <- config_env
  }

  result
}

# stanoviste_chu_adapter.R
#
# Spolecny adapter typu hodnoceneho uzemi pro hlavni workflow stanovist
# i samostatny workflow pasek.
#
# Podporovane hodnoty:
#   evl
#   mzchu
#   aktokr
#
# Vnitrni rozhrani workflow zustava pro vsechny typy stejne:
#   SITECODE
#   NAZEV
#   SHAPE_AREA
#   geometry

stanoviste_normalize_typ_chu <- function(
    typ_chu
) {

  if (
    base::length(typ_chu) != 1 ||
    base::is.na(typ_chu) ||
    !base::nzchar(
      base::as.character(
        typ_chu
      )
    )
  ) {
    base::stop(
      "`typ_chu` musi byt jedna z hodnot: evl, mzchu, aktokr.",
      call. = FALSE
    )
  }

  typ_chu <- base::tolower(
    base::trimws(
      base::as.character(
        typ_chu
      )
    )
  )

  allowed <- base::c(
    "evl",
    "mzchu",
    "aktokr"
  )

  if (!typ_chu %in% allowed) {
    base::stop(
      "Neplatna hodnota `typ_chu`: ",
      typ_chu,
      ". Povolene hodnoty: ",
      base::paste(
        allowed,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  typ_chu
}


stanoviste_get_chu_spec <- function(
    typ_chu
) {

  typ_chu <- stanoviste_normalize_typ_chu(
    typ_chu
  )

  specs <- base::list(
    evl = base::list(
      site_candidates = base::c(
        "evl",
        "evl_sjtsk"
      ),
      target_candidates = base::c(
        "sites_habitats",
        "sites_habitats_evl"
      ),
      habitat_col = "HABITAT",
      label = "EVL"
    ),
    mzchu = base::list(
      site_candidates = "mzchu",
      target_candidates = base::c(
        "sites_habitats_mzchu",
        "sites_habitats_mzchu_test"
      ),
      habitat_col = "BIOTOP",
      label = "MZCHU"
    ),
    aktokr = base::list(
      site_candidates = base::c(
        "akt_okrsky",
        "akt_okrsek"
      ),
      target_candidates = base::c(
        "sites_habitats_aktokr",
        "sites_habitats_okrsky"
      ),
      habitat_col = "BIOTOP",
      label = "AktOkr"
    )
  )

  specs[[typ_chu]]
}


stanoviste_normalize_site <- function(
    site,
    typ_chu,
    target_crs = NULL
) {

  typ_chu <- stanoviste_normalize_typ_chu(
    typ_chu
  )

  chu_spec <- stanoviste_get_chu_spec(
    typ_chu
  )

  if (!base::inherits(site, "sf")) {
    base::stop(
      "Polygonova vrstva pro `typ_chu = ",
      typ_chu,
      "` musi byt sf objekt.",
      call. = FALSE
    )
  }

  add_alias <- function(
      x,
      target,
      candidates,
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
          "Ve vrstve `",
          typ_chu,
          "` nelze vytvorit sloupec `",
          target,
          "`. Hledane alternativy: ",
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

  site <- add_alias(
    x = site,
    target = "SITECODE",
    candidates = base::c(
      "sitecode",
      "site_code",
      "SITE_CODE",
      "KOD",
      "kod",
      "kod_chu",
      "REGION_ID",
      "region_id"
    )
  )

  site <- add_alias(
    x = site,
    target = "NAZEV",
    candidates = base::c(
      "nazev",
      "NAZEV_CHU",
      "nazev_chu",
      "site_name",
      "NAME",
      "name"
    ),
    required = FALSE
  )

  site$SITECODE <- base::as.character(
    site$SITECODE
  )

  if (!"NAZEV" %in% base::names(site)) {
    site$NAZEV <- base::paste(
      chu_spec$label,
      site$SITECODE
    )
  } else {
    site$NAZEV <- base::as.character(
      site$NAZEV
    )

    missing_name <-
      base::is.na(site$NAZEV) |
      !base::nzchar(
        base::trimws(
          site$NAZEV
        )
      )

    site$NAZEV[missing_name] <- base::paste(
      chu_spec$label,
      site$SITECODE[missing_name]
    )
  }

  if (
    base::any(
      base::is.na(
        site$SITECODE
      )
    ) ||
    base::any(
      !base::nzchar(
        base::trimws(
          site$SITECODE
        )
      )
    )
  ) {
    base::stop(
      "Vrstva pro `typ_chu = ",
      typ_chu,
      "` obsahuje prazdny nebo NA `SITECODE`.",
      call. = FALSE
    )
  }

  site <- sf::st_make_valid(
    site
  )

  if (!base::is.null(target_crs)) {

    if (base::is.na(sf::st_crs(site))) {
      base::stop(
        "Vrstva pro `typ_chu = ",
        typ_chu,
        "` nema definovane CRS.",
        call. = FALSE
      )
    }

    if (
      !base::isTRUE(
        sf::st_crs(site) ==
          sf::st_crs(target_crs)
      )
    ) {
      site <- sf::st_transform(
        site,
        sf::st_crs(target_crs)
      )
    }
  }

  if (sf::st_is_longlat(site)) {
    base::stop(
      "Vrstva pro `typ_chu = ",
      typ_chu,
      "` musi byt v projektovanem CRS.",
      call. = FALSE
    )
  }

  site$SHAPE_AREA <- base::as.numeric(
    units::drop_units(
      sf::st_area(
        site
      )
    )
  )

  site$TYP_CHU <- base::toupper(
    typ_chu
  )

  site
}

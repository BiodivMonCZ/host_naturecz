# stanoviste_hodnoceni_inputs.R
#
# Adapter pro vstupy stanoviste_hodnoceni() a stanoviste_export().
#
# Vraci:
#   limits
#   minimisize
#   site_context
#   sdo_ii_sites
#   indicator_lookup
#   habitat_lookup
#
# `site_context` je sjednocen na:
#   SITECODE
#   oop
#   pracoviste
#
# Ocekava se, ze load_stanoviste_inputs(..., keep_config_env = TRUE)
# vratil take `config_env`.

stanoviste_get_hodnoceni_inputs <- function(
    stanoviste_inputs,
    repo_root = "."
) {
  
  if (
    !base::is.list(stanoviste_inputs) ||
    base::is.null(stanoviste_inputs$tables)
  ) {
    base::stop(
      "stanoviste_get_hodnoceni_inputs(): neplatny objekt `stanoviste_inputs`.",
      call. = FALSE
    )
  }
  
  if (
    base::is.null(stanoviste_inputs$tables$minimisize)
  ) {
    base::stop(
      "stanoviste_get_hodnoceni_inputs(): chybi `tables$minimisize`.",
      call. = FALSE
    )
  }
  
  config_env <- stanoviste_inputs$config_env
  
  if (
    base::is.null(config_env) ||
    !base::is.environment(config_env)
  ) {
    base::stop(
      "stanoviste_get_hodnoceni_inputs(): chybi `config_env`. ",
      "Nacti vstupy pomoci load_stanoviste_inputs(..., keep_config_env = TRUE).",
      call. = FALSE
    )
  }
  
  
  get_first_object <- function(
    candidates,
    required = TRUE
  ) {
    
    for (candidate in candidates) {
      
      if (
        base::exists(
          candidate,
          envir = config_env,
          inherits = TRUE
        )
      ) {
        return(
          base::get(
            candidate,
            envir = config_env,
            inherits = TRUE
          )
        )
      }
    }
    
    if (base::isTRUE(required)) {
      base::stop(
        "stanoviste_get_hodnoceni_inputs(): nenalezen zadny z objektu: ",
        base::paste(
          candidates,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    NULL
  }
  
  
  drop_geometry <- function(x) {
    
    if (base::inherits(x, "sf")) {
      return(
        sf::st_drop_geometry(x)
      )
    }
    
    x
  }
  
  
  normalize_site_key <- function(x) {
    
    x <- drop_geometry(x)
    
    if ("SITECODE" %in% base::names(x)) {
      x$SITECODE <- base::as.character(
        x$SITECODE
      )
      
      return(x)
    }
    
    if ("kod_chu" %in% base::names(x)) {
      
      x <- x |>
        dplyr::rename(
          SITECODE = kod_chu
        )
      
      x$SITECODE <- base::as.character(
        x$SITECODE
      )
      
      return(x)
    }
    
    NULL
  }
  
  
  # ===========================================================================
  # 1. Limity
  # ===========================================================================
  
  limits <- get_first_object(
    base::c(
      "limity_stan",
      "limity_stanoviste"
    ),
    required = FALSE
  )
  
  if (base::is.null(limits)) {
    
    limits_path <- base::file.path(
      repo_root,
      "Data",
      "Input",
      "limity_stanoviste.csv"
    )
    
    if (!base::file.exists(limits_path)) {
      base::stop(
        "stanoviste_get_hodnoceni_inputs(): limity nejsou v configu ",
        "a chybi soubor: ",
        limits_path,
        call. = FALSE
      )
    }
    
    limits <- readr::read_csv(
      limits_path,
      locale = readr::locale(
        encoding = "Windows-1250"
      ),
      show_col_types = FALSE
    )
  }
  
  
  # ===========================================================================
  # 2. Site context pro hodnoceni i export
  # ===========================================================================
  
  site_data <- normalize_site_key(
    stanoviste_inputs$data$site
  )
  
  if (base::is.null(site_data)) {
    base::stop(
      "stanoviste_get_hodnoceni_inputs(): site vrstva nema SITECODE.",
      call. = FALSE
    )
  }
  
  site_context <- site_data |>
    dplyr::select(
      SITECODE,
      dplyr::any_of(
        base::c(
          "oop",
          "pracoviste"
        )
      )
    ) |>
    dplyr::distinct(
      SITECODE,
      .keep_all = TRUE
    )
  
  if (!"oop" %in% base::names(site_context)) {
    site_context$oop <- NA_character_
  }
  
  if (!"pracoviste" %in% base::names(site_context)) {
    site_context$pracoviste <- NA_character_
  }
  
  
  add_context_columns <- function(
    target,
    source
  ) {
    
    if (base::is.null(source)) {
      return(target)
    }
    
    source <- normalize_site_key(
      source
    )
    
    if (base::is.null(source)) {
      return(target)
    }
    
    available_cols <- base::intersect(
      base::c(
        "oop",
        "pracoviste"
      ),
      base::names(source)
    )
    
    if (base::length(available_cols) == 0) {
      return(target)
    }
    
    source <- source |>
      dplyr::select(
        SITECODE,
        dplyr::all_of(
          available_cols
        )
      ) |>
      dplyr::distinct(
        SITECODE,
        .keep_all = TRUE
      )
    
    for (column in available_cols) {
      
      tmp_name <- base::paste0(
        column,
        "_new"
      )
      
      source_tmp <- source |>
        dplyr::select(
          SITECODE,
          dplyr::all_of(
            column
          )
        )
      
      base::names(
        source_tmp
      )[
        base::names(
          source_tmp
        ) == column
      ] <- tmp_name
      
      target <- target |>
        dplyr::left_join(
          source_tmp,
          by = "SITECODE"
        )
      
      target[[column]] <- dplyr::coalesce(
        target[[column]],
        target[[tmp_name]]
      )
      
      target[[tmp_name]] <- NULL
    }
    
    target
  }
  
  
  n2k_oop <- get_first_object(
    "n2k_oop",
    required = FALSE
  )
  
  rp_code <- get_first_object(
    "rp_code",
    required = FALSE
  )
  
  evl_sdo <- get_first_object(
    "evl_sdo",
    required = FALSE
  )
  
  site_context <- add_context_columns(
    site_context,
    n2k_oop
  )
  
  site_context <- add_context_columns(
    site_context,
    rp_code
  )
  
  site_context <- add_context_columns(
    site_context,
    evl_sdo
  )
  
  
  if (
    base::all(
      base::is.na(
        site_context$oop
      )
    )
  ) {
    base::stop(
      "stanoviste_get_hodnoceni_inputs(): nepodarilo se doplnit `oop`.",
      call. = FALSE
    )
  }
  
  
  # ===========================================================================
  # 3. SDO II
  # ===========================================================================
  
  sdo_ii_sites <- get_first_object(
    base::c(
      "sdo_II_sites",
      "sdo_ii_sites"
    ),
    required = TRUE
  )
  
  
  # ===========================================================================
  # 4. Ciselnik indikatoru pro systemovy export
  # ===========================================================================
  
  indicator_lookup <- get_first_object(
    base::c(
      "indikatory_id",
      "indicator_lookup"
    ),
    required = FALSE
  )
  
  
  # ===========================================================================
  # 5. Ciselnik habitatu pro systemovy export
  # ===========================================================================
  
  habitat_lookup <- get_first_object(
    base::c(
      "cis_habitat",
      "habitat_lookup"
    ),
    required = FALSE
  )
  
  
  # ===========================================================================
  # 6. Vystup
  # ===========================================================================
  
  base::list(
    limits = limits,
    minimisize = stanoviste_inputs$tables$minimisize,
    site_context = site_context,
    sdo_ii_sites = sdo_ii_sites,
    indicator_lookup = indicator_lookup,
    habitat_lookup = habitat_lookup
  )
}

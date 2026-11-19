# FUN_stanoviste_paseky.R
#
# Pasekove parametry pro jednu kombinaci site x habitat.
#
# Metadata se pouziji pouze k priprave chronologicky platnych kandidatu.
# Definitivni par se vybira az pro konkretni:
#
#   SITECODE x HABITAT_CODE x REGION_ID
#
# a pouze z paru, pro ktere skutecne vznikl prostorovy vysledek.
#
# Tim se reprodukuje logika priorot VMB kombinaci:
#   1. VMB2_VMB0
#   2. VMB1_VMB2
#   3. VMB1_VMB0
#
# Pro batch workflow lze dodat:
#   selected_pairs - predem spocitane KANDIDATNI pary z paseky_select_pairs()
#   site_regions   - predem ziskane REGION_ID zasahujici site
#
# return_selected_pairs = TRUE vrati vedle agregovaneho vysledku take
# skutecne zvoleny par pro kazdy REGION_ID.

stanoviste_paseky <- function(
    hab_code,
    site_code,
    site,
    vmb1_base,
    vmb2_base,
    vmb2_update,
    vmb0_update,
    vmb1_meta = vmb1_base,
    vmb2_meta = vmb2_update,
    vmb0_meta = vmb0_update,
    selected_pairs = NULL,
    site_regions = NULL,
    return_selected_pairs = FALSE,
    habitat_col = "HABITAT",
    biotop_col = "BIOTOP",
    share_col = "STEJ_PR",
    segment_id_col = "SEGMENT_ID",
    region_id_col = "REGION_ID",
    date_col = "DATUM",
    update_year_col = "ROK_AKT"
) {
  
  hab_code <- base::as.character(
    hab_code
  )
  
  site_code <- base::as.character(
    site_code
  )
  
  
  empty_selected_pairs <- function() {
    tibble::tibble(
      SITECODE = character(),
      HABITAT_CODE = character(),
      REGION_ID = character(),
      PAIR = character(),
      DATUM_OLD = base::as.Date(character()),
      DATUM_NEW = base::as.Date(character()),
      PAIR_PRIORITY = integer()
    )
  }
  
  
  return_result <- function(
    result,
    selected
  ) {
    
    if (base::isTRUE(return_selected_pairs)) {
      return(
        base::list(
          result = result,
          selected_pairs = selected
        )
      )
    }
    
    result
  }
  
  
  # ---------------------------------------------------------------------------
  # 0. Nelesni habitat
  # ---------------------------------------------------------------------------
  
  if (
    !base::substr(
      hab_code,
      1,
      1
    ) %in% base::c(
      "9",
      "L"
    )
  ) {
    
    result <- paseky_sum(
      hab_code = hab_code,
      site_code = site_code,
      paseky_detail = NULL
    )
    
    return(
      return_result(
        result,
        empty_selected_pairs()
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 1. Chronologicky platne kandidatni pary
  # ---------------------------------------------------------------------------
  
  if (base::is.null(selected_pairs)) {
    
    selected_pairs <- paseky_select_pairs(
      vmb1_meta = vmb1_meta,
      vmb2_meta = vmb2_meta,
      vmb0_meta = vmb0_meta,
      region_id_col = region_id_col,
      date_col = date_col
    )
  }
  
  
  required_pair_cols <- base::c(
    "REGION_ID",
    "PAIR",
    "DATUM_OLD",
    "DATUM_NEW"
  )
  
  missing_pair_cols <- base::setdiff(
    required_pair_cols,
    base::names(selected_pairs)
  )
  
  if (base::length(missing_pair_cols) > 0) {
    base::stop(
      "stanoviste_paseky(): v `selected_pairs` chybi sloupce: ",
      base::paste(
        missing_pair_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  if (!"PAIR_PRIORITY" %in% base::names(selected_pairs)) {
    selected_pairs <- selected_pairs |>
      dplyr::mutate(
        PAIR_PRIORITY = dplyr::case_when(
          PAIR == "VMB2_VMB0" ~ 1L,
          PAIR == "VMB1_VMB2" ~ 2L,
          PAIR == "VMB1_VMB0" ~ 3L,
          TRUE ~ 999L
        )
      )
  }
  
  
  if (base::nrow(selected_pairs) == 0) {
    
    result <- paseky_sum(
      hab_code = hab_code,
      site_code = site_code,
      paseky_detail = NULL
    )
    
    return(
      return_result(
        result,
        empty_selected_pairs()
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 2. Site
  # ---------------------------------------------------------------------------
  
  if (!"SITECODE" %in% base::names(site)) {
    base::stop(
      "stanoviste_paseky(): vrstva `site` neobsahuje `SITECODE`.",
      call. = FALSE
    )
  }
  
  
  site_target <- site |>
    dplyr::filter(
      base::as.character(SITECODE) == site_code
    )
  
  if (base::nrow(site_target) == 0) {
    base::stop(
      "stanoviste_paseky(): site `",
      site_code,
      "` nebyla nalezena.",
      call. = FALSE
    )
  }
  
  site_target <- sf::st_make_valid(
    site_target
  )
  
  
  # ---------------------------------------------------------------------------
  # 3. REGION_ID zasahujici site
  # ---------------------------------------------------------------------------
  
  if (base::is.null(site_regions)) {
    
    get_site_regions <- function(vmb) {
      
      if (!region_id_col %in% base::names(vmb)) {
        base::stop(
          "stanoviste_paseky(): prostorova VMB neobsahuje `",
          region_id_col,
          "`.",
          call. = FALSE
        )
      }
      
      if (
        !base::isTRUE(
          sf::st_crs(vmb) ==
          sf::st_crs(site_target)
        )
      ) {
        vmb <- sf::st_transform(
          vmb,
          sf::st_crs(site_target)
        )
      }
      
      vmb |>
        dplyr::select(
          dplyr::all_of(
            region_id_col
          ),
          geometry
        ) |>
        sf::st_filter(
          site_target,
          .predicate = sf::st_intersects
        ) |>
        sf::st_drop_geometry() |>
        dplyr::pull(
          dplyr::all_of(
            region_id_col
          )
        ) |>
        base::as.character() |>
        base::unique()
    }
    
    
    site_regions <- base::unique(
      base::c(
        get_site_regions(
          vmb2_update
        ),
        get_site_regions(
          vmb0_update
        )
      )
    )
  }
  
  
  site_regions <- base::unique(
    base::as.character(
      site_regions
    )
  )
  
  site_regions <- site_regions[
    !base::is.na(site_regions)
  ]
  
  
  pair_candidates_site <- selected_pairs |>
    dplyr::mutate(
      REGION_ID = base::as.character(
        REGION_ID
      )
    ) |>
    dplyr::filter(
      REGION_ID %in% site_regions
    )
  
  
  if (base::nrow(pair_candidates_site) == 0) {
    
    result <- paseky_sum(
      hab_code = hab_code,
      site_code = site_code,
      paseky_detail = NULL
    )
    
    return(
      return_result(
        result,
        empty_selected_pairs()
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 4. Spocitat vsechny chronologicky platne kandidatni pary
  # ---------------------------------------------------------------------------
  #
  # To je zasadni rozdil proti chybne metadata-only verzi:
  # par nesmime definitivne vybrat drive, nez vime, ze pro konkretni
  # site x habitat x REGION_ID skutecne existuje prostorovy vysledek.
  
  regions_vmb2_vmb0 <- pair_candidates_site |>
    dplyr::filter(
      PAIR == "VMB2_VMB0"
    ) |>
    dplyr::pull(
      REGION_ID
    )
  
  regions_vmb1_vmb2 <- pair_candidates_site |>
    dplyr::filter(
      PAIR == "VMB1_VMB2"
    ) |>
    dplyr::pull(
      REGION_ID
    )
  
  regions_vmb1_vmb0 <- pair_candidates_site |>
    dplyr::filter(
      PAIR == "VMB1_VMB0"
    ) |>
    dplyr::pull(
      REGION_ID
    )
  
  
  result_vmb2_vmb0 <- if (
    base::length(
      regions_vmb2_vmb0
    ) > 0
  ) {
    paseky_spat(
      hab_code = hab_code,
      site_code = site_code,
      site = site,
      vmb_old = vmb2_base,
      vmb_new = vmb0_update,
      region_ids = regions_vmb2_vmb0,
      pair = "VMB2_VMB0",
      habitat_col = habitat_col,
      biotop_col = biotop_col,
      share_col = share_col,
      segment_id_col = segment_id_col,
      region_id_col = region_id_col,
      date_col = date_col,
      update_year_col = update_year_col
    )
  } else {
    NULL
  }
  
  
  result_vmb1_vmb2 <- if (
    base::length(
      regions_vmb1_vmb2
    ) > 0
  ) {
    paseky_spat(
      hab_code = hab_code,
      site_code = site_code,
      site = site,
      vmb_old = vmb1_base,
      vmb_new = vmb2_update,
      region_ids = regions_vmb1_vmb2,
      pair = "VMB1_VMB2",
      habitat_col = habitat_col,
      biotop_col = biotop_col,
      share_col = share_col,
      segment_id_col = segment_id_col,
      region_id_col = region_id_col,
      date_col = date_col,
      update_year_col = update_year_col
    )
  } else {
    NULL
  }
  
  
  result_vmb1_vmb0 <- if (
    base::length(
      regions_vmb1_vmb0
    ) > 0
  ) {
    paseky_spat(
      hab_code = hab_code,
      site_code = site_code,
      site = site,
      vmb_old = vmb1_base,
      vmb_new = vmb0_update,
      region_ids = regions_vmb1_vmb0,
      pair = "VMB1_VMB0",
      habitat_col = habitat_col,
      biotop_col = biotop_col,
      share_col = share_col,
      segment_id_col = segment_id_col,
      region_id_col = region_id_col,
      date_col = date_col,
      update_year_col = update_year_col
    )
  } else {
    NULL
  }
  
  
  detail_list <- base::Filter(
    Negate(
      base::is.null
    ),
    base::list(
      result_vmb2_vmb0,
      result_vmb1_vmb2,
      result_vmb1_vmb0
    )
  )
  
  
  if (base::length(detail_list) == 0) {
    
    result <- paseky_sum(
      hab_code = hab_code,
      site_code = site_code,
      paseky_detail = NULL
    )
    
    return(
      return_result(
        result,
        empty_selected_pairs()
      )
    )
  }
  
  
  all_detail <- dplyr::bind_rows(
    detail_list
  )
  
  
  # ---------------------------------------------------------------------------
  # 5. Definitivni vyber paru podle SITE x HABITAT x REGION_ID
  # ---------------------------------------------------------------------------
  #
  # Reprodukce stareho n2k_paseky_latest.R:
  #   group_by(sitecode, habitat, region_id, pair)
  #   max(datum_new), max(datum_old)
  #   pouze datum_new > datum_old
  #   priorita paru
  #   group_by(sitecode, habitat, region_id)
  #   slice(1)
  
  max_date_safe <- function(x) {
    
    x <- base::as.Date(
      x
    )
    
    if (
      base::length(x) == 0 ||
      base::all(
        base::is.na(x)
      )
    ) {
      return(
        base::as.Date(NA)
      )
    }
    
    base::max(
      x,
      na.rm = TRUE
    )
  }
  
  
  pair_summary <- all_detail |>
    sf::st_drop_geometry() |>
    dplyr::group_by(
      SITECODE,
      HABITAT_CODE,
      REGION_ID,
      PAIR
    ) |>
    dplyr::summarise(
      DATUM_NEW = max_date_safe(
        DATUM_NEW
      ),
      DATUM_OLD = max_date_safe(
        DATUM_OLD
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      HAS_UPDATE =
        !base::is.na(DATUM_NEW) &
        !base::is.na(DATUM_OLD) &
        DATUM_NEW > DATUM_OLD,
      
      PAIR_PRIORITY = dplyr::case_when(
        PAIR == "VMB2_VMB0" ~ 1L,
        PAIR == "VMB1_VMB2" ~ 2L,
        PAIR == "VMB1_VMB0" ~ 3L,
        TRUE ~ 999L
      )
    )
  
  
  latest_choice <- pair_summary |>
    dplyr::filter(
      HAS_UPDATE
    ) |>
    dplyr::group_by(
      SITECODE,
      HABITAT_CODE,
      REGION_ID
    ) |>
    dplyr::arrange(
      PAIR_PRIORITY,
      dplyr::desc(
        DATUM_NEW
      ),
      dplyr::desc(
        DATUM_OLD
      ),
      .by_group = TRUE
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      REGION_ID,
      PAIR,
      DATUM_OLD,
      DATUM_NEW,
      PAIR_PRIORITY
    )
  
  
  if (base::nrow(latest_choice) == 0) {
    
    result <- paseky_sum(
      hab_code = hab_code,
      site_code = site_code,
      paseky_detail = NULL
    )
    
    return(
      return_result(
        result,
        latest_choice
      )
    )
  }
  
  
  selected_keys <- base::paste(
    latest_choice$REGION_ID,
    latest_choice$PAIR,
    sep = "\r"
  )
  
  
  paseky_detail <- all_detail |>
    dplyr::mutate(
      .PAIR_KEY = base::paste(
        REGION_ID,
        PAIR,
        sep = "\r"
      )
    ) |>
    dplyr::filter(
      .PAIR_KEY %in% selected_keys
    ) |>
    dplyr::select(
      -.PAIR_KEY
    )
  
  
  # ---------------------------------------------------------------------------
  # 6. Agregace na site x habitat
  # ---------------------------------------------------------------------------
  
  result <- paseky_sum(
    hab_code = hab_code,
    site_code = site_code,
    paseky_detail = paseky_detail
  )
  
  
  return_result(
    result,
    latest_choice
  )
}

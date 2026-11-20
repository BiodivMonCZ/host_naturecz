# stanoviste_export.R
#
# Centralni export noveho workflow se zachovanim vystupu stareho workflow.
#
# Vytvari dve skupiny souboru:
#
# A) ARCHIVNI / NOVE VYSTUPY
#   - results_habitats_<period_id>_<YYYYMMDD>.csv
#   - results_habitats_<period_id>_<YYYYMMDD>_evaluated.csv
#   - results_habitats_<period_id>_<YYYYMMDD>_final.csv
#   - results_habitats_<period_id>_<YYYYMMDD>_final_long.csv
#   - results_habitats_<period_id>_<YYYYMMDD>_batch_log.csv
#
# B) KOMPATIBILNI VYSTUPY STAREHO WORKFLOW
#   - results_habitats_long_<period_id>_<YYYYMMDD>.csv
#   - stanoviste_<YYYYMMDD>.xlsx
#   - n2k_stanoviste_<rok>_<YYYYMMDD>_Windows-1250.csv
#   - n2k_stanoviste_<rok>_<YYYYMMDD>_UTF-8_part<N>.csv
#
# RAW wide soubor zustava zamerne kompatibilni s historickymi
# results_habitats_*.csv a muze byt pozdeji pouzit jako `previous_source`
# ve stanoviste_trend_workflow().
#
# Systemove exporty zachovavaji strukturu hab_export() z puvodniho
# n2k_stanoviste_srovnani.R:
#   - Windows-1250: jeden soubor, "," jako separator, quote = TRUE
#   - UTF-8: ";" jako separator, quote = FALSE, po 10 000 radcich
#   - oba formaty obsahuji stejna data
#
# Pro systemove exporty jsou potreba ciselniky:
#   indicator_lookup: ind_r, ind_popis, ind_id
#   habitat_lookup:   KOD_HABITAT, NAZEV_HABITAT
#   site_context:     SITECODE, oop, pracoviste
#
# Poznamka k jednotce ROZLOHA:
# Puvodni skript vytvarel text "hektary", ale systemove kodovani prevadelo
# pouze hodnotu "ha" na kod "7". Tato funkce tuto skutecnou logiku stareho
# skriptu zamerne zachovava, aby se export nechoval jinak nez puvodni kod.

stanoviste_export <- function(
    raw_results,
    evaluated_results = NULL,
    final_results = NULL,
    batch_log = NULL,
    indicator_lookup = NULL,
    habitat_lookup = NULL,
    site_context = NULL,
    output_dir = "Outputs/Data/stanoviste",
    period_id = base::format(
      base::Sys.Date(),
      "%y"
    ),
    assessment_year = base::as.integer(
      base::format(
        base::Sys.Date(),
        "%Y"
      )
    ),
    date_stamp = base::Sys.Date(),
    encoding = "Windows-1250",
    write_raw = TRUE,
    write_raw_long = TRUE,
    write_evaluated = TRUE,
    write_final = TRUE,
    write_final_long = TRUE,
    write_batch_log = TRUE,
    write_legacy_system = TRUE,
    write_legacy_xlsx = TRUE,
    legacy_chunk_size = 10000L,
    overwrite = FALSE
) {
  
  # ===========================================================================
  # 1. Pomocne funkce
  # ===========================================================================
  
  drop_geometry <- function(x) {
    
    if (base::inherits(x, "sf")) {
      return(
        sf::st_drop_geometry(x)
      )
    }
    
    x
  }
  
  
  validate_table <- function(
    x,
    object_name,
    allow_null = FALSE
  ) {
    
    if (
      base::is.null(x) &&
      base::isTRUE(allow_null)
    ) {
      return(
        base::invisible(TRUE)
      )
    }
    
    if (!base::is.data.frame(x)) {
      base::stop(
        "stanoviste_export(): `",
        object_name,
        "` musi byt data.frame/tibble.",
        call. = FALSE
      )
    }
    
    list_cols <- base::names(x)[
      base::vapply(
        x,
        base::is.list,
        FUN.VALUE = base::logical(1)
      )
    ]
    
    if (base::length(list_cols) > 0) {
      base::stop(
        "stanoviste_export(): `",
        object_name,
        "` obsahuje list-columns: ",
        base::paste(
          list_cols,
          collapse = ", "
        ),
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
        "stanoviste_export(): v `",
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
  
  
  check_overwrite <- function(path) {
    
    if (
      base::file.exists(path) &&
      !base::isTRUE(overwrite)
    ) {
      base::stop(
        "stanoviste_export(): soubor uz existuje: ",
        path,
        "\nPouzij `overwrite = TRUE`, pokud ho chces prepsat.",
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  
  normalize_habitat_code <- function(x) {
    
    x <- base::as.character(x)
    
    dplyr::case_when(
      x == "91" ~ "91E0",
      x == "9.10E+01" ~ "91E0",
      x == "9,10E+01" ~ "91E0",
      TRUE ~ x
    )
  }
  
  
  format_export_date <- function(x) {
    
    if (base::inherits(x, "Date")) {
      return(
        base::format(
          x,
          "%Y-%m-%d"
        )
      )
    }
    
    if (base::inherits(x, "POSIXt")) {
      return(
        base::format(
          base::as.Date(x),
          "%Y-%m-%d"
        )
      )
    }
    
    base::as.character(x)
  }
  
  
  write_csv2_compat <- function(
    x,
    path
  ) {
    
    check_overwrite(path)
    
    utils::write.table(
      x = drop_geometry(x),
      file = path,
      sep = ";",
      dec = ",",
      quote = TRUE,
      row.names = FALSE,
      col.names = TRUE,
      na = "NA",
      qmethod = "double",
      fileEncoding = encoding
    )
    
    base::invisible(path)
  }
  
  
  write_legacy_windows <- function(
    x,
    path
  ) {
    
    check_overwrite(path)
    
    utils::write.table(
      x = x,
      file = path,
      row.names = FALSE,
      sep = ",",
      quote = TRUE,
      fileEncoding = "Windows-1250"
    )
    
    base::invisible(path)
  }
  
  
  write_legacy_utf8 <- function(
    x,
    path
  ) {
    
    check_overwrite(path)
    
    utils::write.table(
      x = x,
      file = path,
      row.names = FALSE,
      sep = ";",
      quote = FALSE,
      fileEncoding = "UTF-8"
    )
    
    base::invisible(path)
  }
  
  
  # ===========================================================================
  # 2. Long format puvodniho results_habitats_*.csv
  # ===========================================================================
  
  make_raw_long_legacy <- function(x) {
    
    x <- drop_geometry(x)
    
    require_cols(
      x,
      base::c(
        "SITECODE",
        "NAZEV",
        "HABITAT_CODE"
      ),
      "raw_results"
    )
    
    x <- x |>
      dplyr::mutate(
        HABITAT_CODE = normalize_habitat_code(
          HABITAT_CODE
        ),
        dplyr::across(
          dplyr::where(base::is.numeric),
          ~ base::round(
            .x,
            3
          )
        )
      )
    
    id_cols <- base::intersect(
      base::c(
        "SITECODE",
        "NAZEV",
        "HABITAT_CODE",
        "DATE_MIN",
        "DATE_MAX"
      ),
      base::names(x)
    )
    
    excluded_cols <- base::intersect(
      base::c(
        "DATE_MIN",
        "DATE_MAX",
        "DATE_MEAN",
        "DATE_MEDIAN"
      ),
      base::names(x)
    )
    
    value_cols <- base::setdiff(
      base::names(x),
      base::c(
        "SITECODE",
        "NAZEV",
        "HABITAT_CODE",
        excluded_cols
      )
    )
    
    out <- x |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(
          value_cols
        ),
        names_to = "PAR_NAZEV",
        values_to = "PAR_HODNOTA",
        values_transform = base::list(
          PAR_HODNOTA = base::as.character
        )
      ) |>
      dplyr::mutate(
        ROK_HODNOCENI = assessment_year
      ) |>
      dplyr::select(
        dplyr::all_of(id_cols),
        PAR_NAZEV,
        PAR_HODNOTA,
        ROK_HODNOCENI
      )
    
    out
  }
  
  
  # ===========================================================================
  # 3. Novy final_long
  # ===========================================================================
  
  make_final_long <- function(x) {
    
    x <- drop_geometry(x)
    
    require_cols(
      x,
      base::c(
        "SITECODE",
        "HABITAT_CODE"
      ),
      "final_results"
    )
    
    id_cols <- base::intersect(
      base::c(
        "SITECODE",
        "NAZEV",
        "HABITAT_CODE",
        "DATE_MIN",
        "DATE_MAX"
      ),
      base::names(x)
    )
    
    ignored_cols <- base::intersect(
      base::c(
        "DATE_MEAN",
        "DATE_MEDIAN"
      ),
      base::names(x)
    )
    
    value_cols <- base::setdiff(
      base::names(x),
      base::c(
        id_cols,
        ignored_cols
      )
    )
    
    x |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(base::is.numeric),
          ~ base::round(
            .x,
            4
          )
        )
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(
          value_cols
        ),
        names_to = "PAR_NAZEV",
        values_to = "PAR_HODNOTA",
        values_transform = base::list(
          PAR_HODNOTA = base::as.character
        )
      ) |>
      dplyr::mutate(
        ROK_HODNOCENI = assessment_year
      ) |>
      dplyr::select(
        dplyr::all_of(id_cols),
        PAR_NAZEV,
        PAR_HODNOTA,
        ROK_HODNOCENI
      )
  }
  
  
  # ===========================================================================
  # 4. Systemovy long objekt podle stareho results_comp / hab_export()
  # ===========================================================================
  
  make_legacy_system_data <- function(
    raw_results,
    evaluated_results,
    final_results,
    indicator_lookup,
    habitat_lookup,
    site_context
  ) {
    
    raw_results <- drop_geometry(
      raw_results
    )
    
    evaluated_results <- drop_geometry(
      evaluated_results
    )
    
    final_results <- drop_geometry(
      final_results
    )
    
    indicator_lookup <- drop_geometry(
      indicator_lookup
    )
    
    habitat_lookup <- drop_geometry(
      habitat_lookup
    )
    
    site_context <- drop_geometry(
      site_context
    )
    
    require_cols(
      raw_results,
      base::c(
        "SITECODE",
        "NAZEV",
        "HABITAT_CODE"
      ),
      "raw_results"
    )
    
    require_cols(
      evaluated_results,
      base::c(
        "SITECODE",
        "HABITAT_CODE",
        "ROZLOHA_LIMIT",
        "ROZLOHA_ZDROJ",
        "ROZLOHA_STAV",
        "KVALITA_LIMIT",
        "KVALITA_ZDROJ",
        "KVALITA_STAV",
        "CELKOVE_HODNOCENI"
      ),
      "evaluated_results"
    )
    
    require_cols(
      indicator_lookup,
      base::c(
        "ind_r",
        "ind_popis",
        "ind_id"
      ),
      "indicator_lookup"
    )
    
    require_cols(
      habitat_lookup,
      base::c(
        "KOD_HABITAT",
        "NAZEV_HABITAT"
      ),
      "habitat_lookup"
    )
    
    require_cols(
      site_context,
      base::c(
        "SITECODE",
        "oop"
      ),
      "site_context"
    )
    
    if (!"pracoviste" %in% base::names(site_context)) {
      site_context$pracoviste <- NA_character_
    }
    
    # -------------------------------------------------------------------------
    # 4.1 RAW wide -> long
    # -------------------------------------------------------------------------
    
    raw_results <- raw_results |>
      dplyr::mutate(
        SITECODE = base::as.character(
          SITECODE
        ),
        HABITAT_CODE = normalize_habitat_code(
          HABITAT_CODE
        ),
        dplyr::across(
          dplyr::where(base::is.numeric),
          ~ base::round(
            .x,
            4
          )
        )
      )
    
    date_cols <- base::intersect(
      base::c(
        "DATE_MIN",
        "DATE_MAX",
        "DATE_MEAN",
        "DATE_MEDIAN"
      ),
      base::names(raw_results)
    )
    
    value_cols <- base::setdiff(
      base::names(raw_results),
      base::c(
        "SITECODE",
        "NAZEV",
        "HABITAT_CODE",
        date_cols
      )
    )
    
    current_long <- raw_results |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(
          value_cols
        ),
        names_to = "parametr_nazev",
        values_to = "parametr_hodnota",
        values_transform = base::list(
          parametr_hodnota = base::as.character
        )
      )
    
    # Stary workflow pridaval radek CELKOVE_HODNOCENI jako samostatny indikator.
    overall_rows <- raw_results |>
      dplyr::transmute(
        SITECODE,
        NAZEV,
        HABITAT_CODE,
        dplyr::across(
          dplyr::any_of(
            base::c(
              "DATE_MIN",
              "DATE_MAX"
            )
          )
        ),
        parametr_nazev = "CELKOVE_HODNOCENI",
        parametr_hodnota = NA_character_
      )
    
    current_long <- dplyr::bind_rows(
      current_long,
      overall_rows
    )
    
    # -------------------------------------------------------------------------
    # 4.2 Hodnoceni / limity / zdroj
    # -------------------------------------------------------------------------
    
    eval_meta <- evaluated_results |>
      dplyr::mutate(
        SITECODE = base::as.character(
          SITECODE
        ),
        HABITAT_CODE = normalize_habitat_code(
          HABITAT_CODE
        )
      ) |>
      dplyr::select(
        SITECODE,
        HABITAT_CODE,
        ROZLOHA_LIMIT,
        ROZLOHA_ZDROJ,
        ROZLOHA_STAV,
        KVALITA_LIMIT,
        KVALITA_ZDROJ,
        KVALITA_STAV,
        CELKOVE_HODNOCENI
      ) |>
      dplyr::distinct()
    
    current_long <- current_long |>
      dplyr::left_join(
        eval_meta,
        by = base::c(
          "SITECODE",
          "HABITAT_CODE"
        )
      ) |>
      dplyr::mutate(
        parametr_limit = dplyr::case_when(
          parametr_nazev == "ROZLOHA" ~ ROZLOHA_LIMIT,
          parametr_nazev == "KVALITA" ~ KVALITA_LIMIT,
          TRUE ~ NA_real_
        ),
        poznamka = dplyr::case_when(
          parametr_nazev == "ROZLOHA" ~ ROZLOHA_ZDROJ,
          parametr_nazev == "KVALITA" ~ KVALITA_ZDROJ,
          TRUE ~ NA_character_
        ),
        stav = dplyr::case_when(
          parametr_nazev == "ROZLOHA" ~ ROZLOHA_STAV,
          parametr_nazev == "KVALITA" ~ KVALITA_STAV,
          parametr_nazev == "CELKOVE_HODNOCENI" ~ CELKOVE_HODNOCENI,
          TRUE ~ "nehodnocen"
        )
      ) |>
      dplyr::select(
        -ROZLOHA_LIMIT,
        -ROZLOHA_ZDROJ,
        -ROZLOHA_STAV,
        -KVALITA_LIMIT,
        -KVALITA_ZDROJ,
        -KVALITA_STAV,
        -CELKOVE_HODNOCENI
      )
    
    # -------------------------------------------------------------------------
    # 4.3 Trend
    # -------------------------------------------------------------------------
    
    trend_cols <- base::grep(
      "^TREND_",
      base::names(final_results),
      value = TRUE
    )
    
    if (base::length(trend_cols) > 0) {
      
      trend_long <- final_results |>
        dplyr::mutate(
          SITECODE = base::as.character(
            SITECODE
          ),
          HABITAT_CODE = normalize_habitat_code(
            HABITAT_CODE
          )
        ) |>
        dplyr::select(
          SITECODE,
          HABITAT_CODE,
          dplyr::all_of(
            trend_cols
          )
        ) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(
            trend_cols
          ),
          names_to = "parametr_nazev",
          values_to = "trend"
        ) |>
        dplyr::mutate(
          parametr_nazev = base::sub(
            "^TREND_",
            "",
            parametr_nazev
          ),
          trend = base::as.character(
            trend
          )
        )
      
      current_long <- current_long |>
        dplyr::left_join(
          trend_long,
          by = base::c(
            "SITECODE",
            "HABITAT_CODE",
            "parametr_nazev"
          )
        )
      
    } else {
      
      # Pokud trend nebyl pocitan, zachovej NA.
      # Puvodni workflow nerozlisitelny/nepocitany trend nekodovalo jako 1.
      current_long$trend <- NA_character_
    }
    
    # -------------------------------------------------------------------------
    # 4.4 Jednotky - presna logika stareho skriptu
    # -------------------------------------------------------------------------
    
    current_long <- current_long |>
      dplyr::mutate(
        parametr_jednotka = dplyr::case_when(
          parametr_nazev == "ROZLOHA" ~ "hektary",
          parametr_nazev == "KVALITA" ~ "kvalita",
          parametr_nazev == "TYPICKE_DRUHY" ~ "typické druhy",
          parametr_nazev == "MINIMIAREAL" ~
            "% rozlohy v celistvém uspořádání splňující hodnotu minimiareálu",
          parametr_nazev == "MINIMIAREAL_JADRA" ~
            "počet lokalit splňujících hodnotu minimiareálu",
          parametr_nazev == "MINIMIAREAL_HODNOTA" ~
            "hodnota minimiareálu",
          parametr_nazev == "MOZAIKA_VNEJSI" ~
            "% hranice s nepřírodními biotopy",
          parametr_nazev == "MOZAIKA_VNITRNI" ~
            "zastoupení nepřírodních biotopů v segmentech stanoviště",
          parametr_nazev == "MOZAIKA_FIN" ~
            "mozaikovitost",
          parametr_nazev == "RED_LIST" ~
            "druhy červeného seznamu",
          parametr_nazev == "INVASIVE" ~
            "% rozlohy zasažené invazními druhy",
          parametr_nazev == "EXPANSIVE" ~
            "% rozlohy zasažené expanzivními druhy",
          parametr_nazev == "MRTVE_DREVO" ~
            "mrtvé dřevo",
          parametr_nazev == "KALAMITA_POLOM" ~
            "kalamita/polom",
          parametr_nazev == "RED_LIST_SPECIES" ~
            "seznam druhů červeného seznamu",
          parametr_nazev == "INVASIVE_LIST" ~
            "seznam invazních druhů",
          parametr_nazev == "EXPANSIVE_LIST" ~
            "seznam expanzivních druhů",
          TRUE ~ NA_character_
        )
      )
    
    # -------------------------------------------------------------------------
    # 4.5 Habitat + OOP + pracoviste
    # -------------------------------------------------------------------------
    
    habitat_lookup_export <- habitat_lookup |>
      dplyr::transmute(
        HABITAT_CODE = normalize_habitat_code(
          KOD_HABITAT
        ),
        NAZEV_HABITAT = base::as.character(
          NAZEV_HABITAT
        )
      ) |>
      dplyr::distinct(
        HABITAT_CODE,
        .keep_all = TRUE
      )
    
    site_context_export <- site_context |>
      dplyr::transmute(
        SITECODE = base::as.character(
          SITECODE
        ),
        oop = base::as.character(
          oop
        ),
        pracoviste = base::as.character(
          pracoviste
        )
      ) |>
      dplyr::distinct(
        SITECODE,
        .keep_all = TRUE
      )
    
    current_long <- current_long |>
      dplyr::left_join(
        habitat_lookup_export,
        by = "HABITAT_CODE"
      ) |>
      dplyr::left_join(
        site_context_export,
        by = "SITECODE"
      )
    
    # -------------------------------------------------------------------------
    # 4.6 Struktura results_comp
    # -------------------------------------------------------------------------
    
    if (!"DATE_MIN" %in% base::names(current_long)) {
      current_long$DATE_MIN <- NA_character_
    }
    
    if (!"DATE_MAX" %in% base::names(current_long)) {
      current_long$DATE_MAX <- NA_character_
    }
    
    results_comp <- current_long |>
      dplyr::transmute(
        typ_predmetu_hodnoceni = "Stanoviště",
        kod_chu = SITECODE,
        nazev_chu = stringr::str_replace_all(
          NAZEV,
          "–|—",
          "-"
        ),
        druh = NAZEV_HABITAT,
        feature_code = base::as.character(
          HABITAT_CODE
        ),
        datum_hodnoceni_od = format_export_date(
          DATE_MIN
        ),
        datum_hodnoceni_do = format_export_date(
          DATE_MAX
        ),
        parametr_nazev = base::as.character(
          parametr_nazev
        ),
        parametr_hodnota = base::as.character(
          parametr_hodnota
        ),
        parametr_limit = parametr_limit,
        parametr_jednotka = parametr_jednotka,
        stav = stav,
        trend = trend,
        datum_hodnoceni = base::as.character(
          date_stamp
        ),
        oop = oop,
        pracoviste = pracoviste,
        poznamka = poznamka
      ) |>
      dplyr::left_join(
        indicator_lookup |>
          dplyr::select(
            ind_r,
            ind_popis,
            ind_id
          ) |>
          dplyr::distinct(),
        by = base::c(
          "parametr_nazev" = "ind_r"
        )
      ) |>
      dplyr::mutate(
        stav = dplyr::case_when(
          stav == "dobrý" ~ 11,
          stav == "zhoršený" ~ 12,
          stav == "špatný" ~ 13,
          stav == "neznámý" ~ 1,
          stav == "nehodnocen" ~ 8,
          TRUE ~ NA_real_
        ),
        trend = dplyr::case_when(
          trend == "zhoršující se" ~ 4,
          trend == "zlepšující se" ~ 2,
          trend == "stabilní" ~ 3,
          trend == "neznámý" ~ 1,
          TRUE ~ NA_real_
        ),
        parametr_jednotka = dplyr::case_when(
          parametr_jednotka == "ha" ~ "7",
          parametr_jednotka == "kvalita" ~ "140",
          parametr_jednotka == "mrtvé dřevo" ~ "140",
          parametr_jednotka == "typické druhy" ~ "140",
          TRUE ~ parametr_jednotka
        )
      ) |>
      dplyr::distinct()
    
    # -------------------------------------------------------------------------
    # 4.7 Systemovy CSV objekt - n2k_stanoviste_write
    # -------------------------------------------------------------------------
    
    n2k_stanoviste_write <- results_comp |>
      dplyr::mutate(
        parametr_nazev = ind_id,
        feature_code = base::as.character(
          feature_code
        )
      ) |>
      dplyr::select(
        -ind_popis,
        -ind_id
      ) |>
      dplyr::filter(
        !base::is.na(
          parametr_nazev
        )
      ) |>
      dplyr::mutate(
        parametr_hodnota = dplyr::if_else(
          !base::is.na(
            parametr_hodnota
          ),
          base::gsub(
            "\\.",
            ",",
            base::as.character(
              parametr_hodnota
            )
          ),
          NA_character_
        ),
        `Poznámka` = NA_character_
      )
    
    # -------------------------------------------------------------------------
    # 4.8 XLSX objekt
    # -------------------------------------------------------------------------
    
    ind_order_xlsx <- base::c(
      "celkové hodnocení",
      "rozloha",
      "kvalita"
    )
    
    export_data_xlsx <- results_comp |>
      dplyr::mutate(
        parametr_hodnota = base::suppressWarnings(
          base::as.numeric(
            parametr_hodnota
          )
        ),
        stav = dplyr::case_when(
          stav == 11 ~ "dobrý",
          stav == 12 ~ "zhoršený",
          stav == 13 ~ "špatný",
          stav == 1 ~ "neznámý",
          stav == 8 ~ "nehodnocen",
          TRUE ~ base::as.character(
            stav
          )
        ),
        trend = dplyr::case_when(
          trend == 4 ~ "zhoršující se",
          trend == 2 ~ "zlepšující se",
          trend == 3 ~ "stabilní",
          trend == 1 ~ "neznámý",
          TRUE ~ base::as.character(
            trend
          )
        ),
        parametr_jednotka = dplyr::case_when(
          parametr_jednotka == "7" ~ "ha",
          parametr_jednotka == "140" ~ "kvalita",
          TRUE ~ base::as.character(
            parametr_jednotka
          )
        ),
        parametr_nazev = ind_popis,
        feature_code = base::as.character(
          feature_code
        )
      ) |>
      dplyr::select(
        -ind_popis,
        -ind_id
      ) |>
      dplyr::filter(
        !base::is.na(
          parametr_nazev
        )
      ) |>
      dplyr::rename(
        `kód EVL` = kod_chu,
        `název EVL` = nazev_chu,
        `typ předmětu hodnocení` = typ_predmetu_hodnoceni,
        `předmět hodnocení` = druh,
        `kód předmětu hodn.` = feature_code,
        `počátek hodnoceného období` = datum_hodnoceni_od,
        `konec hodnoceného období` = datum_hodnoceni_do,
        `indikátor` = parametr_nazev,
        `hodnota` = parametr_hodnota,
        `limit` = parametr_limit,
        `jednotka` = parametr_jednotka,
        `datum hodnocení` = datum_hodnoceni,
        `OOP` = oop,
        `pracoviště AOPK` = pracoviste,
        `Způsob určení limitu` = poznamka
      ) |>
      dplyr::mutate(
        `Poznámka` = NA_character_,
        ind_order_tmp = base::match(
          `indikátor`,
          ind_order_xlsx,
          nomatch = base::length(
            ind_order_xlsx
          ) + 1
        )
      ) |>
      dplyr::arrange(
        `kód předmětu hodn.`,
        `název EVL`,
        ind_order_tmp
      ) |>
      dplyr::filter(
        `indikátor` %in%
          ind_order_xlsx
      ) |>
      dplyr::select(
        -ind_order_tmp
      )
    
    base::list(
      results_comp = results_comp,
      system = n2k_stanoviste_write,
      xlsx = export_data_xlsx
    )
  }
  
  
  # ===========================================================================
  # 5. Validace hlavniho volani
  # ===========================================================================
  
  validate_table(
    raw_results,
    "raw_results"
  )
  
  validate_table(
    evaluated_results,
    "evaluated_results",
    allow_null = TRUE
  )
  
  validate_table(
    final_results,
    "final_results",
    allow_null = TRUE
  )
  
  validate_table(
    batch_log,
    "batch_log",
    allow_null = TRUE
  )
  
  if (
    base::length(period_id) != 1 ||
    base::is.na(period_id) ||
    !base::nzchar(
      base::as.character(
        period_id
      )
    )
  ) {
    base::stop(
      "stanoviste_export(): `period_id` musi byt jedna ne-prazdna hodnota.",
      call. = FALSE
    )
  }
  
  if (
    base::length(legacy_chunk_size) != 1 ||
    base::is.na(legacy_chunk_size) ||
    legacy_chunk_size < 1
  ) {
    base::stop(
      "stanoviste_export(): `legacy_chunk_size` musi byt kladne cele cislo.",
      call. = FALSE
    )
  }
  
  legacy_chunk_size <- base::as.integer(
    legacy_chunk_size
  )
  
  date_stamp <- base::as.Date(
    date_stamp
  )
  
  stamp <- base::gsub(
    "-",
    "",
    base::as.character(
      date_stamp
    )
  )
  
  output_dir <- base::normalizePath(
    output_dir,
    winslash = "/",
    mustWork = FALSE
  )
  
  base::dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  period_id <- base::as.character(
    period_id
  )
  
  prefix <- base::paste0(
    "results_habitats_",
    period_id,
    "_",
    stamp
  )
  
  
  # ===========================================================================
  # 6. Cesty
  # ===========================================================================
  
  paths <- base::list(
    
    # nove + archivni
    raw = base::file.path(
      output_dir,
      base::paste0(
        prefix,
        ".csv"
      )
    ),
    
    raw_long = base::file.path(
      output_dir,
      base::paste0(
        "results_habitats_long_",
        period_id,
        "_",
        stamp,
        ".csv"
      )
    ),
    
    evaluated = base::file.path(
      output_dir,
      base::paste0(
        prefix,
        "_evaluated.csv"
      )
    ),
    
    final = base::file.path(
      output_dir,
      base::paste0(
        prefix,
        "_final.csv"
      )
    ),
    
    final_long = base::file.path(
      output_dir,
      base::paste0(
        prefix,
        "_final_long.csv"
      )
    ),
    
    batch_log = base::file.path(
      output_dir,
      base::paste0(
        prefix,
        "_batch_log.csv"
      )
    ),
    
    # stare workflow
    legacy_xlsx = base::file.path(
      output_dir,
      base::paste0(
        "stanoviste_",
        stamp,
        ".xlsx"
      )
    ),
    
    legacy_windows = base::file.path(
      output_dir,
      base::paste0(
        "n2k_stanoviste_",
        assessment_year,
        "_",
        stamp,
        "_Windows-1250.csv"
      )
    )
  )
  
  
  # ===========================================================================
  # 7. Manifest
  # ===========================================================================
  
  manifest <- dplyr::tibble(
    artifact = base::character(),
    path = base::character(),
    rows = base::integer(),
    columns = base::integer()
  )
  
  
  add_manifest <- function(
    artifact,
    path,
    x
  ) {
    
    manifest <<- dplyr::bind_rows(
      manifest,
      dplyr::tibble(
        artifact = artifact,
        path = path,
        rows = base::nrow(x),
        columns = base::ncol(x)
      )
    )
  }
  
  
  # ===========================================================================
  # 8. Archivni / nove exporty
  # ===========================================================================
  
  if (base::isTRUE(write_raw)) {
    
    write_csv2_compat(
      raw_results,
      paths$raw
    )
    
    add_manifest(
      "raw",
      paths$raw,
      raw_results
    )
  }
  
  
  if (base::isTRUE(write_raw_long)) {
    
    raw_long <- make_raw_long_legacy(
      raw_results
    )
    
    write_csv2_compat(
      raw_long,
      paths$raw_long
    )
    
    add_manifest(
      "raw_long_legacy",
      paths$raw_long,
      raw_long
    )
  }
  
  
  if (
    base::isTRUE(write_evaluated) &&
    !base::is.null(
      evaluated_results
    )
  ) {
    
    write_csv2_compat(
      evaluated_results,
      paths$evaluated
    )
    
    add_manifest(
      "evaluated",
      paths$evaluated,
      evaluated_results
    )
  }
  
  
  if (
    base::isTRUE(write_final) &&
    !base::is.null(
      final_results
    )
  ) {
    
    write_csv2_compat(
      final_results,
      paths$final
    )
    
    add_manifest(
      "final",
      paths$final,
      final_results
    )
  }
  
  
  if (
    base::isTRUE(write_final_long) &&
    !base::is.null(
      final_results
    )
  ) {
    
    final_long <- make_final_long(
      final_results
    )
    
    write_csv2_compat(
      final_long,
      paths$final_long
    )
    
    add_manifest(
      "final_long",
      paths$final_long,
      final_long
    )
  }
  
  
  if (
    base::isTRUE(write_batch_log) &&
    !base::is.null(
      batch_log
    )
  ) {
    
    write_csv2_compat(
      batch_log,
      paths$batch_log
    )
    
    add_manifest(
      "batch_log",
      paths$batch_log,
      batch_log
    )
  }
  
  
  # ===========================================================================
  # 9. Systemove exporty stareho workflow
  # ===========================================================================
  
  if (
    base::isTRUE(
      write_legacy_system
    ) ||
    base::isTRUE(
      write_legacy_xlsx
    )
  ) {
    
    if (
      base::is.null(
        evaluated_results
      ) ||
      base::is.null(
        final_results
      )
    ) {
      base::stop(
        "stanoviste_export(): systemovy export vyzaduje ",
        "`evaluated_results` i `final_results`.",
        call. = FALSE
      )
    }
    
    validate_table(
      indicator_lookup,
      "indicator_lookup"
    )
    
    validate_table(
      habitat_lookup,
      "habitat_lookup"
    )
    
    validate_table(
      site_context,
      "site_context"
    )
    
    legacy_data <- make_legacy_system_data(
      raw_results = raw_results,
      evaluated_results = evaluated_results,
      final_results = final_results,
      indicator_lookup = indicator_lookup,
      habitat_lookup = habitat_lookup,
      site_context = site_context
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 9.1 XLSX
  # ---------------------------------------------------------------------------
  
  if (
    base::isTRUE(
      write_legacy_xlsx
    )
  ) {
    
    if (
      !base::requireNamespace(
        "openxlsx",
        quietly = TRUE
      )
    ) {
      base::stop(
        "stanoviste_export(): pro XLSX export je potreba balicek `openxlsx`.",
        call. = FALSE
      )
    }
    
    check_overwrite(
      paths$legacy_xlsx
    )
    
    openxlsx::write.xlsx(
      legacy_data$xlsx,
      file = paths$legacy_xlsx
    )
    
    add_manifest(
      "legacy_xlsx",
      paths$legacy_xlsx,
      legacy_data$xlsx
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 9.2 Windows-1250 - jeden soubor
  # ---------------------------------------------------------------------------
  
  if (
    base::isTRUE(
      write_legacy_system
    )
  ) {
    
    write_legacy_windows(
      legacy_data$system,
      paths$legacy_windows
    )
    
    add_manifest(
      "legacy_system_windows1250",
      paths$legacy_windows,
      legacy_data$system
    )
    
    
    # -------------------------------------------------------------------------
    # 9.3 UTF-8 - po castech
    # -------------------------------------------------------------------------
    
    num_chunks <- base::ceiling(
      base::nrow(
        legacy_data$system
      ) /
        legacy_chunk_size
    )
    
    if (num_chunks > 0) {
      
      for (
        i in base::seq_len(
          num_chunks
        )
      ) {
        
        start_row <-
          ((i - 1L) *
             legacy_chunk_size) +
          1L
        
        end_row <- base::min(
          i *
            legacy_chunk_size,
          base::nrow(
            legacy_data$system
          )
        )
        
        chunk <- legacy_data$system[
          start_row:end_row,
          ,
          drop = FALSE
        ]
        
        chunk_path <- base::file.path(
          output_dir,
          base::paste0(
            "n2k_stanoviste_",
            assessment_year,
            "_",
            stamp,
            "_UTF-8_part",
            i,
            ".csv"
          )
        )
        
        write_legacy_utf8(
          chunk,
          chunk_path
        )
        
        add_manifest(
          base::paste0(
            "legacy_system_utf8_part",
            i
          ),
          chunk_path,
          chunk
        )
      }
    }
  }
  
  
  # ===========================================================================
  # 10. Vystup
  # ===========================================================================
  
  manifest
}

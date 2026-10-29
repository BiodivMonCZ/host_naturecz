# FUN_stanoviste_hodnoceni.R
#
# Hodnocení stavu stanoviště.
#
# Funkce přebírá široký výstup z:
#
#   stanoviste_eval()
#   nebo budoucího stanoviste_batch()
#
# na dva indikátory, které určují celkový stav:
#
#   - ROZLOHA
#   - KVALITA
#
# Výstup:
#   standardně původní `results` rozšířený o hodnocení stavu.
#
# Při return_detail = TRUE:
#
#   list(
#     result = ...,
#     detail = ...
#   )
#
# `detail` obsahuje přesný limit, jeho zdroj a stav pro ROZLOHA/KVALITA.

stanoviste_hodnoceni <- function(
    results,
    limits,
    minimisize,
    site_context,
    sdo_ii_sites,
    tolerance = 0.05,
    non_evaluated_habitats = base::c(
      "91T0",
      "3140",
      "3130",
      "8310"
    ),
    return_detail = FALSE
) {
  
  # ===========================================================================
  # 1. Pomocné funkce
  # ===========================================================================
  
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
        "stanoviste_hodnoceni(): v `",
        object_name,
        "` chybí sloupce: ",
        base::paste(
          missing_cols,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    base::invisible(TRUE)
  }
  
  
  safe_floor_local <- function(
    x,
    digits = 2
  ) {
    
    x <- base::as.numeric(x)
    
    factor <- 10 ^ digits
    
    base::floor(
      x * factor
    ) / factor
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
  
  
  grepl_safe <- function(
    pattern,
    x
  ) {
    
    x <- base::as.character(x)
    
    out <- base::grepl(
      pattern,
      x
    )
    
    out[
      base::is.na(out)
    ] <- FALSE
    
    out
  }
  
  
  first_or_na <- function(
    x,
    type = base::c(
      "character",
      "numeric"
    )
  ) {
    
    type <- base::match.arg(type)
    
    if (base::length(x) == 0) {
      
      if (type == "character") {
        return(NA_character_)
      }
      
      return(NA_real_)
    }
    
    x[[1]]
  }
  
  
  # ===========================================================================
  # 2. Validace
  # ===========================================================================
  
  if (!base::is.data.frame(results)) {
    base::stop(
      "stanoviste_hodnoceni(): `results` musí být data.frame/tibble.",
      call. = FALSE
    )
  }
  
  if (!base::is.data.frame(limits)) {
    base::stop(
      "stanoviste_hodnoceni(): `limits` musí být data.frame/tibble.",
      call. = FALSE
    )
  }
  
  if (!base::is.data.frame(minimisize)) {
    base::stop(
      "stanoviste_hodnoceni(): `minimisize` musí být data.frame/tibble.",
      call. = FALSE
    )
  }
  
  if (
    !base::is.data.frame(site_context) &&
    !base::inherits(site_context, "sf")
  ) {
    base::stop(
      "stanoviste_hodnoceni(): `site_context` musí být data.frame/tibble nebo sf.",
      call. = FALSE
    )
  }
  
  if (
    base::length(tolerance) != 1 ||
    base::is.na(tolerance) ||
    tolerance < 0
  ) {
    base::stop(
      "stanoviste_hodnoceni(): `tolerance` musí být jedno nezáporné číslo.",
      call. = FALSE
    )
  }
  
  require_cols(
    results,
    base::c(
      "SITECODE",
      "HABITAT_CODE",
      "ROZLOHA",
      "KVALITA"
    ),
    "results"
  )
  
  require_cols(
    limits,
    base::c(
      "SITECODE",
      "HABITAT_CODE",
      "ID_IND",
      "LIM_IND",
      "ZDROJ"
    ),
    "limits"
  )
  
  require_cols(
    minimisize,
    base::c(
      "HABITAT",
      "MINIMISIZE"
    ),
    "minimisize"
  )
  
  if (base::inherits(site_context, "sf")) {
    site_context <- sf::st_drop_geometry(
      site_context
    )
  }
  
  require_cols(
    site_context,
    base::c(
      "SITECODE",
      "oop"
    ),
    "site_context"
  )
  
  
  # ===========================================================================
  # 3. Seznam lokalit SDO II
  # ===========================================================================
  
  if (base::is.data.frame(sdo_ii_sites)) {
    
    sdo_col <- base::c(
      "sitecode",
      "SITECODE",
      "kod_chu"
    )
    
    sdo_col <- sdo_col[
      sdo_col %in% base::names(sdo_ii_sites)
    ]
    
    if (base::length(sdo_col) == 0) {
      base::stop(
        "stanoviste_hodnoceni(): `sdo_ii_sites` nemá sloupec ",
        "`sitecode`, `SITECODE` ani `kod_chu`.",
        call. = FALSE
      )
    }
    
    sdo_codes <- sdo_ii_sites[[sdo_col[[1]]]]
    
  } else {
    
    sdo_codes <- sdo_ii_sites
  }
  
  sdo_codes <- sdo_codes |>
    base::as.character() |>
    stats::na.omit() |>
    base::unique()
  
  
  # ===========================================================================
  # 4. Příprava výsledků
  # ===========================================================================
  
  results_eval <- results |>
    dplyr::mutate(
      SITECODE = base::as.character(SITECODE),
      HABITAT_CODE = normalize_habitat_code(
        HABITAT_CODE
      )
    )
  
  
  # ===========================================================================
  # 5. Příprava limitů
  # ===========================================================================
  #
  # Zachovává logiku původního skriptu:
  #
  #   AVMB2 -> VMB3
  #   AVMB1 / AVMB -> VMB2
  #   *SDF* -> SDF
  #
  # ROZLOHA:
  #   MINIMI / EXPERT / SDF bez zaokrouhlení
  #   ostatní floor na 2 desetinná místa
  #
  # KVALITA:
  #   ceiling na desetiny
  #
  # Původní skript si dále vytváří objekty `kval` a `rozl`, ve kterých
  # vybírá první limit pro site × habitat × indikátor. Níže tuto zamýšlenou
  # deduplikaci provedeme přímo, aby jeden indikátor nemohl násobit řádky.
  
  limits_eval <- limits |>
    dplyr::mutate(
      .limit_order = dplyr::row_number(),
      SITECODE = base::as.character(SITECODE),
      HABITAT_CODE = normalize_habitat_code(
        HABITAT_CODE
      ),
      ID_IND = base::as.character(ID_IND),
      LIM_IND = base::suppressWarnings(
        base::as.numeric(LIM_IND)
      ),
      ZDROJ = base::as.character(ZDROJ)
    ) |>
    dplyr::mutate(
      LIM_IND = dplyr::case_when(
        ID_IND == "ROZLOHA" &
          ZDROJ %in% base::c(
            "MINIMI",
            "EXPERT",
            "SDF"
          ) ~ LIM_IND,
        
        ID_IND == "ROZLOHA" ~
          safe_floor_local(
            LIM_IND,
            digits = 2
          ),
        
        ID_IND == "KVALITA" ~
          base::ceiling(
            LIM_IND * 10
          ) / 10,
        
        TRUE ~ LIM_IND
      ),
      
      ZDROJ = dplyr::case_when(
        ZDROJ == "AVMB2" ~ "VMB3",
        ZDROJ %in% base::c(
          "AVMB1",
          "AVMB"
        ) ~ "VMB2",
        grepl_safe(
          "SDF",
          ZDROJ
        ) ~ "SDF",
        TRUE ~ ZDROJ
      )
    ) |>
    dplyr::filter(
      ID_IND %in% base::c(
        "ROZLOHA",
        "KVALITA"
      )
    ) |>
    dplyr::group_by(
      SITECODE,
      HABITAT_CODE,
      ID_IND
    ) |>
    dplyr::arrange(
      .limit_order,
      LIM_IND,
      .by_group = TRUE
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  
  # ===========================================================================
  # 6. Minimiareál pro hodnocení ROZLOHY
  # ===========================================================================
  
  minimisize_eval <- minimisize |>
    dplyr::transmute(
      HABITAT_CODE = normalize_habitat_code(
        HABITAT
      ),
      MINIMISIZE = base::as.numeric(
        MINIMISIZE
      )
    ) |>
    dplyr::group_by(
      HABITAT_CODE
    ) |>
    dplyr::summarise(
      MINIMISIZE = if (
        base::all(
          base::is.na(MINIMISIZE)
        )
      ) {
        NA_real_
      } else {
        base::max(
          MINIMISIZE,
          na.rm = TRUE
        )
      },
      .groups = "drop"
    )
  
  
  # ===========================================================================
  # 7. Kontext lokality
  # ===========================================================================
  
  site_context_eval <- site_context |>
    dplyr::mutate(
      SITECODE = base::as.character(
        SITECODE
      )
    ) |>
    dplyr::select(
      SITECODE,
      oop,
      dplyr::any_of(
        "pracoviste"
      )
    ) |>
    dplyr::distinct(
      SITECODE,
      .keep_all = TRUE
    )
  
  
  # ===========================================================================
  # 8. Převod dvou stavových parametrů do long formátu
  # ===========================================================================
  
  detail <- results_eval |>
    dplyr::select(
      SITECODE,
      HABITAT_CODE,
      ROZLOHA,
      KVALITA
    ) |>
    tidyr::pivot_longer(
      cols = base::c(
        "ROZLOHA",
        "KVALITA"
      ),
      names_to = "parametr_nazev",
      values_to = "parametr_hodnota"
    ) |>
    dplyr::mutate(
      parametr_hodnota =
        base::suppressWarnings(
          base::as.numeric(
            parametr_hodnota
          )
        )
    ) |>
    dplyr::left_join(
      limits_eval |>
        dplyr::select(
          SITECODE,
          HABITAT_CODE,
          parametr_nazev = ID_IND,
          LIM_IND,
          ZDROJ
        ),
      by = base::c(
        "SITECODE",
        "HABITAT_CODE",
        "parametr_nazev"
      )
    ) |>
    dplyr::left_join(
      site_context_eval,
      by = "SITECODE"
    ) |>
    dplyr::left_join(
      minimisize_eval,
      by = "HABITAT_CODE"
    )
  
  
  # ===========================================================================
  # 9. Určení zdroje limitu
  # ===========================================================================
  #
  # Pořadí podmínek je záměrně stejné jako v n2k_stanoviste_srovnani.R.
  # První splněná podmínka má přednost.
  
  detail <- detail |>
    dplyr::rowwise() |>
    dplyr::mutate(
      
      ZDROJ = dplyr::case_when(
        
        HABITAT_CODE %in%
          non_evaluated_habitats ~
          NA_character_,
        
        ZDROJ == "EXPERT" ~
          ZDROJ,
        
        ZDROJ == "VMB3" ~
          ZDROJ,
        
        SITECODE %in% sdo_codes &
          !base::is.na(ZDROJ) ~
          ZDROJ,
        
        grepl_safe(
          "Karlo",
          oop
        ) &
          !base::is.na(ZDROJ) ~
          ZDROJ,
        
        grepl_safe(
          "Libereckého",
          oop
        ) &
          !grepl_safe(
            "Správa KRNAP",
            oop
          ) &
          !base::is.na(ZDROJ) ~
          ZDROJ,
        
        grepl_safe(
          "Plz",
          oop
        ) &
          !grepl_safe(
            "Správa NP",
            oop
          ) &
          !base::is.na(ZDROJ) ~
          ZDROJ,
        
        grepl_safe(
          "Král",
          oop
        ) &
          !base::is.na(ZDROJ) ~
          ZDROJ,
        
        grepl_safe(
          "Pardu",
          oop
        ) &
          !base::is.na(ZDROJ) ~
          ZDROJ,
        
        grepl_safe(
          "Jihočeského",
          oop
        ) &
          !grepl_safe(
            "Správa NP",
            oop
          ) &
          !base::is.na(ZDROJ) ~
          ZDROJ,
        
        parametr_nazev == "KVALITA" &
          ZDROJ %in%
          base::c(
            "SDF",
            "VMB2",
            "VMB3"
          ) &
          !base::is.na(LIM_IND) &
          LIM_IND <= 2 ~
          ZDROJ,
        
        parametr_nazev == "KVALITA" &
          base::is.na(LIM_IND) &
          !base::is.na(parametr_hodnota) &
          parametr_hodnota <= 2 ~
          "VMB2",
        
        parametr_nazev == "KVALITA" &
          base::is.na(LIM_IND) &
          !base::is.na(parametr_hodnota) &
          parametr_hodnota > 2 ~
          "MINIMI",
        
        parametr_nazev == "KVALITA" &
          !base::is.na(parametr_hodnota) &
          parametr_hodnota > 2 ~
          "MINIMI",
        
        parametr_nazev == "KVALITA" &
          base::is.na(LIM_IND) ~
          "MINIMI",
        
        # ROZLOHA - SDF
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "SDF" &
          !base::is.na(LIM_IND) &
          !base::is.na(MINIMISIZE) &
          LIM_IND >= MINIMISIZE ~
          ZDROJ,
        
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "SDF" &
          !base::is.na(LIM_IND) &
          !base::is.na(MINIMISIZE) &
          LIM_IND < MINIMISIZE ~
          "MINIMI",
        
        # ROZLOHA - VMB2
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "VMB2" &
          !base::is.na(LIM_IND) &
          !base::is.na(MINIMISIZE) &
          LIM_IND >= MINIMISIZE ~
          ZDROJ,
        
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "VMB2" &
          !base::is.na(parametr_hodnota) &
          !base::is.na(MINIMISIZE) &
          parametr_hodnota >= MINIMISIZE ~
          ZDROJ,
        
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "VMB2" &
          !base::is.na(parametr_hodnota) &
          !base::is.na(MINIMISIZE) &
          parametr_hodnota < MINIMISIZE ~
          "MINIMI",
        
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "VMB2" &
          !base::is.na(LIM_IND) &
          !base::is.na(MINIMISIZE) &
          LIM_IND < MINIMISIZE ~
          "MINIMI",
        
        parametr_nazev == "ROZLOHA" &
          !base::is.na(parametr_hodnota) &
          !base::is.na(MINIMISIZE) &
          parametr_hodnota < MINIMISIZE ~
          "MINIMI",
        
        base::is.na(ZDROJ) &
          !base::is.na(parametr_hodnota) &
          parametr_hodnota == 0 ~
          "MINIMI",
        
        base::is.na(ZDROJ) &
          base::is.na(parametr_hodnota) ~
          "MINIMI",
        
        TRUE ~
          ZDROJ
      )
    )
  
  
  # ===========================================================================
  # 10. Určení efektivního limitu
  # ===========================================================================
  #
  # Opět je zachováno pořadí původního case_when().
  #
  # Zvláštní případ CZ0514672 je převzat beze změny.
  
  detail <- detail |>
    dplyr::mutate(
      
      LIM_IND = dplyr::case_when(
        
        HABITAT_CODE %in%
          non_evaluated_habitats ~
          NA_real_,
        
        parametr_nazev == "ROZLOHA" &
          SITECODE == "CZ0514672" ~
          safe_floor_local(
            parametr_hodnota,
            digits = 2
          ),
        
        ZDROJ == "EXPERT" ~
          LIM_IND,
        
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "SDF" ~
          LIM_IND,
        
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "VMB3" ~
          LIM_IND,
        
        parametr_nazev == "ROZLOHA" &
          SITECODE %in% sdo_codes ~
          LIM_IND,
        
        parametr_nazev == "ROZLOHA" &
          !base::is.na(LIM_IND) &
          (
            grepl_safe(
              "Karlo",
              oop
            ) |
              grepl_safe(
                "Libereckého",
                oop
              ) |
              grepl_safe(
                "Plz",
                oop
              ) |
              grepl_safe(
                "Král",
                oop
              ) |
              grepl_safe(
                "Pardu",
                oop
              )
          ) &
          !grepl_safe(
            "Správa NP",
            oop
          ) &
          !grepl_safe(
            "Správa KRNAP",
            oop
          ) ~
          LIM_IND,
        
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "MINIMI" ~
          MINIMISIZE,
        
        parametr_nazev == "ROZLOHA" &
          ZDROJ == "VMB2" ~
          safe_floor_local(
            parametr_hodnota,
            digits = 2
          ),
        
        parametr_nazev == "KVALITA" &
          ZDROJ == "MINIMI" ~
          2,
        
        parametr_nazev == "KVALITA" &
          ZDROJ == "VMB2" ~
          base::ceiling(
            parametr_hodnota * 10
          ) / 10,
        
        parametr_nazev == "KVALITA" &
          base::is.na(LIM_IND) &
          !base::is.na(parametr_hodnota) &
          parametr_hodnota <= 2 ~
          base::ceiling(
            parametr_hodnota * 10
          ) / 10,
        
        parametr_nazev == "KVALITA" &
          base::is.na(LIM_IND) ~
          2,
        
        TRUE ~
          LIM_IND
      )
    ) |>
    dplyr::ungroup()
  
  
  # ===========================================================================
  # 11. Stav jednotlivých indikátorů
  # ===========================================================================
  
  detail <- detail |>
    dplyr::mutate(
      
      stav = dplyr::case_when(
        
        base::is.na(LIM_IND) ~
          "nehodnocen",
        
        parametr_nazev == "KVALITA" &
          base::is.na(parametr_hodnota) ~
          "špatný",
        
        parametr_nazev == "ROZLOHA" &
          parametr_hodnota < LIM_IND ~
          "špatný",
        
        parametr_nazev == "ROZLOHA" &
          parametr_hodnota >= LIM_IND ~
          "dobrý",
        
        parametr_nazev == "KVALITA" &
          parametr_hodnota > LIM_IND ~
          "špatný",
        
        parametr_nazev == "KVALITA" &
          parametr_hodnota <= LIM_IND ~
          "dobrý",
        
        TRUE ~
          NA_character_
      ),
      
      stav_toler = dplyr::case_when(
        
        base::is.na(LIM_IND) ~
          "nehodnocen",
        
        parametr_nazev == "KVALITA" &
          base::is.na(parametr_hodnota) ~
          "špatný",
        
        parametr_nazev == "ROZLOHA" &
          parametr_hodnota == 0 ~
          "špatný",
        
        parametr_nazev == "ROZLOHA" &
          parametr_hodnota *
          (1 + tolerance) <
          LIM_IND ~
          "špatný",
        
        parametr_nazev == "ROZLOHA" &
          parametr_hodnota *
          (1 + tolerance) >=
          LIM_IND ~
          "dobrý",
        
        parametr_nazev == "KVALITA" &
          parametr_hodnota *
          (1 - tolerance) >
          LIM_IND ~
          "špatný",
        
        parametr_nazev == "KVALITA" &
          parametr_hodnota *
          (1 - tolerance) <=
          LIM_IND ~
          "dobrý",
        
        TRUE ~
          NA_character_
      )
    )
  
  
  # ===========================================================================
  # 12. Celkové hodnocení
  # ===========================================================================
  #
  # Přesně jako v původním skriptu:
  #
  #   0 dobrých klíčových parametrů -> špatný
  #   1 dobrý                        -> zhoršený
  #   2 dobré                        -> dobrý
  #
  # Vyjmenované habitaty jsou vždy "nehodnocen".
  
  summary <- detail |>
    dplyr::group_by(
      SITECODE,
      HABITAT_CODE
    ) |>
    dplyr::summarise(
      
      ROZLOHA_LIMIT = first_or_na(
        LIM_IND[
          parametr_nazev == "ROZLOHA"
        ],
        "numeric"
      ),
      
      ROZLOHA_ZDROJ = first_or_na(
        ZDROJ[
          parametr_nazev == "ROZLOHA"
        ],
        "character"
      ),
      
      ROZLOHA_STAV = first_or_na(
        stav[
          parametr_nazev == "ROZLOHA"
        ],
        "character"
      ),
      
      ROZLOHA_STAV_TOLER = first_or_na(
        stav_toler[
          parametr_nazev == "ROZLOHA"
        ],
        "character"
      ),
      
      KVALITA_LIMIT = first_or_na(
        LIM_IND[
          parametr_nazev == "KVALITA"
        ],
        "numeric"
      ),
      
      KVALITA_ZDROJ = first_or_na(
        ZDROJ[
          parametr_nazev == "KVALITA"
        ],
        "character"
      ),
      
      KVALITA_STAV = first_or_na(
        stav[
          parametr_nazev == "KVALITA"
        ],
        "character"
      ),
      
      KVALITA_STAV_TOLER = first_or_na(
        stav_toler[
          parametr_nazev == "KVALITA"
        ],
        "character"
      ),
      
      N_DOBRYCH = base::sum(
        stav == "dobrý",
        na.rm = TRUE
      ),
      
      N_DOBRYCH_TOLER = base::sum(
        stav_toler == "dobrý",
        na.rm = TRUE
      ),
      
      CELKOVE_HODNOCENI = dplyr::case_when(
        
        dplyr::first(
          HABITAT_CODE
        ) %in%
          non_evaluated_habitats ~
          "nehodnocen",
        
        base::sum(
          stav == "dobrý",
          na.rm = TRUE
        ) == 0 ~
          "špatný",
        
        base::sum(
          stav == "dobrý",
          na.rm = TRUE
        ) == 1 ~
          "zhoršený",
        
        base::sum(
          stav == "dobrý",
          na.rm = TRUE
        ) == 2 ~
          "dobrý",
        
        TRUE ~
          NA_character_
      ),
      
      CELKOVE_HODNOCENI_TOLER =
        dplyr::case_when(
          
          dplyr::first(
            HABITAT_CODE
          ) %in%
            non_evaluated_habitats ~
            "nehodnocen",
          
          base::sum(
            stav_toler == "dobrý",
            na.rm = TRUE
          ) == 0 ~
            "špatný",
          
          base::sum(
            stav_toler == "dobrý",
            na.rm = TRUE
          ) == 1 ~
            "zhoršený",
          
          base::sum(
            stav_toler == "dobrý",
            na.rm = TRUE
          ) == 2 ~
            "dobrý",
          
          TRUE ~
            NA_character_
        ),
      
      .groups = "drop"
    )
  
  
  # ===========================================================================
  # 13. Připojení k širokému výsledku workflow
  # ===========================================================================
  
  result <- results_eval |>
    dplyr::left_join(
      summary,
      by = base::c(
        "SITECODE",
        "HABITAT_CODE"
      )
    )
  
  
  # ===========================================================================
  # 14. Výstup
  # ===========================================================================
  
  if (base::isTRUE(return_detail)) {
    
    return(
      base::list(
        result = result,
        detail = detail |>
          dplyr::select(
            SITECODE,
            HABITAT_CODE,
            parametr_nazev,
            parametr_hodnota,
            MINIMISIZE,
            ZDROJ,
            LIM_IND,
            stav,
            stav_toler,
            oop,
            dplyr::any_of(
              "pracoviste"
            )
          )
      )
    )
  }
  
  result
}

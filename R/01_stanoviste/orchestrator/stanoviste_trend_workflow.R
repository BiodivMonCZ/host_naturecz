# stanoviste_trend_workflow.R
#
# Univerzalni wrapper trendove vetve.
#
# Umi porovnat:
#   1) current z noveho workflow + stary historicky CSV,
#   2) current + previous vypoctene oba novym workflow,
#   3) dva jiz vyhodnocene wide datasety.
#
# `current_results` a `previous_results` mohou byt raw i evaluated.
# Pokud chybi sloupce hodnoceni, wrapper zavola stanoviste_hodnoceni().
#
# Pro previous lze misto objektu pouzit `previous_source`.

stanoviste_trend_workflow <- function(
    current_results,
    previous_results = NULL,
    previous_source = NULL,
    limits,
    minimisize,
    site_context,
    sdo_ii_sites,
    tolerance = 0.05,
    return_components = FALSE
) {
  
  required_functions <- base::c(
    "stanoviste_load_previous",
    "stanoviste_hodnoceni",
    "stanoviste_trend"
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
      "stanoviste_trend_workflow(): nejsou nacteny funkce: ",
      base::paste(
        missing_functions,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (
    !base::is.null(previous_results) &&
    !base::is.null(previous_source)
  ) {
    base::stop(
      "stanoviste_trend_workflow(): zadej pouze jedno z ",
      "`previous_results` nebo `previous_source`.",
      call. = FALSE
    )
  }
  
  if (
    base::is.null(previous_results) &&
    base::is.null(previous_source)
  ) {
    base::stop(
      "stanoviste_trend_workflow(): je nutne zadat ",
      "`previous_results` nebo `previous_source`.",
      call. = FALSE
    )
  }
  
  if (!base::is.null(previous_source)) {
    previous_raw <- stanoviste_load_previous(
      source = previous_source
    )
  } else {
    previous_raw <- previous_results
  }
  
  is_evaluated <- function(x) {
    base::all(
      base::c(
        "CELKOVE_HODNOCENI",
        "ROZLOHA_STAV",
        "KVALITA_STAV"
      ) %in% base::names(x)
    )
  }
  
  evaluate_if_needed <- function(x) {
    
    if (is_evaluated(x)) {
      return(x)
    }
    
    stanoviste_hodnoceni(
      results = x,
      limits = limits,
      minimisize = minimisize,
      site_context = site_context,
      sdo_ii_sites = sdo_ii_sites
    )
  }
  
  current_evaluated <- evaluate_if_needed(
    current_results
  )
  
  previous_evaluated <- evaluate_if_needed(
    previous_raw
  )
  
  trend_result <- stanoviste_trend(
    current = current_evaluated,
    previous = previous_evaluated,
    tolerance = tolerance,
    return_detail = return_components
  )
  
  if (base::isTRUE(return_components)) {
    return(
      base::list(
        result = trend_result$result,
        trend_detail = trend_result$detail,
        current_evaluated = current_evaluated,
        previous_evaluated = previous_evaluated,
        previous_raw = previous_raw
      )
    )
  }
  
  trend_result
}

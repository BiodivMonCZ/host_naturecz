# RUN_stanoviste.R
#
# Vstupni bod hlavniho workflow hodnoceni stanovist.
#
# Paseky jsou v hlavnim workflow hotovy vstup (`tables$paseky`).
# Funkce pro jejich vypocet se zde nesourcuji; budou mit samostatny runner.
#
# Urovne:
#
#   run_stanoviste_once()
#       -> jedna kombinace site x habitat
#
#   run_stanoviste_batch()
#       -> batch raw vypoctu
#
#   run_stanoviste_workflow()
#       -> batch
#       -> hodnoceni stavu
#       -> volitelne trend
#       -> volitelne export


# =============================================================================
# 0. Koren repozitare
# =============================================================================

get_script_path <- function() {
  
  source_path <- base::tryCatch(
    base::sys.frame(1)$ofile,
    error = function(e) NULL
  )
  
  if (
    !base::is.null(source_path) &&
    base::length(source_path) == 1 &&
    base::nzchar(source_path)
  ) {
    return(
      base::normalizePath(
        source_path,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  args_all <- base::commandArgs(
    trailingOnly = FALSE
  )
  
  file_arg <- base::grep(
    "^--file=",
    args_all,
    value = TRUE
  )
  
  if (base::length(file_arg) > 0) {
    return(
      base::normalizePath(
        base::sub(
          "^--file=",
          "",
          file_arg[[1]]
        ),
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  NULL
}


script_path <- get_script_path()

if (!base::is.null(script_path)) {
  
  repo_root <- base::normalizePath(
    base::file.path(
      base::dirname(script_path),
      "..",
      ".."
    ),
    winslash = "/",
    mustWork = TRUE
  )
  
} else {
  
  repo_root <- base::normalizePath(
    ".",
    winslash = "/",
    mustWork = TRUE
  )
}


# =============================================================================
# 1. Adresare
# =============================================================================

stanoviste_dir <- base::file.path(
  repo_root,
  "R",
  "01_stanoviste"
)

functions_dir <- base::file.path(
  stanoviste_dir,
  "functions"
)

orchestrator_dir <- base::file.path(
  stanoviste_dir,
  "orchestrator"
)

io_dir <- base::file.path(
  stanoviste_dir,
  "io"
)


# =============================================================================
# 2. Source helper
# =============================================================================

source_required <- function(
    directory,
    files,
    group_name
) {
  
  paths <- base::file.path(
    directory,
    files
  )
  
  missing <- paths[
    !base::file.exists(paths)
  ]
  
  if (base::length(missing) > 0) {
    base::stop(
      "RUN_stanoviste.R: chybi ",
      group_name,
      ": ",
      base::paste(
        missing,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  base::invisible(
    base::lapply(
      paths,
      FUN = base::source
    )
  )
}


# =============================================================================
# 3. Vypocetni funkce hlavniho workflow
# =============================================================================

source_required(
  functions_dir,
  base::c(
    "FUN_stanoviste_klicove_parametry.R",
    "FUN_stanoviste_druhy.R",
    "FUN_stanoviste_prostor.R",
    "FUN_stanoviste_batch.R",
    "FUN_stanoviste_hodnoceni.R",
    "FUN_stanoviste_load_previous.R",
    "FUN_stanoviste_trend.R"
  ),
  "funkcni skripty"
)


# =============================================================================
# 4. Orchestracni funkce
# =============================================================================

source_required(
  orchestrator_dir,
  base::c(
    "stanoviste_load_inputs.R",
    "stanoviste_input_validator.R",
    "stanoviste_central_orchestrator.R",
    "stanoviste_hodnoceni_inputs.R",
    "stanoviste_trend_workflow.R"
  ),
  "orchestracni skripty"
)


# =============================================================================
# 5. I/O
# =============================================================================

source_required(
  io_dir,
  "stanoviste_export.R",
  "I/O skripty"
)


# =============================================================================
# 6. Nacteni vstupu
# =============================================================================

stanoviste_inputs <- load_stanoviste_inputs(
  repo_root = repo_root,
  keep_config_env = TRUE
)

stanoviste_hodnoceni_inputs <- stanoviste_get_hodnoceni_inputs(
  stanoviste_inputs = stanoviste_inputs,
  repo_root = repo_root
)

base::message(
  "\nMapovani vstupu hlavniho workflow:"
)

base::print(
  stanoviste_inputs$manifest,
  n = Inf
)


# =============================================================================
# 7. Jedna kombinace site x habitat
# =============================================================================

run_stanoviste_once <- function(
    site_code,
    hab_code,
    return_components = FALSE
) {
  
  stanoviste_eval(
    hab_code = hab_code,
    site_code = site_code,
    data = stanoviste_inputs$data,
    tables = stanoviste_inputs$tables,
    return_components = return_components
  )
}


# =============================================================================
# 8. Batch raw vypoctu
# =============================================================================

run_stanoviste_batch <- function(
    targets,
    parallel = FALSE,
    workers = NULL,
    parallel_plan = "multisession",
    future_seed = TRUE,
    return_components = FALSE,
    stop_on_error = TRUE
) {
  
  stanoviste_batch(
    targets = targets,
    data = stanoviste_inputs$data,
    tables = stanoviste_inputs$tables,
    parallel = parallel,
    workers = workers,
    parallel_plan = parallel_plan,
    future_seed = future_seed,
    return_components = return_components,
    stop_on_error = stop_on_error
  )
}


# =============================================================================
# 9. Kompletni workflow
# =============================================================================

run_stanoviste_workflow <- function(
    targets,
    previous_results = NULL,
    previous_source = NULL,
    calculate_trend = NULL,
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
    output_dir = base::file.path(
      repo_root,
      "Outputs",
      "Data",
      "stanoviste"
    ),
    write_results = TRUE,
    overwrite = FALSE,
    parallel = FALSE,
    workers = NULL,
    parallel_plan = "multisession",
    future_seed = TRUE,
    return_components = FALSE,
    stop_on_error = TRUE
) {
  
  # ---------------------------------------------------------------------------
  # 9.1 Trend - auto rezim
  # ---------------------------------------------------------------------------
  
  if (base::is.null(calculate_trend)) {
    calculate_trend <-
      !base::is.null(previous_results) |
      !base::is.null(previous_source)
  }
  
  if (
    base::isTRUE(calculate_trend) &&
    base::is.null(previous_results) &&
    base::is.null(previous_source)
  ) {
    base::stop(
      "run_stanoviste_workflow(): `calculate_trend = TRUE`, ale chybi ",
      "`previous_results` nebo `previous_source`.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 9.2 Batch
  # ---------------------------------------------------------------------------
  
  batch_result <- run_stanoviste_batch(
    targets = targets,
    parallel = parallel,
    workers = workers,
    parallel_plan = parallel_plan,
    future_seed = future_seed,
    return_components = return_components,
    stop_on_error = stop_on_error
  )
  
  if (base::is.data.frame(batch_result)) {
    
    raw_results <- batch_result
    batch_log <- NULL
    
  } else if (
    base::is.list(batch_result) &&
    "results" %in% base::names(batch_result)
  ) {
    
    raw_results <- batch_result$results
    
    batch_log <- if (
      "log" %in% base::names(batch_result)
    ) {
      batch_result$log
    } else {
      NULL
    }
    
  } else {
    
    base::stop(
      "run_stanoviste_workflow(): neocekavany vystup stanoviste_batch().",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 9.3 Hodnoceni aktualniho stavu
  # ---------------------------------------------------------------------------
  
  evaluated_results <- stanoviste_hodnoceni(
    results = raw_results,
    limits = stanoviste_hodnoceni_inputs$limits,
    minimisize = stanoviste_hodnoceni_inputs$minimisize,
    site_context = stanoviste_hodnoceni_inputs$site_context,
    sdo_ii_sites = stanoviste_hodnoceni_inputs$sdo_ii_sites
  )
  
  # ---------------------------------------------------------------------------
  # 9.4 Trend
  # ---------------------------------------------------------------------------
  
  trend_components <- NULL
  
  if (base::isTRUE(calculate_trend)) {
    
    trend_result <- stanoviste_trend_workflow(
      current_results = evaluated_results,
      previous_results = previous_results,
      previous_source = previous_source,
      limits = stanoviste_hodnoceni_inputs$limits,
      minimisize = stanoviste_hodnoceni_inputs$minimisize,
      site_context = stanoviste_hodnoceni_inputs$site_context,
      sdo_ii_sites = stanoviste_hodnoceni_inputs$sdo_ii_sites,
      return_components = return_components
    )
    
    if (base::isTRUE(return_components)) {
      final_results <- trend_result$result
      trend_components <- trend_result
    } else {
      final_results <- trend_result
    }
    
  } else {
    
    final_results <- evaluated_results
  }
  
  # ---------------------------------------------------------------------------
  # 9.5 Export
  # ---------------------------------------------------------------------------
  
  export_manifest <- NULL
  
  if (base::isTRUE(write_results)) {
    
    export_manifest <- stanoviste_export(
      raw_results = raw_results,
      evaluated_results = evaluated_results,
      final_results = final_results,
      batch_log = batch_log,
      indicator_lookup = stanoviste_hodnoceni_inputs$indicator_lookup,
      habitat_lookup = stanoviste_hodnoceni_inputs$habitat_lookup,
      site_context = stanoviste_hodnoceni_inputs$site_context,
      output_dir = output_dir,
      period_id = period_id,
      assessment_year = assessment_year,
      overwrite = overwrite
    )
    
    base::message(
      "\nExport dokonceny:"
    )
    
    base::print(
      export_manifest,
      n = Inf
    )
  }
  
  # ---------------------------------------------------------------------------
  # 9.6 Vystup runneru
  # ---------------------------------------------------------------------------
  
  out <- base::list(
    result = final_results,
    raw = raw_results,
    evaluated = evaluated_results,
    batch_log = batch_log,
    export = export_manifest
  )
  
  if (base::isTRUE(return_components)) {
    out$batch <- batch_result
    out$trend <- trend_components
  }
  
  out
}


# =============================================================================
# 10. Volitelne CLI pro jednu kombinaci
# =============================================================================

args <- base::commandArgs(
  trailingOnly = TRUE
)

if (
  !base::interactive() &&
  base::length(args) > 0
) {
  
  if (base::length(args) != 2) {
    base::stop(
      "Pouziti pro jednu kombinaci: ",
      "Rscript R/01_stanoviste/RUN_stanoviste.R <site_code> <hab_code>",
      call. = FALSE
    )
  }
  
  result <- run_stanoviste_once(
    site_code = args[[1]],
    hab_code = args[[2]]
  )
  
  base::print(result)
}

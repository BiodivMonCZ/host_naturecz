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
    "stanoviste_chu_adapter.R",
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
# 6. Lazy nacteni vstupu podle typu CHU
# =============================================================================

.stanoviste_inputs_cache <- base::list()
.stanoviste_hodnoceni_cache <- base::list()


get_stanoviste_inputs <- function(
    typ_chu = "evl",
    force_reload = FALSE
) {

  typ_chu <- stanoviste_normalize_typ_chu(
    typ_chu
  )

  use_cache <-
    !base::isTRUE(force_reload) &&
    !base::is.null(
      .stanoviste_inputs_cache[[typ_chu]]
    )

  if (base::isTRUE(use_cache)) {
    return(
      .stanoviste_inputs_cache[[typ_chu]]
    )
  }

  inputs <- load_stanoviste_inputs(
    repo_root = repo_root,
    typ_chu = typ_chu,
    keep_config_env = TRUE
  )

  .stanoviste_inputs_cache[[typ_chu]] <<- inputs
  .stanoviste_hodnoceni_cache[[typ_chu]] <<- NULL

  base::message(
    "\nMapovani vstupu hlavniho workflow (",
    typ_chu,
    "):"
  )

  base::print(
    inputs$manifest,
    n = Inf
  )

  inputs
}


get_stanoviste_hodnoceni_inputs <- function(
    typ_chu = "evl",
    force_reload = FALSE
) {

  typ_chu <- stanoviste_normalize_typ_chu(
    typ_chu
  )

  if (typ_chu != "evl") {
    base::stop(
      "Stavove hodnoceni je zatim pripravene pouze pro `typ_chu = \"evl\"`. ",
      "Pro ostatni typy chybi explicitni limity.",
      call. = FALSE
    )
  }

  inputs <- get_stanoviste_inputs(
    typ_chu = typ_chu,
    force_reload = force_reload
  )

  if (
    !base::isTRUE(force_reload) &&
    !base::is.null(
      .stanoviste_hodnoceni_cache[[typ_chu]]
    )
  ) {
    return(
      .stanoviste_hodnoceni_cache[[typ_chu]]
    )
  }

  hodnoceni_inputs <- stanoviste_get_hodnoceni_inputs(
    stanoviste_inputs = inputs,
    repo_root = repo_root
  )

  .stanoviste_hodnoceni_cache[[typ_chu]] <<-
    hodnoceni_inputs

  hodnoceni_inputs
}


run_stanoviste_once <- function(
    site_code,
    hab_code,
    typ_chu = "evl",
    return_components = FALSE,
    force_reload = FALSE
) {

  inputs <- get_stanoviste_inputs(
    typ_chu = typ_chu,
    force_reload = force_reload
  )

  stanoviste_eval(
    hab_code = hab_code,
    site_code = site_code,
    data = inputs$data,
    tables = inputs$tables,
    return_components = return_components
  )
}


run_stanoviste_batch <- function(
    targets,
    typ_chu = "evl",
    parallel = FALSE,
    workers = NULL,
    parallel_plan = "multisession",
    future_seed = TRUE,
    return_components = FALSE,
    stop_on_error = TRUE,
    force_reload = FALSE
) {

  inputs <- get_stanoviste_inputs(
    typ_chu = typ_chu,
    force_reload = force_reload
  )

  stanoviste_batch(
    targets = targets,
    data = inputs$data,
    tables = inputs$tables,
    parallel = parallel,
    workers = workers,
    parallel_plan = parallel_plan,
    future_seed = future_seed,
    return_components = return_components,
    stop_on_error = stop_on_error
  )
}


run_stanoviste_workflow <- function(
    targets,
    typ_chu = "evl",
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
    output_dir = NULL,
    write_results = TRUE,
    overwrite = FALSE,
    parallel = FALSE,
    workers = NULL,
    parallel_plan = "multisession",
    future_seed = TRUE,
    return_components = FALSE,
    stop_on_error = TRUE,
    force_reload = FALSE
) {

  typ_chu <- stanoviste_normalize_typ_chu(
    typ_chu
  )

  if (base::is.null(output_dir)) {
    output_dir <- base::file.path(
      repo_root,
      "Outputs",
      "Data",
      "stanoviste",
      typ_chu
    )
  }

  inputs <- get_stanoviste_inputs(
    typ_chu = typ_chu,
    force_reload = force_reload
  )

  is_evl <- base::identical(
    typ_chu,
    "evl"
  )

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

  if (
    !is_evl &&
    base::isTRUE(calculate_trend)
  ) {
    base::stop(
      "Trend je zatim podporovan pouze pro `typ_chu = \"evl\"`, ",
      "protoze pro ostatni typy zatim chybi stavove limity.",
      call. = FALSE
    )
  }

  batch_result <- stanoviste_batch(
    targets = targets,
    data = inputs$data,
    tables = inputs$tables,
    parallel = parallel,
    workers = workers,
    parallel_plan = parallel_plan,
    future_seed = future_seed,
    return_components = return_components,
    stop_on_error = stop_on_error
  )

  raw_results <- batch_result$results
  batch_log <- batch_result$log

  hodnoceni_inputs <- NULL
  evaluated_results <- NULL
  final_results <- NULL
  trend_components <- NULL

  if (is_evl) {

    hodnoceni_inputs <- get_stanoviste_hodnoceni_inputs(
      typ_chu = typ_chu
    )

    evaluated_results <- stanoviste_hodnoceni(
      results = raw_results,
      limits = hodnoceni_inputs$limits,
      minimisize = hodnoceni_inputs$minimisize,
      site_context = hodnoceni_inputs$site_context,
      sdo_ii_sites = hodnoceni_inputs$sdo_ii_sites
    )

    if (base::isTRUE(calculate_trend)) {

      trend_result <- stanoviste_trend_workflow(
        current_results = evaluated_results,
        previous_results = previous_results,
        previous_source = previous_source,
        limits = hodnoceni_inputs$limits,
        minimisize = hodnoceni_inputs$minimisize,
        site_context = hodnoceni_inputs$site_context,
        sdo_ii_sites = hodnoceni_inputs$sdo_ii_sites,
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
  }

  export_manifest <- NULL

  if (base::isTRUE(write_results)) {

    export_manifest <- stanoviste_export(
      raw_results = raw_results,
      evaluated_results = evaluated_results,
      final_results = final_results,
      batch_log = batch_log,
      indicator_lookup = if (is_evl) {
        hodnoceni_inputs$indicator_lookup
      } else {
        NULL
      },
      habitat_lookup = if (is_evl) {
        hodnoceni_inputs$habitat_lookup
      } else {
        NULL
      },
      site_context = if (is_evl) {
        hodnoceni_inputs$site_context
      } else {
        NULL
      },
      output_dir = output_dir,
      period_id = period_id,
      assessment_year = assessment_year,
      write_evaluated = is_evl,
      write_final = is_evl,
      write_final_long = is_evl,
      write_legacy_system = is_evl,
      write_legacy_xlsx = is_evl,
      overwrite = overwrite
    )
  }

  out <- base::list(
    typ_chu = typ_chu,
    result = if (is_evl) final_results else raw_results,
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


args <- base::commandArgs(
  trailingOnly = TRUE
)

if (
  !base::interactive() &&
  base::length(args) > 0
) {

  if (!base::length(args) %in% base::c(2, 3)) {
    base::stop(
      "Pouziti: Rscript R/01_stanoviste/RUN_stanoviste.R ",
      "<site_code> <hab_code> [typ_chu]",
      call. = FALSE
    )
  }

  result <- run_stanoviste_once(
    site_code = args[[1]],
    hab_code = args[[2]],
    typ_chu = if (base::length(args) == 3) args[[3]] else "evl"
  )

  base::print(result)
}

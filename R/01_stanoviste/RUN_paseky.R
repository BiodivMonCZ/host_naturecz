# RUN_paseky.R
#
# Samostatny aktualizacni workflow pasek.
#
# Tento runner se spousti pouze tehdy, kdyz se zmeni vstupni VMB nebo kdyz je
# potreba paseky prepocitat. Hlavni workflow stanovist pak pouziva hotovy
# soubor:
#
#   Outputs/Data/stanoviste/paseky/paseky_results_latest.csv
#
# Interaktivne:
#
#   base::source("R/01_stanoviste/RUN_paseky.R")
#
#   x <- run_paseky_workflow(
#     parallel = TRUE,
#     workers = 8
#   )
#
#   x$result
#   x$log
#   x$export


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

paseky_dir <- base::file.path(
  stanoviste_dir,
  "paseky"
)

orchestrator_dir <- base::file.path(
  stanoviste_dir,
  "orchestrator"
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
      "RUN_paseky.R: chybi ",
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
# 3. Adapter typu CHU
# =============================================================================

source_required(
  orchestrator_dir,
  "stanoviste_chu_adapter.R",
  "adapter typu CHU"
)


# =============================================================================
# 4. Funkce pasek
# =============================================================================

source_required(
  functions_dir,
  base::c(
    "FUN_paseky_select_pairs.R",
    "FUN_paseky_spat.R",
    "FUN_paseky_sum.R",
    "FUN_stanoviste_paseky.R",
    "FUN_paseky_batch.R"
  ),
  "funkcni skripty pasek"
)


# =============================================================================
# 4. I/O a validace pasek
# =============================================================================

source_required(
  paseky_dir,
  base::c(
    "paseky_load_inputs.R",
    "paseky_input_validator.R",
    "paseky_export.R"
  ),
  "pasekove workflow skripty"
)


# =============================================================================
# 5. Lazy nacteni VMB podle typu CHU
# =============================================================================

.paseky_inputs_cache <- base::list()


get_paseky_inputs <- function(
    typ_chu = "evl",
    targets = NULL,
    force_reload = FALSE
) {

  typ_chu <- stanoviste_normalize_typ_chu(
    typ_chu
  )

  use_cache <-
    !base::isTRUE(force_reload) &&
    base::is.null(targets) &&
    !base::is.null(
      .paseky_inputs_cache[[typ_chu]]
    )

  if (base::isTRUE(use_cache)) {
    return(
      .paseky_inputs_cache[[typ_chu]]
    )
  }

  inputs <- load_paseky_inputs(
    repo_root = repo_root,
    typ_chu = typ_chu,
    targets = targets
  )

  paseky_validate_inputs(
    data = inputs$data,
    targets = inputs$targets
  )

  if (base::is.null(targets)) {
    .paseky_inputs_cache[[typ_chu]] <<- inputs
  }

  inputs
}


run_paseky_once <- function(
    site_code,
    hab_code,
    typ_chu = "evl",
    force_reload = FALSE
) {

  inputs <- get_paseky_inputs(
    typ_chu = typ_chu,
    force_reload = force_reload
  )

  selected_pairs <- paseky_select_pairs(
    vmb1_meta = inputs$data$vmb1_meta,
    vmb2_meta = inputs$data$vmb2_meta,
    vmb0_meta = inputs$data$vmb0_meta
  )

  stanoviste_paseky(
    hab_code = hab_code,
    site_code = site_code,
    site = inputs$data$site,
    vmb1_base = inputs$data$vmb1_base,
    vmb2_base = inputs$data$vmb2_base,
    vmb2_update = inputs$data$vmb2_update,
    vmb0_update = inputs$data$vmb0_update,
    vmb1_meta = inputs$data$vmb1_meta,
    vmb2_meta = inputs$data$vmb2_meta,
    vmb0_meta = inputs$data$vmb0_meta,
    selected_pairs = selected_pairs
  )
}


run_paseky_workflow <- function(
    typ_chu = "evl",
    targets = NULL,
    output_dir = NULL,
    calculation_date = base::Sys.Date(),
    parallel = TRUE,
    workers = NULL,
    parallel_plan = NULL,
    future_seed = TRUE,
    stop_on_error = TRUE,
    write_results = TRUE,
    overwrite_archive = FALSE,
    allow_partial_export = FALSE,
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
      typ_chu,
      "paseky"
    )
  }

  inputs <- get_paseky_inputs(
    typ_chu = typ_chu,
    targets = targets,
    force_reload = force_reload
  )

  batch <- paseky_batch(
    targets = inputs$targets,
    data = inputs$data,
    parallel = parallel,
    workers = workers,
    parallel_plan = parallel_plan,
    future_seed = future_seed,
    stop_on_error = stop_on_error
  )

  export_manifest <- NULL

  if (base::isTRUE(write_results)) {

    export_manifest <- paseky_export(
      batch_result = batch,
      output_dir = output_dir,
      typ_chu = typ_chu,
      calculation_date = calculation_date,
      overwrite_archive = overwrite_archive,
      allow_partial = allow_partial_export
    )
  }

  base::list(
    typ_chu = typ_chu,
    result = batch$results,
    log = batch$log,
    selected_pairs = batch$selected_pairs,
    settings = batch$settings,
    export = export_manifest
  )
}

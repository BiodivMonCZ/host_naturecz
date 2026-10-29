# FUN_stanoviste_batch.R
#
# Batch výpočet pro kombinace site x habitat.
#
# Funkce obaluje stanoviste_eval()
#
# a zajišťuje:
#   - validaci target tabulky,
#   - sekvenční nebo paralelní běh,
#   - zachycení chyb po jednotlivých kombinacích,
#   - spojení výsledků do jedné tabulky,
#   - volitelně vrácení detailních komponent z jednotlivých běhů.
#
# Doporučené použití:
#
#   batch <- stanoviste_batch(
#     targets = targets,
#     data = stanoviste_inputs$data,
#     tables = stanoviste_inputs$tables,
#     parallel = TRUE,
#     workers = 8
#   )
#
#   batch$results
#   batch$log


stanoviste_batch <- function(
    targets,
    data,
    tables,
    parallel = TRUE,
    workers = NULL,
    parallel_plan = NULL,
    future_seed = TRUE,
    return_components = FALSE,
    stop_on_error = FALSE
) {
  
  # ---------------------------------------------------------------------------
  # 1. Základní validace
  # ---------------------------------------------------------------------------
  
  if (
    !base::exists(
      "stanoviste_eval",
      mode = "function",
      inherits = TRUE
    )
  ) {
    base::stop(
      "stanoviste_batch(): není načtena funkce `stanoviste_eval()`.",
      call. = FALSE
    )
  }
  
  if (!base::is.data.frame(targets)) {
    base::stop(
      "stanoviste_batch(): `targets` musí být data.frame/tibble.",
      call. = FALSE
    )
  }
  
  required_target_cols <- base::c(
    "SITECODE",
    "HABITAT_CODE"
  )
  
  missing_target_cols <- base::setdiff(
    required_target_cols,
    base::names(targets)
  )
  
  if (base::length(missing_target_cols) > 0) {
    base::stop(
      "stanoviste_batch(): v `targets` chybí sloupce: ",
      base::paste(
        missing_target_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (!base::is.list(data)) {
    base::stop(
      "stanoviste_batch(): `data` musí být list.",
      call. = FALSE
    )
  }
  
  if (!base::is.list(tables)) {
    base::stop(
      "stanoviste_batch(): `tables` musí být list.",
      call. = FALSE
    )
  }
  
  targets_run <- targets |>
    dplyr::mutate(
      SITECODE = base::as.character(SITECODE),
      HABITAT_CODE = base::as.character(HABITAT_CODE)
    ) |>
    dplyr::filter(
      !base::is.na(SITECODE),
      !base::is.na(HABITAT_CODE),
      base::nzchar(SITECODE),
      base::nzchar(HABITAT_CODE)
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      .target_id = dplyr::row_number()
    ) |>
    dplyr::relocate(
      .target_id
    )
  
  if (base::nrow(targets_run) == 0) {
    empty_log <- tibble::tibble(
      .target_id = base::integer(),
      SITECODE = base::character(),
      HABITAT_CODE = base::character(),
      status = base::character(),
      error_message = base::character()
    )
    
    return(
      base::list(
        results = tibble::tibble(),
        log = empty_log,
        components = NULL,
        settings = base::list(
          parallel = parallel,
          parallel_plan = "sequential",
          workers = 0L,
          n_targets = 0L,
          n_success = 0L,
          n_error = 0L
        )
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # 2. Nastavení paralelizace
  # ---------------------------------------------------------------------------
  
  if (base::isTRUE(parallel)) {
    
    if (
      !requireNamespace(
        "future",
        quietly = TRUE
      )
    ) {
      base::stop(
        "stanoviste_batch(): pro paralelní běh chybí balík `future`.",
        call. = FALSE
      )
    }
    
    if (
      !requireNamespace(
        "future.apply",
        quietly = TRUE
      )
    ) {
      base::stop(
        "stanoviste_batch(): pro paralelní běh chybí balík `future.apply`.",
        call. = FALSE
      )
    }
    
    if (base::is.null(workers)) {
      workers <- future::availableCores()
      workers <- base::max(
        1L,
        base::as.integer(workers) - 1L
      )
    }
    
    if (base::is.null(parallel_plan)) {
      
      parallel_plan <- if (
        .Platform$OS.type == "windows"
      ) {
        "multisession"
      } else {
        "multicore"
      }
    }
    
    parallel_plan <- base::match.arg(
      parallel_plan,
      choices = base::c(
        "multicore",
        "multisession",
        "sequential"
      )
    )
  } else {
    
    workers <- 1L
    parallel_plan <- "sequential"
  }
  
  # ---------------------------------------------------------------------------
  # 3. Jedna kombinace site x habitat
  # ---------------------------------------------------------------------------
  
  run_one <- function(i) {
    
    if (!base::requireNamespace(
      "sf",
      quietly = TRUE
    )) {
      base::stop(
        "stanoviste_batch(): na workeru chybi balik `sf`.",
        call. = FALSE
      )
    }
    
    target_i <- targets_run |>
      dplyr::filter(
        .target_id == i
      )
    
    site_code_i <- target_i$SITECODE[[1]]
    hab_code_i <- target_i$HABITAT_CODE[[1]]
    
    base::tryCatch(
      
      {
        eval_out <- stanoviste_eval(
          hab_code = hab_code_i,
          site_code = site_code_i,
          data = data,
          tables = tables,
          return_components = return_components
        )
        
        result_row <- if (base::isTRUE(return_components)) {
          eval_out$result
        } else {
          eval_out
        }
        
        result_row <- result_row |>
          dplyr::mutate(
            .target_id = i
          ) |>
          dplyr::relocate(
            .target_id
          )
        
        base::list(
          success = TRUE,
          result = result_row,
          components = if (base::isTRUE(return_components)) {
            eval_out
          } else {
            NULL
          },
          log = tibble::tibble(
            .target_id = i,
            SITECODE = site_code_i,
            HABITAT_CODE = hab_code_i,
            status = "ok",
            error_message = NA_character_
          )
        )
      },
      
      error = function(e) {
        
        base::list(
          success = FALSE,
          result = NULL,
          components = NULL,
          log = tibble::tibble(
            .target_id = i,
            SITECODE = site_code_i,
            HABITAT_CODE = hab_code_i,
            status = "error",
            error_message = base::conditionMessage(e)
          )
        )
      }
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4. Běh batch
  # ---------------------------------------------------------------------------
  
  ids <- targets_run$.target_id
  
  if (parallel_plan == "sequential") {
    
    run_list <- base::lapply(
      ids,
      FUN = run_one
    )
    
  } else {
    
    old_plan <- future::plan()
    base::on.exit(
      future::plan(old_plan),
      add = TRUE
    )
    
    if (parallel_plan == "multicore") {
      future::plan(
        future::multicore,
        workers = workers
      )
    }
    
    if (parallel_plan == "multisession") {
      future::plan(
        future::multisession,
        workers = workers
      )
    }
    
    run_list <- future.apply::future_lapply(
      X = ids,
      FUN = run_one,
      future.seed = future_seed
    )
  }
  
  # ---------------------------------------------------------------------------
  # 5. Složení výsledků
  # ---------------------------------------------------------------------------
  
  log_tbl <- dplyr::bind_rows(
    base::lapply(
      run_list,
      `[[`,
      "log"
    )
  ) |>
    dplyr::arrange(
      .target_id
    )
  
  result_list <- base::Filter(
    f = function(x) {
      !base::is.null(x)
    },
    x = base::lapply(
      run_list,
      `[[`,
      "result"
    )
  )
  
  if (base::length(result_list) > 0) {
    
    results_tbl <- dplyr::bind_rows(
      result_list
    ) |>
      dplyr::left_join(
        targets_run,
        by = ".target_id",
        suffix = base::c(
          "",
          "_TARGET"
        )
      ) |>
      dplyr::arrange(
        .target_id
      )
    
  } else {
    
    results_tbl <- tibble::tibble()
  }
  
  components_out <- NULL
  
  if (base::isTRUE(return_components)) {
    
    component_indices <- base::which(
      base::vapply(
        run_list,
        FUN = function(x) {
          !base::is.null(x$components)
        },
        FUN.VALUE = base::logical(1)
      )
    )
    
    if (base::length(component_indices) > 0) {
      
      components_out <- stats::setNames(
        object = base::lapply(
          component_indices,
          FUN = function(idx) {
            run_list[[idx]]$components
          }
        ),
        nm = base::vapply(
          component_indices,
          FUN = function(idx) {
            base::paste0(
              run_list[[idx]]$log$SITECODE[[1]],
              "__",
              run_list[[idx]]$log$HABITAT_CODE[[1]]
            )
          },
          FUN.VALUE = base::character(1)
        )
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # 6. Kontrola chyb
  # ---------------------------------------------------------------------------
  
  n_error <- log_tbl |>
    dplyr::filter(
      status == "error"
    ) |>
    base::nrow()
  
  if (
    base::isTRUE(stop_on_error) &&
    n_error > 0
  ) {
    base::stop(
      "stanoviste_batch(): selhalo ",
      n_error,
      " kombinací site x habitat. Zkontroluj `batch$log`.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 7. Výstup
  # ---------------------------------------------------------------------------
  
  base::list(
    results = results_tbl,
    log = log_tbl,
    components = components_out,
    settings = base::list(
      parallel = parallel,
      parallel_plan = parallel_plan,
      workers = workers,
      n_targets = base::nrow(targets_run),
      n_success = base::sum(
        log_tbl$status == "ok",
        na.rm = TRUE
      ),
      n_error = n_error
    )
  )
}

# FUN_paseky_batch.R
#
# Batch vypocet pasek pro kombinace site x habitat.
#
# paseky_select_pairs() zde pripravi pouze CHRONOLOGICKY PLATNE KANDIDATY.
# Definitivni vyber paru se dela az uvnitr stanoviste_paseky()
# pro konkretni SITECODE x HABITAT_CODE x REGION_ID podle skutecneho
# prostoroveho vysledku.
#
# REGION_ID zasahujici kazdou site se stale predpocitaji pouze jednou.
#
# Vystup:
#   list(
#     results = ...,
#     log = ...,
#     selected_pairs = ...,  # skutecne vybrane pary po site/habitat/region
#     pair_candidates = ..., # metadata kandidati
#     settings = ...
#   )

paseky_batch <- function(
    targets,
    data,
    parallel = TRUE,
    workers = NULL,
    parallel_plan = NULL,
    future_seed = TRUE,
    stop_on_error = TRUE
) {
  
  # ---------------------------------------------------------------------------
  # 1. Validace zakladni struktury
  # ---------------------------------------------------------------------------
  
  required_functions <- base::c(
    "paseky_select_pairs",
    "paseky_spat",
    "paseky_sum",
    "stanoviste_paseky"
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
      "paseky_batch(): nejsou nacteny funkce: ",
      base::paste(
        missing_functions,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  if (!base::is.data.frame(targets)) {
    base::stop(
      "paseky_batch(): `targets` musi byt data.frame/tibble.",
      call. = FALSE
    )
  }
  
  
  if (!base::is.list(data)) {
    base::stop(
      "paseky_batch(): `data` musi byt pojmenovany list.",
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
      "paseky_batch(): v `targets` chybi sloupce: ",
      base::paste(
        missing_target_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  
  targets_run <- targets |>
    dplyr::transmute(
      SITECODE = base::as.character(
        SITECODE
      ),
      HABITAT_CODE = base::as.character(
        HABITAT_CODE
      )
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
    return(
      base::list(
        results = tibble::tibble(),
        log = tibble::tibble(),
        selected_pairs = tibble::tibble(),
        pair_candidates = tibble::tibble(),
        settings = base::list(
          n_targets = 0L,
          n_success = 0L,
          n_error = 0L,
          parallel = FALSE,
          workers = 0L,
          parallel_plan = "sequential"
        )
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 2. Metadata kandidati pouze jednou za cely batch
  # ---------------------------------------------------------------------------
  
  pair_candidates <- paseky_select_pairs(
    vmb1_meta = data$vmb1_meta,
    vmb2_meta = data$vmb2_meta,
    vmb0_meta = data$vmb0_meta
  )
  
  
  # ---------------------------------------------------------------------------
  # 3. REGION_ID po site - pouze jednou pro kazdou lesni site
  # ---------------------------------------------------------------------------
  
  forest_targets <- targets_run |>
    dplyr::filter(
      base::substr(
        HABITAT_CODE,
        1,
        1
      ) %in% base::c(
        "9",
        "L"
      )
    )
  
  
  unique_sites <- base::unique(
    forest_targets$SITECODE
  )
  
  
  get_site_regions <- function(
    site_code
  ) {
    
    site_target <- data$site |>
      dplyr::filter(
        base::as.character(SITECODE) == site_code
      )
    
    if (base::nrow(site_target) == 0) {
      base::stop(
        "paseky_batch(): site `",
        site_code,
        "` nebyla nalezena.",
        call. = FALSE
      )
    }
    
    site_target <- sf::st_make_valid(
      site_target
    )
    
    
    regions_one_layer <- function(vmb) {
      
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
          REGION_ID,
          geometry
        ) |>
        sf::st_filter(
          site_target,
          .predicate = sf::st_intersects
        ) |>
        sf::st_drop_geometry() |>
        dplyr::pull(
          REGION_ID
        ) |>
        base::as.character() |>
        base::unique()
    }
    
    
    out <- base::unique(
      base::c(
        regions_one_layer(
          data$vmb2_update
        ),
        regions_one_layer(
          data$vmb0_update
        )
      )
    )
    
    out[
      !base::is.na(out)
    ]
  }
  
  
  site_regions <- stats::setNames(
    object = base::lapply(
      unique_sites,
      FUN = get_site_regions
    ),
    nm = unique_sites
  )
  
  
  # ---------------------------------------------------------------------------
  # 4. Nastaveni paralelizace
  # ---------------------------------------------------------------------------
  
  if (base::isTRUE(parallel)) {
    
    if (
      !requireNamespace(
        "future",
        quietly = TRUE
      )
    ) {
      base::stop(
        "paseky_batch(): chybi balicek `future`.",
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
        "paseky_batch(): chybi balicek `future.apply`.",
        call. = FALSE
      )
    }
    
    if (base::is.null(workers)) {
      workers <- base::max(
        1L,
        base::as.integer(
          future::availableCores()
        ) - 1L
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
      base::c(
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
  # 5. Jedna kombinace
  # ---------------------------------------------------------------------------
  
  run_one <- function(i) {
    
    target_i <- targets_run[
      targets_run$.target_id == i,
      ,
      drop = FALSE
    ]
    
    site_code_i <- target_i$SITECODE[[1]]
    hab_code_i <- target_i$HABITAT_CODE[[1]]
    
    started <- base::Sys.time()
    
    
    base::tryCatch(
      {
        
        calc_i <- stanoviste_paseky(
          hab_code = hab_code_i,
          site_code = site_code_i,
          site = data$site,
          vmb1_base = data$vmb1_base,
          vmb2_base = data$vmb2_base,
          vmb2_update = data$vmb2_update,
          vmb0_update = data$vmb0_update,
          vmb1_meta = data$vmb1_meta,
          vmb2_meta = data$vmb2_meta,
          vmb0_meta = data$vmb0_meta,
          selected_pairs = pair_candidates,
          site_regions = site_regions[[site_code_i]],
          return_selected_pairs = TRUE
        )
        
        
        result_i <- calc_i$result |>
          dplyr::mutate(
            .target_id = i
          ) |>
          dplyr::relocate(
            .target_id
          )
        
        
        selected_i <- calc_i$selected_pairs
        
        if (
          base::is.data.frame(selected_i) &&
          base::nrow(selected_i) > 0
        ) {
          selected_i <- selected_i |>
            dplyr::mutate(
              .target_id = i
            ) |>
            dplyr::relocate(
              .target_id
            )
        }
        
        
        elapsed <- base::as.numeric(
          base::difftime(
            base::Sys.time(),
            started,
            units = "secs"
          )
        )
        
        
        base::list(
          result = result_i,
          selected_pairs = selected_i,
          log = tibble::tibble(
            .target_id = i,
            SITECODE = site_code_i,
            HABITAT_CODE = hab_code_i,
            status = "ok",
            elapsed_sec = elapsed,
            error_message = NA_character_
          )
        )
      },
      
      error = function(e) {
        
        elapsed <- base::as.numeric(
          base::difftime(
            base::Sys.time(),
            started,
            units = "secs"
          )
        )
        
        base::list(
          result = NULL,
          selected_pairs = NULL,
          log = tibble::tibble(
            .target_id = i,
            SITECODE = site_code_i,
            HABITAT_CODE = hab_code_i,
            status = "error",
            elapsed_sec = elapsed,
            error_message = base::conditionMessage(e)
          )
        )
      }
    )
  }
  
  
  ids <- targets_run$.target_id
  
  
  # ---------------------------------------------------------------------------
  # 6. Spusteni
  # ---------------------------------------------------------------------------
  
  if (parallel_plan == "sequential") {
    
    run_list <- base::lapply(
      ids,
      FUN = run_one
    )
    
  } else {
    
    old_plan <- future::plan()
    
    base::on.exit(
      future::plan(
        old_plan
      ),
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
  # 7. Slozeni vystupu
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
    Negate(
      base::is.null
    ),
    base::lapply(
      run_list,
      `[[`,
      "result"
    )
  )
  
  
  results_tbl <- if (
    base::length(result_list) == 0
  ) {
    tibble::tibble()
  } else {
    dplyr::bind_rows(
      result_list
    ) |>
      dplyr::arrange(
        .target_id
      )
  }
  
  
  selected_list <- base::Filter(
    f = function(x) {
      !base::is.null(x) &&
        base::is.data.frame(x) &&
        base::nrow(x) > 0
    },
    x = base::lapply(
      run_list,
      `[[`,
      "selected_pairs"
    )
  )
  
  
  selected_pairs_tbl <- if (
    base::length(selected_list) == 0
  ) {
    tibble::tibble(
      .target_id = integer(),
      SITECODE = character(),
      HABITAT_CODE = character(),
      REGION_ID = character(),
      PAIR = character(),
      DATUM_OLD = base::as.Date(character()),
      DATUM_NEW = base::as.Date(character()),
      PAIR_PRIORITY = integer()
    )
  } else {
    dplyr::bind_rows(
      selected_list
    ) |>
      dplyr::arrange(
        .target_id,
        REGION_ID
      )
  }
  
  
  n_error <- base::sum(
    log_tbl$status == "error",
    na.rm = TRUE
  )
  
  
  if (
    base::isTRUE(stop_on_error) &&
    n_error > 0
  ) {
    
    error_rows <- log_tbl |>
      dplyr::filter(
        status == "error"
      )
    
    base::stop(
      "paseky_batch(): selhalo ",
      n_error,
      " kombinaci site x habitat. Prvni chyba: ",
      error_rows$SITECODE[[1]],
      " x ",
      error_rows$HABITAT_CODE[[1]],
      " -> ",
      error_rows$error_message[[1]],
      call. = FALSE
    )
  }
  
  
  base::list(
    results = results_tbl,
    log = log_tbl,
    selected_pairs = selected_pairs_tbl,
    pair_candidates = pair_candidates,
    settings = base::list(
      n_targets = base::nrow(
        targets_run
      ),
      n_success = base::sum(
        log_tbl$status == "ok",
        na.rm = TRUE
      ),
      n_error = n_error,
      parallel = parallel,
      workers = workers,
      parallel_plan = parallel_plan
    )
  )
}

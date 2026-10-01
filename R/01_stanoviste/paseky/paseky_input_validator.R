# paseky_input_validator.R
#
# Validace vstupu samostatneho workflow pasek.
#
# Pri uspechu vraci invisible(TRUE).

paseky_validate_inputs <- function(
    data,
    targets
) {

  require_sf <- function(
      x,
      object_name
  ) {

    if (!base::inherits(x, "sf")) {
      base::stop(
        "paseky_validate_inputs(): `",
        object_name,
        "` musi byt sf objekt.",
        call. = FALSE
      )
    }
  }


  require_table <- function(
      x,
      object_name
  ) {

    if (!base::is.data.frame(x)) {
      base::stop(
        "paseky_validate_inputs(): `",
        object_name,
        "` musi byt data.frame/tibble.",
        call. = FALSE
      )
    }
  }


  require_cols <- function(
      x,
      cols,
      object_name
  ) {

    missing <- base::setdiff(
      cols,
      base::names(x)
    )

    if (base::length(missing) > 0) {
      base::stop(
        "paseky_validate_inputs(): v `",
        object_name,
        "` chybi: ",
        base::paste(
          missing,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
  }

  if (!base::is.list(data)) {
    base::stop(
      "paseky_validate_inputs(): `data` musi byt pojmenovany list.",
      call. = FALSE
    )
  }

  required_data <- base::c(
    "site",
    "vmb1_base",
    "vmb2_base",
    "vmb2_update",
    "vmb0_update",
    "vmb1_meta",
    "vmb2_meta",
    "vmb0_meta"
  )

  missing_data <- base::setdiff(
    required_data,
    base::names(data)
  )

  if (base::length(missing_data) > 0) {
    base::stop(
      "paseky_validate_inputs(): v `data` chybi: ",
      base::paste(
        missing_data,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  null_data <- required_data[
    base::vapply(
      data[required_data],
      base::is.null,
      FUN.VALUE = base::logical(1)
    )
  ]

  if (base::length(null_data) > 0) {
    base::stop(
      "paseky_validate_inputs(): NULL polozky v `data`: ",
      base::paste(
        null_data,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  require_sf(
    data$site,
    "data$site"
  )

  require_cols(
    data$site,
    "SITECODE",
    "data$site"
  )

  require_sf(
    data$vmb1_base,
    "data$vmb1_base"
  )

  require_sf(
    data$vmb2_base,
    "data$vmb2_base"
  )

  require_sf(
    data$vmb2_update,
    "data$vmb2_update"
  )

  require_sf(
    data$vmb0_update,
    "data$vmb0_update"
  )

  required_base_cols <- base::c(
    "HABITAT",
    "BIOTOP",
    "STEJ_PR",
    "SEGMENT_ID",
    "DATUM",
    "REGION_ID"
  )

  require_cols(
    data$vmb1_base,
    required_base_cols,
    "data$vmb1_base"
  )

  require_cols(
    data$vmb2_base,
    required_base_cols,
    "data$vmb2_base"
  )

  required_update_cols <- base::c(
    "BIOTOP",
    "STEJ_PR",
    "SEGMENT_ID",
    "REGION_ID",
    "DATUM",
    "ROK_AKT"
  )

  require_cols(
    data$vmb2_update,
    required_update_cols,
    "data$vmb2_update"
  )

  require_cols(
    data$vmb0_update,
    required_update_cols,
    "data$vmb0_update"
  )

  base::lapply(
    base::c(
      "vmb1_meta",
      "vmb2_meta",
      "vmb0_meta"
    ),
    FUN = function(x) {

      require_table(
        data[[x]],
        base::paste0(
          "data$",
          x
        )
      )

      require_cols(
        data[[x]],
        base::c(
          "REGION_ID",
          "DATUM"
        ),
        base::paste0(
          "data$",
          x
        )
      )
    }
  )

  require_table(
    targets,
    "targets"
  )

  require_cols(
    targets,
    base::c(
      "SITECODE",
      "HABITAT_CODE"
    ),
    "targets"
  )

  targets_check <- targets |>
    dplyr::transmute(
      SITECODE = base::as.character(
        SITECODE
      ),
      HABITAT_CODE = base::as.character(
        HABITAT_CODE
      )
    )

  if (
    base::any(
      base::is.na(
        targets_check$SITECODE
      )
    ) ||
    base::any(
      base::is.na(
        targets_check$HABITAT_CODE
      )
    )
  ) {
    base::stop(
      "paseky_validate_inputs(): `targets` obsahuji NA v klicich.",
      call. = FALSE
    )
  }

  duplicates <- targets_check |>
    dplyr::count(
      SITECODE,
      HABITAT_CODE,
      name = "n"
    ) |>
    dplyr::filter(
      n > 1
    )

  if (base::nrow(duplicates) > 0) {
    base::stop(
      "paseky_validate_inputs(): `targets` obsahuji duplicitni ",
      "kombinace SITECODE x HABITAT_CODE.",
      call. = FALSE
    )
  }

  missing_sites <- base::setdiff(
    base::unique(
      targets_check$SITECODE
    ),
    base::unique(
      base::as.character(
        data$site$SITECODE
      )
    )
  )

  if (base::length(missing_sites) > 0) {
    base::stop(
      "paseky_validate_inputs(): tyto SITECODE nejsou ve vrstve site: ",
      base::paste(
        missing_sites,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  base::invisible(TRUE)
}

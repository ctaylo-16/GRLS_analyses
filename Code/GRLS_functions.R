# Shared GRLS data-derivation helpers.
#
# These functions deliberately stop on missing columns, inconsistent group-level
# values, invalid dates, or ambiguous ties. They do not repair malformed data.

.require_columns <- function(data, columns) {
  missing_columns <- setdiff(columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.require_positive_whole_number <- function(value, name) {
  if (
    length(value) != 1L ||
      is.na(value) ||
      !is.numeric(value) ||
      value <= 0 ||
      value != as.integer(value)
  ) {
    stop(name, " must be one positive whole number.", call. = FALSE)
  }

  invisible(TRUE)
}

.as_date_strict <- function(value, column_name) {
  parsed <- tryCatch(
    as.Date(value),
    error = function(error) {
      stop("Invalid date value in column '", column_name, "'.", call. = FALSE)
    }
  )
  supplied <- !is.na(value) & as.character(value) != ""

  if (any(supplied & is.na(parsed))) {
    stop("Invalid date value in column '", column_name, "'.", call. = FALSE)
  }

  parsed
}

.single_group_value <- function(value, column_name, subject_id) {
  values <- unique(value[!is.na(value)])

  if (length(values) != 1L) {
    stop(
      "Expected exactly one non-missing '", column_name,
      "' value for subject_id ", subject_id, ".",
      call. = FALSE
    )
  }

  values[[1]]
}

.mode_or_fail <- function(value, column_name, subject_id) {
  value <- value[!is.na(value)]

  if (length(value) == 0L) {
    return(NA_character_)
  }

  counts <- table(value)
  modes <- names(counts)[counts == max(counts)]

  if (length(modes) != 1L) {
    stop(
      "Tied mode for column '", column_name,
      "' and subject_id ", subject_id, ".",
      call. = FALSE
    )
  }

  modes[[1]]
}

# Return the earliest dated exposure for each dog, repeated across that dog's
# rows. exposure_column and record_date use tidy-evaluation syntax.
first_exposure_point <- function(data, exposure_column, record_date) {
  exposure_quo <- rlang::enquo(exposure_column)
  record_date_quo <- rlang::enquo(record_date)
  exposure_name <- rlang::as_name(exposure_quo)
  record_date_name <- rlang::as_name(record_date_quo)
  output_name <- paste0(exposure_name, "_first_exposure_date")

  .require_columns(data, c("subject_id", exposure_name, record_date_name))

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      !!output_name := {
        dates <- .as_date_strict(!!record_date_quo, record_date_name)
        exposed <- !is.na(!!exposure_quo) & !!exposure_quo == 1 & !is.na(dates)

        if (any(exposed)) min(dates[exposed]) else as.Date(NA)
      }
    ) |>
    dplyr::ungroup()
}

# Add age in days and years at first exposure. Column names are strings.
first_exposure_age2 <- function(data, first_exposure_point_col, birth_date_col) {
  .require_columns(data, c(first_exposure_point_col, birth_date_col))

  output_name <- paste0(first_exposure_point_col, "_age")
  exposure_date <- .as_date_strict(
    data[[first_exposure_point_col]],
    first_exposure_point_col
  )
  birth_date <- .as_date_strict(data[[birth_date_col]], birth_date_col)
  age_days <- as.numeric(exposure_date - birth_date)

  if (any(age_days < 0, na.rm = TRUE)) {
    stop("First exposure occurs before birth for at least one row.", call. = FALSE)
  }

  data[[output_name]] <- age_days
  data[[paste0(output_name, "_years")]] <- age_days / 365.25
  data
}

# Count dated exposures before an event for each dog. Column names are strings.
num_exposures <- function(data, exposure_column, record_date, date_of_event) {
  .require_columns(
    data,
    c("subject_id", exposure_column, record_date, date_of_event)
  )

  record_dates <- .as_date_strict(data[[record_date]], record_date)
  event_dates <- .as_date_strict(data[[date_of_event]], date_of_event)
  data$.grls_record_date <- record_dates
  data$.grls_event_date <- event_dates
  output_name <- paste0(exposure_column, "_num_exposures")

  result <- data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      !!output_name := sum(
        .data[[exposure_column]] == 1 &
          .data$.grls_record_date < .data$.grls_event_date,
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()

  dplyr::select(result, -dplyr::all_of(c(".grls_record_date", ".grls_event_date")))
}

years_prior_to <- function(data, event, num_years) {
  event_quo <- rlang::enquo(event)
  event_name <- rlang::as_name(event_quo)
  .require_columns(data, event_name)
  .require_positive_whole_number(num_years, "num_years")

  output_name <- paste0("prediagnosis", num_years, "year")
  dplyr::mutate(data, !!output_name := !!event_quo - num_years)
}

# Determine whether an exposure occurred in the specified number of years before
# diagnosis. diagnosis_year uses tidy-evaluation syntax; exposure_year is an
# explicit required input column retained for compatibility with current calls.
check_exposure <- function(data, columns_to_check, diagnosis_year, exposure_years) {
  diagnosis_quo <- rlang::enquo(diagnosis_year)
  diagnosis_name <- rlang::as_name(diagnosis_quo)
  .require_positive_whole_number(exposure_years, "exposure_years")
  .require_columns(
    data,
    c("subject_id", diagnosis_name, "exposure_year", columns_to_check)
  )

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns_to_check),
        ~ {
          diagnosis <- .single_group_value(
            !!diagnosis_quo,
            diagnosis_name,
            dplyr::first(.data$subject_id)
          )
          lag_years <- diagnosis - .data$exposure_year
          exposed <- any(
            .x == 1 & lag_years >= 0 & lag_years <= exposure_years,
            na.rm = TRUE
          )

          if (exposed) {
            paste0("Within ", exposure_years, "y")
          } else {
            paste0("Not within ", exposure_years, "y")
          }
        },
        .names = "{.col}_{exposure_years}y"
      )
    ) |>
    dplyr::ungroup()
}

check_exposure2 <- function(data, columns_to_check, study_years, year_column) {
  .require_columns(data, c("subject_id", year_column, columns_to_check))

  if (length(study_years) == 0L || anyNA(study_years)) {
    stop("study_years must contain at least one non-missing year.", call. = FALSE)
  }

  suffix <- paste(study_years, collapse = "_")

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns_to_check),
        ~ if (any(.x == 1 & .data[[year_column]] %in% study_years, na.rm = TRUE)) {
          paste0("Within ", paste(study_years, collapse = ","), " years")
        } else {
          paste0("Not within ", paste(study_years, collapse = ","), " years")
        },
        .names = paste0("{.col}_study_years_", suffix)
      ),
      dplyr::across(
        dplyr::all_of(columns_to_check),
        ~ sum(.x == 1 & .data[[year_column]] %in% study_years, na.rm = TRUE),
        .names = paste0("{.col}_sum_", suffix)
      )
    ) |>
    dplyr::ungroup()
}

check_exposure_binary <- function(
    data,
    columns_to_check,
    year_range,
    year_column,
    study_year_column) {
  .require_positive_whole_number(year_range, "year_range")
  .require_columns(
    data,
    c("subject_id", year_column, study_year_column, columns_to_check)
  )

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns_to_check),
        ~ {
          endpoint_year <- .single_group_value(
            .data[[year_column]],
            year_column,
            dplyr::first(.data$subject_id)
          )
          years_to_check <- seq.int(endpoint_year - year_range, endpoint_year - 1L)
          values <- .x[.data[[study_year_column]] %in% years_to_check]
          if (any(values == 1, na.rm = TRUE)) "Yes" else "No"
        },
        .names = paste0("{.col}_exposed_", year_range, "yrs_prior")
      )
    ) |>
    dplyr::ungroup()
}

check_exposure_mode <- function(data, columns_to_check, study_years, year_column) {
  .require_columns(data, c("subject_id", year_column, columns_to_check))

  if (length(study_years) == 0L || anyNA(study_years)) {
    stop("study_years must contain at least one non-missing year.", call. = FALSE)
  }

  suffix <- paste(study_years, collapse = "_")

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns_to_check),
        ~ .mode_or_fail(
          .x[.data[[year_column]] %in% study_years],
          dplyr::cur_column(),
          dplyr::first(.data$subject_id)
        ),
        .names = paste0("{.col}_mode_", suffix)
      )
    ) |>
    dplyr::ungroup()
}

check_exposure_mode_X_prev_years <- function(
    data,
    columns_to_check,
    year_range,
    year_column,
    study_year_column) {
  .require_positive_whole_number(year_range, "year_range")
  .require_columns(
    data,
    c("subject_id", year_column, study_year_column, columns_to_check)
  )

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns_to_check),
        ~ {
          endpoint_year <- .single_group_value(
            .data[[year_column]],
            year_column,
            dplyr::first(.data$subject_id)
          )
          years_to_check <- seq.int(endpoint_year - year_range, endpoint_year - 1L)
          .mode_or_fail(
            .x[.data[[study_year_column]] %in% years_to_check],
            dplyr::cur_column(),
            dplyr::first(.data$subject_id)
          )
        },
        .names = paste0("{.col}_mode_", year_range, "yrs_prior")
      )
    ) |>
    dplyr::ungroup()
}

exposure_dosage <- function(data, column_of_exposure, record_date, date_of_event) {
  .require_columns(
    data,
    c("subject_id", column_of_exposure, record_date, date_of_event)
  )
  output_name <- paste0(column_of_exposure, "_prediagnosis")

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      !!output_name := sum(
        dplyr::if_else(
          .data[[record_date]] < .data[[date_of_event]],
          .data[[column_of_exposure]],
          0
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()
}

exposure_dosage2 <- function(data, column_of_exposure, study_years) {
  .require_columns(data, c("subject_id", "year_in_study", column_of_exposure))

  if (length(study_years) == 0L || anyNA(study_years)) {
    stop("study_years must contain at least one non-missing year.", call. = FALSE)
  }

  output_name <- paste0(
    column_of_exposure,
    "_",
    paste(study_years, collapse = "_"),
    "_total_dosage"
  )

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      !!output_name := sum(
        dplyr::if_else(
          .data$year_in_study %in% study_years,
          .data[[column_of_exposure]],
          0
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()
}

check_exposure_hours <- function(
    data,
    columns_to_check,
    year_range,
    year_column,
    study_year_column) {
  .require_positive_whole_number(year_range, "year_range")
  .require_columns(
    data,
    c("subject_id", year_column, study_year_column, columns_to_check)
  )

  data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(columns_to_check),
        ~ {
          endpoint_year <- .single_group_value(
            .data[[year_column]],
            year_column,
            dplyr::first(.data$subject_id)
          )
          years_to_check <- seq.int(endpoint_year - year_range, endpoint_year - 1L)
          sum(.x[.data[[study_year_column]] %in% years_to_check], na.rm = TRUE)
        },
        .names = paste0("{.col}_exposure_hours_", year_range, "yrs_prior")
      )
    ) |>
    dplyr::ungroup()
}

split_column <- function(data, original_column, outcomes) {
  if (length(original_column) != nrow(data)) {
    stop("original_column must have one value per data row.", call. = FALSE)
  }
  if (length(outcomes) == 0L || anyNA(outcomes) || anyDuplicated(outcomes)) {
    stop("outcomes must be non-missing and unique.", call. = FALSE)
  }

  for (outcome in outcomes) {
    data[[paste0(outcome, "_YN")]] <- ifelse(
      is.na(original_column),
      NA_character_,
      ifelse(original_column == outcome, "1", "0")
    )
  }

  data
}

majority_location <- function(data, columns_to_count) {
  .require_columns(data, c("subject_id", columns_to_count))

  result <- data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(columns_to_count),
        ~ sum(.x == 1, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  counts <- as.matrix(result[, columns_to_count, drop = FALSE])
  maximum <- apply(counts, 1L, max)
  tied <- rowSums(counts == maximum) != 1L

  if (any(tied)) {
    stop(
      "No unique majority location for subject_id: ",
      paste(result$subject_id[tied], collapse = ", "),
      call. = FALSE
    )
  }

  result$majority_location <- columns_to_count[
    max.col(counts, ties.method = "first")
  ]
  result
}

map_frequency_to_df <- function(
    data,
    column_to_map,
    mapping_list,
    new_column_name = "mapped_frequency") {
  .require_columns(data, column_to_map)

  if (is.null(names(mapping_list)) || any(names(mapping_list) == "")) {
    stop("mapping_list must have a name for every mapping group.", call. = FALSE)
  }

  map_frequency <- function(frequency) {
    matches <- vapply(
      mapping_list,
      function(values) frequency %in% values,
      FUN.VALUE = logical(1)
    )

    if (sum(matches) > 1L) {
      stop("A value appears in more than one mapping group.", call. = FALSE)
    }

    if (any(matches)) names(mapping_list)[which(matches)] else "unspecified"
  }

  data[[new_column_name]] <- vapply(
    data[[column_to_map]],
    map_frequency,
    FUN.VALUE = character(1)
  )
  data
}

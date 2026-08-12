.weight_mode_max_tie <- function(value) {
  value <- value[!is.na(value)]

  if (length(value) == 0L) {
    return(NA_character_)
  }

  counts <- table(value)
  modes <- as.numeric(names(counts)[counts == max(counts)])

  as.character(max(modes))
}

weight_feature_columns <- function() {
  c(
    "avg_weight_5y_prior_endpoint",
    "avg_height_5y_prior_endpoint",
    "median_weight_5y_prior_endpoint",
    "median_weight_5y_prior_endpoint_any_records",
    "median_height_5y_prior_endpoint",
    "median_height_5y_prior_endpoint_any_records",
    "avg_purina_BCS_5y_prior_endpoint",
    "mode_purina_BCS_5y_prior_endpoint",
    "avg_weight_5y_prior_endpoint_kg",
    "median_weight_5y_prior_endpoint_kg",
    "avg_height_5y_prior_endpoint_cm",
    "median_height_5y_prior_endpoint_cm"
  )
}

build_weight_features <- function(
    cohort,
    physical_exams,
    minimum_records = 5L) {
  if (anyDuplicated(cohort$subject_id)) {
    stop("The pre-weight dataset is not one row per subject_id.", call. = FALSE)
  }
  if (anyNA(cohort$year_in_study_diagnosis_or_final_record_year)) {
    stop(
      "Missing year_in_study_diagnosis_or_final_record_year for weight features.",
      call. = FALSE
    )
  }

  endpoints <- cohort |>
    dplyr::select(
      "subject_id",
      "year_in_study_diagnosis_or_final_record_year",
      "age_at_final_date"
    )

  summaries <- physical_exams |>
    dplyr::select(
      "subject_id",
      "year_in_study",
      "height",
      "weight",
      "purina_body_condition_score"
    ) |>
    dplyr::inner_join(endpoints, by = "subject_id", relationship = "many-to-one") |>
    dplyr::mutate(
      year_in_study = as.numeric(.data$year_in_study),
      year_in_study_diagnosis_or_final_record_year = as.numeric(
        .data$year_in_study_diagnosis_or_final_record_year
      )
    ) |>
    dplyr::filter(
      .data$year_in_study >=
        .data$year_in_study_diagnosis_or_final_record_year - 5,
      .data$year_in_study <=
        .data$year_in_study_diagnosis_or_final_record_year
    ) |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      count_rows = dplyr::n(),
      age_at_final_date = dplyr::first(.data$age_at_final_date),
      avg_weight_5y_prior_endpoint = if (count_rows >= minimum_records) {
        mean(.data$weight, na.rm = TRUE)
      } else {
        NA_real_
      },
      avg_height_5y_prior_endpoint = if (count_rows >= minimum_records) {
        mean(.data$height, na.rm = TRUE)
      } else {
        NA_real_
      },
      median_weight_5y_prior_endpoint = if (count_rows >= minimum_records) {
        stats::median(.data$weight, na.rm = TRUE)
      } else {
        NA_real_
      },
      median_weight_5y_prior_endpoint_any_records = stats::median(
        .data$weight,
        na.rm = TRUE
      ),
      median_height_5y_prior_endpoint = if (count_rows >= minimum_records) {
        stats::median(.data$height, na.rm = TRUE)
      } else {
        NA_real_
      },
      median_height_5y_prior_endpoint_any_records = stats::median(
        .data$height,
        na.rm = TRUE
      ),
      avg_purina_BCS_5y_prior_endpoint = if (count_rows >= minimum_records) {
        mean(.data$purina_body_condition_score, na.rm = TRUE)
      } else {
        NA_real_
      },
      mode_purina_BCS_5y_prior_endpoint = .weight_mode_max_tie(
        .data$purina_body_condition_score
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      avg_weight_5y_prior_endpoint_kg =
        .data$avg_weight_5y_prior_endpoint * 0.453,
      median_weight_5y_prior_endpoint_kg =
        .data$median_weight_5y_prior_endpoint * 0.453,
      avg_height_5y_prior_endpoint_cm =
        .data$avg_height_5y_prior_endpoint * 2.54,
      median_height_5y_prior_endpoint_cm =
        .data$median_height_5y_prior_endpoint * 2.54,
      dplyr::across(
        dplyr::all_of(c(
          "avg_weight_5y_prior_endpoint_kg",
          "median_weight_5y_prior_endpoint_kg",
          "avg_height_5y_prior_endpoint_cm",
          "median_height_5y_prior_endpoint_cm"
        )),
        ~ dplyr::if_else(.data$age_at_final_date < 1.5, NA_real_, .x)
      )
    ) |>
    dplyr::select("subject_id", dplyr::all_of(weight_feature_columns()))

  if (anyDuplicated(summaries$subject_id)) {
    stop("Weight features are not one row per subject_id.", call. = FALSE)
  }

  cohort |>
    dplyr::left_join(summaries, by = "subject_id", relationship = "one-to-one")
}

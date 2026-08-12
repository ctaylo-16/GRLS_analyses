finalise_cancer_dataset <- function(data, case_column) {
  if (anyDuplicated(data$subject_id)) {
    stop("The final dataset is not one row per subject_id.", call. = FALSE)
  }
  if (!case_column %in% names(data)) {
    stop("The requested cancer case column is missing.", call. = FALSE)
  }

  result <- data |>
    dplyr::select(-dplyr::any_of(c("record_date.x", "X")))

  structural_columns <- c(
    "subject_id",
    case_column,
    "date_2",
    "exposure_year",
    "enrolled_date",
    "birth_date",
    "diagnosis_date",
    "withdrawn_date",
    "inactive_date",
    "death_date",
    "end_date",
    "year_in_study_diagnosis_or_final_record_year",
    "diagnosis_year_month_date",
    "diagnosis_year",
    "diagnosis_year_month_date2",
    "diagnosis_year_month",
    "birth_date2",
    "spay_neuter_date2",
    "end_date2"
  )
  feature_columns <- setdiff(names(result), structural_columns)

  result |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(feature_columns),
        ~ replace(.x, is.na(.x), "records_not_available")
      )
    )
}

prepare_cancer_cohort <- function(cohort) {
  cohort |>
    dplyr::select(-record_date) |>
    dplyr::mutate(
      diagnosis_year_month_date = as.Date(end_date),
      diagnosis_year = as.numeric(format(diagnosis_year_month_date, "%Y")),
      diagnosis_year_month_date2 = as.POSIXct(
        lubridate::floor_date(diagnosis_year_month_date, unit = "month")
      ),
      diagnosis_year_month = format(diagnosis_year_month_date, "%Y-%m")
    )
}

add_deprivation_features <- function(cohort, deprivation) {
  deprivation <- deprivation |>
    dplyr::select(subject_id, MDI_quintile, owner_MDI_quintile)

  cohort |>
    dplyr::left_join(deprivation, by = "subject_id")
}

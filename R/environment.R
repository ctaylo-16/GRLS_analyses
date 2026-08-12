environment_exposure_columns <- function() {
  c(
    "use_aerosol",
    "use_air_cleaner",
    "use_hepa_filter",
    "use_moth_balls",
    "use_incense_or_candles",
    "smoke_exposure",
    "any_treated_weeds",
    "any_treated_insects",
    "any_treated_fertilizer"
  )
}

rename_exposure_windows <- function(data) {
  data |>
    dplyr::rename_with(
      ~ gsub("_0_1_2$", "_early_life", .x),
      dplyr::ends_with("_0_1_2")
    ) |>
    dplyr::rename_with(
      ~ gsub(
        "_0_1_2_3_4_5_6_7_8_9_10$",
        "_whole_life",
        .x
      ),
      dplyr::ends_with("_0_1_2_3_4_5_6_7_8_9_10")
    ) |>
    dplyr::rename_with(
      ~ gsub("_3_4_5_6_7_8_9_10$", "_rest_of_life", .x),
      dplyr::ends_with("_3_4_5_6_7_8_9_10")
    )
}

build_environment_binary_features <- function(environment_exposures, cohort) {
  exposure_columns <- environment_exposure_columns()

  longitudinal <- environment_exposures |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character), tolower)
    ) |>
    dplyr::select(
      subject_id,
      year_in_study,
      record_date,
      dplyr::all_of(exposure_columns[1:6]),
      hours_of_smoke,
      dplyr::all_of(exposure_columns[7:9])
    ) |>
    dplyr::mutate(
      date_2 = lubridate::parse_date_time(record_date, "ym"),
      exposure_year = as.numeric(format(date_2, "%Y"))
    ) |>
    dplyr::filter(!is.na(record_date)) |>
    dplyr::left_join(cohort, by = "subject_id")

  with_early <- check_exposure2(
    longitudinal,
    exposure_columns,
    study_years = 0:2,
    year_column = "year_in_study"
  )

  with_rest <- check_exposure2(
    with_early,
    exposure_columns,
    study_years = 3:10,
    year_column = "year_in_study"
  )

  with_whole_life <- check_exposure2(
    with_rest,
    exposure_columns,
    study_years = 0:10,
    year_column = "year_in_study"
  ) |>
    rename_exposure_windows()

  within_five_years <- check_exposure(
    longitudinal,
    exposure_columns,
    diagnosis_year,
    exposure_years = 5
  )

  list(
    longitudinal = longitudinal,
    exposure_windows = with_whole_life,
    within_five_years = within_five_years
  )
}

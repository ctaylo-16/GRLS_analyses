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

build_smoke_dosage_features <- function(exposure_windows, within_five_years) {
  smoking <- exposure_windows |>
    dplyr::filter(!grepl("see detail rows", hours_of_smoke)) |>
    dplyr::mutate(
      hours_of_smoke = as.numeric(hours_of_smoke),
      record_date2 = lubridate::ym(record_date),
      record_year = as.numeric(format(record_date2, "%Y"))
    )

  smoking <- exposure_dosage(
    smoking,
    "hours_of_smoke",
    "record_year",
    "diagnosis_year"
  )
  smoking <- exposure_dosage2(smoking, "hours_of_smoke", 0:2)
  smoking <- exposure_dosage2(smoking, "hours_of_smoke", 3:10)
  smoking <- exposure_dosage2(smoking, "hours_of_smoke", 0:10) |>
    dplyr::rename(
      hours_of_smoke_early_life_total_dosage =
        hours_of_smoke_0_1_2_total_dosage,
      hours_of_smoke_rest_of_life_total_dosage =
        hours_of_smoke_3_4_5_6_7_8_9_10_total_dosage,
      hours_of_smoke_whole_life_total_dosage =
        hours_of_smoke_0_1_2_3_4_5_6_7_8_9_10_total_dosage
    )

  five_year_binary <- within_five_years |>
    dplyr::distinct(subject_id, .keep_all = TRUE) |>
    dplyr::select(
      subject_id,
      dplyr::all_of(paste0(environment_exposure_columns(), "_5y"))
    )

  smoking_one_row <- smoking |>
    dplyr::distinct(subject_id, .keep_all = TRUE)

  five_year_binary |>
    dplyr::left_join(smoking_one_row, by = "subject_id")
}

build_sleep_location_features <- function(sleep_environment, environment_smoke) {
  sleep_columns <- c("in the garage_YN", "in the house_YN", "outside_YN")

  sleep <- sleep_environment |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character), tolower)
    ) |>
    dplyr::select(
      subject_id,
      year_in_study,
      record_date,
      sleep_location,
      specific_sleep_location
    ) |>
    dplyr::mutate(
      record_date2 = lubridate::parse_date_time(record_date, "ym"),
      exposure_year = as.numeric(format(record_date2, "%Y"))
    ) |>
    unique()

  sleep <- split_column(
    sleep,
    sleep$sleep_location,
    c("in the garage", "in the house", "outside")
  )

  sleep_environment_smoke <- sleep |>
    dplyr::left_join(
      environment_smoke,
      by = c("subject_id", "year_in_study")
    ) |>
    dplyr::rename(exposure_year = exposure_year.x)

  sleep_windows <- check_exposure2(
    sleep_environment_smoke,
    sleep_columns,
    study_years = 0:2,
    year_column = "year_in_study"
  )
  sleep_windows <- check_exposure2(
    sleep_windows,
    sleep_columns,
    study_years = 3:10,
    year_column = "year_in_study"
  )
  sleep_windows <- check_exposure2(
    sleep_windows,
    sleep_columns,
    study_years = 0:10,
    year_column = "year_in_study"
  )
  sleep_windows <- check_exposure(
    sleep_windows,
    sleep_columns,
    diagnosis_year,
    exposure_years = 5
  ) |>
    rename_exposure_windows()

  majority <- majority_location(sleep_windows, sleep_columns)

  environment_smoke |>
    dplyr::left_join(majority, by = "subject_id")
}

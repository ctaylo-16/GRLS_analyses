.stop_with_subjects <- function(message, subject_ids) {
  stop(
    message,
    ": ",
    paste(unique(subject_ids), collapse = ", "),
    call. = FALSE
  )
}

prepare_reproductive_profile <- function(profile) {
  result <- profile |>
    dplyr::select(
      "subject_id",
      "sex_status",
      "spay_neuter_date",
      "birth_date"
    ) |>
    dplyr::mutate(
      sex = dplyr::recode(
        .data$sex_status,
        "Spayed Female" = "F",
        "Intact Female" = "F",
        "Neutered Male" = "M",
        "Intact Male" = "M"
      ),
      neuter = dplyr::recode(
        .data$sex_status,
        "Spayed Female" = "Neutered",
        "Neutered Male" = "Neutered",
        "Intact Female" = "Entire",
        "Intact Male" = "Entire"
      ),
      birth_date2 = as.Date(.data$birth_date),
      spay_neuter_date2 = as.Date(.data$spay_neuter_date)
    )

  invalid_neuter_dates <- result |>
    dplyr::filter(
      !is.na(.data$spay_neuter_date2),
      !is.na(.data$birth_date2),
      .data$spay_neuter_date2 < .data$birth_date2
    )

  if (nrow(invalid_neuter_dates) > 0L) {
    .stop_with_subjects(
      "Spay/neuter date precedes birth date for subject_id",
      invalid_neuter_dates$subject_id
    )
  }

  result |>
    dplyr::mutate(
      age_at_neuter_years = dplyr::if_else(
        is.na(.data$spay_neuter_date2),
        "not_neutered",
        as.character(round(
          as.numeric(.data$spay_neuter_date2 - .data$birth_date2) / 365.25,
          1
        ))
      )
    ) |>
    dplyr::select(
      "subject_id",
      "neuter",
      "sex",
      "sex_status",
      "birth_date2",
      "age_at_neuter_years",
      "spay_neuter_date2"
    )
}

summarise_female_reproduction <- function(female_history) {
  female_history |>
    dplyr::mutate(record_date2 = as.Date(.data$record_date)) |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      first_heat_report_date = {
        dates <- .data$record_date2[.data$heat_last_year == 1]
        if (length(dates) == 0L || all(is.na(dates))) {
          as.Date(NA)
        } else {
          min(dates, na.rm = TRUE)
        }
      },
      heat_reporting_years = dplyr::n_distinct(
        .data$year_in_study[.data$heat_last_year == 1]
      ),
      ever_pregnancy_recorded = if (
        any(.data$no_pregnancy_last_year == 1, na.rm = TRUE)
      ) "yes" else "no",
      .groups = "drop"
    )
}

summarise_male_reproduction <- function(male_history) {
  male_history |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      ever_mating_recorded = if (
        any(.data$intact_naturally_bred_last_year == 1, na.rm = TRUE) ||
          any(.data$neutered_ever_bred == 1, na.rm = TRUE)
      ) "yes" else "no",
      .groups = "drop"
    )
}

build_reproduction_features <- function(
    cohort,
    profile,
    female_history,
    male_history) {
  cohort_subjects <- cohort |>
    dplyr::select("subject_id") |>
    dplyr::distinct()

  profile_features <- profile |>
    dplyr::semi_join(cohort_subjects, by = "subject_id") |>
    prepare_reproductive_profile()

  if (anyDuplicated(profile_features$subject_id)) {
    stop("Dog profile contains duplicate subject_id values.", call. = FALSE)
  }

  female <- female_history |>
    dplyr::semi_join(cohort_subjects, by = "subject_id") |>
    summarise_female_reproduction()

  male <- male_history |>
    dplyr::semi_join(cohort_subjects, by = "subject_id") |>
    summarise_male_reproduction()

  result <- cohort |>
    dplyr::left_join(
      profile_features,
      by = "subject_id",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(female, by = "subject_id", relationship = "one-to-one") |>
    dplyr::left_join(male, by = "subject_id", relationship = "one-to-one") |>
    dplyr::mutate(
      age_first_heat_years = dplyr::case_when(
        .data$sex == "M" ~ "male_dog",
        is.na(.data$first_heat_report_date) ~ "not_recorded",
        TRUE ~ as.character(round(
          as.numeric(.data$first_heat_report_date - .data$birth_date2) / 365.25,
          1
        ))
      ),
      heat_reporting_years = dplyr::case_when(
        .data$sex == "M" ~ "male_dog",
        is.na(.data$heat_reporting_years) ~ "not_recorded",
        TRUE ~ as.character(.data$heat_reporting_years)
      ),
      ever_pregnancy_recorded = dplyr::case_when(
        .data$sex == "M" ~ "not_applicable",
        is.na(.data$ever_pregnancy_recorded) ~ "not_recorded",
        TRUE ~ .data$ever_pregnancy_recorded
      ),
      ever_mating_recorded = dplyr::case_when(
        .data$sex == "F" ~ "not_applicable",
        is.na(.data$ever_mating_recorded) ~ "not_recorded",
        TRUE ~ .data$ever_mating_recorded
      ),
      end_date2 = as.Date(.data$end_date)
    )

  neutered_after_endpoint <- result |>
    dplyr::filter(
      !is.na(.data$spay_neuter_date2),
      !is.na(.data$end_date2),
      .data$spay_neuter_date2 > .data$end_date2
    )

  if (nrow(neutered_after_endpoint) > 0L) {
    .stop_with_subjects(
      "Spay/neuter date follows the cancer cohort endpoint for subject_id",
      neutered_after_endpoint$subject_id
    )
  }

  result |>
    dplyr::mutate(
      time_between_neuter_endpoint_diagnosis_years = dplyr::if_else(
        is.na(.data$spay_neuter_date2),
        "not_neutered",
        as.character(round(
          as.numeric(.data$end_date2 - .data$spay_neuter_date2) / 365.25,
          1
        ))
      ),
      time_between_neuter_endpoint_diagnosis_months = dplyr::if_else(
        is.na(.data$spay_neuter_date2),
        "not_neutered",
        as.character(round(
          as.numeric(.data$end_date2 - .data$spay_neuter_date2) / 30.4375,
          1
        ))
      ),
      age_at_final_date = as.numeric(.data$end_date2 - .data$birth_date2) / 365.25
    ) |>
    dplyr::select(-"first_heat_report_date")
}

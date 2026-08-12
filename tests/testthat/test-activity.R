source(here::here("R", "activity.R"))

.activity_test_sources <- function() {
  list(
    early = data.frame(
      subject_id = c("A", "A"),
      year_in_study = c(0, 1),
      record_date = c("2020-01-01", "2021-01-01"),
      walk_duration = c("Less than 10 minutes", "Greater than 60 minutes"),
      aerobic_duration = c("", "Greater than 60 minutes"),
      walk_frequency = c("Never", "Once/day"),
      aerobic_frequency = c("", "Once/day"),
      walk_pace = c("Slow", "Run"),
      aerobic_pace = c("", "Engages in strenuous exercise most of the time (greater than 60%)")
    ),
    later = data.frame(
      subject_id = c("A", "A", "A"),
      year_in_study = c(3, 4, 5),
      record_date = c("2023-01-01", "2024-01-01", "2025-01-01"),
      frequency = c("Weekly", "Daily", "More than daily"),
      pace = c("Average walk", "Brisk walk", "Run")
    ),
    overview = data.frame(
      subject_id = c("A", "A", "A"),
      year_in_study = c(0, 1, 5),
      record_date = c("2020-01", "2021-01", "2025-01"),
      activity_level = c("Little", "Moderate", "Very active")
    )
  )
}

test_that("early activity uses the maximum category when modes tie", {
  sources <- .activity_test_sources()
  cohort <- data.frame(
    subject_id = "A",
    end_date = "2024-06-01",
    year_in_study_diagnosis_or_final_record_year = 4
  )

  result <- build_activity_features(
    cohort,
    sources$early,
    sources$later,
    sources$overview
  )

  expect_identical(result$USDA_30min_sy1_2, "more_than_USDA")
  expect_identical(result$activity_1h_min_sy1_2, "more_than_1h")
  expect_identical(result$activity_KC_2h_min_sy1_2, "more_than_KC")
  expect_equal(result$avg_activity_duration_sy1_2, 60)
})

test_that("later and overview scores are averaged once and stop at endpoint", {
  sources <- .activity_test_sources()
  cohort <- data.frame(
    subject_id = "A",
    end_date = "2024-06-01",
    year_in_study_diagnosis_or_final_record_year = 4
  )

  result <- build_activity_features(
    cohort,
    sources$early,
    sources$later,
    sources$overview
  )

  expect_equal(result$avg_activity_freq_sy3, 70)
  expect_equal(result$avg_activity_intensity_sy3, 50)
  expect_equal(result$mean_activity_level_overview, 62.5)
})

test_that("five-year activity is not overwritten by the all-SY3 score", {
  sources <- .activity_test_sources()
  cohort <- data.frame(
    subject_id = "A",
    end_date = "2024-06-01",
    year_in_study_diagnosis_or_final_record_year = 4
  )

  result <- build_activity_features(
    cohort,
    sources$early,
    sources$later,
    sources$overview
  )

  expect_equal(result$avg_frequency_5y_prior_endpoint, 65)
  expect_equal(result$avg_intensity_5y_prior_endpoint, 58.325)
  expect_identical(result$avg_frequency_5y_prior_endpoint_splits, "daily")
})

test_that("ordinal activity mode uses the maximum value on ties", {
  sources <- .activity_test_sources()
  cohort <- data.frame(
    subject_id = "A",
    end_date = "2024-06-01",
    year_in_study_diagnosis_or_final_record_year = 4
  )

  result <- build_activity_features(
    cohort,
    sources$early,
    sources$later,
    sources$overview
  )

  expect_identical(result$whole_activity_mode, "2")
  expect_identical(result$early_activity_mode, "2")
  expect_identical(
    result$rest_activity_mode,
    "no_activity_records_for_this_time_period"
  )
})

test_that("activity features require an endpoint year", {
  sources <- .activity_test_sources()
  cohort <- data.frame(
    subject_id = "A",
    end_date = "2024-06-01",
    year_in_study_diagnosis_or_final_record_year = NA_real_
  )

  expect_error(
    build_activity_features(
      cohort,
      sources$early,
      sources$later,
      sources$overview
    ),
    "Missing year_in_study"
  )
})

test_that("overview distributions can be omitted for the MCT output schema", {
  sources <- .activity_test_sources()
  cohort <- data.frame(
    subject_id = "A",
    end_date = "2024-06-01",
    year_in_study_diagnosis_or_final_record_year = 4
  )

  result <- build_activity_features(
    cohort,
    sources$early,
    sources$later,
    sources$overview,
    include_overview_distribution = FALSE
  )

  expect_false("whole_activity_mode" %in% names(result))
  expect_true("mean_activity_level_overview" %in% names(result))
})

test_that("activity features require an endpoint date", {
  sources <- .activity_test_sources()
  cohort <- data.frame(
    subject_id = "A",
    end_date = NA_character_,
    year_in_study_diagnosis_or_final_record_year = 4
  )

  expect_error(
    build_activity_features(
      cohort,
      sources$early,
      sources$later,
      sources$overview
    ),
    "Missing end_date"
  )
})

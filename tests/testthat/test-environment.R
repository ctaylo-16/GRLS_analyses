source(here::here("Code", "GRLS_functions.R"))
source(here::here("R", "environment.R"))

test_that("environment exposure columns remain explicit", {
  expect_identical(
    environment_exposure_columns(),
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
  )
})

test_that("binary environment features cover each analysis window", {
  exposure_columns <- environment_exposure_columns()
  environment <- data.frame(
    subject_id = rep("001", 4),
    year_in_study = c(0, 3, 7, 9),
    record_date = c("2015-01", "2018-01", "2022-01", "2024-01"),
    hours_of_smoke = c("0", "1", "2", "3")
  )
  for (column in exposure_columns) {
    environment[[column]] <- 0
  }
  environment$use_aerosol[c(1, 4)] <- 1
  environment$any_treated_weeds[2] <- 1

  cohort <- data.frame(
    subject_id = "001",
    diagnosis_year = 2024
  )

  result <- build_environment_binary_features(environment, cohort)

  expect_true("use_aerosol_study_years_early_life" %in% names(result$exposure_windows))
  expect_true("use_aerosol_study_years_rest_of_life" %in% names(result$exposure_windows))
  expect_true("use_aerosol_study_years_whole_life" %in% names(result$exposure_windows))
  expect_true("use_aerosol_5y" %in% names(result$within_five_years))
  expect_true(all(result$within_five_years$use_aerosol_5y == "Within 5y"))
  expect_true(all(result$within_five_years$any_treated_weeds_5y == "Not within 5y"))
})

test_that("smoke dosage is summarised for each analysis window", {
  exposure_windows <- data.frame(
    subject_id = rep("001", 3),
    year_in_study = c(0, 3, 9),
    record_date = c("2015-01", "2018-01", "2024-01"),
    diagnosis_year = 2024,
    hours_of_smoke = c("1", "2", "3")
  )
  within_five_years <- exposure_windows
  for (column in environment_exposure_columns()) {
    within_five_years[[paste0(column, "_5y")]] <- "Not within 5y"
  }

  result <- build_smoke_dosage_features(
    exposure_windows,
    within_five_years
  )

  expect_identical(result$hours_of_smoke_early_life_total_dosage, 1)
  expect_identical(result$hours_of_smoke_rest_of_life_total_dosage, 5)
  expect_identical(result$hours_of_smoke_whole_life_total_dosage, 6)
  expect_identical(result$hours_of_smoke_prediagnosis, 3)
})

test_that("sleep location features retain the unique majority location", {
  sleep <- data.frame(
    subject_id = rep("001", 3),
    year_in_study = 0:2,
    record_date = c("2015-01", "2016-01", "2017-01"),
    sleep_location = c("in the house", "in the house", "outside"),
    specific_sleep_location = c("bed", "bed", "kennel")
  )
  environment_smoke <- data.frame(
    subject_id = "001",
    year_in_study = 0,
    diagnosis_year = 2024,
    exposure_year = 2015
  )

  result <- build_sleep_location_features(sleep, environment_smoke)

  expect_identical(result$majority_location, "in the house_YN")
  expect_identical(result$`in the house_YN`, 2L)
  expect_identical(result$outside_YN, 1L)
})

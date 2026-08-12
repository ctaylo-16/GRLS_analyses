source(here::here("R", "reproduction.R"))

test_that("reproduction features use annual heat flags and ever-recorded mating", {
  cohort <- data.frame(
    subject_id = c("F", "M"),
    end_date = c("2022-01-01", "2022-01-01")
  )
  profile <- data.frame(
    subject_id = c("F", "M"),
    sex_status = c("Spayed Female", "Intact Male"),
    spay_neuter_date = c("2020-01-01", NA),
    birth_date = c("2015-01-01", "2015-01-01")
  )
  female <- data.frame(
    subject_id = c("F", "F"),
    year_in_study = c(1, 2),
    record_date = c("2016-01-01", "2017-01-01"),
    heat_last_year = c(1, 1),
    no_pregnancy_last_year = c(0, 1)
  )
  male <- data.frame(
    subject_id = c("M", "M"),
    intact_naturally_bred_last_year = c(0, 1),
    neutered_ever_bred = c(0, 0)
  )

  result <- build_reproduction_features(cohort, profile, female, male)

  expect_identical(result$age_first_heat_years, c("1", "male_dog"))
  expect_identical(result$heat_reporting_years, c("2", "male_dog"))
  expect_identical(result$ever_pregnancy_recorded, c("yes", "not_applicable"))
  expect_identical(result$ever_mating_recorded, c("not_applicable", "yes"))
  expect_identical(result$neuter, c("Neutered", "Entire"))
})

test_that("reproduction features reject a neuter date before birth", {
  profile <- data.frame(
    subject_id = "A",
    sex_status = "Spayed Female",
    spay_neuter_date = "2014-01-01",
    birth_date = "2015-01-01"
  )

  expect_error(
    prepare_reproductive_profile(profile),
    "precedes birth date"
  )
})

test_that("reproduction features reject neutering after cohort endpoint", {
  cohort <- data.frame(subject_id = "M", end_date = "2020-01-01")
  profile <- data.frame(
    subject_id = "M",
    sex_status = "Neutered Male",
    spay_neuter_date = "2021-01-01",
    birth_date = "2015-01-01"
  )
  female <- data.frame(
    subject_id = character(),
    year_in_study = numeric(),
    record_date = character(),
    heat_last_year = numeric(),
    no_pregnancy_last_year = numeric()
  )
  male <- data.frame(
    subject_id = "M",
    intact_naturally_bred_last_year = 0,
    neutered_ever_bred = 0
  )

  expect_error(
    build_reproduction_features(cohort, profile, female, male),
    "follows the cancer cohort endpoint"
  )
})

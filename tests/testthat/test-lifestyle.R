source(here::here("R", "lifestyle.R"))

test_that("lifestyle features retain the unique most frequent role", {
  lifestyle <- data.frame(
    subject_id = c("A", "A", "A", "B", "B", "B", "C"),
    year_in_study = c(0:2, 0:2, 0),
    record_date = c(
      "2015-01",
      "2016-01",
      "2017-01",
      "2015-01",
      "2016-01",
      "2017-01",
      "2015-01"
    ),
    lifestyle = c(
      "agility",
      "agility",
      "companion/pet",
      "service dog",
      "service dog",
      "therapy dog",
      "companion/pet"
    )
  )

  result <- build_lifestyle_features(lifestyle)

  expect_identical(result$main_lifestyle, c("agility", "service dog", "companion/pet"))
  expect_identical(
    result$main_lifestyle_category,
    c("competitive", "working", "companion/pet")
  )
})

test_that("lifestyle features reject tied main roles", {
  lifestyle <- data.frame(
    subject_id = c("A", "A"),
    year_in_study = c(0, 1),
    record_date = c("2015-01", "2016-01"),
    lifestyle = c("agility", "companion/pet")
  )

  expect_error(
    build_lifestyle_features(lifestyle),
    "No unique main lifestyle"
  )
})

test_that("window labels are standardised without coercing numeric columns", {
  cohort <- data.frame(
    subject_id = "A",
    exposure = "Within 0,1,2 years",
    count = 2
  )

  result <- standardise_exposure_window_labels(cohort)

  expect_identical(result$exposure, "Within early life")
  expect_identical(result$count, 2)
})

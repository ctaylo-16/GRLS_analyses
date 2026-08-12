source(here::here("R", "final_dataset.R"))

test_that("final datasets use explicit feature missingness and retain date NAs", {
  input <- data.frame(
    X = 1,
    subject_id = "A",
    record_date.x = "2020-01-01",
    lymphoma = 0,
    end_date = "2024-12-01",
    withdrawn_date = NA_character_,
    spay_neuter_date2 = as.Date(NA),
    MDI_quintile = NA_integer_,
    avg_weight_5y_prior_endpoint = NA_real_,
    lifestyle = NA_character_
  )

  result <- finalise_cancer_dataset(input, "lymphoma")

  expect_false(any(c("X", "record_date.x") %in% names(result)))
  expect_true(is.na(result$withdrawn_date))
  expect_true(is.na(result$spay_neuter_date2))
  expect_identical(result$MDI_quintile, "records_not_available")
  expect_identical(
    result$avg_weight_5y_prior_endpoint,
    "records_not_available"
  )
  expect_identical(result$lifestyle, "records_not_available")
})

test_that("final datasets remain one row per dog", {
  input <- data.frame(
    subject_id = c("A", "A"),
    lymphoma = c(0, 0)
  )

  expect_error(
    finalise_cancer_dataset(input, "lymphoma"),
    "not one row per subject_id"
  )
})

test_that("final datasets require the named cancer outcome", {
  input <- data.frame(subject_id = "A", lymphoma = 0)

  expect_error(
    finalise_cancer_dataset(input, "had_MCT"),
    "case column is missing"
  )
})

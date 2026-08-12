source(here::here("Code", "GRLS_functions.R"))

testthat::test_that("first exposure uses the earliest exposed date per dog", {
  input <- data.frame(
    subject_id = c("a", "a", "a", "b"),
    exposed = c(0, 1, 1, 0),
    record_date = as.Date(
      c("2020-01-01", "2020-03-01", "2020-02-01", "2020-01-01")
    )
  )

  result <- first_exposure_point(input, exposed, record_date)

  testthat::expect_equal(
    result$exposed_first_exposure_date,
    as.Date(c("2020-02-01", "2020-02-01", "2020-02-01", NA))
  )
})

testthat::test_that("mode calculations reject ties", {
  input <- data.frame(
    subject_id = c("a", "a"),
    study_year = c(1, 2),
    location = c("inside", "outside")
  )

  testthat::expect_error(
    check_exposure_mode(input, "location", 1:2, "study_year"),
    "Tied mode"
  )
})

testthat::test_that("majority location rejects ambiguous dogs", {
  input <- data.frame(
    subject_id = c("a", "a"),
    inside = c(1, 0),
    outside = c(0, 1)
  )

  testthat::expect_error(
    majority_location(input, c("inside", "outside")),
    "No unique majority"
  )
})

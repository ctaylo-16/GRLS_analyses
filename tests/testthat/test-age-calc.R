source(here::here("Code", "functions", "age_calc_funct.R"))

testthat::test_that("age_calc preserves row grain and returns numeric durations", {
  input <- data.frame(
    subject_id = c("a", "a", "b"),
    start = c("2020-01-01", "2020-02-01", NA),
    end = c("2020-01-08", "2020-02-29", NA)
  )

  result <- age_calc(input, "subject_id", "start", "end")

  testthat::expect_equal(nrow(result), nrow(input))
  testthat::expect_type(result$time_between_start_end_days, "double")
  testthat::expect_equal(result$time_between_start_end_days, c(7, 28, NA))
})

testthat::test_that("age_calc rejects negative and invalid dates", {
  negative <- data.frame(
    subject_id = "a",
    start = "2020-01-02",
    end = "2020-01-01"
  )
  invalid <- data.frame(
    subject_id = "a",
    start = "not a date",
    end = "2020-01-01"
  )

  testthat::expect_error(
    age_calc(negative, "subject_id", "start", "end"),
    "precedes"
  )
  testthat::expect_error(
    age_calc(invalid, "subject_id", "start", "end"),
    "Invalid date"
  )
})

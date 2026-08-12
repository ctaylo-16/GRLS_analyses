source(here::here("R", "comorbidities.R"))

testthat::test_that("condition frames are aligned by key rather than row order", {
  cardio <- data.frame(
    subject_id = c("a", "b"),
    year_in_study = c(1, 1),
    to_date = c(0, 0),
    relationship_category = "dog",
    record_date = c("2020-01-01", "2020-01-02"),
    heart_condition = c(1, 0)
  )
  dental <- data.frame(
    subject_id = c("b", "a"),
    year_in_study = c(1, 1),
    to_date = c(0, 0),
    relationship_category = "dog",
    record_date = c("2020-01-02", "2020-01-01"),
    dental_condition = c(1, 0)
  )

  result <- combine_condition_frames(list(cardio = cardio, dental = dental))

  testthat::expect_equal(result$subject_id, c("a", "b"))
  testthat::expect_equal(result$dental_condition, c(0, 1))
})

testthat::test_that("condition assembly rejects different or duplicated keys", {
  reference <- data.frame(
    subject_id = c("a", "b"),
    year_in_study = c(1, 1),
    to_date = c(0, 0),
    relationship_category = "dog",
    record_date = c("2020-01-01", "2020-01-02"),
    condition_a = c(1, 0)
  )
  different <- transform(
    reference,
    subject_id = c("a", "c"),
    condition_b = condition_a
  )
  different$condition_a <- NULL
  duplicated <- transform(
    reference[c(1, 1), ],
    condition_b = condition_a
  )
  duplicated$condition_a <- NULL

  testthat::expect_error(
    combine_condition_frames(list(a = reference, b = different)),
    "same keys"
  )
  testthat::expect_error(
    combine_condition_frames(list(a = reference, b = duplicated)),
    "duplicated key"
  )
})

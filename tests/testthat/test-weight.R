source(here::here("R", "weight.R"))

test_that("weight features summarise the endpoint window", {
  cohort <- data.frame(
    subject_id = "A",
    year_in_study_diagnosis_or_final_record_year = 6,
    age_at_final_date = 8
  )
  exams <- data.frame(
    subject_id = rep("A", 7),
    year_in_study = 0:6,
    height = 20:26,
    weight = 50:56,
    purina_body_condition_score = c(4, 4, 5, 5, 5, 5, 5)
  )

  result <- build_weight_features(cohort, exams)

  expect_identical(result$avg_weight_5y_prior_endpoint, 53.5)
  expect_identical(result$median_weight_5y_prior_endpoint, 53.5)
  expect_equal(result$avg_weight_5y_prior_endpoint_kg, 53.5 * 0.453)
  expect_equal(result$avg_height_5y_prior_endpoint_cm, 23.5 * 2.54)
  expect_identical(result$mode_purina_BCS_5y_prior_endpoint, "5")
})

test_that("weight features require five examination rows for primary summaries", {
  cohort <- data.frame(
    subject_id = "A",
    year_in_study_diagnosis_or_final_record_year = 3,
    age_at_final_date = 8
  )
  exams <- data.frame(
    subject_id = rep("A", 3),
    year_in_study = 1:3,
    height = c(20, 21, 22),
    weight = c(50, 51, 52),
    purina_body_condition_score = c(4, 4, 5)
  )

  result <- build_weight_features(cohort, exams)

  expect_true(is.na(result$avg_weight_5y_prior_endpoint))
  expect_identical(result$median_weight_5y_prior_endpoint_any_records, 51)
})

test_that("tied BCS modes use the maximum value", {
  cohort <- data.frame(
    subject_id = "A",
    year_in_study_diagnosis_or_final_record_year = 2,
    age_at_final_date = 8
  )
  exams <- data.frame(
    subject_id = c("A", "A"),
    year_in_study = c(1, 2),
    height = c(20, 20),
    weight = c(50, 50),
    purina_body_condition_score = c(4, 5)
  )

  result <- build_weight_features(cohort, exams)

  expect_identical(result$mode_purina_BCS_5y_prior_endpoint, "5")
})

test_that("adult converted summaries exclude endpoints before 18 months", {
  cohort <- data.frame(
    subject_id = "A",
    year_in_study_diagnosis_or_final_record_year = 1,
    age_at_final_date = 1.4
  )
  exams <- data.frame(
    subject_id = rep("A", 5),
    year_in_study = rep(1, 5),
    height = rep(20, 5),
    weight = rep(50, 5),
    purina_body_condition_score = rep(5, 5)
  )

  result <- build_weight_features(cohort, exams)

  expect_true(is.na(result$avg_weight_5y_prior_endpoint_kg))
  expect_true(is.na(result$avg_height_5y_prior_endpoint_cm))
})

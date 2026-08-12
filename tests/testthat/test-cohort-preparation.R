source(here::here("R", "cohort_preparation.R"))

test_that("cancer cohort endpoints are represented at annual and monthly levels", {
  cohort <- data.frame(
    subject_id = c("A", "B"),
    record_date = c("unused", "unused"),
    end_date = c("2024-12-17", "2025-05-31")
  )

  result <- prepare_cancer_cohort(cohort)

  expect_false("record_date" %in% names(result))
  expect_identical(result$diagnosis_year, c(2024, 2025))
  expect_identical(result$diagnosis_year_month, c("2024-12", "2025-05"))
  expect_identical(
    format(result$diagnosis_year_month_date2, "%Y-%m-%d"),
    c("2024-12-01", "2025-05-01")
  )
})

test_that("deprivation features join by subject ID", {
  cohort <- data.frame(subject_id = c("B", "A"), value = c(1, 2))
  deprivation <- data.frame(
    subject_id = c("A", "B"),
    MDI_quintile = c(2, 4),
    owner_MDI_quintile = c(3, 5),
    unused = c("x", "y")
  )

  result <- add_deprivation_features(cohort, deprivation)

  expect_identical(result$subject_id, c("B", "A"))
  expect_identical(result$MDI_quintile, c(4, 2))
  expect_identical(result$owner_MDI_quintile, c(5, 3))
  expect_false("unused" %in% names(result))
})

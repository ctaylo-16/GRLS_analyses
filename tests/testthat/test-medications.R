source(here::here("R", "medications.R"))

test_that("last study year selection is per dog", {
  medications <- data.frame(
    subject_id = c(rep("A", 6), "B", "B"),
    year_in_study = c(1:6, 1, 2)
  )

  result <- select_last_study_years(medications, 5)

  expect_setequal(result$year_in_study[result$subject_id == "A"], 2:6)
  expect_setequal(result$year_in_study[result$subject_id == "B"], 1:2)
})

test_that("medication counts are categorised and widened", {
  medications <- data.frame(
    subject_id = c(rep("A", 4), rep("B", 3), rep("C", 2), "D", "A"),
    active_ingredient_groups = c(rep("drug", 10), "other")
  )

  result <- summarise_medication_quartiles(
    medications,
    suffix = "lifetime",
    missing_value = "medication_not_prescribed"
  )

  drug_categories <- result$categorised |>
    dplyr::filter(active_ingredient_groups == "drug") |>
    dplyr::arrange(subject_id)

  expect_identical(drug_categories$quartile, c("Q4", "Q3", "Q2", "Q1"))
  expect_true(all(grepl("_lifetime$", names(result$wide)[-1])))
  expect_identical(
    result$wide$quartile_other_lifetime[result$wide$subject_id == "B"],
    "medication_not_prescribed"
  )
})

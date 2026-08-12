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

test_that("specific medication flags use all named text fields", {
  medications <- data.frame(
    subject_id = c("A", "A", "B"),
    medication_name = c("Rimadyl", NA, "other"),
    medication_name_specify = c(NA, "PANACUR", NA),
    medication_ingredients = c("carprofen", NA, "prednisone"),
    stringsAsFactors = FALSE
  )

  result <- summarise_specific_medication_flags(
    medications,
    medications[2:3, ],
    list(NSAID = c("rimadyl"), fenbendazole = c("panacur"))
  ) |>
    dplyr::arrange(subject_id)

  expect_identical(result$NSAID_ever, c(1L, 0L))
  expect_identical(result$NSAID_5y, c(0L, 0L))
  expect_identical(result$fenbendazole_ever, c(1L, 0L))
  expect_identical(result$fenbendazole_5y, c(1L, 0L))
})

test_that("ingredient counts distinguish no use from no records", {
  medications <- data.frame(
    subject_id = c("A", "A", "A", "B"),
    medication_ingredients = c("carprofen", "carprofen, other", "other", "other"),
    stringsAsFactors = FALSE
  )
  categories <- function(count) {
    dplyr::case_when(
      count == 0L ~ "no_NSAID_admin",
      count == 1L ~ "Q1(1)",
      count == 2L ~ "Q2(1-2)",
      count == 3L ~ "Q3(2-3)",
      count > 3L ~ "Q4(3-8)"
    )
  }

  features <- summarise_medication_count_feature(
    medications,
    "carprofen",
    "NSAID",
    categories
  )
  cohort <- data.frame(subject_id = c("A", "B", "C"))
  result <- join_medication_features(cohort, features)

  expect_identical(result$NSAID_use, c("NSAID_yes", "NSAID_no", "no_medication_records_for_this_time_period"))
  expect_identical(result$NSAID_5y_amount, c("Q2(1-2)", "no_NSAID_admin", "no_medication_records_for_this_time_period"))
})

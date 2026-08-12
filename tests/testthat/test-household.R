source(here::here("Code", "GRLS_functions.R"))
source(here::here("R", "household.R"))

test_that("household records use curated fuel, pipe, and water groups", {
  house <- data.frame(
    subject_id = c("A", "B"),
    year_in_study = c(0, 0),
    house_age = c(10, 20),
    water_source = c("Other", "Well"),
    water_source_other = c("Bottled spring", ""),
    pipes_copper_metal = c(1, 0),
    sec_pipes_copper_metal = c(0, 0),
    pipes_pvc_plastic = c(0, 1),
    sec_pipes_pvc_plastic = c(0, 0),
    pipes_other_specify = c("", ""),
    sec_pipes_other_specify = c("", ""),
    heating_fuel_primary = c("Other", "Wood"),
    heating_fuel_primary_other = c("electric", ""),
    heating_fuel_secondary = c("None", "None"),
    heating_fuel_secondary_other = c("", ""),
    cooking_fuel_primary = c("Electric", "Electric"),
    cooking_fuel_primary_other = c("", "")
  )

  result <- prepare_household_records(house)

  expect_identical(result$water_source, c("bottled", "well"))
  expect_identical(result$heating_fuel_primary, c("electric", "wood"))
  expect_identical(result$pipes_metal_any, c("1", "0"))
  expect_identical(result$pipes_plastic_any, c("0", "1"))
})

test_that("household features use named windows and grouped water", {
  house <- data.frame(
    subject_id = rep("A", 3),
    year_in_study = 0:2,
    record_date = c("2015-01", "2016-01", "2017-01"),
    house_age = c(10, 11, 12),
    area_type = rep("Suburban", 3),
    house_type = rep("Single family", 3),
    water_source = rep("Other", 3),
    water_source_other = rep("Filtered municipal", 3),
    pipes_copper_metal = rep(1, 3),
    sec_pipes_copper_metal = rep(0, 3),
    pipes_pvc_plastic = rep(0, 3),
    sec_pipes_pvc_plastic = rep(0, 3),
    pipes_other_specify = rep("", 3),
    sec_pipes_other_specify = rep("", 3),
    heating_fuel_primary = rep("Electric", 3),
    heating_fuel_primary_other = rep("", 3),
    heating_fuel_secondary = rep("None", 3),
    heating_fuel_secondary_other = rep("", 3),
    cooking_fuel_primary = rep("Electric", 3),
    cooking_fuel_primary_other = rep("", 3),
    cooking_fuel_secondary = rep("None", 3)
  )
  location <- data.frame(
    subject_id = rep("A", 3),
    year_in_study = 0:2,
    travel_mode = rep("Car", 3),
    travel_mode_other = rep("", 3),
    country = rep("USA", 3),
    region_name = rep("West", 3),
    state = rep("CA", 3),
    zip = rep("90210", 3)
  )
  cohort <- data.frame(
    subject_id = "a",
    year_in_study_diagnosis_or_final_record_year = 3
  )

  result <- build_household_features(house, location, cohort)

  expect_identical(result$water_source_mode_early_life, "municipal")
  expect_identical(result$water_source_mode_whole_life, "municipal")
  expect_identical(result$water_source_mode_5yrs_prior, "municipal")
  expect_identical(result$region_name_mode_whole_life, "west")
  expect_identical(result$avg_house_age, 11)
})

test_that("tied household modes are retained as multiple", {
  input <- data.frame(
    subject_id = c("A", "A"),
    year_in_study = c(0, 1),
    area_type = c("rural", "urban")
  )

  result <- add_household_mode_windows(input, "area_type")

  expect_true(all(result$area_type_mode_early_life == "multiple"))
  expect_true(all(result$area_type_mode_whole_life == "multiple"))
})

test_that("tied recent household modes are retained as multiple", {
  input <- data.frame(
    subject_id = c("A", "A"),
    year_in_study = c(1, 2),
    endpoint_year = c(3, 3),
    area_type = c("rural", "urban")
  )

  result <- add_household_recent_modes(
    input,
    columns = "area_type",
    endpoint_column = "endpoint_year",
    number_of_years = 5
  )

  expect_true(all(result$area_type_mode_5yrs_prior == "multiple"))
})

test_that("recent household modes require an explicit endpoint year", {
  input <- data.frame(
    subject_id = "A",
    year_in_study = 0,
    endpoint_year = NA_real_,
    area_type = "rural"
  )

  expect_error(
    add_household_recent_modes(
      input,
      columns = "area_type",
      endpoint_column = "endpoint_year",
      number_of_years = 5
    ),
    "Expected exactly one non-missing 'endpoint_year'"
  )
})

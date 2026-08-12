source(here::here("R", "deprivation.R"))

test_that("US ZIP codes are reduced to their five-digit numeric form", {
  expect_identical(
    normalise_us_zip(c("12345-6789", "01234", NA_character_)),
    c(12345, 1234, NA_real_)
  )
})

test_that("first mode preserves the established tie rule", {
  expect_identical(first_mode(c("10000", "20000", "10000")), "10000")
  expect_identical(first_mode(c("20000", "10000")), "20000")
})

test_that("clinic and modal owner ZIP codes receive MDI values", {
  clinics <- data.frame(
    subject_id = c("A", "B"),
    clinic_name = c("Clinic A", "Clinic B"),
    postal_code = c("10000-1234", "20000")
  )
  owner_addresses <- data.frame(
    subject_id = c("A", "A", "A", "B"),
    primary_zip_code = c("30000", "30000", "40000", "50000")
  )
  zip_lookup <- data.frame(
    zip = c(10000, 20000, 30000, 40000, 50000),
    county_fips = 1:5
  )
  mdi <- data.frame(
    County = 1:5,
    MDI.rate = 1:5
  )

  result <- build_deprivation_dataset(
    clinics,
    owner_addresses,
    zip_lookup,
    mdi
  )

  expect_identical(
    names(result),
    c(
      "subject_id",
      "clinic_name",
      "MDI.rate",
      "MDI_quintile",
      "owner_MDI.rate",
      "owner_MDI_quintile"
    )
  )
  expect_identical(result$MDI.rate, c(1L, 2L))
  expect_identical(result$owner_MDI.rate, c(3L, 5L))
})

source(here::here("R", "project_setup.R"))

test_that("Quarto setup fails clearly when the executable is unavailable", {
  expect_error(check_quarto(""), "Quarto is not installed")
})

test_that("Quarto setup accepts an available executable path", {
  expect_invisible(check_quarto("C:/Program Files/Quarto/bin/quarto.exe"))
})

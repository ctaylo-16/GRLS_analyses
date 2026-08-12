if (!requireNamespace("here", quietly = TRUE)) {
  stop("The 'here' package is required to run tests.", call. = FALSE)
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("The 'testthat' package is required to run tests.", call. = FALSE)
}

testthat::test_dir(here::here("tests", "testthat"))

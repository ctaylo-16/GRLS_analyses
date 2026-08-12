# Project-level setup and fail-fast dependency checks.

grls_required_packages <- c(
  "broom", "cardx", "condsurv", "crosstable", "dplyr", "epiDisplay",
  "ezfun", "finalfit", "flextable", "forcats", "ggplot2", "ggsurvfit",
  "gtable", "gtsummary", "here", "kableExtra", "lme4", "lubridate",
  "officer", "patchwork", "pROC", "pscl", "purrr", "RColorBrewer",
  "readr", "stringr", "survival", "survminer", "tibble", "tidycmprsk",
  "tidyr", "tidyverse", "writexl"
)

check_required_packages <- function(packages = grls_required_packages) {
  installed <- vapply(
    packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )

  if (!all(installed)) {
    stop(
      "Missing required R packages: ",
      paste(packages[!installed], collapse = ", "),
      ". Restore the project environment before running an analysis.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

check_project_layout <- function() {
  required_directories <- c("Code", "Data", "Output", "R")
  missing_directories <- required_directories[
    !dir.exists(here::here(required_directories))
  ]

  if (length(missing_directories) > 0) {
    stop(
      "Required project directories are missing: ",
      paste(missing_directories, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

check_project_setup <- function() {
  check_project_layout()
  check_required_packages()
  invisible(TRUE)
}

grls_input_path <- function(...) {
  path <- here::here("Data", ...)

  if (!file.exists(path)) {
    stop("Required input file does not exist: ", path, call. = FALSE)
  }

  path
}

grls_output_path <- function(...) {
  output_directory <- here::here("Output")

  if (!dir.exists(output_directory)) {
    stop("Required output directory does not exist: ", output_directory, call. = FALSE)
  }

  here::here("Output", ...)
}

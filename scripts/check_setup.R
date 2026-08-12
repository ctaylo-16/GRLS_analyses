if (!requireNamespace("here", quietly = TRUE)) {
  stop("The 'here' package must be installed before checking the project.", call. = FALSE)
}

source(here::here("R", "project_setup.R"))
check_project_setup()

message("GRLS analysis project setup is complete.")

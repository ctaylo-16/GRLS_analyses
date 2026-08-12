# Add row-level elapsed time between two date columns.
age_calc <- function(original_df, patientid_col, first_date_col, second_date_col) {
  required_columns <- c(patientid_col, first_date_col, second_date_col)
  missing_columns <- setdiff(required_columns, names(original_df))

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  parse_date <- function(value, column_name) {
    parsed <- tryCatch(
      as.Date(value),
      error = function(error) {
        stop("Invalid date value in column '", column_name, "'.", call. = FALSE)
      }
    )
    supplied <- !is.na(value) & as.character(value) != ""

    if (any(supplied & is.na(parsed))) {
      stop("Invalid date value in column '", column_name, "'.", call. = FALSE)
    }

    parsed
  }

  first_date <- parse_date(original_df[[first_date_col]], first_date_col)
  second_date <- parse_date(original_df[[second_date_col]], second_date_col)
  elapsed_days <- as.numeric(second_date - first_date)

  if (any(elapsed_days < 0, na.rm = TRUE)) {
    stop(
      "The second date precedes the first date for at least one row.",
      call. = FALSE
    )
  }

  prefix <- paste0("time_between_", first_date_col, "_", second_date_col)
  original_df[[paste0(prefix, "_days")]] <- elapsed_days
  original_df[[paste0(prefix, "_weeks")]] <- elapsed_days / 7
  original_df[[paste0(prefix, "_years")]] <- elapsed_days / 365.25
  original_df[[paste0(prefix, "_months")]] <- elapsed_days / (365.25 / 12)
  original_df
}

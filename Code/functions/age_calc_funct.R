age_calc <- function(original_df, patientid_col, first_date_col, second_date_col) {
  
  # Convert column names to symbols
  patientid_sym <- sym(patientid_col)
  first_date_sym <- sym(first_date_col)
  second_date_sym <- sym(second_date_col)
  
  # Dynamic column names
  days_col_name <- paste0("time_between_", first_date_col, "_", second_date_col, "_days")
  weeks_col_name <- paste0("time_between_", first_date_col, "_", second_date_col, "_weeks")
  years_col_name <- paste0("time_between_", first_date_col, "_", second_date_col, "_years")
  months_col_name <- paste0("time_between_", first_date_col, "_", second_date_col, "_months")
  
  new_df <- original_df %>%
    filter(!is.na(!!first_date_sym), !is.na(!!second_date_sym)) %>%
    mutate(
      !!first_date_sym := as.Date(!!first_date_sym),
      !!second_date_sym := as.Date(!!second_date_sym),
      !!days_col_name := as.numeric(difftime(!!second_date_sym, !!first_date_sym, units = "days")),
      !!weeks_col_name := round(!!sym(days_col_name) / 7, 2),
      !!years_col_name := round(!!sym(weeks_col_name) / 52, 2),
      !!months_col_name := round(!!sym(years_col_name) * 12, 1)
    ) %>%
    mutate(across(all_of(c(days_col_name, weeks_col_name, years_col_name, months_col_name)),
                  ~ replace_na(as.character(.), "not recorded"))) %>%
    select(!!patientid_sym, all_of(c(days_col_name, weeks_col_name, years_col_name, months_col_name)))
  
  results_df <- left_join(original_df, new_df, by = patientid_col)
  
  return(results_df)
}

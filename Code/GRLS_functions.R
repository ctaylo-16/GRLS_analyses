##Current functions to be used in GRLS 
# explanatory file currently at this location - "C:/Users/ctaylor18/GitHub/GRLS_analyses/Code/240103_GRLS_functions.qmd"



#create first exposure date function
#3 inputs required
first_exposure_point <- function(data, exposure_column, record_date) {
  #dynamic column naming code to create a column name depending on what exposure column is called
  col_name <- paste0(quo_name(enquo(exposure_column)), "_", "first_exposure_date")
  
  data %>%
    mutate(
      #!! and := used in dynamic col naming. if the dog is exposed (==1) and the record date is not NA then find earliest record date , else record NA
      !!col_name := ifelse({{ exposure_column }} == 1,
                           ifelse(!is.na(first(record_date, order_by = record_date)),
                                  format(first(record_date, order_by = record_date), "%Y-%m-%d"),
                                  NA),
                           NA)
    )
}


#requires 3 inputs
first_exposure_age <- function(data, first_exposure_point_col, birth_date_col) {
  # code checks that the columns listed are in df or else stops and prints an error message
  required_cols <- c(first_exposure_point_col, birth_date_col)
  
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more required columns not found in the data frame.")
  }
  #create new columns in data that calculate difference between first exposure date and birth date and then turn into years or months values
  data %>%
    mutate(
      first_exposure_point_col = as.POSIXct(.data[[first_exposure_point_col]], format = "%Y-%m-%d"),
      birth_date_col = as.POSIXct(.data[[birth_date_col]], format = "%Y-%m-%d"),
      first_exposure_age = as.numeric(first_exposure_point_col - birth_date_col, units = "days"),
      first_exposure_age_years = first_exposure_age / 365.25,
      first_exposure_age_months = first_exposure_age / 12
    )
}
#Calculate age at first exposure to a variable
#this function creates dynamic column names so can apply to all variables and will have what the 'exposure' is in col names
first_exposure_age2 <- function(data, first_exposure_point_col, birth_date_col) {
  # code checks that the columns listed are in df or else stops and prints an error message
  required_cols <- c(first_exposure_point_col, birth_date_col)
  
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more required columns not found in the data frame.")
  }
  
  # Extract the dynamic column name
  dynamic_col_name <- paste0(quo_name(enquo(first_exposure_point_col)), "_age")
  
  data %>%
    mutate(
      first_exposure_point_col = as.POSIXct(.data[[first_exposure_point_col]], format = "%Y-%m-%d"),
      birth_date_col = as.POSIXct(.data[[birth_date_col]], format = "%Y-%m-%d"),
      !!dynamic_col_name := as.numeric(first_exposure_point_col - birth_date_col, units = "days"),
      !!paste0(dynamic_col_name, "_years") := .data[[dynamic_col_name]] / 365.25,
    )
}


#this function creates dynamic column names so can apply to all variables and will have what the 'exposure' is in col names
first_exposure_age2 <- function(data, first_exposure_point_col, birth_date_col) {
  # code checks that the columns listed are in df or else stops and prints an error message
  required_cols <- c(first_exposure_point_col, birth_date_col)
  
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more required columns not found in the data frame.")
  }
  
  # Extract the dynamic column name
  dynamic_col_name <- paste0(quo_name(enquo(first_exposure_point_col)), "_age")
  
  data %>%
    mutate(
      first_exposure_point_col = as.POSIXct(.data[[first_exposure_point_col]], format = "%Y-%m-%d"),
      birth_date_col = as.POSIXct(.data[[birth_date_col]], format = "%Y-%m-%d"),
      !!dynamic_col_name := as.numeric(first_exposure_point_col - birth_date_col, units = "days"),
      !!paste0(dynamic_col_name, "_years") := .data[[dynamic_col_name]] / 365.25,
    )
}

#number of times an exposure happens before an event
# requires 4 inputs
num_exposures <- function(data,exposure_column,record_date,date_of_event){
  
  #this creates a column name dynamically in function using exposure column name, underscore then exposure name
  col_name <- paste0(quo_name(enquo(exposure_column)), "_", "num_exposures")
  #code checks that the columns listed are in df or else stops and prints an error message
  required_cols <- c(exposure_column, record_date,date_of_event)
  if (!all(required_cols %in% colnames(data))) {
    stop("One or more required columns not found in the data frame.")}
  
  data %>%
    group_by(subject_id) %>%
    #new col which sums the number of rows per dog with a 1 that happen before a specific event happened
    mutate(!!col_name := sum((.data[[exposure_column]] == 1) & (.data[[record_date]] < .data[[date_of_event]]), na.rm = TRUE))
}

#Calculate the year for a specific number of years prior to diagnosis date
#3 inputs
years_prior_to <- function(data, event, num_years) {
  col_name <- paste0("prediagnosis", num_years, "year")
  # calculate the date of event minus X num of years 
  data %>% 
    mutate({{ col_name }} := (as.numeric({{ event }} - num_years)))
}


# Example usage
#end_HSA <- years_prior_to(end_HSA,diagnosis_year,5)

# were they exposed in previous X years
#4 inputs
check_exposure <- function(data, columns_to_check, diagnosis_year, exposure_years) {
  data %>%
    group_by(subject_id) %>%
    #check for each dog for each column in list if there is a 1 for exposure recorded and that this is within the desired range between diagnosis and exposure
    mutate(across(all_of(columns_to_check), 
                  list(~ if_else(
                    any(. == 1 & (diagnosis_year - exposure_year) <= exposure_years),
                    #within = 1, not within = 0
                    paste0("Within ", exposure_years, "y"),
                    paste0("Not within ", exposure_years, "y")
                  )), 
                  .names = "{.col}_{exposure_years}y"))
}


#variant on check_exposure to apply it to different subsets of years to just get a Y N exposed eg. was dog exposed to aerosols in SY 0-2
check_exposure2 <- function(data, columns_to_check, study_years, year_column) {
  suffix <- paste(study_years, collapse = "_")
  data %>%
    group_by(subject_id) %>%
    mutate(across(
      all_of(columns_to_check),
      ~ if_else(
        any(. == 1 & !!sym(year_column) %in% study_years),
        paste0("Within ", paste(study_years, collapse = ","), " years"),
        paste0("Not within ", paste(study_years, collapse = ","), " years")
      ),
      .names = "{.col}_study_years_{suffix}"
    )) %>%
    # Calculate the count of 1s within the study years for each column = sum of exposure
    mutate(across(
      all_of(columns_to_check),
      ~ sum(. == 1 & !!sym(year_column) %in% study_years, na.rm = TRUE),
      .names = "{.col}_sum_{suffix}"
    )) %>%
    ungroup()
}





# Example usage:
#columns_to_check <- c("use_aerosol", "use_air_cleaner", "use_hepa_filter",
#                     "use_moth_balls", "use_incense_or_candles", "smoke_exposure",
#                   "any_treated_weeds", "any_treated_insects", "any_treated_fertilizer")

#exposure_years <- 3

#result_df <- check_exposure(merged_df, columns_to_check, diagnosis_year, exposure_years)


#3rd variant on check exposure function to check if exposed in X years. for categorical variables - what was the mode class in X years

check_exposure_mode <- function(data, columns_to_check, study_years, year_column) {
  suffix <- paste(study_years, collapse = "_")
  
  data %>%
    group_by(subject_id) %>%
    mutate(
      # Determine exposure status for the study years
      across(
        all_of(columns_to_check),
        ~ if_else(
          any(. == 1 & !!sym(year_column) %in% study_years),
          paste0("Within ", paste(study_years, collapse = ","), " years"),
          paste0("Not within ", paste(study_years, collapse = ","), " years")
        ),
        .names = "{.col}_study_years_{suffix}"
      ),
      
      # Calculate the count of 1s within the study years for each column
      across(
        all_of(columns_to_check),
        ~ sum(. == 1 & !!sym(year_column) %in% study_years, na.rm = TRUE),
        .names = "{.col}_sum_{suffix}"
      ),
      
      # Determine the mode string within the study years
      across(
        all_of(columns_to_check),
        ~ {
          values <- .[!!sym(year_column) %in% study_years]
          if (length(values) == 0) return(NA_character_)
          mode_val <- names(which.max(table(values)))
          mode_val
        },
        .names = "{.col}_mode_{suffix}"
      )
    ) %>%
    ungroup()
}




#function to calculate dosage/amount exposed to


exposure_dosage <- function(data, column_of_exposure, record_date, date_of_event) {
  col_name <- paste0(column_of_exposure, "_prediagnosis")
  
  data %>%
    group_by(subject_id) %>%
    mutate(!!col_name := sum(ifelse(.data[[record_date]] < .data[[date_of_event]], .data[[column_of_exposure]], 0), na.rm = TRUE)) %>%
    ungroup()
}



###modification of exposure_dosage function to calculate amount of exposure using year_in_study rather than time between 2 dates as above
exposure_dosage2 <- function(data, column_of_exposure, study_years) {
  # Create a dynamic column name based on exposure column and study years
  col_name <- paste0(column_of_exposure, "_", paste(study_years, collapse = "_"), "_total_dosage")
  
  data %>%
    group_by(subject_id) %>%
    mutate(!!col_name := sum(ifelse(year_in_study %in% study_years, .data[[column_of_exposure]], 0), na.rm = TRUE)) %>%
    ungroup()
}





#main location -do they ever sleep at X location
#3 inputs
split_column <- function(data, original_column, outcomes) {
  #create a new column name where the name of the outcome column and a seperater of_ are made
  outcome_columns <- paste(outcomes, sep = "_")
  #for each outcome in the list of outcomes if it ever = 1 for the dog then will be a 1 in the new column
  for (outcome in outcomes) {
    data <- mutate(data, !!paste(outcome, "YN", sep = "_") := if_else(original_column == outcome, "1", "0"))
  }
  
  return(data)
}


#majority location function
# 2 inputs
majority_location <- function(data,columns_to_count) {
  data %>%
    group_by(subject_id)%>%
    #count the number of 1s for each dog for each column seperately
    summarise(across(all_of(columns_to_count), ~sum(. == 1, na.rm = TRUE))) %>%
    #majority location is the column with biggest count in the previous step, a +1 is added for the indexing between relative df created in summarise vs actual df the fucntion is applied to
    mutate(majority_location = names(.)[max.col(select(., all_of(columns_to_count)))+1]) %>%
    ungroup()
}
#same function but dynamic column labelling - cant make this work currently
majority_location2 <- function(data, columns_to_count) {
  col_name <- paste0(columns_to_count, "_lifetime_exposure")
  
  data %>%
    group_by(subject_id) %>%
    summarise(across(all_of(columns_to_count), ~sum(. == 1, na.rm = TRUE))) %>%
    mutate(!!col_name := names(.data)[max.col(select(., all_of(columns_to_count)))+1])
}




### function for using dictionary to remap terms
map_frequency_to_df <- function(data, column_to_map, mapping_list, new_column_name = "mapped_frequency") {
  # Check if the specified column exists
  if (!column_to_map %in% names(data)) {
    stop("The specified column does not exist in the data frame.")
  }
  
  # Define the mapping function
  map_frequency <- function(frequency, mapping_list) {
    match <- sapply(mapping_list, function(x) frequency %in% x)
    if (any(match)) {
      return(names(mapping_list)[which(match)])
    } else {
      return("unspecified")
    }
  }
  
  # Apply the mapping function to the specified column and create the new column
  data[[new_column_name]] <- sapply(data[[column_to_map]], map_frequency, mapping_list = mapping_list)
  
  # Return the updated data frame
  return(data)
}



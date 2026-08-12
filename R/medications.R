select_last_study_years <- function(medications, number_of_years) {
  medications |>
    dplyr::group_by(subject_id) |>
    dplyr::filter(
      year_in_study %in%
        sort(unique(year_in_study), decreasing = TRUE)[seq_len(number_of_years)]
    ) |>
    dplyr::ungroup()
}

summarise_medication_quartiles <- function(
    medications,
    suffix,
    missing_value) {
  counts <- medications |>
    dplyr::group_by(subject_id, active_ingredient_groups) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  summary <- counts |>
    dplyr::group_by(active_ingredient_groups) |>
    dplyr::summarise(
      min = min(count),
      Q1 = stats::quantile(count, 0.25, na.rm = TRUE),
      median_count_per_dog = stats::quantile(count, 0.50, na.rm = TRUE),
      Q3 = stats::quantile(count, 0.75, na.rm = TRUE),
      .groups = "drop",
      max = max(count)
    )

  categorised <- counts |>
    dplyr::left_join(summary, by = "active_ingredient_groups") |>
    dplyr::mutate(
      quartile = dplyr::case_when(
        count <= Q1 ~ "Q1",
        count > Q1 & count <= median_count_per_dog ~ "Q2",
        count > median_count_per_dog & count <= Q3 ~ "Q3",
        count > Q3 ~ "Q4"
      ),
      half = ifelse(
        count <= median_count_per_dog,
        "lower_half",
        "upper_half"
      )
    ) |>
    dplyr::select(subject_id, active_ingredient_groups, quartile, half)

  wide <- categorised |>
    tidyr::pivot_wider(
      names_from = active_ingredient_groups,
      values_from = c(half, quartile)
    ) |>
    dplyr::rename_with(
      ~ paste0(.x, "_", suffix),
      .cols = -dplyr::all_of("subject_id")
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character),
        ~ ifelse(is.na(.x), missing_value, .x)
      )
    )

  list(
    counts = counts,
    summary = summary,
    categorised = categorised,
    wide = wide
  )
}

medication_text_columns <- c(
  "medication_name",
  "medication_name_specify",
  "medication_ingredients"
)

normalise_medication_text <- function(x) {
  trimws(tolower(as.character(x)))
}

match_medication_rows_exact <- function(
    medications,
    terms,
    columns = medication_text_columns) {
  terms <- unique(normalise_medication_text(terms))
  terms <- terms[nzchar(terms) & !is.na(terms)]

  matches <- lapply(medications[columns], function(values) {
    values <- normalise_medication_text(values)
    !is.na(values) & values %in% terms
  })

  Reduce(`|`, matches)
}

summarise_medication_flag <- function(
    medications,
    terms,
    output_column,
    columns = medication_text_columns) {
  flagged <- data.frame(
    subject_id = medications$subject_id,
    matched = match_medication_rows_exact(medications, terms, columns),
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(subject_id) |>
    dplyr::summarise(matched = as.integer(any(matched)), .groups = "drop")

  names(flagged)[names(flagged) == "matched"] <- output_column
  flagged
}

summarise_specific_medication_flags <- function(
    medications,
    medications_5y,
    term_groups) {
  features <- data.frame(
    subject_id = union(medications$subject_id, medications_5y$subject_id),
    stringsAsFactors = FALSE
  )

  for (group_name in names(term_groups)) {
    lifetime <- summarise_medication_flag(
      medications,
      term_groups[[group_name]],
      paste0(group_name, "_ever")
    )
    recent <- summarise_medication_flag(
      medications_5y,
      term_groups[[group_name]],
      paste0(group_name, "_5y")
    )

    features <- features |>
      dplyr::left_join(lifetime, by = "subject_id", relationship = "one-to-one") |>
      dplyr::left_join(recent, by = "subject_id", relationship = "one-to-one")
  }

  features
}

match_medication_ingredients <- function(medications, terms) {
  ingredients <- normalise_medication_text(medications$medication_ingredients)
  terms <- unique(normalise_medication_text(terms))
  terms <- terms[nzchar(terms) & !is.na(terms)]

  vapply(ingredients, function(value) {
    !is.na(value) && any(vapply(terms, grepl, logical(1), x = value, fixed = TRUE))
  }, logical(1))
}

summarise_medication_count_feature <- function(
    medications,
    terms,
    feature_name,
    categorise_count,
    amount_column = paste0(feature_name, "_5y_amount")) {
  counts <- data.frame(
    subject_id = medications$subject_id,
    matched = match_medication_ingredients(medications, terms),
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(subject_id) |>
    dplyr::summarise(count = sum(matched), .groups = "drop") |>
    dplyr::mutate(
      use = ifelse(
        count > 0L,
        paste0(feature_name, "_yes"),
        paste0(feature_name, "_no")
      ),
      amount = categorise_count(count)
    ) |>
    dplyr::select(subject_id, use, amount)

  names(counts)[names(counts) == "use"] <- paste0(feature_name, "_use")
  names(counts)[names(counts) == "amount"] <- amount_column
  counts
}

join_medication_features <- function(
    cohort,
    features,
    missing_value = "no_medication_records_for_this_time_period") {
  feature_columns <- setdiff(names(features), "subject_id")

  cohort |>
    dplyr::left_join(features, by = "subject_id", relationship = "one-to-one") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(feature_columns),
        ~ ifelse(is.na(.x), missing_value, as.character(.x))
      )
    )
}

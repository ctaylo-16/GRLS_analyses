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

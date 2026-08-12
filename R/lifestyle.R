lifestyle_category_patterns <- function() {
  list(
    competitive = c(
      "agility",
      "dog athlete",
      "field trials",
      "obedience",
      "hunt test",
      "demonstration dog",
      "tracking"
    ),
    working = c("hunting", "search and rescue", "service dog"),
    `breed/showing` = c("breeder", "show"),
    miscellaneous = c("other", "therapy dog")
  )
}

categorise_main_lifestyle <- function(main_lifestyle) {
  patterns <- lifestyle_category_patterns()

  dplyr::case_when(
    stringr::str_detect(
      main_lifestyle,
      paste(patterns$competitive, collapse = "|")
    ) ~ "competitive",
    stringr::str_detect(
      main_lifestyle,
      paste(patterns$working, collapse = "|")
    ) ~ "working",
    stringr::str_detect(
      main_lifestyle,
      paste(patterns[["breed/showing"]], collapse = "|")
    ) ~ "breed/showing",
    stringr::str_detect(
      main_lifestyle,
      paste(patterns$miscellaneous, collapse = "|")
    ) ~ "miscellaneous",
    main_lifestyle == "companion/pet" ~ "companion/pet",
    TRUE ~ "other"
  )
}

build_lifestyle_features <- function(lifestyle) {
  counts <- lifestyle |>
    dplyr::count(.data$subject_id, .data$lifestyle, name = ".observations")

  maxima <- counts |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::filter(.data$.observations == max(.data$.observations)) |>
    dplyr::ungroup()

  main_lifestyle <- maxima |>
    dplyr::arrange(.data$lifestyle, .by_group = TRUE) |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      main_lifestyle = if (dplyr::n() == 1L) {
        .data$lifestyle[[1L]]
      } else {
        paste(
          ifelse(is.na(.data$lifestyle), "not recorded", .data$lifestyle),
          collapse = ", "
        )
      },
      main_lifestyle_category = if (dplyr::n() == 1L) {
        categorise_main_lifestyle(.data$lifestyle[[1L]])
      } else {
        "multiple"
      },
      .groups = "drop"
    )

  lifestyle |>
    dplyr::left_join(
      main_lifestyle,
      by = "subject_id",
      relationship = "many-to-one"
    ) |>
    dplyr::distinct(.data$subject_id, .keep_all = TRUE) |>
    dplyr::select(
      "subject_id",
      "year_in_study",
      "record_date",
      "lifestyle",
      "main_lifestyle",
      "main_lifestyle_category"
    )
}

standardise_exposure_window_labels <- function(data) {
  replacements <- c(
    "Not within 3,4,5,6,7,8,9,10 years" = "Not within rest of life",
    "Not within 0,1,2 years" = "Not within early life",
    "Not within 0,1,2,3,4,5,6,7,8,9,10 years" = "Not within whole life",
    "Within 3,4,5,6,7,8,9,10 years" = "Within rest of life",
    "Within 0,1,2 years" = "Within early life",
    "Within 0,1,2,3,4,5,6,7,8,9,10 years" = "Within whole life"
  )

  data |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character),
        ~ {
          replacement <- unname(replacements[match(.x, names(replacements))])
          ifelse(is.na(replacement), .x, replacement)
        }
      )
    )
}

add_lifestyle_features <- function(cohort, lifestyle) {
  lifestyle_by_dog <- build_lifestyle_features(lifestyle)

  cohort |>
    dplyr::left_join(
      lifestyle_by_dog,
      by = "subject_id",
      relationship = "one-to-one"
    ) |>
    standardise_exposure_window_labels()
}

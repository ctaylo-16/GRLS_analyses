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

  tied_subjects <- maxima |>
    dplyr::count(.data$subject_id, name = ".number_of_maxima") |>
    dplyr::filter(.data$.number_of_maxima > 1L) |>
    dplyr::pull("subject_id")

  if (length(tied_subjects) > 0L) {
    shown <- head(tied_subjects, 10L)
    remainder <- length(tied_subjects) - length(shown)
    suffix <- if (remainder > 0L) {
      paste0(" (and ", remainder, " more)")
    } else {
      ""
    }

    stop(
      "No unique main lifestyle for subject_id: ",
      paste(shown, collapse = ", "),
      suffix,
      call. = FALSE
    )
  }

  main_lifestyle <- maxima |>
    dplyr::select("subject_id", "lifestyle") |>
    dplyr::rename(main_lifestyle = "lifestyle")

  lifestyle |>
    dplyr::left_join(
      main_lifestyle,
      by = "subject_id",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      main_lifestyle_category = categorise_main_lifestyle(.data$main_lifestyle)
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

# Key-safe assembly of the condition-domain extracts.

grls_condition_domains <- c(
  "cardio",
  "dental",
  "ear_nose_throat",
  "endocrine",
  "eye",
  "gastrointestinal",
  "hematologic",
  "infectious",
  "musculoskeletal"
)

grls_condition_key <- c("subject_id", "year_in_study", "to_date")

combine_condition_frames <- function(
    condition_frames,
    key = grls_condition_key) {
  if (
    is.null(names(condition_frames)) ||
      any(names(condition_frames) == "") ||
      anyDuplicated(names(condition_frames))
  ) {
    stop("condition_frames must have unique, non-empty domain names.", call. = FALSE)
  }
  if (length(condition_frames) < 2L) {
    stop("At least two condition-domain data frames are required.", call. = FALSE)
  }

  required_metadata <- c(key, "relationship_category", "record_date")
  allowed_duplicate_columns <- c(
    required_metadata,
    "any",
    "other",
    "other_specify",
    "trauma_injury"
  )

  for (domain in names(condition_frames)) {
    frame <- condition_frames[[domain]]
    missing_columns <- setdiff(required_metadata, names(frame))

    if (length(missing_columns) > 0L) {
      stop(
        "Condition domain '", domain, "' is missing columns: ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }
    if (anyNA(frame[key])) {
      stop("Condition domain '", domain, "' has a missing key.", call. = FALSE)
    }
    if (anyDuplicated(frame[key])) {
      stop("Condition domain '", domain, "' has a duplicated key.", call. = FALSE)
    }
  }

  all_column_names <- unlist(lapply(condition_frames, names), use.names = FALSE)
  duplicated_columns <- unique(
    all_column_names[duplicated(all_column_names)]
  )
  unexpected_duplicates <- setdiff(
    duplicated_columns,
    allowed_duplicate_columns
  )

  if (length(unexpected_duplicates) > 0L) {
    stop(
      "Unexpected columns occur in more than one condition domain: ",
      paste(unexpected_duplicates, collapse = ", "),
      call. = FALSE
    )
  }

  reference_domain <- names(condition_frames)[[1]]
  combined <- condition_frames[[reference_domain]]
  reference_keys <- combined[key]
  reference_relationship <- combined[c(key, "relationship_category")]

  for (domain in names(condition_frames)[-1]) {
    frame <- condition_frames[[domain]]

    missing_from_domain <- dplyr::anti_join(reference_keys, frame[key], by = key)
    extra_in_domain <- dplyr::anti_join(frame[key], reference_keys, by = key)

    if (nrow(missing_from_domain) > 0L || nrow(extra_in_domain) > 0L) {
      stop(
        "Condition domain '", domain,
        "' does not contain exactly the same keys as '",
        reference_domain, "'.",
        call. = FALSE
      )
    }

    relationship_check <- dplyr::left_join(
      reference_relationship,
      frame[c(key, "relationship_category")],
      by = key,
      suffix = c(".reference", ".current"),
      relationship = "one-to-one"
    )
    relationship_mismatch <- (
      relationship_check$relationship_category.reference !=
        relationship_check$relationship_category.current
    )

    if (any(relationship_mismatch, na.rm = TRUE)) {
      stop(
        "Condition domain '", domain,
        "' has a different relationship_category for at least one key.",
        call. = FALSE
      )
    }

    new_columns <- setdiff(names(frame), names(combined))
    combined <- dplyr::left_join(
      combined,
      frame[c(key, new_columns)],
      by = key,
      relationship = "one-to-one"
    )
  }

  if (nrow(combined) != nrow(reference_keys)) {
    stop("Condition-domain assembly changed the expected row count.", call. = FALSE)
  }

  combined
}

read_condition_domains <- function(
    data_directory,
    domains = grls_condition_domains) {
  paths <- file.path(data_directory, paste0("conditions_", domains, ".csv"))
  missing_files <- paths[!file.exists(paths)]

  if (length(missing_files) > 0L) {
    stop(
      "Missing condition-domain files: ",
      paste(missing_files, collapse = ", "),
      call. = FALSE
    )
  }

  frames <- lapply(paths, read.csv)
  names(frames) <- domains
  combine_condition_frames(frames)
}

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

condition_metadata_columns <- c(
  "subject_id",
  "relationship_category",
  "year_in_study",
  "record_date",
  "to_date"
)

condition_generic_columns <- c("any", "other", "other_specify")

condition_feature_columns <- function(conditions) {
  setdiff(
    names(conditions),
    c(condition_metadata_columns, condition_generic_columns)
  )
}

complete_condition_records <- function(conditions) {
  feature_columns <- condition_feature_columns(conditions)
  conditions[
    conditions$to_date != 1 &
      stats::complete.cases(conditions[feature_columns]),
    ,
    drop = FALSE
  ]
}

select_last_condition_years <- function(conditions, number_of_years = 5L) {
  conditions |>
    dplyr::group_by(subject_id) |>
    dplyr::filter(
      year_in_study %in%
        sort(unique(year_in_study), decreasing = TRUE)[seq_len(number_of_years)]
    ) |>
    dplyr::ungroup()
}

summarise_condition_groups <- function(conditions, groups, suffix) {
  matched_groups <- lapply(groups, intersect, y = names(conditions))
  empty_groups <- names(matched_groups)[lengths(matched_groups) == 0L]

  if (length(empty_groups) > 0L) {
    stop(
      "No condition columns were found for groups: ",
      paste(empty_groups, collapse = ", "),
      call. = FALSE
    )
  }

  matched_columns <- unique(unlist(matched_groups, use.names = FALSE))
  present <- conditions |>
    dplyr::group_by(subject_id) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(matched_columns),
        ~ as.integer(any(.x == 1))
      ),
      .groups = "drop"
    )

  result <- present["subject_id"]
  for (group_name in names(matched_groups)) {
    output_column <- paste0(group_name, suffix)
    result[[output_column]] <- as.integer(
      rowSums(present[matched_groups[[group_name]]]) > 0L
    )
  }

  result
}

build_condition_group_features <- function(
    conditions,
    groups,
    permanent_conditions,
    combined_recent_groups = c(
      "orthopaedic",
      "cardiovascular",
      "immune_mediated",
      "gastrointestinal",
      "inflammatory_other",
      "chronic_inflammatory"
    ),
    recent_years = 5L) {
  missing_permanent <- setdiff(permanent_conditions, names(conditions))
  if (length(missing_permanent) > 0L) {
    stop(
      "Permanent condition columns are missing: ",
      paste(missing_permanent, collapse = ", "),
      call. = FALSE
    )
  }

  records <- complete_condition_records(conditions)
  recent_records <- select_last_condition_years(records, recent_years)

  lifetime <- summarise_condition_groups(records, groups, "_lifetime")
  recent <- summarise_condition_groups(recent_records, groups, "_5y")

  permanent_groups <- lapply(groups[combined_recent_groups], function(columns) {
    intersect(columns, permanent_conditions)
  })
  permanent <- summarise_condition_groups(
    records[c("subject_id", permanent_conditions)],
    permanent_groups,
    "_lifelong"
  )

  combined <- recent |>
    dplyr::left_join(permanent, by = "subject_id", relationship = "one-to-one")

  for (group_name in combined_recent_groups) {
    recent_column <- paste0(group_name, "_5y")
    permanent_column <- paste0(group_name, "_lifelong")
    combined_column <- paste0(group_name, "_5y2")
    combined[[combined_column]] <- pmax(
      combined[[recent_column]],
      combined[[permanent_column]],
      na.rm = TRUE
    )
  }

  combined_columns <- paste0(combined_recent_groups, "_5y2")
  combined |>
    dplyr::select(subject_id, dplyr::all_of(combined_columns)) |>
    dplyr::left_join(lifetime, by = "subject_id", relationship = "one-to-one")
}

summarise_neoplasia_window <- function(
    neoplasia,
    all_cancers,
    malignant_cancers,
    benign_cancers = NULL,
    suffix) {
  scored <- neoplasia
  scored$all_cancers_count <- rowSums(scored[all_cancers])
  scored$malig_cancers_count <- rowSums(scored[malignant_cancers])
  if (!is.null(benign_cancers)) {
    scored$benign_cancers_count <- rowSums(scored[benign_cancers])
  }

  count_columns <- c(
    "malig_cancers_count",
    if (!is.null(benign_cancers)) "benign_cancers_count",
    "all_cancers_count"
  )
  result <- scored |>
    dplyr::group_by(subject_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(count_columns), max),
      .groups = "drop"
    )

  for (count_column in count_columns) {
    yn_column <- sub("_count$", "_YN", count_column)
    result[[yn_column]] <- as.integer(result[[count_column]] >= 1L)
  }

  names(result)[-1] <- paste0(names(result)[-1], "_", suffix)
  result
}

build_neoplasia_features <- function(
    neoplasia,
    target_cancer,
    malignant_cancers,
    benign_cancers = NULL,
    recent_years = 5L,
    recent_missing = "no_records_available") {
  all_cancers <- setdiff(condition_feature_columns(neoplasia), target_cancer)
  defined_cancers <- unique(c(all_cancers, target_cancer))
  requested_cancers <- unique(c(malignant_cancers, benign_cancers))
  missing_cancers <- setdiff(requested_cancers, defined_cancers)

  if (!target_cancer %in% names(neoplasia)) {
    stop("Target cancer column is missing: ", target_cancer, call. = FALSE)
  }
  if (length(missing_cancers) > 0L) {
    stop(
      "Neoplasia definition columns are missing: ",
      paste(missing_cancers, collapse = ", "),
      call. = FALSE
    )
  }
  if (target_cancer %in% requested_cancers) {
    stop("The target cancer must not be included in a cancer subgroup.", call. = FALSE)
  }

  cancer_columns <- condition_feature_columns(neoplasia)
  records <- neoplasia[
    neoplasia$to_date != 1 &
      stats::complete.cases(neoplasia[cancer_columns]),
    ,
    drop = FALSE
  ]
  recent_records <- select_last_condition_years(records, recent_years)

  lifetime <- summarise_neoplasia_window(
    records,
    all_cancers,
    malignant_cancers,
    benign_cancers,
    "lifetime"
  )
  recent <- summarise_neoplasia_window(
    recent_records,
    all_cancers,
    malignant_cancers,
    benign_cancers,
    "5y"
  )
  recent_columns <- setdiff(names(recent), "subject_id")

  lifetime |>
    dplyr::left_join(recent, by = "subject_id", relationship = "one-to-one") |>
    dplyr::mutate(
      dplyr::across(
        -subject_id,
        ~ as.character(.x)
      ),
      dplyr::across(
        dplyr::all_of(recent_columns),
        ~ ifelse(is.na(.x), recent_missing, .x)
      )
    )
}

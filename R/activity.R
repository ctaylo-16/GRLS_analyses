.activity_mode_max_tie <- function(value) {
  value <- value[!is.na(value)]

  if (length(value) == 0L) {
    return(NA_real_)
  }

  counts <- table(value)
  modes <- as.numeric(names(counts)[counts == max(counts)])
  max(modes)
}

.activity_mean <- function(value) {
  if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE)
}

.activity_median <- function(value) {
  if (all(is.na(value))) NA_real_ else stats::median(value, na.rm = TRUE)
}

.activity_min <- function(value) {
  if (all(is.na(value))) NA_real_ else min(value, na.rm = TRUE)
}

.activity_max <- function(value) {
  if (all(is.na(value))) NA_real_ else max(value, na.rm = TRUE)
}

.score_early_activity <- function(activity) {
  less_than_30min <- c(
    "_10-30 minutes", "10-30 minutes_", "Less than 10 minutes_",
    "Less than 10 minutes_Less than 10 minutes", "_Less than 10 minutes",
    "10-30 minutes_Less than 10 minutes", "_"
  )
  between_31_60min <- c(
    "_30-60 minutes", "10-30 minutes_30-60 minutes", "30-60 minutes_",
    "Less than 10 minutes_10-30 minutes", "10-30 minutes_10-30 minutes",
    "30-60 minutes_Less than 10 minutes", "Less than 10 minutes_30-60 minutes",
    "30-60 minutes_10-30 minutes"
  )
  between_61_90min <- c(
    "Greater than 60 minutes_", "_Greater than 60 minute",
    "10-30 minutes_Greater than 60 minutes",
    "Greater than 60 minutes_10-30 minutes", "_Greater than 60 minutes",
    "Greater than 60 minutes_Less than 10 minutes",
    "Less than 10 minutes_Greater than 60 minutes"
  )
  between_91_120min <- c(
    "Greater than 60 minutes_30-60 minutes",
    "30-60 minutes_Greater than 60 minutes",
    "30-60 minutes_30-60 minutes"
  )
  more_than_120min <- "Greater than 60 minutes_Greater than 60 minutes"

  never <- c("Never_", "Never_Less than once/month")
  less_than_weekly <- c(
    "Less than once/month_", "Less than once/month_Less than once/month",
    "Less than once/month_Less than once/week",
    "Less than once/week_Less than once/month", "Never_Less than once/week",
    "Less than once/week_", "Less than once/week_Less than once/week"
  )
  min_weekly <- c(
    "Once or twice/week_Once or twice/week", "Once or twice/week_",
    "Never_Once or twice/week", "Once or twice/week_Less than once/month",
    "Once or twice/week_Less than once/week",
    "Less than once/week_Once or twice/week",
    "Less than once/month_Once or twice/week"
  )
  daily <- c(
    "Once/day_", "Less than once/week_Once/day", "Never_Once/day",
    "Once or twice/week_Once/day", "Once/day_Once or twice/week",
    "Once/day_Less than once/week", "Once/day_Less than once/month",
    "Less than once/month_Once/day"
  )
  multi_daily <- c(
    "Once/day_Once/day", "Once/day_More than once a day",
    "More than once a day_", "Once or twice/week_More than once a day",
    "Less than once/month_More than once a day",
    "More than once a day_Once/day", "Never_More than once a day",
    "More than once a day_Less than once/month",
    "More than once a day_Once or twice/week",
    "Less than once/week_More than once a day",
    "More than once a day_More than once a day",
    "More than once a day_Less than once/week"
  )

  low_only <- c(
    "_Bursts of exercise less than 30% of the time", "Slow_",
    "Slow_Bursts of exercise less than 30% of the time"
  )
  moderate <- c(
    "Average_",
    "_Engages in moderate exercise (greater than 30% of the time but less than 60% of the time)",
    "Brisk_", "Brisk_Bursts of exercise less than 30% of the time",
    "Average_Engages in moderate exercise (greater than 30% of the time but less than 60% of the time)",
    "Jog_", "Average_Bursts of exercise less than 30% of the time",
    "Jog_Bursts of exercise less than 30% of the time",
    "Brisk_Engages in moderate exercise (greater than 30% of the time but less than 60% of the time)",
    "Slow_Engages in moderate exercise (greater than 30% of the time but less than 60% of the time)"
  )
  high <- c(
    " _Engages in strenuous exercise most of the time (greater than 60%)",
    "Run_",
    "Average_Engages in strenuous exercise most of the time (greater than 60%)",
    "Brisk_Engages in strenuous exercise most of the time (greater than 60%)",
    "_Engages in strenuous exercise most of the time (greater than 60%)",
    "Jog_Engages in strenuous exercise most of the time (greater than 60%)",
    "Run_Engages in strenuous exercise most of the time (greater than 60%)",
    "Run_Engages in moderate exercise (greater than 30% of the time but less than 60% of the time)",
    "Slow_Engages in strenuous exercise most of the time (greater than 60%)",
    "Jog_Engages in moderate exercise (greater than 30% of the time but less than 60% of the time)",
    "Run_Bursts of exercise less than 30% of the time"
  )

  activity |>
    dplyr::mutate(
      total_activity_duration = paste(
        .data$walk_duration,
        .data$aerobic_duration,
        sep = "_"
      ),
      total_activity_freq = paste(
        .data$walk_frequency,
        .data$aerobic_frequency,
        sep = "_"
      ),
      total_activity_intensity = paste(
        .data$walk_pace,
        .data$aerobic_pace,
        sep = "_"
      ),
      duration_score = dplyr::case_when(
        .data$total_activity_duration %in% less_than_30min ~ 20,
        .data$total_activity_duration %in% between_31_60min ~ 40,
        .data$total_activity_duration %in% between_61_90min ~ 60,
        .data$total_activity_duration %in% between_91_120min ~ 80,
        .data$total_activity_duration %in% more_than_120min ~ 100
      ),
      frequency_score = dplyr::case_when(
        .data$total_activity_freq %in% never ~ 20,
        .data$total_activity_freq %in% less_than_weekly ~ 40,
        .data$total_activity_freq %in% min_weekly ~ 60,
        .data$total_activity_freq %in% daily ~ 80,
        .data$total_activity_freq %in% multi_daily ~ 100
      ),
      intensity_score = dplyr::case_when(
        .data$total_activity_intensity %in% low_only ~ 33.3,
        .data$total_activity_intensity %in% moderate ~ 66.7,
        .data$total_activity_intensity %in% high ~ 100,
        .data$total_activity_intensity == "_" ~ 0
      ),
      USDA_rank = dplyr::case_when(
        .data$duration_score == 20 ~ 1,
        .data$duration_score > 20 ~ 2
      ),
      one_hour_rank = dplyr::case_when(
        .data$duration_score <= 40 ~ 1,
        .data$duration_score > 40 ~ 2
      ),
      KC_rank = dplyr::case_when(
        .data$duration_score <= 80 ~ 1,
        .data$duration_score == 100 ~ 2
      )
    )
}

.score_later_activity <- function(activity) {
  activity |>
    dplyr::mutate(
      pace_score = dplyr::case_when(
        .data$pace == "Slow walk" ~ 20,
        .data$pace == "Average walk" ~ 40,
        .data$pace == "Brisk walk" ~ 60,
        .data$pace == "Jog" ~ 80,
        .data$pace == "Run" ~ 100,
        .data$pace == "" ~ 0
      ),
      frequency_score = dplyr::case_when(
        .data$frequency == "Rarely" ~ 20,
        .data$frequency == "Monthly" ~ 40,
        .data$frequency == "Weekly" ~ 60,
        .data$frequency == "Daily" ~ 80,
        .data$frequency == "More than daily" ~ 100,
        .data$frequency == "" ~ 0
      )
    )
}

.activity_overview_summaries <- function(activity, prefix) {
  activity |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      mode = .activity_mode_max_tie(.data$activity_level_ordinal),
      mean = .activity_mean(.data$activity_level_ordinal),
      median = .activity_median(.data$activity_level_ordinal),
      min = .activity_min(.data$activity_level_ordinal),
      max = .activity_max(.data$activity_level_ordinal),
      .groups = "drop"
    ) |>
    dplyr::rename_with(
      ~ paste0(prefix, "_activity_", .x),
      .cols = c("mode", "mean", "median", "min", "max")
    )
}

build_activity_features <- function(
    cohort,
    early_activity,
    later_activity,
    activity_overview,
    include_overview_distribution = TRUE) {
  if (anyDuplicated(cohort$subject_id)) {
    stop("The pre-activity dataset is not one row per subject_id.", call. = FALSE)
  }
  if (anyNA(cohort$year_in_study_diagnosis_or_final_record_year)) {
    stop(
      "Missing year_in_study_diagnosis_or_final_record_year for activity features.",
      call. = FALSE
    )
  }
  if (anyNA(as.Date(cohort$end_date))) {
    stop("Missing end_date for activity features.", call. = FALSE)
  }

  endpoints <- cohort |>
    dplyr::transmute(
      .data$subject_id,
      endpoint_date = as.Date(.data$end_date),
      endpoint_month = format(.data$endpoint_date, "%Y-%m"),
      endpoint_year = as.numeric(
        .data$year_in_study_diagnosis_or_final_record_year
      )
    )

  early_scored <- early_activity |>
    dplyr::inner_join(endpoints, by = "subject_id", relationship = "many-to-one") |>
    dplyr::filter(as.Date(.data$record_date) <= .data$endpoint_date) |>
    .score_early_activity()

  later_scored <- later_activity |>
    dplyr::inner_join(endpoints, by = "subject_id", relationship = "many-to-one") |>
    dplyr::filter(as.Date(.data$record_date) <= .data$endpoint_date) |>
    .score_later_activity()

  overview_scored <- activity_overview |>
    dplyr::inner_join(endpoints, by = "subject_id", relationship = "many-to-one") |>
    dplyr::filter(.data$record_date <= .data$endpoint_month) |>
    dplyr::mutate(
      activity_level_score = dplyr::case_when(
        .data$activity_level == "None" ~ 25,
        .data$activity_level == "Little" ~ 50,
        .data$activity_level == "Moderate" ~ 75,
        .data$activity_level == "Very active" ~ 100,
        .data$activity_level == "" ~ 0
      ),
      activity_level_ordinal = dplyr::case_when(
        .data$activity_level == "None" ~ 0,
        .data$activity_level == "Little" ~ 1,
        .data$activity_level == "Moderate" ~ 2,
        .data$activity_level == "Very active" ~ 3,
        .data$activity_level == "" ~ NA_real_
      )
    )

  early_summaries <- early_scored |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      USDA_rank = .activity_mode_max_tie(.data$USDA_rank),
      one_hour_rank = .activity_mode_max_tie(.data$one_hour_rank),
      KC_rank = .activity_mode_max_tie(.data$KC_rank),
      avg_activity_duration_sy1_2 = .activity_mean(.data$duration_score),
      avg_activity_intensity_sy1_2 = .activity_mean(.data$intensity_score),
      avg_activity_freq_sy1_2 = .activity_mean(.data$frequency_score),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      USDA_30min_sy1_2 = dplyr::case_when(
        .data$USDA_rank == 1 ~ "below_USDA",
        .data$USDA_rank == 2 ~ "more_than_USDA"
      ),
      activity_1h_min_sy1_2 = dplyr::case_when(
        .data$one_hour_rank == 1 ~ "below_1h",
        .data$one_hour_rank == 2 ~ "more_than_1h"
      ),
      activity_KC_2h_min_sy1_2 = dplyr::case_when(
        .data$KC_rank == 1 ~ "below_KC",
        .data$KC_rank == 2 ~ "more_than_KC"
      )
    ) |>
    dplyr::select(
      "subject_id",
      "USDA_30min_sy1_2",
      "activity_1h_min_sy1_2",
      "activity_KC_2h_min_sy1_2",
      "avg_activity_duration_sy1_2",
      "avg_activity_intensity_sy1_2",
      "avg_activity_freq_sy1_2"
    )

  later_summaries <- later_scored |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      avg_activity_freq_sy3 = .activity_mean(.data$frequency_score),
      avg_activity_intensity_sy3 = .activity_mean(.data$pace_score),
      .groups = "drop"
    )

  overview_mean <- overview_scored |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      mean_activity_level_overview = .activity_mean(.data$activity_level_score),
      .groups = "drop"
    )

  five_year_summaries <- dplyr::bind_rows(
    early_scored |>
      dplyr::transmute(
        .data$subject_id,
        year_in_study = as.numeric(.data$year_in_study),
        pace_score = .data$intensity_score,
        frequency_score = .data$frequency_score,
        .data$endpoint_year
      ),
    later_scored |>
      dplyr::transmute(
        .data$subject_id,
        year_in_study = as.numeric(.data$year_in_study),
        .data$pace_score,
        .data$frequency_score,
        .data$endpoint_year
      )
  ) |>
    dplyr::filter(.data$year_in_study >= .data$endpoint_year - 5) |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      avg_intensity_5y_prior_endpoint = .activity_mean(.data$pace_score),
      avg_frequency_5y_prior_endpoint = .activity_mean(.data$frequency_score),
      .groups = "drop"
    )

  early_overview <- overview_scored |>
    dplyr::filter(as.numeric(.data$year_in_study) < 3) |>
    .activity_overview_summaries("early")
  rest_overview <- overview_scored |>
    dplyr::filter(as.numeric(.data$year_in_study) > 2) |>
    .activity_overview_summaries("rest")
  whole_overview <- .activity_overview_summaries(overview_scored, "whole")

  overview_summaries <- whole_overview |>
    dplyr::full_join(early_overview, by = "subject_id", relationship = "one-to-one") |>
    dplyr::full_join(rest_overview, by = "subject_id", relationship = "one-to-one") |>
    dplyr::mutate(
      dplyr::across(
        -"subject_id",
        ~ dplyr::if_else(
          is.na(.x) | is.infinite(.x),
          "no_activity_records_for_this_time_period",
          as.character(.x)
        )
      )
    )

  features <- overview_mean |>
    dplyr::full_join(early_summaries, by = "subject_id", relationship = "one-to-one") |>
    dplyr::full_join(later_summaries, by = "subject_id", relationship = "one-to-one") |>
    dplyr::full_join(
      five_year_summaries,
      by = "subject_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      avg_activity_duration_sy1_2_splits = dplyr::case_when(
        .data$avg_activity_duration_sy1_2 <= 20 ~ "less_than_30min",
        .data$avg_activity_duration_sy1_2 <= 40 ~ "between_30_60_min",
        .data$avg_activity_duration_sy1_2 <= 60 ~ "between_60_90_min",
        .data$avg_activity_duration_sy1_2 <= 80 ~ "between_90_120_min",
        .data$avg_activity_duration_sy1_2 <= 100 ~ "more_than_120_min"
      ),
      avg_activity_freq_sy1_2_splits = dplyr::case_when(
        .data$avg_activity_freq_sy1_2 <= 20 ~ "never",
        .data$avg_activity_freq_sy1_2 <= 40 ~ "less_than_weekly",
        .data$avg_activity_freq_sy1_2 <= 60 ~ "min_weekly",
        .data$avg_activity_freq_sy1_2 <= 80 ~ "daily",
        .data$avg_activity_freq_sy1_2 <= 100 ~ "multi_daily"
      ),
      avg_activity_intensity_sy1_2_splits = dplyr::case_when(
        .data$avg_activity_intensity_sy1_2 <= 33.3 ~ "low",
        .data$avg_activity_intensity_sy1_2 <= 66.7 ~ "moderate",
        .data$avg_activity_intensity_sy1_2 <= 100 ~ "high"
      ),
      avg_activity_freq_sy3_splits = dplyr::case_when(
        .data$avg_activity_freq_sy3 <= 20 ~ "rarely",
        .data$avg_activity_freq_sy3 <= 40 ~ "monthly",
        .data$avg_activity_freq_sy3 <= 60 ~ "weekly",
        .data$avg_activity_freq_sy3 <= 80 ~ "daily",
        .data$avg_activity_freq_sy3 <= 100 ~ "multi_daily",
        is.na(.data$avg_activity_freq_sy3) ~ "no_records_for_this_time_period"
      ),
      avg_activity_intensity_sy3_splits = dplyr::case_when(
        .data$avg_activity_intensity_sy3 <= 20 ~ "slow_walks",
        .data$avg_activity_intensity_sy3 <= 40 ~ "average_walks",
        .data$avg_activity_intensity_sy3 <= 60 ~ "brisk_walks",
        .data$avg_activity_intensity_sy3 <= 80 ~ "jogs",
        .data$avg_activity_intensity_sy3 <= 100 ~ "runs",
        is.na(.data$avg_activity_intensity_sy3) ~
          "no_records_for_this_time_period"
      ),
      avg_frequency_5y_prior_endpoint_splits = dplyr::case_when(
        .data$avg_frequency_5y_prior_endpoint <= 20 ~ "rarely",
        .data$avg_frequency_5y_prior_endpoint <= 40 ~ "monthly",
        .data$avg_frequency_5y_prior_endpoint <= 60 ~ "weekly",
        .data$avg_frequency_5y_prior_endpoint <= 80 ~ "daily",
        .data$avg_frequency_5y_prior_endpoint <= 100 ~ "multi_daily",
        is.na(.data$avg_frequency_5y_prior_endpoint) ~
          "no_records_for_this_time_period"
      ),
      avg_intensity_5y_prior_endpoint_splits = dplyr::case_when(
        .data$avg_intensity_5y_prior_endpoint <= 20 ~ "slow_walks",
        .data$avg_intensity_5y_prior_endpoint <= 40 ~ "average_walks",
        .data$avg_intensity_5y_prior_endpoint <= 60 ~ "brisk_walks",
        .data$avg_intensity_5y_prior_endpoint <= 80 ~ "jogs",
        .data$avg_intensity_5y_prior_endpoint <= 100 ~ "runs",
        is.na(.data$avg_intensity_5y_prior_endpoint) ~
          "no_records_for_this_time_period"
      ),
      avg_activity_freq_sy3_splits_grouped = dplyr::case_when(
        .data$avg_activity_freq_sy3_splits %in% c("rarely", "monthly") ~
          "rarely",
        .data$avg_activity_freq_sy3_splits == "weekly" ~ "weekly",
        .data$avg_activity_freq_sy3_splits %in% c("daily", "multi_daily") ~
          "daily_or_more",
        TRUE ~ .data$avg_activity_freq_sy3_splits
      ),
      avg_activity_intensity_sy3_splits_grouped = dplyr::case_when(
        .data$avg_activity_intensity_sy3_splits %in%
          c("slow_walks", "average_walks") ~ "low_intensity",
        .data$avg_activity_intensity_sy3_splits == "brisk_walks" ~
          "moderate_intensity",
        .data$avg_activity_intensity_sy3_splits %in% c("jogs", "runs") ~
          "high_intensity",
        TRUE ~ .data$avg_activity_intensity_sy3_splits
      ),
      avg_frequency_5y_prior_endpoint_splits_grouped = dplyr::case_when(
        .data$avg_frequency_5y_prior_endpoint_splits %in%
          c("rarely", "monthly") ~ "rarely",
        .data$avg_frequency_5y_prior_endpoint_splits == "weekly" ~ "weekly",
        .data$avg_frequency_5y_prior_endpoint_splits %in%
          c("daily", "multi_daily") ~ "daily_or_more",
        TRUE ~ .data$avg_frequency_5y_prior_endpoint_splits
      ),
      avg_intensity_5y_prior_endpoint_splits_grouped = dplyr::case_when(
        .data$avg_intensity_5y_prior_endpoint_splits %in%
          c("slow_walks", "average_walks") ~ "low_intensity",
        .data$avg_intensity_5y_prior_endpoint_splits == "brisk_walks" ~
          "moderate_intensity",
        .data$avg_intensity_5y_prior_endpoint_splits %in% c("jogs", "runs") ~
          "high_intensity",
        TRUE ~ .data$avg_intensity_5y_prior_endpoint_splits
      )
    )

  if (include_overview_distribution) {
    features <- features |>
      dplyr::left_join(
        overview_summaries,
        by = "subject_id",
        relationship = "one-to-one"
      )
  }

  if (anyDuplicated(features$subject_id)) {
    stop("Activity features are not one row per subject_id.", call. = FALSE)
  }

  cohort |>
    dplyr::left_join(features, by = "subject_id", relationship = "one-to-one")
}

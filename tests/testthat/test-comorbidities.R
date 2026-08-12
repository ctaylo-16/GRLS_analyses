source(here::here("R", "comorbidities.R"))

testthat::test_that("condition frames are aligned by key rather than row order", {
  cardio <- data.frame(
    subject_id = c("a", "b"),
    year_in_study = c(1, 1),
    to_date = c(0, 0),
    relationship_category = "dog",
    record_date = c("2020-01-01", "2020-01-02"),
    heart_condition = c(1, 0)
  )
  dental <- data.frame(
    subject_id = c("b", "a"),
    year_in_study = c(1, 1),
    to_date = c(0, 0),
    relationship_category = "dog",
    record_date = c("2020-01-02", "2020-01-01"),
    dental_condition = c(1, 0)
  )

  result <- combine_condition_frames(list(cardio = cardio, dental = dental))

  testthat::expect_equal(result$subject_id, c("a", "b"))
  testthat::expect_equal(result$dental_condition, c(0, 1))
})

testthat::test_that("condition assembly rejects different or duplicated keys", {
  reference <- data.frame(
    subject_id = c("a", "b"),
    year_in_study = c(1, 1),
    to_date = c(0, 0),
    relationship_category = "dog",
    record_date = c("2020-01-01", "2020-01-02"),
    condition_a = c(1, 0)
  )
  different <- transform(
    reference,
    subject_id = c("a", "c"),
    condition_b = condition_a
  )
  different$condition_a <- NULL
  duplicated <- transform(
    reference[c(1, 1), ],
    condition_b = condition_a
  )
  duplicated$condition_a <- NULL

  testthat::expect_error(
    combine_condition_frames(list(a = reference, b = different)),
    "same keys"
  )
  testthat::expect_error(
    combine_condition_frames(list(a = reference, b = duplicated)),
    "duplicated key"
  )
})

testthat::test_that("condition groups use named lifetime and recent records", {
  conditions <- data.frame(
    subject_id = c("A", "A", "A", "B"),
    relationship_category = "dog",
    year_in_study = c(1, 2, 3, 1),
    record_date = c("2018-01-01", "2019-01-01", "2020-01-01", "2020-01-01"),
    to_date = 0,
    any = c(1, 0, 1, 0),
    permanent_orthopaedic = c(1, 0, 0, 0),
    recent_infectious = c(0, 0, 1, 0),
    other = NA,
    other_specify = NA,
    stringsAsFactors = FALSE
  )
  groups <- list(
    orthopaedic = "permanent_orthopaedic",
    infectious = "recent_infectious"
  )

  result <- build_condition_group_features(
    conditions,
    groups,
    permanent_conditions = "permanent_orthopaedic",
    combined_recent_groups = "orthopaedic",
    recent_years = 2
  ) |>
    dplyr::arrange(subject_id)

  testthat::expect_identical(result$orthopaedic_5y2, c(1L, 0L))
  testthat::expect_identical(result$orthopaedic_lifetime, c(1L, 0L))
  testthat::expect_identical(result$infectious_lifetime, c(1L, 0L))
})

testthat::test_that("condition group definitions fail when columns are absent", {
  conditions <- data.frame(subject_id = "A", condition_a = 1)

  testthat::expect_error(
    summarise_condition_groups(
      conditions,
      list(missing_group = "condition_b"),
      "_lifetime"
    ),
    "No condition columns"
  )
})

testthat::test_that("neoplasia counts exclude the target and count each cancer once", {
  neoplasia <- data.frame(
    subject_id = c("A", "A", "B"),
    relationship_category = "dog",
    year_in_study = c(1, 2, 1),
    record_date = c("2019-01-01", "2020-01-01", "2020-01-01"),
    to_date = 0,
    any = c(1, 1, 1),
    lymphoma = c(1, 1, 0),
    leukemia = c(1, 0, 0),
    melanoma = c(1, 0, 1),
    other = NA,
    other_specify = NA,
    stringsAsFactors = FALSE
  )

  result <- build_neoplasia_features(
    neoplasia,
    target_cancer = "lymphoma",
    malignant_cancers = c("leukemia", "melanoma"),
    recent_years = 1
  ) |>
    dplyr::arrange(subject_id)

  testthat::expect_identical(result$all_cancers_count_lifetime, c("2", "1"))
  testthat::expect_identical(result$all_cancers_count_5y, c("0", "1"))
  testthat::expect_identical(result$malig_cancers_YN_lifetime, c("1", "1"))
})

testthat::test_that("neoplasia definitions reject the target in a subgroup", {
  neoplasia <- data.frame(
    subject_id = "A",
    relationship_category = "dog",
    year_in_study = 1,
    record_date = "2020-01-01",
    to_date = 0,
    any = 1,
    lymphoma = 1,
    melanoma = 0
  )

  testthat::expect_error(
    build_neoplasia_features(
      neoplasia,
      target_cancer = "lymphoma",
      malignant_cancers = "lymphoma"
    ),
    "must not be included"
  )
})

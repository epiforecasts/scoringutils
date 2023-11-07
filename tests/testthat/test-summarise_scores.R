test_that("summarise_scores() works without any arguments", {
  expect_true("quantile" %in% names(scores))

  scores <- summarise_scores(scores)
  expect_false("quantile" %in% names(scores))

  s2 <- summarise_scores(scores,
    by = c(
      "location", "target_end_date", "target_type",
      "location_name", "forecast_date", "model",
      "horizon"
    )
  )

  expect_equal(nrow(scores), nrow(s2))
})

test_that("summarise_scores() handles wrong by argument well", {

  expect_error(
    summarise_scores(scores, by = "not_present"),
    "Column 'not_present' not found in data.", # nolint
    fixed = TRUE
  )

  expect_error(
    summarise_scores(scores, by = "sample_id"),
    "Column 'sample_id' not found in data.",
    fixed = TRUE
  )
})

test_that("summarise_scores() works with point forecasts in a quantile format", {
  ex <- data.table::copy(example_quantile)
  ex <- ex[quantile == 0.5][, quantile := NA_real_]

  scores <- suppressMessages(score(ex))

  scores_summarised <-summarise_scores(scores, by = "model", na.rm = TRUE)
  expect_warning(
    expect_warning(
      add_pairwise_comparison(scores_summarised)
    )
  )

  scores_point <- suppressMessages(score(example_point))
  summarised_scores <- summarise_scores(scores_point, by = "model")

  expect_no_condition(
    pw_point <- add_pairwise_comparison(
      summarised_scores,
      relative_skill_metric = "se_point"
    )
  )

  pw_manual <- pairwise_comparison(
    scores_point, by = "model", metric = "se_point"
  )

  expect_equal(
    pw_point$relative_skill,
    unique(pw_manual$relative_skill)
  )
})

test_that("summarise_scores() can compute relative measures", {
  ex <- data.table::copy(example_quantile)
  scores <- suppressMessages(score(ex))

  scores_with <- add_pairwise_comparison(
    summarise_scores(scores, by = "model")
  )

  expect_equal(
    scores_with[, relative_skill],
    c(1.6, 0.81, 0.75, 1.03), tolerance = 0.01
  )

  scores_with <- add_pairwise_comparison(
    summarise_scores(scores, by = "model"),
    relative_skill_metric = "ae_median"
  )

  expect_equal(
    scores_with[, relative_skill],
    c(1.6, 0.78, 0.77, 1.04), tolerance = 0.01
  )
})

test_that("summarise_scores() across argument works as expected", {
  ex <- data.table::copy(example_quantile)
  scores <- suppressMessages(score(ex))[, location_name := NULL]

  expect_error(
    summarise_scores(
      scores, by = "model", across = "horizon"
    ),
    regexp = "You cannot specify both"
  )
  expect_error(
    summarise_scores(
      scores, across = "horizons"
    ),
    regexp = "The columns specified in 'across' must be a subset "
  )
  expect_error(
    summarise_scores(
      scores, across = c("horizon", "horizons"),
    ),
    regexp = "The columns specified in 'across' must be a subset"
  )
  expect_equal(
    summarise_scores(
      scores, across = c("horizon", "model", "forecast_date", "target_end_date")
    ),
    summarise_scores(
      scores, by = c("location", "target_type")
    )
  )
})

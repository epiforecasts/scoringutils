test_that("summarise_scores() works without any arguments", {
  summarised_scores <- summarise_scores(
    scores_quantile,
    by = get_forecast_unit(scores_quantile)
  )
  expect_false("quantile" %in% names(summarised_scores))

  s2 <- summarise_scores(scores_quantile,
    by = c(
      "location", "target_end_date", "target_type",
      "location_name", "forecast_date", "model",
      "horizon"
    )
  )

  expect_equal(nrow(summarised_scores), nrow(s2))
})

test_that("summarise_scores() handles wrong by argument well", {

  expect_error(
    summarise_scores(scores_quantile, by = "not_present"),
    "Assertion on 'by' failed: Must be a subset of",
    fixed = TRUE
  )

  expect_error(
    summarise_scores(scores_quantile, by = "sample_id"),
    "Assertion on 'by' failed: Must be a subset of",
    fixed = TRUE
  )
})

test_that("summarise_scores() handles the `score_names` attribute correctly", {
  test <- data.table::copy(scores_quantile)
  attr(test, "score_names") <- NULL

  expect_error(
    summarise_scores(test, by = "model"),
    "`scores` needs to have an attribute `score_names` with the names"
  )

  # expect warning if a score name changed
  test <- data.table::copy(scores_continuous)
  data.table::setnames(test, old = "crps", new = "crp2")
  expect_warning(
    summarise_scores(test, by = "model"),
    "The names of the scores previously computed do not match the names"
  )
})

test_that("summarise_scores() can compute relative measures", {
  scores_with <- add_pairwise_comparison(
    scores_quantile,
  )
  scores_with <- summarise_scores(scores_with, by = "model")

  expect_equal(
    scores_with[, wis_relative_skill],
    c(1.6, 0.81, 0.75, 1.03), tolerance = 0.01
  )

  scores_with <- add_pairwise_comparison(
    scores_quantile, by = "model",
    metric = "ae_median"
  )
  scores_with <- summarise_scores(scores_with, by = "model")

  expect_equal(
    scores_with[, ae_median_relative_skill],
    c(1.6, 0.78, 0.77, 1.04), tolerance = 0.01
  )
})

test_that("summarise_scores() across argument works as expected", {
  ex <- data.table::copy(example_quantile)
  ex <- suppressMessages(as_forecast(ex))
  scores <- score(ex)[, location_name := NULL]

  expect_warning(
    summarise_scores(
      scores, by = c("model", "target_type"), across = "horizon"
    ),
    regexp = "You specified `across` and `by` at the same time."
  )
  expect_error(
    summarise_scores(
      scores, across = "horizons"
    ),
    regexp = "Assertion on 'across' failed: Must be a subset of "
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

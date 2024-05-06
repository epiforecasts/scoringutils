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

test_that("summarise_scores() handles the `metrics` attribute correctly", {
  test <- data.table::copy(scores_quantile)
  attr(test, "metrics") <- NULL

  expect_error(
    summarise_scores(test, by = "model"),
    "Input needs an attribute `metrics` with the names"
  )

  # expect warning if a score name changed
  test <- data.table::copy(scores_continuous)
  data.table::setnames(test, old = "crps", new = "crp2")
  expect_warning(
    summarise_scores(test, by = "model"),
    "The following scores have been previously computed, but are no longer"
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

  expect_warning(
    summarise_scores(
      scores, across = c("horizon", "model", "forecast_date", "target_end_date"),
      by = c("model", "target_type")
    ),
    "You specified `across` and `by` at the same time.`by` will be ignored"
  )
})

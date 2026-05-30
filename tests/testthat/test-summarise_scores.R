test_that("summarise_scores() works as expected with by = forecast unit", {
  summarised_scores <- expect_no_condition(
    summarise_scores(scores_quantile)
  )
  expect_s3_class(summarised_scores, c("scores", "data.table", "data.frame"), exact = TRUE)
})

test_that("summarise_scores() works as expected with by = forecast unit", {
  # the only effect of running summarise_scores with by = forecast unit is
  # that coverage is now a numeric instead of a boolean
  summarised_scores <- summarise_scores(
    scores_quantile,
    by = get_forecast_unit(scores_quantile)
  )

  expect_identical(dim(summarised_scores), dim(scores_quantile))
  expect_equal(summarised_scores$wis, scores_quantile$wis) # nolint: expect_identical_linter

  s2 <- summarise_scores(scores_quantile,
    by = c(
      "location", "target_end_date", "target_type",
      "location_name", "forecast_date", "model",
      "horizon"
    )
  )
  expect_identical(dim(summarised_scores), dim(s2))
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
  test <- data.table::copy(scores_sample_continuous)
  data.table::setnames(test, old = "crps", new = "crp2")
  expect_warning(
    summarise_scores(test, by = "model"),
    "The following scores have been previously computed, but are no longer"
  )
})

test_that("summarise_scores() handles data.frames correctly", {
  test <- as.data.frame(scores_quantile)
  expect_no_condition(
    summarise_scores(test, by = "model")
  )
})

test_that("summarise_scores() errors if `by = NULL", {
  expect_error(
    summarise_scores(scores_quantile, by = NULL),
    "Assertion on 'by' failed: Must be a subset of"
  )
})

test_that("summarise_scores() errors if there are no score columns", {
  # mimics the situation in which every metric passed to `score()` failed:
  # `scores` carries an empty `metrics` attribute, so there is nothing to
  # summarise. Previously this silently produced a data.table with a
  # duplicate `by` column (gh #1179).
  empty_scores <- data.table::copy(scores_quantile)
  metric_cols <- attr(empty_scores, "metrics")
  empty_scores[, (metric_cols) := NULL]
  attr(empty_scores, "metrics") <- character(0)

  expect_error(
    summarise_scores(empty_scores, by = "model"),
    "No score columns to summarise"
  )
})

test_that("summarise_scores() errors on the empty-metrics reprex (#1179)", {
  # end-to-end version of the issue reprex: the 55% interval requires the
  # 0.225 and 0.775 quantiles, which are absent from `example_quantile`, so
  # the only metric warns and produces no score columns. `summarise_scores()`
  # should then error rather than return a data.table with a duplicate `by`
  # column (gh #1179).
  fc <- as_forecast_quantile(example_quantile)
  expect_warning(
    sc <- score(fc, metrics = list(
      interval_coverage_55 = purrr::partial(
        interval_coverage,
        interval_range = 55
      )
    )),
    "interval coverage"
  )
  expect_length(intersect(colnames(sc), attr(sc, "metrics")), 0)

  expect_error(
    summarise_scores(sc, by = "model"),
    "No score columns to summarise"
  )
})

test_that("summarise_scores() does not partial-match metric names", {
  # ensures we use exact column matching rather than regex partial matching:
  # a metric named e.g. "wis" should not pull in a column called
  # "wis_something_else" that happens to share a prefix.
  test <- data.table::copy(scores_quantile)
  test[, wis_extra := 0]
  result <- summarise_scores(test, by = "model")
  expect_false("wis_extra" %in% colnames(result))
})

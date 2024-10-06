test_that("summarise_scores() works as expected with by = forecast unit", {
  expect_no_condition(
    summarised_scores <- summarise_scores(scores_quantile)
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

  expect_equal(dim(summarised_scores), dim(scores_quantile))
  expect_equal(summarised_scores$wis, scores_quantile$wis)

  s2 <- summarise_scores(scores_quantile,
    by = c(
      "location", "target_end_date", "target_type",
      "location_name", "forecast_date", "model",
      "horizon"
    )
  )
  expect_equal(dim(summarised_scores), dim(s2))
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



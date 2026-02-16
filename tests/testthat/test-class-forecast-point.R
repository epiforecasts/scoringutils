# ==============================================================================
# as_forecast_point() # nolint: commented_code_linter
# ==============================================================================

test_that("as_forecast_point() works", {
  expect_no_condition(
    as_forecast_point(as_forecast_quantile(na.omit(example_quantile)))
  )
})

test_that("as_forecast_point.forecast_quantile() extracts median without extra args", {
  quantile_forecast <- as_forecast_quantile(na.omit(example_quantile))
  result <- as_forecast_point(quantile_forecast)
  expect_true(is_forecast_point(result))
  expect_false("quantile_level" %in% colnames(result))
  expect_false("sample_id" %in% colnames(result))
  expect_no_condition(as_forecast_point(quantile_forecast))
})


# ==============================================================================
# is_forecast_point() # nolint: commented_code_linter
# ==============================================================================
test_that("is_forecast_point() works as expected", {
  expect_true(is_forecast_point(example_point))
  expect_false(is_forecast_point(example_binary))
  expect_false(is_forecast_point(example_quantile))
  expect_false(is_forecast_point(example_sample_continuous))
  expect_false(is_forecast_point(example_nominal))
})


# ==============================================================================
# assert_forecast.forecast_point() # nolint: commented_code_linter
# ==============================================================================

test_that("assert_forecast.forecast_point() works as expected", {
  test <- na.omit(data.table::as.data.table(example_point))
  test <- as_forecast_point(test)

  # expect an error if column is changed to character after initial validation.
  expect_warning(
    test[, "predicted" := as.character(predicted)],
    "Input looks like a point forecast, but found the following issue"
  )
  expect_error(
    assert_forecast(test),
    "Input looks like a point forecast, but found the following issue"
  )
})

test_that("assert_forecast.forecast_point() complains if the forecast type is wrong", {
  expect_error(
    assert_forecast(na.omit(example_point), forecast_type = "quantile"),
    "Forecast type determined by scoringutils based on input:"
  )
})


# ==============================================================================
# score.forecast_point() # nolint: commented_code_linter
# ==============================================================================
test_that("function produces output for a point case", {
  expect_named(
    scores_binary,
    c(get_forecast_unit(example_binary), names(get_metrics(example_binary)))
  )

  eval <- summarise_scores(scores_point, by = c("model", "target_type"))

  expect_gt(
    nrow(eval), 1
  )
  expect_identical(
    colnames(eval),
    c("model", "target_type", names(get_metrics(example_point)))
  )

  expect_s3_class(eval, c("scores", "data.table", "data.frame"), exact = TRUE)
})

test_that("Changing metrics names works", {
  metrics_test <- get_metrics(example_point)
  names(metrics_test)[1] <- "just_testing"
  eval <- suppressMessages(score(as_forecast_point(example_point),
    metrics = metrics_test
  ))
  eval_summarised <- summarise_scores(eval, by = "model")
  expect_identical(
    colnames(eval_summarised),
    c("model", "just_testing", names(get_metrics(example_point))[-1])
  )
})

test_that("score.forecast_point() errors with only NA values", {
  # [.forecast()` will warn even before score()
  only_nas <- suppressWarnings(
    copy(example_point)[, predicted := NA_real_]
  )
  expect_error(
    score(only_nas),
    "After removing rows with NA values in the data, no forecasts are left."
  )
})

# ==============================================================================
# get_metrics.forecast_point() # nolint: commented_code_linter
# ==============================================================================
test_that("get_metrics.forecast_point() works as expected", {
  expect_type(
    get_metrics(example_point), "list"
  )

  expect_identical(
    get_metrics.scores(scores_point),
    c("ae_point", "se_point", "ape")
  )
})

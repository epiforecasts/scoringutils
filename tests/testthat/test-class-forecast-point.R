# ==============================================================================
# as_forecast_point() # nolint: commented_code_linter
# ==============================================================================

test_that("as_forecast_point() works", {
  expect_no_condition(
    as_forecast_point(as_forecast_quantile(na.omit(example_quantile)))
  )
})

test_that("as_forecast_point.forecast_quantile() extracts the requested quantile", {
  fq <- as_forecast_quantile(na.omit(example_quantile))
  quantiles <- as.data.table(fq)

  # default is the median
  point <- as_forecast_point(fq)
  expect_s3_class(point, "forecast_point")
  expect_false("quantile_level" %in% colnames(point))
  expect_identical(
    point$predicted, quantiles[quantile_level == 0.5]$predicted
  )

  # a different quantile level can be requested
  point_75 <- as_forecast_point(fq, quantile_level = 0.75)
  expect_s3_class(point_75, "forecast_point")
  expect_false("quantile_level" %in% colnames(point_75))
  expect_identical(
    point_75$predicted, quantiles[quantile_level == 0.75]$predicted
  )
  expect_identical(point_75$observed, point$observed)
  expect_identical(nrow(point_75), nrow(point))
})

test_that("as_forecast_point.forecast_quantile() matches quantile levels that are not exactly representable", {
  # seq() produces levels such as 0.15000000000000002 that are not exactly
  # equal to the literal 0.15
  quantile_levels <- seq(0.05, 0.95, 0.05)
  data <- data.table(
    model = "m",
    id = rep(1:2, each = length(quantile_levels)),
    quantile_level = rep(quantile_levels, 2),
    observed = rep(c(5, 6), each = length(quantile_levels))
  )
  data[, predicted := observed + quantile_level]
  fq <- as_forecast_quantile(data)

  point_15 <- as_forecast_point(fq, quantile_level = 0.15)
  expect_identical(point_15$predicted, c(5, 6) + quantile_levels[3])
  point_70 <- as_forecast_point(fq, quantile_level = 0.7)
  expect_identical(point_70$predicted, c(5, 6) + quantile_levels[14])
})

test_that("as_forecast_point.forecast_quantile() validates `quantile_level`", {
  fq <- as_forecast_quantile(na.omit(example_quantile))
  # quantile level not present in the data
  expect_error(
    as_forecast_point(fq, quantile_level = 0.33),
    "`quantile_level` 0.33 is not present in the data"
  )
  # invalid inputs
  expect_error(
    as_forecast_point(fq, quantile_level = c(0.25, 0.75)),
    "quantile_level"
  )
  expect_error(
    as_forecast_point(fq, quantile_level = "0.5"),
    "quantile_level"
  )
  expect_error(
    as_forecast_point(fq, quantile_level = 1.5),
    "quantile_level"
  )
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

test_that("point metric implementations match Metrics package", {
  skip_if_not_installed("Metrics")
  observed <- c(1, -15, 22, 0, 5.5)
  predicted <- c(5, 6, 7, 0, 5.5)
  metrics <- get_metrics(example_point)
  expect_identical(
    metrics$ae_point(observed, predicted),
    Metrics::ae(observed, predicted)
  )
  expect_identical(
    metrics$se_point(observed, predicted),
    Metrics::se(observed, predicted)
  )
  expect_equal(
    metrics$ape(observed, predicted),
    Metrics::ape(observed, predicted)
  )
})

test_that("get_metrics.forecast_point() returns expected functions", {
  metrics <- get_metrics(example_point)
  expect_type(metrics, "list")
  expect_named(metrics, c("ae_point", "se_point", "ape"))
  expect_true(all(vapply(metrics, is.function, logical(1))))
  expect_true(
    all(vapply(
      metrics, function(f) length(formals(f)) == 2, logical(1)
    ))
  )
})

# ==============================================================================
# get_non_monotonic_forecasts() # nolint: commented_code_linter
# ==============================================================================

test_that("get_non_monotonic_forecasts() returns empty data.table for well-formed quantile forecasts", {
  result <- get_non_monotonic_forecasts(example_quantile)
  expect_identical(nrow(result), 0L)
  expect_s3_class(result, c("data.table", "data.frame"))
})

test_that("get_non_monotonic_forecasts() detects predictions that decrease with increasing quantile level", {
  data <- data.table(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 7, 4)
  )
  data <- suppressWarnings(suppressMessages(as_forecast_quantile(data)))
  result <- get_non_monotonic_forecasts(data)
  expect_gt(nrow(result), 0L)
  expect_identical(nrow(result), 3L)
})

test_that("get_non_monotonic_forecasts() handles multiple forecast units with mixed monotonicity", {
  data <- data.table(
    model = rep(c("good_model", "bad_model"), each = 3),
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = rep(c(0.25, 0.5, 0.75), 2),
    predicted = c(2, 5, 8, 3, 7, 4)
  )
  data <- suppressWarnings(suppressMessages(as_forecast_quantile(data)))
  result <- get_non_monotonic_forecasts(data)
  expect_identical(nrow(result), 3L)
  expect_identical(unique(result$model), "bad_model")
})

test_that("get_non_monotonic_forecasts() works with counts argument", {
  data <- data.table(
    model = rep(c("bad1", "bad2", "good"), each = 3),
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = rep(c(0.25, 0.5, 0.75), 3),
    predicted = c(3, 7, 4, 5, 9, 6, 2, 5, 8)
  )
  data <- suppressWarnings(suppressMessages(as_forecast_quantile(data)))
  result <- get_non_monotonic_forecasts(data, counts = TRUE)
  expect_identical(nrow(result), 2L)
})

test_that("get_non_monotonic_forecasts() accepts custom forecast_unit argument", {
  expect_no_condition(
    get_non_monotonic_forecasts(
      example_quantile,
      forecast_unit = c(
        "location", "target_end_date", "target_type",
        "location_name", "forecast_date", "model"
      )
    )
  )
  result <- get_non_monotonic_forecasts(
    example_quantile,
    forecast_unit = c(
      "location", "target_end_date", "target_type",
      "location_name", "forecast_date", "model"
    )
  )
  expect_identical(nrow(result), 0L)
})

test_that("get_non_monotonic_forecasts() returns expected class", {
  result <- get_non_monotonic_forecasts(example_quantile)
  expect_s3_class(result, c("data.table", "data.frame"))
})

test_that("get_non_monotonic_forecasts() works with a plain data.frame input", {
  data <- data.frame(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 7, 4)
  )
  result <- get_non_monotonic_forecasts(data)
  expect_gt(nrow(result), 0L)
  expect_s3_class(result, c("data.table", "data.frame"))
})

test_that("get_non_monotonic_forecasts() handles equal predictions at adjacent quantile levels", {
  data <- data.table(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 5, 5)
  )
  data <- suppressWarnings(suppressMessages(as_forecast_quantile(data)))
  result <- get_non_monotonic_forecasts(data)
  expect_identical(nrow(result), 0L)
})


# ==============================================================================
# check_monotonicity() # nolint: commented_code_linter
# ==============================================================================

test_that("check_monotonicity() returns TRUE for well-formed data", {
  expect_true(check_monotonicity(example_quantile))
})

test_that("check_monotonicity() returns message string for non-monotonic data", {
  data <- data.table(
    model = "model1",
    date = as.Date("2020-01-01"),
    observed = 5,
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 7, 4)
  )
  data <- suppressWarnings(suppressMessages(as_forecast_quantile(data)))
  result <- check_monotonicity(data)
  expect_match(result, "non-monotonic|decrease|get_non_monotonic_forecasts", ignore.case = TRUE)
})

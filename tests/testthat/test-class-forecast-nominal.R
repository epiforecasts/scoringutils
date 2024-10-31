# ==============================================================================
# as_forecast_nominal()
# ==============================================================================
test_that("as_forecast.forecast_nominal() works as expected", {
  ex <- data.table::copy(example_nominal) %>%
    na.omit()


  expect_s3_class(
    as_forecast_nominal(ex),
    c("forecast_nominal", "forecast", "data.table", "data.frame"),
    exact = TRUE
  )

  setnames(ex, old = "predicted_label", new = "label")
  expect_no_condition(
    as_forecast_nominal(ex, predicted_label = "label")
  )
})

test_that("as_forecast.forecast_nominal() breaks when rows with zero probability are missing", {
  ex_faulty <- as.data.table(example_nominal)
  ex_faulty <- ex_faulty[predicted != 0]
  expect_warning(
    expect_error(
      as_forecast_nominal(ex_faulty),
      "Found incomplete forecasts"
    ),
    "Some forecasts have different numbers of rows"
  )
})


# ==============================================================================
# is_forecast_nominal()
# ==============================================================================
test_that("is_forecast_nominal() works as expected", {
  expect_true(is_forecast_nominal(example_nominal))
  expect_false(is_forecast_nominal(example_binary))
  expect_false(is_forecast_nominal(example_point))
  expect_false(is_forecast_nominal(example_quantile))
  expect_false(is_forecast_nominal(example_sample_continuous))
  expect_false(is_forecast_nominal(1:10))
})


# ==============================================================================
# get_metrics.forecast_nominal()
# ==============================================================================

test_that("get_metrics.forecast_nominal() works as expected", {
  expect_true(
    is.list(get_metrics(example_nominal))
  )
})


# ==============================================================================
# Printing
# ==============================================================================
test_that("Printing works as expected", {
  suppressMessages(
    expect_message(
      expect_message(
        capture.output(print(example_nominal)),
        "Forecast type: nominal"
      ),
      "Forecast unit:")
  )
})

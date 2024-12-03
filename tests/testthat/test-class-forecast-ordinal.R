# ==============================================================================
# as_forecast_nominal()
# ==============================================================================
test_that("as_forecast.forecast_ordinal() works as expected", {
  ex <- data.table::copy(example_ordinal) %>%
    na.omit()


  expect_s3_class(
    as_forecast_ordinal(ex),
    c("forecast_ordinal", "forecast", "data.table", "data.frame"),
    exact = TRUE
  )

  setnames(ex, old = "predicted_label", new = "label")
  expect_no_condition(
    as_forecast_ordinal(ex, predicted_label = "label")
  )
})

test_that("as_forecast.forecast_ordinal() breaks when rows with zero probability are missing", {
  ex_faulty <- as.data.table(example_ordinal)
  ex_faulty <- ex_faulty[predicted != 0]
  expect_warning(
    expect_error(
      as_forecast_ordinal(ex_faulty),
      "Found incomplete forecasts"
    ),
    "Some forecasts have different numbers of rows"
  )
})

test_that("assert_forecast.forecast_ordinal() fails if factors are not ordered", {
  ex_faulty <- na.omit(data.table::copy(example_nominal))
  expect_error(
    as_forecast_ordinal(ex_faulty),
    "Assertion on 'forecast\\$observed' failed: Must be an ordered factor, but is unordered."
  )
})

# ==============================================================================
# is_forecast_nominal()
# ==============================================================================
test_that("is_forecast_nominal() works as expected", {
  expect_true(is_forecast_ordinal(example_ordinal))
  expect_false(is_forecast_ordinal(example_binary))
  expect_false(is_forecast_ordinal(example_point))
  expect_false(is_forecast_ordinal(example_quantile))
  expect_false(is_forecast_ordinal(example_sample_continuous))
  expect_false(is_forecast_ordinal(1:10))
})


# ==============================================================================
# get_metrics.forecast_nominal()
# ==============================================================================

test_that("get_metrics.forecast_nominal() works as expected", {
  expect_true(
    is.list(get_metrics(example_ordinal))
  )
})


# ==============================================================================
# Printing
# ==============================================================================
test_that("Printing works as expected", {
  suppressMessages(
    expect_message(
      expect_message(
        capture.output(print(example_ordinal)),
        "Forecast type: ordinal"
      ),
      "Forecast unit:")
  )
})

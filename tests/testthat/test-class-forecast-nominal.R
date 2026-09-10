# ==============================================================================
# as_forecast_nominal() # nolint: commented_code_linter
# ==============================================================================
test_that("as_forecast.forecast_nominal() works as expected", {
  ex <- data.table::copy(example_nominal) |>
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

test_that("assert_forecast.forecast_nominal() names the incomplete forecast in its error message", {
  # modA is complete, modB is missing the "high" outcome
  dat <- data.table(
    model = rep(c("modA", "modB"), times = c(3, 2)),
    observed = factor("high", levels = c("low", "medium", "high")),
    predicted_label = factor(
      c("low", "medium", "high", "low", "medium"),
      levels = c("low", "medium", "high")
    ),
    predicted = c(0.2, 0.3, 0.5, 0.4, 0.6)
  )
  expect_warning(
    expect_error(
      as_forecast_nominal(dat),
      "modB"
    ),
    "Some forecasts have different numbers of rows"
  )

  # all forecasts incomplete - the first one should be named, not NA
  dat_all_incomplete <- data.table(
    model = c("modA", "modB"),
    observed = factor("high", levels = c("low", "medium", "high")),
    predicted_label = factor("low", levels = c("low", "medium", "high")),
    predicted = c(1, 1)
  )
  expect_error(
    as_forecast_nominal(dat_all_incomplete),
    "modA"
  )
})

test_that("assert_forecast.forecast_nominal() returns invisible(NULL)", {
  fc <- as_forecast_nominal(na.omit(example_nominal))
  expect_invisible(assert_forecast(fc))
  expect_null(assert_forecast(fc))
})


# ==============================================================================
# is_forecast_nominal() # nolint: commented_code_linter
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
# get_metrics.forecast_nominal() # nolint: commented_code_linter
# ==============================================================================

test_that("get_metrics.forecast_nominal() works as expected", {
  expect_type(
    get_metrics(example_nominal), "list"
  )
})


# ==============================================================================
# Printing # nolint: commented_code_linter
# ==============================================================================
test_that("Printing works as expected", {
  suppressMessages(
    expect_message(
      expect_message(
        capture.output(print(example_nominal)),
        "Forecast type: nominal"
      ),
      "Forecast unit:"
    )
  )
})

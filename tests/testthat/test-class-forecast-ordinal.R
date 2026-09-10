# ==============================================================================
# as_forecast_nominal() # nolint: commented_code_linter
# ==============================================================================
test_that("as_forecast.forecast_ordinal() works as expected", {
  ex <- data.table::copy(example_ordinal) |>
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

test_that("assert_forecast.forecast_ordinal() names the incomplete forecast in its error message", {
  # modA is complete, modB is missing the "high" outcome
  dat <- data.table(
    model = rep(c("modA", "modB"), times = c(3, 2)),
    observed = factor(
      "high", levels = c("low", "medium", "high"), ordered = TRUE
    ),
    predicted_label = factor(
      c("low", "medium", "high", "low", "medium"),
      levels = c("low", "medium", "high"),
      ordered = TRUE
    ),
    predicted = c(0.2, 0.3, 0.5, 0.4, 0.6)
  )
  expect_warning(
    expect_error(
      as_forecast_ordinal(dat),
      "modB"
    ),
    "Some forecasts have different numbers of rows"
  )

  # all forecasts incomplete - the first one should be named, not NA
  dat_all_incomplete <- data.table(
    model = c("modA", "modB"),
    observed = factor(
      "high", levels = c("low", "medium", "high"), ordered = TRUE
    ),
    predicted_label = factor(
      "low", levels = c("low", "medium", "high"), ordered = TRUE
    ),
    predicted = c(1, 1)
  )
  expect_error(
    as_forecast_ordinal(dat_all_incomplete),
    "modA"
  )
})

test_that("assert_forecast.forecast_ordinal() returns invisible(NULL)", {
  fc <- as_forecast_ordinal(na.omit(example_ordinal))
  expect_invisible(assert_forecast(fc))
  expect_null(assert_forecast(fc))
})

test_that("assert_forecast.forecast_ordinal() fails if factors are not ordered", {
  ex_faulty <- na.omit(data.table::copy(example_nominal))
  expect_error(
    as_forecast_ordinal(ex_faulty),
    "Assertion on 'forecast\\$observed' failed: Must be an ordered factor, but is unordered."
  )
})

# ==============================================================================
# is_forecast_nominal() # nolint: commented_code_linter
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
# get_metrics.forecast_nominal() # nolint: commented_code_linter
# ==============================================================================

test_that("get_metrics.forecast_nominal() works as expected", {
  expect_type(
    get_metrics(example_ordinal), "list"
  )
})


# ==============================================================================
# Printing # nolint: commented_code_linter
# ==============================================================================
test_that("Printing works as expected", {
  suppressMessages(
    expect_message(
      expect_message(
        capture.output(print(example_ordinal)),
        "Forecast type: ordinal"
      ),
      "Forecast unit:"
    )
  )
})

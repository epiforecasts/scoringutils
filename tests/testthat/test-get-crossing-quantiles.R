# ==============================================================================
# get_crossing_quantiles() # nolint: commented_code_linter
# ==============================================================================
crossing_example <- data.table(
  model = rep(c("a", "b"), each = 3),
  observed = 5,
  quantile_level = rep(c(0.25, 0.5, 0.75), 2),
  predicted = c(4, 6, 5, 4, 5, 6)
)

test_that("get_crossing_quantiles() returns all rows of crossing forecasts", {
  out <- get_crossing_quantiles(crossing_example)
  expect_s3_class(out, "data.table")
  expect_identical(out, crossing_example[model == "a"])

  # works the same on a forecast object
  fc <- suppressWarnings(as_forecast_quantile(crossing_example))
  expect_identical(get_crossing_quantiles(fc), out)
})

test_that("get_crossing_quantiles() finds nothing in valid forecasts", {
  expect_identical(nrow(get_crossing_quantiles(example_quantile)), 0L)

  # row order, ties and missing values don't count as crossing
  valid <- data.table(
    model = "a",
    observed = 5,
    quantile_level = c(0.75, 0.25, 0.5, 0.1, NA),
    predicted = c(6, 4, 6, NA, 3)
  )
  expect_identical(nrow(get_crossing_quantiles(valid)), 0L)
})

test_that("get_crossing_quantiles() respects the forecast_unit argument", {
  data <- data.table(
    model = "a",
    target = c("x", "y"),
    observed = 5,
    quantile_level = c(0.25, 0.75),
    predicted = c(4, 3)
  )
  expect_identical(nrow(get_crossing_quantiles(data)), 0L)

  # without `target`, both rows are pooled into one crossing forecast
  out <- get_crossing_quantiles(data, forecast_unit = "model")
  expect_identical(nrow(out), 2L)
  expect_false("target" %in% colnames(out))
})

test_that("get_crossing_quantiles() validates its input", {
  expect_error(
    get_crossing_quantiles(crossing_example[, -"quantile_level"]),
    "Must be a subset of"
  )
  expect_error(get_crossing_quantiles("a"), "Must be of type 'data.frame'")
})

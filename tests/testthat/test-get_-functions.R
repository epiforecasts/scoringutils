# ==============================================================================
# `get_forecast_unit()`
# ==============================================================================

test_that("get_forecast_unit() works as expected", {
  expect_equal(
    get_forecast_unit(example_quantile),
    c("location", "target_end_date", "target_type", "location_name",
      "forecast_date", "model", "horizon")
  )

  expect_equal(
    get_forecast_unit(scores_quantile),
    c("location", "target_end_date", "target_type", "location_name",
      "forecast_date", "model", "horizon")
  )

  data <- validate(example_quantile)
  ex <- data[, location := NULL]
  expect_warning(
    get_forecast_unit(ex, check_conflict = TRUE),
    "Object has an attribute `forecast_unit`, but it looks different from what's expected based on the data.
Existing: forecast_date, horizon, location, location_name, model, target_end_date, target_type
Expected: forecast_date, horizon, location_name, model, target_end_date, target_type
Running `validate()` again might solve the problem",
fixed = TRUE
  )
})


test_that("get_type() works as expected with vectors", {
  expect_equal(get_type(1:3), "integer")
  expect_equal(get_type(factor(1:2)), "classification")
  expect_equal(get_type(c(1.0, 2)), "integer")
  expect_equal(get_type(c(1.0, 2.3)), "continuous")
  expect_error(
    get_type(c("a", "b")),
    "Assertion on 'as.vector(x)' failed: Must be of type 'numeric', not 'character'.",
    fixed = TRUE
  )
})

test_that("get_type() works as expected with matrices", {
  expect_equal(get_type(matrix(1:4, nrow = 2)), "integer")
  expect_equal(get_type(matrix(c(1.0, 2:4))), "integer")
  expect_equal(get_type(matrix(c(1.0, 2.3, 3, 4))), "continuous")

  # matrix of factors doesn't work
  expect_error(
    get_type(matrix(factor(1:4), nrow = 2)),
    "Assertion on 'as.vector(x)' failed: Must be of type 'numeric', not 'character'.",
    fixed = TRUE
  )

  expect_error(
    get_type(matrix(c("a", "b", "c", "d"))),
    "Assertion on 'as.vector(x)' failed: Must be of type 'numeric', not 'character'.",
    fixed = TRUE
  )
})


test_that("new `get_type()` is equal to old `prediction_type()", {
  get_prediction_type <- function(data) {
    if (is.data.frame(data)) {
      data <- data$predicted
    }
    if (
      isTRUE(all.equal(as.vector(data), as.integer(data))) &&
      !all(is.na(as.integer(data)))
    ) {
      return("integer")
    } else if (suppressWarnings(!all(is.na(as.numeric(data))))) {
      return("continuous")
    } else {
      stop("Input is not numeric and cannot be coerced to numeric")
    }
  }

  check_data <- list(
    1:2,
    # factor(1:2) # old function would classify as "continuous"
    c(1.0, 2),
    c(1.0, 2.3),
    matrix(1:4, nrow = 2),
    matrix(c(1.0, 2:4)),
    matrix(c(1.0, 2.3, 3, 4))
  )

  for (i in seq_along(check_data)) {
    expect_equal(
      get_prediction_type(check_data[[i]]),
      get_type(check_data[[i]])
    )
  }
})

test_that("get_type() handles `NA` values", {
  expect_equal(get_type(c(1, NA, 3)), "integer")
  expect_equal(get_type(c(1, NA, 3.2)), "continuous")
  expect_error(get_type(NA), "Can't get type: all values of are NA")
})


# `get_duplicate_forecasts()` ==================================================
test_that("get_duplicate_forecasts() works as expected for quantile", {
  expect_equal(nrow(get_duplicate_forecasts(example_quantile)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_quantile, example_quantile[1000:1010]))),
    22
  )
})

test_that("get_duplicate_forecasts() works as expected for sample", {
  expect_equal(nrow(get_duplicate_forecasts(example_continuous)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_continuous, example_continuous[1040:1050]))),
    22
  )
})


test_that("get_duplicate_forecasts() works as expected for binary", {
  expect_equal(nrow(get_duplicate_forecasts(example_binary)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_binary, example_binary[1000:1010]))),
    22
  )
})

test_that("get_duplicate_forecasts() works as expected for point", {
  expect_equal(nrow(get_duplicate_forecasts(example_binary)), 0)
  expect_equal(
    nrow(
      get_duplicate_forecasts(rbind(example_point, example_point[1010:1020]))),
    22
  )
})


# ==============================================================================
# `get_forecast_type`
# ==============================================================================
test_that("get_forecast_type() works as expected", {
  expect_equal(get_forecast_type(as.data.frame(example_quantile)), "quantile")
  expect_equal(get_forecast_type(example_continuous), "sample")
  expect_equal(get_forecast_type(example_integer), "sample")
  expect_equal(get_forecast_type(example_binary), "binary")
  expect_equal(get_forecast_type(example_point), "point")

  expect_error(
    get_forecast_type(data.frame(x = 1:10)),
    "Assertion on 'data' failed: Columns 'observed', 'predicted' not found in data.",
    fixed = TRUE
  )

  df <- data.frame(observed = 1:10, predicted = factor(1:10))
  expect_error(
    get_forecast_type(df),
    "Checking `data`: input doesn't satisfy criteria for any forecast type. Are you missing a column `quantile` or `sample_id`? Please check the vignette for additional info.",
    fixed = TRUE
  )

  data <- validate(example_integer)
  attr(data, "forecast_type") <- "binary"
  expect_warning(
    get_forecast_type(data),
    "Object has an attribute `forecast_type`, but it looks different from what's expected based on the data.
Existing: binary
Expected: sample
Running `validate()` again might solve the problem",
    fixed = TRUE
  )
})

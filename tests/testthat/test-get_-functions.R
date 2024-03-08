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
})


# ==============================================================================
# Test removing `NA` values from the data
# ==============================================================================
test_that("removing NA rows from data works as expected", {
  expect_equal(nrow(na.omit(example_quantile)), 20401)

  ex <- data.frame(observed = c(NA, 1:3), predicted = 1:4)
  expect_equal(nrow(na.omit(ex)), 3)

  ex$predicted <- c(1:3, NA)
  expect_equal(nrow(na.omit(ex)), 2)

  # test that attributes and classes are retained
  ex <- as_forecast(na.omit(example_integer))
  expect_equal(
    class(na.omit(ex)),
    c("forecast_sample", "data.table", "data.frame")
  )

  attributes <- attributes(ex)
  expect_equal(
    attributes(na.omit(ex)),
    attributes
  )
})


# ==============================================================================
# `get_type()`
# ==============================================================================
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
  expect_error(get_type(NA), "Can't get type: all values of are \"NA\"")
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

  df <- data.frame(observed = 1:10, predicted = factor(1:10), model = "model")
  expect_error(
    get_forecast_type(df),
    "input doesn't satisfy criteria for any forecast type",
    fixed = TRUE
  )
})


# ==============================================================================
# `get_coverage()`
# ==============================================================================
ex_coverage <- example_quantile[model == "EuroCOVIDhub-ensemble"]

test_that("get_coverage() works as expected", {
  cov <- example_quantile %>%
    na.omit() %>%
    as_forecast() %>%
    get_coverage(by = get_forecast_unit(example_quantile))

  expect_equal(
    sort(colnames(cov)),
    sort(c(get_forecast_unit(example_quantile), c(
      "interval_range", "quantile_level", "interval_coverage", "interval_coverage_deviation",
      "quantile_coverage", "quantile_coverage_deviation"
    )))
  )

  expect_equal(nrow(cov), nrow(na.omit(example_quantile)))
})

test_that("get_coverage() outputs an object of class c('data.table', 'data.frame'", {
  ex <- as_forecast(na.omit(example_quantile))
  cov <- get_coverage(ex)
  expect_s3_class(cov, c("data.table", "data.frame"), exact = TRUE)
})

test_that("get_coverage() can deal with non-symmetric prediction intervals", {
  # the expected result is that `get_coverage()` just works. However,
  # all interval coverages with missing values should just be `NA`
  test <- data.table::copy(example_quantile) %>%
    na.omit() %>%
    as_forecast()
  test <- test[!quantile_level %in% c(0.2, 0.3, 0.5)]

  expect_no_condition(cov <- get_coverage(test))

  prediction_intervals <- get_range_from_quantile(c(0.2, 0.3, 0.5))

  missing <- cov[interval_range %in% prediction_intervals]
  not_missing <- cov[!interval_range %in% prediction_intervals]

  expect_true(all(is.na(missing$interval_coverage)))
  expect_false(any(is.na(not_missing)))

  # test for a version where values are not missing, but just `NA`
  # since `get_coverage()` calls `na.omit`, the result should be the same.
  test <- data.table::copy(example_quantile) %>%
    na.omit() %>%
    as_forecast() %>%
    suppressMessages()
  test <- test[quantile_level %in% c(0.2, 0.3, 0.5), predicted := NA]
  cov2 <- get_coverage(test)
  expect_equal(cov, cov2)
})


# ==============================================================================
# `get_forecast_counts()`
# ==============================================================================
test_that("get_forecast_counts() works as expected", {
  af <- suppressMessages(as_forecast(example_quantile))
  af <- get_forecast_counts(
    af,
    by = c("model", "target_type", "target_end_date")
  )

  expect_type(af, "list")
  expect_type(af$target_type, "character")
  expect_type(af$`count`, "integer")
  expect_equal(nrow(af[is.na(`count`)]), 0)
  af <- na.omit(example_quantile) %>%
    as_forecast() %>%
    get_forecast_counts(by = "model")
  expect_equal(nrow(af), 4)
  expect_equal(af$`count`, c(256, 256, 128, 247))

  # Ensure the returning object class is exactly same as a data.table.
  expect_s3_class(af, c("data.table", "data.frame"), exact = TRUE)

  # Setting `collapse = c()` means that all quantiles and samples are counted
  af <- na.omit(example_quantile) %>%
    as_forecast() %>%
    get_forecast_counts(by = "model", collapse = c())
  expect_equal(nrow(af), 4)
  expect_equal(af$`count`, c(5888, 5888, 2944, 5681))

  # setting by = NULL, the default, results in by equal to forecast unit
  af <- na.omit(example_quantile) %>%
    as_forecast() %>%
    get_forecast_counts()
  expect_equal(nrow(af), 50688)

  # check whether collapsing also works for model-based forecasts
  af <- na.omit(example_integer) %>%
    as_forecast() %>%
    get_forecast_counts(by = "model")
  expect_equal(nrow(af), 4)

  af <- na.omit(example_integer) %>%
    as_forecast() %>%
    get_forecast_counts(by = "model", collapse = c())
  expect_equal(af$count, c(10240, 10240, 5120, 9880))
})

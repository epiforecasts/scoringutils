test_that("bias_sample() throws an error when missing observed", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    bias_sample(predicted = predicted),
    'argument "observed" is missing, with no default'
  )
})

test_that("bias_sample() throws an error when missing 'predicted'", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    bias_sample(observed = observed),
    'argument "predicted" is missing, with no default'
  )
})

test_that("bias_sample() works for integer observed and predicted", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(10, rpois(10, lambda = 1:10))
  output <- bias_sample(
    observed = observed,
    predicted = predicted
  )
  expect_equal(
    length(output),
    length(observed)
  )
  expect_equal(
    class(output),
    "numeric"
  )
})

test_that("bias_sample() works for continuous observed values and predicted", {
  observed <- rnorm(10)
  predicted <- replicate(10, rnorm(10))
  output <- bias_sample(
    observed = observed,
    predicted = predicted
  )
  expect_equal(
    length(output),
    length(observed)
  )
  expect_equal(
    class(output),
    "numeric"
  )
})

test_that("bias_sample() works as expected", {
  observed <- rpois(30, lambda = 1:30)
  predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
  expect_true(all(bias_sample(observed, predicted) == bias_sample(observed, predicted)))

  ## continuous forecasts
  observed <- rnorm(30, mean = 1:30)
  predicted <- replicate(200, rnorm(30, mean = 1:30))

  scoringutils2 <- bias_sample(observed, predicted)
  scoringutils <- bias_sample(observed, predicted)

  expect_equal(scoringutils, scoringutils2)
})

test_that("bias_quantile() handles NA values", {
  predicted <- c(NA, 1, 2, 3)
  quantiles <- c(0.1, 0.5, 0.9)

  expect_error(
    bias_quantile(observed = 2, predicted, quantiles),
    "`predicted` and `quantile` must have the same length"
  )
})

test_that("bias_quantile() returns NA if no predictions", {
  expect_true(is.na(bias_quantile(observed = 2, numeric(0), numeric(0))))
})

test_that("bias_quantile() returns correct bias if value below the median", {
  predicted <- c(1, 2, 4, 5)
  quantiles <- c(0.1, 0.3, 0.7, 0.9)
  suppressMessages(
    expect_equal(bias_quantile(observed = 1, predicted, quantiles), 0.8)
  )
})

test_that("bias_quantile() returns correct bias if value above median", {
  predicted <- c(1, 2, 4, 5)
  quantiles <- c(0.1, 0.3, 0.7, 0.9)
  suppressMessages(
    expect_equal(bias_quantile(observed = 5, predicted, quantiles), -0.8)
  )
})

test_that("bias_quantile() returns correct bias if value at the median", {
  predicted <- c(1, 2, 3, 4)
  quantiles <- c(0.1, 0.3, 0.5, 0.7)

  expect_equal(bias_quantile(observed = 3, predicted, quantiles), 0)
})

test_that("bias_quantile() returns 1 if true value below min prediction", {
  predicted <- c(2, 3, 4, 5)
  quantiles <- c(0.1, 0.3, 0.7, 0.9)

  suppressMessages(
    expect_equal(bias_quantile(observed = 1, predicted, quantiles), 1)
  )
})

test_that("bias_quantile() returns -1 if true value above max prediction", {
  predicted <- c(1, 2, 3, 4)
  quantiles <- c(0.1, 0.3, 0.5, 0.7)

  expect_equal(bias_quantile(observed = 6, predicted, quantiles), -1)
})

test_that("bias_quantile(): quantiles must be between 0 and 1", {
  predicted <- 1:4

  # Failing example
  quantiles <- c(-0.1, 0.3, 0.5, 0.8)
  expect_error(bias_quantile(observed = 3, predicted, quantiles),
               "quantiles must be between 0 and 1")

  # Passing counter example
  quantiles <- c(0.1, 0.3, 0.5, 0.8)
  expect_silent(bias_quantile(observed = 3, predicted, quantiles))
})

test_that("bias_quantile(): quantiles must be increasing", {
  predicted <- 1:4

  # Failing example
  quantiles <- c(0.8, 0.3, 0.5, 0.9)
  expect_error(bias_quantile(observed = 3, predicted, quantiles),
               "quantiles must be increasing")

  # Passing counter example
  quantiles <- c(0.3, 0.5, 0.8, 0.9)
  expect_silent(bias_quantile(observed = 3, predicted, quantiles))
})

test_that("bias_quantile(): predictions must be increasing", {
  predicted <- c(1, 2, 4, 3)
  quantiles <- c(0.1, 0.3, 0.5, 0.9)

  expect_error(
    bias_quantile(observed = 3, predicted, quantiles),
    "predictions must be increasing"
  )
  expect_silent(bias_quantile( observed = 3, 1:4, quantiles))
})

test_that("bias_quantile(): quantiles must be unique", {
  predicted <- 1:4

  # Failing example
  quantiles <- c(0.3, 0.3, 0.5, 0.8)
  expect_error(bias_quantile(observed = 3, predicted, quantiles),
               "quantiles must be increasing")

  # Passing example
  quantiles <- c(0.3, 0.5, 0.8, 0.9)
  expect_silent(bias_quantile(observed = 3, predicted, quantiles))
})

test_that("bias_sample() approx equals bias_quantile() for many samples", {
  set.seed(123)

  # Generate true value
  observed <- 3

  # Generate many sample predictions
  predicted <- sample(rnorm(1000, mean = observed, sd = 2), 1000)

  # Get sample based bias
  bias_sample_result <- bias_sample(
    observed, matrix(predicted, nrow = 1)
  )

  # Convert predictions to quantiles
  quantiles <- seq(0, 1, length.out = 100)
  quantile_preds <- quantile(predicted, probs = quantiles)

  # Get quantile based bias
  bias_quantile_result <-   suppressMessages(
    bias_quantile(observed, quantile_preds, quantiles)
  )

  # Difference should be small
  expect_equal(bias_quantile_result, bias_sample_result, tolerance = 0.1)
})

test_that("bias_quantile() and bias_range() give the same result", {
  predicted <- sort(rnorm(23))
  lower <- rev(predicted[1:12])
  upper <- predicted[12:23]

  range <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98)
  quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
  observed <- rnorm(1)

  range_bias <- bias_range(
    lower = lower, upper = upper,
    range = range, observed = observed
  )
  range_quantile <- bias_quantile(
    observed = observed,
    predicted = predicted,
    quantile = quantiles
  )
  expect_equal(range_bias, range_quantile)
})

test_that("bias_range() works with point forecasts", {
  predicted <- 1
  observed <- 1
  range <- c(0)

  expect_equal(bias_range(predicted, predicted, range, observed), 0)
})

test_that("bias_range(): ranges must be between 0 and 100", {
  lower <- 4:1
  upper <- 5:8

  # Failing example
  range <- c(-10, 0, 10, 20)
  expect_error(
    bias_range(lower, upper, range, observed = 3),
    "range must be between 0 and 100"
  )

  # Passing counter example
  range <- c(0, 10, 20, 30)
  expect_silent(bias_range(lower, upper, range, observed = 3))
})


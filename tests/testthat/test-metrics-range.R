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


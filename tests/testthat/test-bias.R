test_that("bias_sample() throws an error when missing true_values", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    bias_sample(predictions = predictions),
    "true_values argument is missing"
  )
})

test_that("bias_sample() throws an error when missing 'predictions'", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    bias_sample(true_values = true_values),
    "argument 'predictions' missing"
  )
})

test_that("bias_sample() works for integer true_values and predictions", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- replicate(10, rpois(10, lambda = 1:10))
  output <- bias_sample(
    true_values = true_values,
    predictions = predictions
  )
  expect_equal(
    length(output),
    length(true_values)
  )
  expect_equal(
    class(output),
    "numeric"
  )
})

test_that("bias_sample() works for continuous true_values and predictions", {
  true_values <- rnorm(10)
  predictions <- replicate(10, rnorm(10))
  output <- bias_sample(
    true_values = true_values,
    predictions = predictions
  )
  expect_equal(
    length(output),
    length(true_values)
  )
  expect_equal(
    class(output),
    "numeric"
  )
})

test_that("bias_sample() works as expected", {
  true_values <- rpois(30, lambda = 1:30)
  predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
  expect_true(all(bias_sample(true_values, predictions) == bias_sample(true_values, predictions)))

  ## continuous forecasts
  true_values <- rnorm(30, mean = 1:30)
  predictions <- replicate(200, rnorm(30, mean = 1:30))

  scoringutils2 <- bias_sample(true_values, predictions)
  scoringutils <- bias_sample(true_values, predictions)

  expect_equal(scoringutils, scoringutils2)
})

test_that("bias_quantile() handles NA values", {
  predictions <- c(NA, 1, 2, 3) 
  quantiles <- c(0.1, 0.5, 0.9)
  
  expect_error(
    bias_quantile(predictions, quantiles, true_value = 2),
    "predictions and quantiles must have the same length"
  )
})

test_that("bias_quantile() returns NA if no predictions", {
  expect_true(is.na(bias_quantile(numeric(0), numeric(0), true_value = 2)))
})

test_that("bias_quantile() returns correct bias if value below the median", {
  predictions <- c(1, 2, 4, 5)
  quantiles <- c(0.1, 0.3, 0.7, 0.9)
  suppressMessages(
    expect_equal(bias_quantile(predictions, quantiles, true_value = 1), 0.8)
  )
})

test_that("bias_quantile() returns correct bias if value above median", {
  predictions <- c(1, 2, 4, 5) 
  quantiles <- c(0.1, 0.3, 0.7, 0.9)
  suppressMessages(
    expect_equal(bias_quantile(predictions, quantiles, true_value = 5), -0.8) 
  )
})

test_that("bias_quantile() returns correct bias if value at the median", {
  predictions <- c(1, 2, 3, 4)
  quantiles <- c(0.1, 0.3, 0.5, 0.7) 
  
  expect_equal(bias_quantile(predictions, quantiles, true_value = 3), 0)
})

test_that("bias_quantile() returns 1 if true value below min prediction", {
  predictions <- c(2, 3, 4, 5)
  quantiles <- c(0.1, 0.3, 0.7, 0.9)
  
  suppressMessages(
    expect_equal(bias_quantile(predictions, quantiles, true_value = 1), 1)
  )
})

test_that("bias_quantile() returns -1 if true value above max prediction", {
  predictions <- c(1, 2, 3, 4)
  quantiles <- c(0.1, 0.3, 0.5, 0.7)

  expect_equal(bias_quantile(predictions, quantiles, true_value = 6), -1)  
})

test_that("bias_quantile(): quantiles must be between 0 and 1", {
  predictions <- 1:4
  
  # Failing example
  quantiles <- c(-0.1, 0.3, 0.5, 0.8)
  expect_error(bias_quantile(predictions, quantiles, true_value = 3),
               "quantiles must be between 0 and 1")
               
  # Passing counter example               
  quantiles <- c(0.1, 0.3, 0.5, 0.8)
  expect_silent(bias_quantile(predictions, quantiles, true_value = 3))
})

test_that("bias_quantile(): quantiles must be increasing", {
  predictions <- 1:4
  
  # Failing example
  quantiles <- c(0.8, 0.3, 0.5, 0.9)
  expect_error(bias_quantile(predictions, quantiles, true_value = 3),
               "quantiles must be increasing")
               
  # Passing counter example
  quantiles <- c(0.3, 0.5, 0.8, 0.9)
  expect_silent(bias_quantile(predictions, quantiles, true_value = 3))
})

test_that("bias_quantile(): predictions must be increasing", {
  predictions <- c(1, 2, 4, 3)
  quantiles <- c(0.1, 0.3, 0.5, 0.9)
  
  expect_error(
    bias_quantile(predictions, quantiles, true_value = 3),
    "predictions must be increasing"
  )
  expect_silent(bias_quantile(1:4, quantiles, true_value = 3))
})

test_that("bias_quantile(): quantiles must be unique", {
  predictions <- 1:4
  
  # Failing example
  quantiles <- c(0.3, 0.3, 0.5, 0.8) 
  expect_error(bias_quantile(predictions, quantiles, true_value = 3),
               "quantiles must be increasing")
               
  # Passing example
  quantiles <- c(0.3, 0.5, 0.8, 0.9)
  expect_silent(bias_quantile(predictions, quantiles, true_value = 3))
})

test_that("bias_sample() approx equals bias_quantile() for many samples", {
  set.seed(123)

  # Generate true value
  true_value <- 3
  
  # Generate many sample predictions
  predictions <- sample(rnorm(1000, mean = true_value, sd = 2), 1000)

  # Get sample based bias
  bias_sample_result <- bias_sample(
    true_value, matrix(predictions, nrow = 1)
  )

  # Convert predictions to quantiles
  quantiles <- seq(0, 1, length.out = 100)
  quantile_preds <- quantile(predictions, probs = quantiles)

  # Get quantile based bias
  bias_quantile_result <-   suppressMessages(
    bias_quantile(quantile_preds, quantiles, true_value)
  )

  # Difference should be small
  expect_equal(bias_quantile_result, bias_sample_result, tolerance = 0.1)
})

test_that("bias_quantile() and bias_range() give the same result", {
  predictions <- sort(rnorm(23))
  lower <- rev(predictions[1:12])
  upper <- predictions[12:23]

  range <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98)
  quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
  true_value <- rnorm(1)

  range_bias <- bias_range(
    lower = lower, upper = upper,
    range = range, true_value = true_value
  )
  range_quantile <- bias_quantile(
    predictions = predictions, quantiles = quantiles,
    true_value = true_value
  )
  expect_equal(range_bias, range_quantile)
})

test_that("bias_range() works with point forecasts", {
  predictions <- 1
  true_value <- 1
  range <- c(0)

  expect_equal(bias_range(predictions, predictions, range, true_value), 0)
})

test_that("bias_range(): ranges must be between 0 and 100", {
  lower <- 4:1 
  upper <- 5:8
  
  # Failing example
  range <- c(-10, 0, 10, 20)
  expect_error(
    bias_range(lower, upper, range, true_value = 3),
    "range must be between 0 and 100"
  )
               
  # Passing counter example               
  range <- c(0, 10, 20, 30)
  expect_silent(bias_range(lower, upper, range, true_value = 3))
})
test_that("Input handling", {
  observed <- rpois(30, lambda = 1:30)
  predicted <- replicate(20, rpois(n = 30, lambda = 1:30))
  expect_equal(length(crps_sample(observed, predicted)), 30)

  # should error when wrong prediction type is given
  predicted2 <- rpois(30, lambda = 1)
  expect_error(crps_sample(observed, predicted2),
               "Assertion on 'predicted' failed: Must be of type 'matrix', not 'integer'",
               fixed = TRUE
  )

  # predictions have wrong number of rows
  predicted3 <- replicate(20, rpois(n = 31, lambda = 1))
  expect_error(
    crps_sample(observed, predicted3),
    "Assertion on 'predicted' failed: Must have exactly 30 rows, but has 31 rows.",
    # "Mismatch: 'observed' has length `30`, but 'predicted' has `31` rows.",
    fixed = TRUE
  )

  # error with missing argument
  expect_error(crps_sample(predicted = predicted),
               'argument "observed" is missing, with no default',
               fixed = TRUE
  )
})



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


# `ae_median_sample` ===========================================================
test_that("ae_median_sample works", {
  observed <- rnorm(30, mean = 1:30)
  predicted_values <- rnorm(30, mean = 1:30)
  scoringutils <- ae_median_sample(observed, matrix(predicted_values))
  ae <- abs(observed - predicted_values)
  expect_equal(ae, scoringutils)
})

# `mad_sample()` ===============================================================
test_that("function throws an error when missing 'predicted'", {
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    mad_sample()
  )
})


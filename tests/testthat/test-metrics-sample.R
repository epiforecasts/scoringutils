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

test_that("crps works with separate results", {
  observed <- rpois(30, lambda = 1:30)
  predicted <- replicate(20, rpois(n = 30, lambda = 1:30))
  crps <- crps_sample(
    observed = observed,
    predicted = predicted,
    separate_results = TRUE
  )
  expect_equal(
    crps$crps, crps$dispersion + crps$overprediction + crps$underprediction
  )
})

test_that("crps is the sum of overprediction, underprediction, dispersion", {
  observed <- rpois(30, lambda = 1:30)
  predicted <- replicate(20, rpois(n = 30, lambda = 1:30))
  crps <- crps_sample(
    observed = observed,
    predicted = predicted
  )

  d <- dispersion_sample(observed, predicted)
  o <- overprediction_sample(observed, predicted)
  u <- underprediction_sample(observed, predicted)

  expect_equal(crps, d + o + u)

  observed <- rnorm(30, mean = 1:30)
  predicted <- replicate(20, rnorm(n = 30, mean = 1:30))
  crps <- crps_sample(
    observed = observed,
    predicted = predicted
  )

  d <- dispersion_sample(observed, predicted)
  o <- overprediction_sample(observed, predicted)
  u <- underprediction_sample(observed, predicted)

  expect_equal(crps, d + o + u)
})


test_that("crps_sample() components correspond to WIS components", {
  set.seed(123)
  nreplicates <- 15
  nsamples <- 2000

  observed <- rnorm(nreplicates, mean = seq_len(nreplicates))
  predicted <- replicate(
    nsamples, rnorm(n = nreplicates, mean = seq_len(nreplicates))
  )
  crps <- crps_sample(
    observed = observed,
    predicted = predicted
  )

  dcrps <- dispersion_sample(observed, predicted)
  ocrps <- overprediction_sample(observed, predicted)
  ucrps <- underprediction_sample(observed, predicted)

  levels <- seq(0.01, 0.99, by = 0.01)
  quantiles <- t(apply(predicted, 1, quantile, probs = levels))

  dwis <- dispersion_quantile(observed, quantiles, levels)
  owis <- overprediction_quantile(observed, quantiles, levels)
  uwis <- underprediction_quantile(observed, quantiles, levels)

  expect_true(all(abs(c(dcrps - dwis, ocrps - owis, ucrps - uwis)) < 0.01))
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


# ============================================================================ #
# pit_histogram_sample()
# ============================================================================ #

test_that("pit_histogram_sample() function throws an error when missing args", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    pit_histogram_sample(predicted = predicted),
    'argument "observed" is missing, with no default'
  )

  expect_error(
    pit_histogram_sample(observed = observed),
    'argument "predicted" is missing, with no default'
  )

  expect_error(
    pit_histogram_sample(predicted = predicted, observed = observed),
    'argument "quantiles" is missing, with no default'
  )

  expect_error(
    pit_histogram_sample(
      predicted = predicted, observed = observed,
      quantiles = seq(0, 1, by = 0.1), integers = "random"
    ),
    "Assertion on 'n_replicates with `integers` =  random' failed:"
  )
})

test_that("pit_histogram_sample() function works for integer observed and predicted", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(10, rpois(10, lambda = 1:10))
  output <- pit_histogram_sample(
    observed = observed,
    predicted = predicted,
    quantiles = seq(0, 1, by = 0.1)
  )
  expect_equal(
    length(output),
    10
  )

  checkmate::expect_class(output, "numeric")

  output2 <- pit_histogram_sample(
    observed = observed,
    predicted = predicted,
    quantiles = seq(0, 1, by = 0.1),
    integers = "random",
    n_replicates = 56
  )
})

test_that("pit_histogram_sample() function works for continuous observed and predicted", {
  observed <- rnorm(10)
  predicted <- replicate(10, rnorm(10))
  output <- pit_histogram_sample(
    observed = observed,
    predicted = predicted,
    quantiles = seq(0, 1, by = 0.1)
  )
  expect_equal(
    length(output),
    10
  )
})

test_that("pit_histogram_sample() works with a single observvation", {
  expect_no_condition(
    output <- pit_histogram_sample(
      observed = 2.5, predicted = 1.5:10.5, quantiles = seq(0, 1, by = 0.1)
    )
  )
  expect_equal(length(output), 10)

  # test discrete case
  expect_no_condition(
    output2 <- pit_histogram_sample(
      observed = 3, predicted = 1:10, quantiles = seq(0, 1, by = 0.1)
    )
  )
  expect_equal(length(output2), 10)
})

test_that("pit_histogram_sample() throws an error if inputs are wrong", {
  observed <- 1.5:20.5
  predicted <- replicate(100, 1.5:20.5)

  # expect an error if predicted cannot be coerced to a matrix
  expect_error(
    pit_histogram_sample(observed, function(x) {}),
    "Assertion on 'predicted' failed: Must be of type 'matrix'"
  )

  # expect an error if the number of rows in predicted does not match the length of observed
  expect_error(
    pit_histogram_sample(observed, predicted[1:10, ]),
    "Assertion on 'predicted' failed: Must have exactly 20 rows, but has 10 rows."
  )

  expect_warning(
    pit_histogram_sample(
      observed, predicted, quantiles = seq(0, 1, by = 0.1), n_replicates = 10
    ),
    "`n_replicates` is ignored when `integers` is not `random`"
  )
})

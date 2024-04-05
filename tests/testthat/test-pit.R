# ============================================================================ #
# Test `pit_sample()` function
# ============================================================================ #

test_that("pit_sample() function throws an error when missing args", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    pit_sample(predicted = predicted),
    'argument "observed" is missing, with no default'
  )

  expect_error(
    pit_sample(observed = observed),
    'argument "predicted" is missing, with no default'
  )
})

test_that("pit_sample() function works for integer observed and predicted", {
  observed <- rpois(10, lambda = 1:10)
  predicted <- replicate(10, rpois(10, lambda = 1:10))
  output <- pit_sample(
    observed = observed,
    predicted = predicted,
    n_replicates = 56
  )
  expect_equal(
    length(output),
    560
  )
})

test_that("pit_sample() function works for continuous observed and predicted", {
  observed <- rnorm(10)
  predicted <- replicate(10, rnorm(10))
  output <- pit_sample(
    observed = observed,
    predicted = predicted,
    n_replicates = 56
  )
  expect_equal(
    length(output),
    10
  )
})

test_that("pit_sample() works with a single observvation", {
  expect_no_condition(
    output <- pit_sample(observed = 2.5, predicted = 1.5:10.5)
  )
  expect_equal(length(output), 1)

  # test discrete case
  expect_no_condition(
    output2 <- pit_sample(
      observed = 3, predicted = 1:10, n_replicates = 24
    )
  )
  expect_equal(length(output2), 24)
})


# ============================================================================ #
# Test `get_pit()` function
# ============================================================================ #

test_that("pit function works for continuous integer and quantile data", {
  pit_quantile <- suppressMessages(as_forecast(example_quantile)) %>%
    get_pit(by = "model")
  pit_continuous <- suppressMessages(as_forecast(example_sample_continuous)) %>%
    get_pit(by = c("model", "target_type"))
  pit_integer <- suppressMessages(as_forecast(example_sample_discrete)) %>%
    get_pit(by = c("model", "location"))

  expect_equal(names(pit_quantile), c("model", "quantile_level", "pit_value"))
  expect_equal(names(pit_continuous), c("model", "target_type", "pit_value"))
  expect_equal(names(pit_integer), c("model", "location", "pit_value"))

  # check printing works
  testthat::expect_output(print(pit_quantile))
  testthat::expect_output(print(pit_continuous))
  testthat::expect_output(print(pit_integer))

  # check class is correct
  expect_s3_class(pit_quantile, c("data.table", "data.frame"), exact = TRUE)
  expect_s3_class(pit_continuous, c("data.table", "data.frame"), exact = TRUE)
  expect_s3_class(pit_integer, c("data.table", "data.frame"), exact = TRUE)
})

test_that("pit_sample() throws an error if inputs are wrong", {
  observed <- 1.5:20.5
  predicted <- replicate(100, 1.5:20.5)

  # expect an error if predicted cannot be coerced to a matrix
  expect_error(
    pit_sample(observed, function(x) {}),
    "Assertion on 'predicted' failed: Must be of type 'matrix'"
  )

  # expect an error if the number of rows in predicted does not match the length of observed
  expect_error(
    pit_sample(observed, predicted[1:10, ]),
    "Assertion on 'predicted' failed: Must have exactly 20 rows, but has 10 rows."
  )
})


test_that("Lower-level input check functions work", {
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

  # checks work for binary forecasts
  observed <- factor(sample(c(0, 1), size = 10, replace = TRUE))
  predicted <- runif(n = 10)
  expect_equal(length(brier_score(observed, predicted)), 10)

  # predictions are not between 0 and 1
  predicted2 <- predicted + 2
  expect_error(
    brier_score(observed, predicted2),
    "Assertion on 'predicted' failed: Element 1 is not <= 1.",
    fixed = TRUE
  )
})


test_that("function throws an error when missing observed or predicted", {
  observed <- sample(c(0, 1), size = 10, replace = TRUE)
  predicted <- replicate(
    20,
    sample(c(0, 1), size = 10, replace = TRUE)
  )

  expect_error(
    brier_score(predicted = predicted),
    'argument "observed" is missing, with no default'
  )

  expect_error(
    brier_score(observed = observed),
    'argument "predicted" is missing, with no default'
  )
})



test_that("function throws an error for wrong format of `observed`", {
  observed <- factor(rpois(10, lambda = 1:10))
  predicted <- runif(10, min = 0, max = 1)

  expect_error(
    brier_score(
      observed = observed,
      predicted = predicted
    ),
    "Assertion on 'observed' failed: Must have exactly 2 levels."
  )

  observed <- rnorm(10)
  expect_error(
    brier_score(
      observed = observed,
      predicted = predicted
    ),
    "Assertion on 'observed' failed: Must be of type 'factor', not 'double'."
  )
})

test_that("function throws an error for wrong format of predictions", {
  observed <- factor(sample(c(0, 1), size = 10, replace = TRUE))
  predicted <- runif(10, min = 0, max = 3)
  expect_error(
    brier_score(
      observed = observed,
      predicted = predicted
    ),
    #"For a binary forecast, all predictions should be probabilities between 0 or 1."
    "Assertion on 'predicted' failed: Element 1 is not <= 1."
  )

  predicted <- runif(10, min = 0, max = 1)
  expect_error(
    brier_score(
      observed = observed,
      predicted = as.list(predicted)
    ),
    "Assertion on 'predicted' failed: Must be of type 'numeric', not 'list'."
  )

  predicted <- runif(15, min = 0, max = 1)
  expect_error(
    brier_score(
      observed = observed,
      predicted = predicted
    ),
    "`observed` and `predicted` need to be of same length when scoring binary forecasts",
    # "Arguments to the following function call: 'brier_score(observed = observed, predicted = predicted)' should have the same length (or length one). Actual lengths: 10, 15",
    fixed = TRUE
  )
})

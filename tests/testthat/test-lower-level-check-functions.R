test_that("Lower-level input check functions work", {
  true_values <- rpois(30, lambda = 1:30)
  predictions <- replicate(20, rpois(n = 30, lambda = 1:30))
  expect_equal(length(crps_sample(true_values, predictions)), 30)

  # should error when wrong prediction type is given
  predictions2 <- rpois(30, lambda = 1)
  expect_error(crps_sample(true_values, predictions2),
    "'predictions' should be a matrix. Instead `integer` was found",
    fixed = TRUE
  )

  # predictions have wrong number of rows
  predictions3 <- replicate(20, rpois(n = 31, lambda = 1))
  expect_error(crps_sample(true_values, predictions3),
    "Mismatch: 'true_values' has length `30`, but 'predictions' has `31` rows.",
    fixed = TRUE
  )

  # error with missing argument
  expect_error(crps_sample(predictions = predictions),
    "true_values argument is missing",
    fixed = TRUE
  )

  # checks work for binary forecasts
  true_values <- sample(c(0, 1), size = 10, replace = TRUE)
  predictions <- runif(n = 10)
  expect_equal(length(brier_score(true_values, predictions)), 10)

  # true values are not either 0 or 1
  true_values2 <- true_values + 2
  expect_error(brier_score(true_values2, predictions),
    "For a binary forecast, all true_values should be either 0 or 1.",
    fixed = TRUE
  )

  # predictions are not between 0 and 1
  predictions2 <- predictions + 2
  expect_error(brier_score(true_values, predictions2),
    "For a binary forecast, all predictions should be probabilities between 0 or 1.",
    fixed = TRUE
  )
})


test_that("function throws an error when missing true_values or predictions", {
  true_values <- sample(c(0, 1), size = 10, replace = TRUE)
  predictions <- replicate(
    20,
    sample(c(0, 1), size = 10, replace = TRUE)
  )

  expect_error(
    brier_score(predictions = predictions),
    "true_values argument is missing"
  )

  expect_error(
    brier_score(true_values = true_values),
    "argument 'predictions' missing"
  )
})



test_that("function throws an error for wrong format of true_value", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- runif(10, min = 0, max = 1)

  expect_error(
    brier_score(
      true_values = true_values,
      predictions = predictions
    ),
    "For a binary forecast, all true_values should be either 0 or 1."
  )

  true_values <- rnorm(10)
  expect_error(
    brier_score(
      true_values = true_values,
      predictions = predictions
    ),
    "For a binary forecast, all true_values should be either 0 or 1."
  )
})

test_that("function throws an error for wrong format of predictions", {
  true_values <- sample(c(0, 1), size = 10, replace = TRUE)
  predictions <- runif(10, min = 0, max = 3)
  expect_error(
    brier_score(
      true_values = true_values,
      predictions = predictions
    ),
    "For a binary forecast, all predictions should be probabilities between 0 or 1."
  )

  predictions <- runif(10, min = 0, max = 1)
  expect_error(
    brier_score(
      true_values = true_values,
      predictions = list(predictions)
    ),
    "Mismatch: 'true_values' has length `10`, but 'predictions' has length `1`"
  )

  predictions <- runif(15, min = 0, max = 1)
  expect_error(
    brier_score(
      true_values = true_values,
      predictions = predictions
    ),
    "Mismatch: 'true_values' has length `10`, but 'predictions' has length `15`"
  )
})

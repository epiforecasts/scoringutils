observed <- factor(rbinom(10, size = 1, prob = 0.5))
predicted <- c(0.425, 0.55, 0.541, 0.52, 0.13, 0.469, 0.86, 0.22, 0.74, 0.9)
df <- data.table(
  observed = observed,
  predicted = predicted,
  model = "m1",
  id = 1:10
)

observed_point <- rnorm(10)
predicted_point <- rnorm(10)

# ==============================================================================
# Test Input Checks - this also checks point inputs where functions are similar
# ==============================================================================
test_that("correct input works", {
  expect_no_condition(assert_input_binary(observed, predicted))
  expect_no_condition(assert_input_point(observed_point, predicted_point))

  # observed is a single number and does not have the same length as predicted
  expect_no_condition(
    assert_input_binary(factor(1, levels = c(0, 1)), predicted)
  )
  expect_no_condition(
    assert_input_point(1, predicted_point)
  )

  # predicted is a single number and does not have the same length as observed
  expect_no_condition(assert_input_binary(observed, predicted = 0.2))
  expect_no_condition(assert_input_point(observed_point, predicted = 0.2))

  # predicted is a matrix with nrow equal to observed
  expect_no_condition(assert_input_binary(observed, matrix(predicted)))
  expect_no_condition(assert_input_point(observed_point, matrix(predicted_point)))
})

# test input handling
test_that("function throws an error for wrong input formats", {
  # observed value not as expected
  expect_error(
    assert_input_binary(observed = rnorm(10), predicted = predicted),
    "Assertion on 'observed' failed: Must be of type 'factor', not 'double'."
  )
  expect_error(
    assert_input_binary(1:10, predicted),
    "Assertion on 'observed' failed: Must be of type 'factor', not 'integer'."
  )
  expect_error(
    assert_input_binary(observed = observed, predicted = as.list(predicted)),
    "Assertion on 'predicted' failed: Must be of type 'numeric', not 'list'."
  )
  expect_error(
    assert_input_point(observed = factor(rnorm(10)), predicted = predicted),
    "Assertion on 'observed' failed: Must be of type 'numeric', not 'factor'."
  )
  expect_error(
    assert_input_point(observed = observed_point, list(predicted_point)),
    "Assertion on 'predicted' failed: Must be of type 'numeric', not 'list'."
  )

  # observed value has not 2 levels
  expect_error(
    assert_input_binary(factor(1:10), predicted),
    "Assertion on 'observed' failed: Must have exactly 2 levels."
  )

  # wrong length
  expect_error(
    assert_input_binary(observed = observed, predicted = runif(15, min = 0, max = 1)),
    "`observed` and `predicted` must either be of length 1 or of equal length.",
    fixed = TRUE
  )
  expect_error(
    assert_input_point(observed_point, runif(15, min = 0, max = 1)),
    "`observed` and `predicted` must either be of length 1 or of equal length",
    fixed = TRUE
  )

  # predicted > 1
  expect_error(
    assert_input_binary(observed, predicted + 1),
    "Assertion on 'predicted' failed: Element 1 is not <= 1."
  )

  # predicted < 0
  expect_error(
    assert_input_binary(observed, predicted - 1),
    "Assertion on 'predicted' failed: Element 1 is not >= 0."
  )

  # predicted is a matrix with one row
  expect_error(
    assert_input_binary(observed, predicted = matrix(0.2)),
    "Assertion failed. One of the following must apply:\n * check_vector(predicted): Must be of type 'vector', not 'matrix'\n * check_matrix(predicted): Must have exactly 10 rows, but has 1 rows",
    fixed = TRUE)
  expect_error(
    assert_input_point(observed_point, predicted = matrix(0.2)),
    "Assertion failed. One of the following must apply:\n * check_vector(predicted): Must be of type 'vector', not 'matrix'\n * check_matrix(predicted): Must have exactly 10 rows, but has 1 rows",
    fixed = TRUE)

  # predicted is a matrix with 2 rows
  expect_error(
    assert_input_binary(observed, matrix(rep(predicted, 2), ncol = 2)),
    "Assertion failed. One of the following must apply:\n * check_vector(predicted): Must be of type 'vector', not 'matrix'\n * check_matrix(predicted): Must have exactly 1 cols, but has 2 cols",
    fixed = TRUE
  )
})


# ==============================================================================
# Test Binary Metrics
# ==============================================================================

test_that("function throws an error when missing observed or predicted", {
  expect_error(
    brier_score(predicted = predicted),
    'argument "observed" is missing, with no default'
  )

  expect_error(
    brier_score(observed = observed),
    'argument "predicted" is missing, with no default'
  )
})

test_that("Brier score works with different inputs", {
  # observed is a single number and does not have the same length as predicted
  expect_equal(
    brier_score(factor(1, levels = c(0, 1)), predicted),
    (1 - predicted)^2
  )

  # predicted is a single number and does not have the same length as observed
  expect_equal(
    brier_score(observed, predicted = 0.2),
    ifelse(observed == 1, (1 - 0.2)^2, (0.2)^2)
  )

  # predicted is a matrix with 1 row
  expect_error(
    brier_score(observed, predicted = matrix(0.2)),
    "Assertion failed. One of the following must apply:\n * check_vector(predicted): Must be of type 'vector', not 'matrix'\n * check_matrix(predicted): Must have exactly 10 rows, but has 1 rows",
    fixed = TRUE
  )

  # predicted is an array
  expect_error(
    brier_score(observed, predicted = array(0.2)),
    "Assertion failed. One of the following must apply:\n * check_vector(predicted): Must be of type 'vector', not 'array'\n * check_matrix(predicted): Must be of type 'matrix', not 'array'",
    fixed = TRUE
  )
})


test_that("Binary metrics work within and outside of `score()`", {
  result <- score(as_forecast_binary(df))
  expect_equal(
    brier_score(observed, predicted),
    result$brier_score
  )

  expect_equal(
    logs_binary(observed, predicted),
    result$log_score
  )
})

test_that("`logs_binary()` works as expected", {
  # check against the function Metrics::ll
  obs2 <- as.numeric(as.character(observed))
  expect_equal(
    logs_binary(observed, predicted),
    Metrics::ll(obs2, predicted)
  )

  # check this works for a single observed value
  expect_equal(
    logs_binary(observed[1], predicted),
    Metrics::ll(obs2[1], predicted)
  )

  # check this works for a single predicted value
  expect_equal(
    logs_binary(observed, predicted[1]),
    Metrics::ll(obs2, predicted[1])
  )
})

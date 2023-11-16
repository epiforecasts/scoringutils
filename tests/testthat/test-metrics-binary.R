observed <- factor(rbinom(10, size = 1, prob = 0.5))
predicted <- c(0.425, 0.55, 0.541, 0.52, 0.13, 0.469, 0.86, 0.22, 0.74, 0.9)
df <- data.table(
  observed = observed,
  predicted = predicted,
  model = "m1",
  id = 1:10
)


test_that("correct input works", {
  expect_no_condition(
    scoringutils:::assert_input_binary(observed, predicted)
  )

  # observed is a single number and does not have the same length as predicted
  expect_no_condition(
    scoringutils:::assert_input_binary(factor(1, levels = c(0, 1)), predicted)
  )

  # predicted is a single number and does not have the same length as observed
  expect_no_condition(
    scoringutils:::assert_input_binary(observed, predicted = 0.2)
  )

  # predicted is a matrix with one row
  expect_no_condition(
    scoringutils:::assert_input_binary(observed, predicted = matrix(0.2))
  )

  # predicted is a matrix with 1 row - this is will throw a warning in the
  # actual scoring rule function
  expect_no_condition(
    scoringutils:::assert_input_binary(observed, matrix(predicted))
  )
})

# test input handling
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


test_that("function throws an error for wrong input formats", {
  expect_error(
    brier_score(
      observed = rnorm(10),
      predicted = predicted
    ),
    "Assertion on 'observed' failed: Must be of type 'factor', not 'double'."
  )

  # observed value not factor
  expect_error(
    scoringutils:::assert_input_binary(1:10, predicted),
    "Assertion on 'observed' failed: Must be of type 'factor', not 'integer'."
  )

  # observed value has not 2 levels
  expect_error(
    scoringutils:::assert_input_binary(factor(1:10), predicted),
    "Assertion on 'observed' failed: Must have exactly 2 levels."
  )

  predicted <- runif(10, min = 0, max = 1)
  expect_error(
    brier_score(observed = observed, predicted = as.list(predicted)),
    "Assertion on 'predicted' failed: Must be of type 'numeric', not 'list'."
  )

  # wrong length
  expect_error(
    brier_score(observed = observed, predicted = runif(15, min = 0, max = 1)),
    "Assertion on 'observed' failed: `observed` and `predicted` must either be of length 1 or of equal length. Found 10 and 15.",
    fixed = TRUE
  )

  # predicted > 1
  expect_error(
    scoringutils:::assert_input_binary(observed, predicted + 1),
    "Assertion on 'predicted' failed: Element 1 is not <= 1."
  )

  # predicted < 0
  expect_error(
    scoringutils:::assert_input_binary(observed, predicted - 1),
    "Assertion on 'predicted' failed: Element 1 is not >= 0."
  )

  # predicted is a matrix with 2 rows
  expect_error(
    scoringutils:::assert_input_binary(observed, matrix(rep(predicted, 2), ncol = 2)),
    "Assertion on 'observed' failed: `predicted` must be a vector or a matrix with one column. Found 2 columns."
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
  expect_warning(
    brier_score(observed, predicted = matrix(0.2)),
    "Recycling array of length 1 in vector-array arithmetic is deprecated.
  Use c() or as.vector() instead.",
  fixed = TRUE
  )

  expect_warning(
    brier_score(observed, predicted = array(0.2)),
    "Recycling array of length 1 in vector-array arithmetic is deprecated.
  Use c() or as.vector() instead.",
  fixed = TRUE
  )
})


test_that("Binary metrics work within and outside of `score()`", {
  result <- score(df)
  expect_equal(
    brier_score(observed, predicted),
    result$brier_score
  )

  expect_equal(
    logs_binary(observed, predicted),
    result$log_score
  )
})

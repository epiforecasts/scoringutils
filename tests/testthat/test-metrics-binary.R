observed <- factor(rbinom(10, size = 1, prob = 0.5))
predicted <- c(0.425, 0.55, 0.541, 0.52, 0.13, 0.469, 0.86, 0.22, 0.74, 0.9)
df <- data.table(
  observed = observed,
  predicted = predicted,
  model = "m1",
  id = 1:10
)

# test input handling
test_that("Input checking for binary forecasts works", {
  # everything correct
  expect_no_condition(
    scoringutils:::assert_input_binary(observed, predicted)
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

  # observed is a single number and does not have the same length as predicted
  expect_error(
    scoringutils:::assert_input_binary(factor(1), predicted),
    "`observed` and `predicted` need to be of same length when scoring binary forecasts."
  )

  # predicted is a matrix
  expect_error(
    scoringutils:::assert_input_binary(observed, matrix(predicted)),
    "Assertion on 'predicted' failed: Must be of type 'atomic vector', not 'matrix'."
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

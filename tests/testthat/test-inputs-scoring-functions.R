# ==============================================================================
# assert_dims_ok_point() # nolint: commented_code_linter
# ==============================================================================

test_that("assert_dims_ok_point() works as expected", {
  # expect no error if dimensions are ok
  expect_no_condition(assert_dims_ok_point(1:10, 1:10))

  expect_error(
    assert_dims_ok_point(1:3, matrix(1:6, nrow = 3, ncol = 2)),
    "Assertion failed. One of the following must apply:"
  )


  # expect error if dimensions are not ok
  expect_error(
    assert_dims_ok_point(1:10, 1:11),
    "`observed` and `predicted` must either be of length 1 or of equal length."
  )
})


# ==============================================================================
# check_input_sample() removed (issue #684) # nolint: commented_code_linter
# ==============================================================================

test_that("check_input_sample is no longer defined", {
  expect_error(check_input_sample(1:10, matrix(1:20, nrow = 10)))
})

test_that("assert_input_sample still works after check_input_sample removal", {
  observed <- 1:10
  predicted <- matrix(1:20, nrow = 10)
  expect_no_condition(assert_input_sample(observed, predicted))
  expect_error(assert_input_sample(1:10, 1:11))
})

test_that("scoring functions still work end-to-end without check_input_* wrappers", {
  observed_s <- 1:5
  predicted_s <- matrix(rnorm(50), nrow = 5)
  observed_q <- 1:5
  predicted_q <- matrix(sort(rnorm(25)), nrow = 5)
  quantile_level <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  expect_no_condition(crps_sample(observed_s, predicted_s))
  expect_no_condition(bias_quantile(observed_q, predicted_q, quantile_level))
})

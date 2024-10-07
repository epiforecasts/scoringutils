


# ==============================================================================
# assert_dims_ok_point()
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
# check_input_sample()
# ==============================================================================

test_that("check_input_sample() works as expected", {
  # expect no error if dimensions are ok
  expect_true(check_input_sample(1:10, matrix(1:20, nrow = 10)))

  # expect error if dimensions are not ok
  expect_match(
    check_input_sample(1:10, 1:11),
    "Assertion on 'predicted' failed: Must be of type 'matrix', not 'integer'."
  )
})
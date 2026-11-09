# ==============================================================================
# assert_dims_ok_scalar() # nolint: commented_code_linter
# ==============================================================================

test_that("assert_dims_ok_scalar() works as expected", {
  # expect no error if dimensions are ok
  expect_no_condition(assert_dims_ok_scalar(1:10, 1:10))

  expect_error(
    assert_dims_ok_scalar(1:3, matrix(1:6, nrow = 3, ncol = 2)),
    "Assertion failed. One of the following must apply:"
  )


  # expect error if dimensions are not ok
  expect_error(
    assert_dims_ok_scalar(1:10, 1:11),
    "`observed` and `predicted` must either be of length 1 or of equal length."
  )
})

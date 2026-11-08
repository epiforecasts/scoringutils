observed <- rnorm(30, mean = 1:30)
interval_range <- rep(90, 30)
alpha <- (100 - interval_range) / 100
lower <- qnorm(alpha / 2, rnorm(30, mean = 1:30))
upper <- qnorm((1 - alpha / 2), rnorm(30, mean = 11:40))


# ==============================================================================
# assert_input_interval() # nolint: commented_code_linter
# ==============================================================================
test_that("assert_input_interval() works as expected", {
  expect_no_condition(
    assert_input_interval(observed, lower, upper, interval_range)
  )

  # expect error if upper < lower
  expect_error(
    assert_input_interval(observed, upper, lower, interval_range),
    "All values in `upper` need to be greater than or equal to the corresponding values in `lower`"
  )

  # The warning for interval ranges < 1 is only shown once per session by
  # design. With rlang's default verbosity, a repeated call must be silent,
  # whether or not the warning was already shown earlier in the session.
  old_options <- options(rlib_warning_verbosity = "default")
  on.exit(options(old_options), add = TRUE)
  suppressWarnings(assert_input_interval(observed, lower, upper, 0.5))
  expect_no_warning(assert_input_interval(observed, lower, upper, 0.5))

  # Force the warning to be shown every time to check its content. Without
  # this, the test fails when the suite is run repeatedly in the same
  # session (#718).
  options(rlib_warning_verbosity = "verbose")
  expect_warning(
    assert_input_interval(observed, lower, upper, 0.5),
    "Found interval ranges between 0 and 1. Are you sure that's right?"
  )
  expect_warning(
    assert_input_interval(observed, lower, upper, 0.5),
    "Found interval ranges between 0 and 1. Are you sure that's right?"
  )
})

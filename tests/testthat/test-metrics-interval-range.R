observed <- rnorm(30, mean = 1:30)
interval_range <- rep(90, 30)
alpha <- (100 - interval_range) / 100
lower <- qnorm(alpha / 2, rnorm(30, mean = 1:30))
upper <- qnorm((1 - alpha / 2), rnorm(30, mean = 11:40))


# ==============================================================================
# assert_input_interval()
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

  # expect warning if interval range is < 1
  expect_warning(
    assert_input_interval(observed, lower, upper, 0.5),
    "Found interval ranges between 0 and 1. Are you sure that's right?"
  )
})


test_that("check_input_interval() works as expected", {
  expect_no_condition(
    check_input_interval(observed, lower, upper, interval_range)
  )
  # expect message return if upper < lower
  expect_match(
    check_input_interval(observed, upper, lower, interval_range),
    regexp = "All values in `upper` need to be greater than or equal"
  )
})

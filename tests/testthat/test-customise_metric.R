test_that("customise_metric works correctly", {
  # Create a customized metric function
  custom_metric <- customise_metric(mean, na.rm = TRUE)

  # Use the customized metric function
  values <- c(1, 2, NA, 4, 5)
  expect_equal(custom_metric(values), 3)

  # Test with a different metric function
  custom_metric <- customise_metric(sum, na.rm = TRUE)
  expect_equal(custom_metric(values), 12)

  # Test with no additional arguments
  custom_metric <- customise_metric(mean)
  expect_error(custom_metric(values), "argument 'na.rm' is missing, with no default")
})

test_that("customise_metric handles errors correctly", {
  # Test with a non-function metric
  expect_error(customise_metric("not_a_function", na.rm = TRUE), "is not a function")
})
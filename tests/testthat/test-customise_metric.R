test_that("customise_metric works correctly", {
  # Create a customised metric function
  custom_metric <- customise_metric(mean, na.rm = TRUE)

  # Use the customised metric function
  values <- c(1, 2, NA, 4, 5)
  expect_equal(custom_metric(values), 3)

  # Test with a different metric function
  custom_metric <- customise_metric(sum, na.rm = TRUE)
  expect_equal(custom_metric(values), 12)

  # Test with no additional arguments
  custom_metric <- customise_metric(mean)
  expect_true(is.na(custom_metric(values)))

  # make sure that customise_metric fails immediately (instead of at runtime)
  # when object doesn't exist
  expect_error(
    custom_metric <- customise_metric(print, x = doesnotexist),
    "object 'doesnotexist' not found"
  )

  # make sure that customise_metric still works even if original object is
  # deleted, meaning that the object is stored as part of the function
  argument <- c("hi", "hello", "I'm here")
  custom_metric <- customise_metric(print, x = argument)
  expect_output(custom_metric(), "I'm here")

  argument <- NULL
  expect_output(custom_metric(), "I'm here")

  # make sure that all of this still works even if argument is called "dots"
  # which is used internally
  dots <- "test"
  expect_output(
    # dots argument should be ignored and output should stay the same
    expect_equal(custom_metric(dots = dots), c("hi", "hello", "I'm here")),
    "I'm here"
  )
})



test_that("customise_metric handles errors correctly", {
  # Test with a non-function metric
  expect_error(
    customise_metric("not_a_function", na.rm = TRUE),
    "Must be a function, not 'character'"
  )
})

test_that("customise_metric is exported", {
  expect_equal(customise_metric, customise_metric)
})

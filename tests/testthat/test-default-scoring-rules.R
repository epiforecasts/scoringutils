# ==============================================================================
# select_metrics()
# ==============================================================================

test_that("`select_metrics` works as expected", {

  expect_equal(
    scoringutils:::select_metrics(metrics_point(), select = NULL),
    metrics_point()
  )

  expect_equal(
    scoringutils:::select_metrics(metrics_point(), select = NULL),
    scoringutils:::select_metrics(metrics_point())
  )

  expect_equal(
    names(scoringutils:::select_metrics(metrics_point(), select = "ape")),
    "ape"
  )

  expect_equal(
    length(scoringutils:::select_metrics(metrics_point(), select = NULL, exclude = "ape")),
    length(metrics_point()) - 1
  )

  # if both select and exclude are specified, exclude is ignored
  expect_equal(
    names(scoringutils:::select_metrics(metrics_point(), select = "ape", exclude = "ape")),
    "ape"
  )

  # expect error if possibilities is not a list
  expect_error(
    scoringutils:::select_metrics(metrics_point, select = NULL),
    "Assertion on 'metrics' failed: Must be of type 'list', not 'closure'."
  )

  expect_type(
    scoringutils:::select_metrics(metrics_point(), select = NULL),
    "list"
  )
})


# ==============================================================================
# customise_metric()
# ==============================================================================

test_that("customise_metric handles errors correctly", {
  # Test with a non-function metric
  expect_error(
    customise_metric("not_a_function", na.rm = TRUE),
    "Must be a function, not 'character'"
  )
})

test_that("customize_metric is exported", {
  expect_equal(customise_metric, customize_metric)
})


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

test_that("customise_metric() has the expected output class", {
  custom_metric <- customise_metric(mean, na.rm = TRUE)
  checkmate::expect_class(custom_metric, "function")
})


# ==============================================================================
# default scoring rules
# ==============================================================================

test_that("default rules work as expected", {

  expect_true(
    all(c(
      is.list(metrics_point()),
      is.list(metrics_binary()),
      is.list(metrics_quantile()),
      is.list(metrics_sample()))
    )
  )

  expect_equal(
    names(metrics_point(select = "ape")),
    "ape"
  )

  expect_equal(
    length(metrics_binary(select = NULL, exclude = "brier_score")),
    length(metrics_binary()) - 1
  )

  # if both select and exclude are specified, exclude is ignored
  expect_equal(
    names(scoringutils:::select_metrics(metrics_quantile(), select = "wis", exclude = "wis")),
    "wis"
  )

  # expect error if select is not included in the default possibilities
  expect_error(
    metrics_sample(select = "not-included"),
    "Must be a subset of"
  )

  # expect error if exclude is not included in the default possibilities
  expect_error(
    metrics_quantile(exclude = "not-included"),
    "Must be a subset of"
  )
})

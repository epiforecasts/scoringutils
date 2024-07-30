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
# Customising metrics using purrr::partial()
# ==============================================================================
test_that("customising metrics via purr::partial works correctly", {
  # Create a customised metric function
  custom_metric <- purrr::partial(mean, na.rm = TRUE)

  # Use the customised metric function
  values <- c(1, 2, NA, 4, 5)
  expect_equal(custom_metric(values), 3)

  # Test with a different metric function
  custom_metric <- purrr::partial(sum, na.rm = TRUE)
  expect_equal(custom_metric(values), 12)

  # Test with no additional arguments
  custom_metric <- purrr::partial(mean)
  expect_true(is.na(custom_metric(values)))

  # make sure that unquoting argument works even if original object is
  # deleted, meaning that the object is stored as part of the function
  argument <- c("hi", "hello", "I'm here")
  custom_metric <- purrr::partial(print, x = !!argument)
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

test_that("purrr::partial() has the expected output class", {
  custom_metric <- purrr::partial(mean, na.rm = TRUE)
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

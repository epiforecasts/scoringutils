# ==============================================================================
# `get_forecast_type` # nolint: commented_code_linter
# ==============================================================================
test_that("get_forecast_type() works as expected", {
  expect_identical(get_forecast_type(example_quantile), "quantile")
  expect_identical(get_forecast_type(example_sample_continuous), "sample")
  expect_identical(get_forecast_type(example_sample_discrete), "sample")
  expect_identical(get_forecast_type(example_binary), "binary")
  expect_identical(get_forecast_type(example_point), "point")
  expect_identical(get_forecast_type(example_nominal), "nominal")

  expect_error(
    get_forecast_type(data.frame(x = 1:10)),
    "Input is not a valid forecast object",
    fixed = TRUE
  )

  test <- test <- data.table::copy(example_quantile)
  class(test) <- c("forecast", "data.table", "data.frame")
  expect_error(
    get_forecast_type(test),
    "Input is not a valid forecast object"
  )

  # get_forecast_type() should still work even if a new class is added
  testclassobject <- data.table::copy(example_quantile)
  class(testclassobject) <- c("something", class(testclassobject))
  expect_identical(get_forecast_type(testclassobject), "quantile")
})


# ==============================================================================
# `get_vector_type()` # nolint: commented_code_linter
# ==============================================================================
test_that("get_vector_type() works as expected with vectors", {
  expect_identical(get_vector_type(1:3), "integer")
  expect_identical(get_vector_type(factor(1:2)), "classification")
  expect_identical(get_vector_type(c(1.0, 2)), "integer")
  expect_identical(get_vector_type(c(1.0, 2.3)), "continuous")
  expect_error(
    get_vector_type(c("a", "b")),
    "Assertion on 'as.vector(x)' failed: Must be of type 'numeric', not 'character'.",
    fixed = TRUE
  )
})

test_that("get_vector_type() works as expected with matrices", {
  expect_identical(get_vector_type(matrix(1:4, nrow = 2)), "integer")
  expect_identical(get_vector_type(matrix(c(1.0, 2:4))), "integer")
  expect_identical(get_vector_type(matrix(c(1.0, 2.3, 3, 4))), "continuous")

  # matrix of factors doesn't work
  expect_error(
    get_vector_type(matrix(factor(1:4), nrow = 2)),
    "Assertion on 'as.vector(x)' failed: Must be of type 'numeric', not 'character'.",
    fixed = TRUE
  )

  expect_error(
    get_vector_type(matrix(c("a", "b", "c", "d"))),
    "Assertion on 'as.vector(x)' failed: Must be of type 'numeric', not 'character'.",
    fixed = TRUE
  )
})


test_that("get_vector_type() is consistent with former get_type()", {
  get_prediction_type <- function(data) {
    if (is.data.frame(data)) {
      data <- data$predicted
    }
    if (
      isTRUE(all.equal(as.vector(data), as.integer(data))) &&
        !all(is.na(as.integer(data)))
    ) {
      return("integer")
    } else if (suppressWarnings(!all(is.na(as.numeric(data))))) {
      return("continuous")
    } else {
      stop("Input is not numeric and cannot be coerced to numeric", call. = FALSE)
    }
  }

  check_data <- list(
    1:2,
    # factor(1:2) # old function would classify as "continuous" # nolint: commented_code_linter
    c(1.0, 2),
    c(1.0, 2.3),
    matrix(1:4, nrow = 2),
    matrix(c(1.0, 2:4)),
    matrix(c(1.0, 2.3, 3, 4))
  )

  for (i in seq_along(check_data)) {
    expect_identical(
      get_prediction_type(check_data[[i]]),
      get_vector_type(check_data[[i]])
    )
  }
})

test_that("get_vector_type() handles `NA` values", {
  expect_identical(get_vector_type(c(1, NA, 3)), "integer")
  expect_identical(get_vector_type(c(1, NA, 3.2)), "continuous")
  expect_error(get_vector_type(NA), "Can't get type: all values of are \"NA\"")
})

test_that("get_vector_type() is exported and accessible", {
  expect_identical(scoringutils::get_vector_type(1:3), "integer")
})


# ==============================================================================
# `get_observed_type()` # nolint: commented_code_linter
# ==============================================================================
test_that("get_observed_type() returns the type of the observed column", {
  expect_identical(get_observed_type(example_sample_discrete), "integer")
  expect_identical(get_observed_type(example_binary), "classification")
  expect_true(get_observed_type(example_sample_continuous) %in%
    c("integer", "continuous"))
})

test_that("get_observed_type() errors on non-forecast objects", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  expect_error(get_observed_type(df))
})

test_that("get_observed_type() is exported and accessible", {
  expect_true(
    scoringutils::get_observed_type(example_sample_continuous) %in%
      c("integer", "continuous")
  )
})


# ==============================================================================
# `get_predicted_type()` # nolint: commented_code_linter
# ==============================================================================
test_that("get_predicted_type() returns the type of the predicted column", {
  expect_identical(get_predicted_type(example_sample_continuous), "continuous")
  expect_identical(get_predicted_type(example_sample_discrete), "integer")
  expect_identical(get_predicted_type(example_binary), "continuous")
})

test_that("get_predicted_type() errors on non-forecast objects", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  expect_error(get_predicted_type(df))
})

test_that("get_predicted_type() is exported and accessible", {
  expect_identical(
    scoringutils::get_predicted_type(example_sample_continuous),
    "continuous"
  )
})

test_that("get_predicted_type() works for quantile forecasts", {
  expect_true(get_predicted_type(example_quantile) %in%
    c("integer", "continuous"))
})

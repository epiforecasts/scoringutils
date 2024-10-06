# ============================================================================ #
# `transform_forecasts()`
# ============================================================================ #
test_that("function transform_forecasts works", {
  predictions_original <- example_quantile$predicted
  predictions <- example_quantile %>%
    transform_forecasts(
    fun = function(x) pmax(0, x),
    append = FALSE
  )

  expect_equal(predictions$predicted, pmax(0, predictions_original))

  one <- transform_forecasts(predictions, offset = 1)
  expect_equal(
    one$predicted,
    c(predictions$predicted, log(predictions$predicted + 1))
  )

  two <- transform_forecasts(predictions, fun = sqrt, label = "sqrt")
  expect_equal(
    two$predicted,
    c(predictions$predicted, sqrt(predictions$predicted))
  )


  # expect a warning + error if you add a second transformation with the same label
  expect_error(
    expect_warning(
      transform_forecasts(one, fun = sqrt),
      "Appending new transformations with label 'log' even though that entry is already present in column 'scale'"
    ),
    "There are instances with more than one forecast for the same target. "
  )


  # multiple transformations
  three <- transform_forecasts(one, fun = sqrt, label = "sqrt")
  expect_equal(unique(three$scale), c("natural", "log", "sqrt"))

  # expect_error if there is a scale column, but no value "natural"
  no_natural <- three[three$scale != "natural", ]
  expect_error(
    transform_forecasts(no_natural, fun = identity),
    "If a column 'scale' is present, entries with scale =='natural' are required for the transformation."
  )

  # multiple transformations without append
  four <- transform_forecasts(two, fun = log_shift, offset = 1, append = FALSE)
  compare <- c(
    one$predicted[one$scale == "log"],
    three$predicted[three$scale == "sqrt"]
  )

  expect_equal(four$predicted, compare)
})

test_that("transform_forecasts() outputs an object of class forecast_*", {
  transformed <- transform_forecasts(example_binary, fun = identity, append = FALSE)
  expect_s3_class(transformed, "forecast_binary")
})


# ============================================================================ #
# `log_shift()`
# ============================================================================ #
test_that("log_shift() works as expected", {
  expect_equal(log_shift(1:10, 1), log(1:10 + 1))

  # expect errors if there are values < 0
  expect_error(
    log_shift(c(1, 0, -1), 1),
    "Detected input values < 0."
  )

  # expect errors if there are zeros
  expect_warning(
    log_shift(c(1, 0, 1), offset = 0),
    "Detected zeros in input values."
  )

  # test that it does not accept a complex number
  expect_error(log_shift(1:10, offset = 1, base = 1i))

  # test that it does not accept a negative base
  expect_error(
    log_shift(1:10, offset = 1, base = -1),
    "Assertion on 'base' failed: Element 1 is not >= 0."
  )

  # test output class is numeric as expected
  checkmate::expect_class(log_shift(1:10, 1), "numeric")
})

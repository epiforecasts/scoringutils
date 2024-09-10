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


# ============================================================================ #
# `set_forecast_unit()`
# ============================================================================ #

test_that("function set_forecast_unit() works", {
  # some columns in the example data have duplicated information. So we can remove
  # these and see whether the result stays the same.
  scores1 <- scores_quantile[order(location, target_end_date, target_type, horizon, model), ]

  # test that if setting the forecast unit results in an invalid object,
  # a warning occurs.
  expect_warning(
    set_forecast_unit(example_quantile, "model"),
    "Assertion on 'data' failed: There are instances with more"
  )

  ex2 <- set_forecast_unit(
    example_quantile,
    c("location", "target_end_date", "target_type", "horizon", "model")
  )
  scores2 <- score(na.omit(ex2))
  scores2 <- scores2[order(location, target_end_date, target_type, horizon, model), ]

  expect_equal(scores1$interval_score, scores2$interval_score)
})

test_that("set_forecast_unit() works on input that's not a data.table", {
  df <- data.frame(
    a = 1:2,
    b = 2:3,
    c = 3:4
  )
  expect_equal(
    colnames(set_forecast_unit(df, c("a", "b"))),
    c("a", "b")
  )

  expect_equal(
    names(set_forecast_unit(as.matrix(df), "a")),
    "a"
  )

  expect_s3_class(
    set_forecast_unit(df, c("a", "b")),
    c("data.table", "data.frame"),
    exact = TRUE
  )
})

test_that("set_forecast_unit() revalidates a forecast object", {
  obj <- as_forecast_quantile(na.omit(example_quantile))
  expect_no_condition(
    set_forecast_unit(obj, c("location", "target_end_date", "target_type", "model", "horizon"))
  )
})


test_that("function set_forecast_unit() errors when column is not there", {
  expect_error(
    set_forecast_unit(
      example_quantile,
      c("location", "target_end_date", "target_type", "horizon", "model", "test1", "test2")
    ),
    "Assertion on 'forecast_unit' failed: Must be a subset of "
  )
})

test_that("function get_forecast_unit() and set_forecast_unit() work together", {
  fu_set <- c("location", "target_end_date", "target_type", "horizon", "model")
  ex <- set_forecast_unit(example_binary, fu_set)
  fu_get <- get_forecast_unit(ex)
  expect_equal(fu_set, fu_get)
})

test_that("output class of set_forecast_unit() is as expected", {
  ex <- as_forecast_binary(na.omit(example_binary))
  expect_equal(
    class(ex),
    class(set_forecast_unit(ex, c("location", "target_end_date", "target_type", "horizon", "model")))
  )
})


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


# ==============================================================================
# `get_forecast_unit()`
# ==============================================================================
test_that("get_forecast_unit() works as expected", {
  fc <- c(
    "location", "target_end_date", "target_type", "location_name",
    "forecast_date", "model", "horizon"
  )

  expect_equal(get_forecast_unit(example_quantile), fc)
  expect_equal(get_forecast_unit(scores_quantile), fc)

  # test with data.frame
  expect_equal(get_forecast_unit(as.data.frame(example_quantile)), fc)
})

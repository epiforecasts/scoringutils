test_that("function transform_forecasts works", {

  predictions_original <- example_quantile$prediction
  predictions <- transform_forecasts(
    example_quantile,
    fun = function(x) pmax(0, x),
    append = FALSE
  )

  expect_equal(predictions$prediction, pmax(0, predictions_original))

  one <- transform_forecasts(predictions, offset = 1)
  expect_equal(one$prediction,
               c(predictions$prediction, log(predictions$prediction + 1)))

  two <- transform_forecasts(predictions, fun = sqrt, label = "sqrt")
  expect_equal(two$prediction,
               c(predictions$prediction, sqrt(predictions$prediction)))


  # expect a warning if existing transformation is overwritten
  expect_warning(
    transform_forecasts(one, fun = sqrt)
  )

  # multiple transformations
  three <- transform_forecasts(one, fun = sqrt, label = "sqrt")
  expect_equal(unique(three$scale), c("natural", "log", "sqrt"))

  # multiple transformations without append
  four <- transform_forecasts(two, fun = log_shift, offset = 1, append = FALSE)
  compare <- c(
    one$prediction[one$scale == "log"],
    three$prediction[three$scale == "sqrt"]
  )

  expect_equal(four$prediction, compare)
})



test_that("function set_forecast_unit() works", {

  # some columns in the example data have duplicated information. So we can remove
  # these and see whether the result stays the same.

  scores1 <- suppressMessages(score(example_quantile))
  scores1 <- scores1[order(location, target_end_date, target_type, horizon, model), ]

  ex2 <- set_forecast_unit(
    example_quantile,
    c("location", "target_end_date", "target_type", "horizon", "model")
  )
  scores2 <- suppressMessages(score(ex2))
  scores2 <- scores2[order(location, target_end_date, target_type, horizon, model), ]

  expect_equal(scores1$interval_score, scores2$interval_score)
})


test_that("function set_forecast_unit() gives warning when column is not there", {

  expect_warning(
    set_forecast_unit(
      example_quantile,
      c("location", "target_end_date", "target_type", "horizon", "model", "test")
    )
  )
})


test_that("function get_forecast_unit() and set_forecast_unit() work together", {

  fu_set <- c("location", "target_end_date", "target_type", "horizon", "model")

  ex <- set_forecast_unit(
    example_binary,
    fu_set
  )

  fu_get <- get_forecast_unit(ex)
  expect_equal(fu_set, fu_get)
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
  # apparently it also works on a matrix... good to know :)
  expect_equal(
    names(set_forecast_unit(as.matrix(df), "a")),
    "a"
  )
})

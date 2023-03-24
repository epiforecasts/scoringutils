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


  # expect a warning if existing transformation is overwritte
  expect_warning(
    transform_forecasts(one, fun = sqrt)
  )

  # multiple transformations
  three <- transform_forecasts(one, fun = sqrt, label = "sqrt")
  expect_equal(unique(three$scale), c("natural", "log", "sqrt"))
})

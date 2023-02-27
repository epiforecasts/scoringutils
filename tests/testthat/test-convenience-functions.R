test_that("function add_transformation works", {

  predictions <- example_quantile$prediction

  one <- suppressWarnings(add_transformation(example_quantile))
  expect_equal(one$prediction, c(predictions, log(predictions + 1)))

  two <- suppressWarnings(
    add_transformation(example_quantile, fun = sqrt, label = "sqrt")
  )
  expect_equal(two$prediction, c(predictions, sqrt(predictions)))
})

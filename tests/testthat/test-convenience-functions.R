test_that("function add_transformation works", {

  prediction <- example_quantile$prediction

  one <- suppressWarnings(add_transformation(example_quantile))
  expect_equal(one$prediction, c(prediction, log(predictions + 1)))

  two <- suppressWarnings(
    add_transformation(example_quantile, fun = sqrt, label = "sqrt")
  )
  expect_equal(two$prediction, c(prediction, sqrt(predictions)))
})

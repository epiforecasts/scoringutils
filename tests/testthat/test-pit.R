test_that("pit_sample() function throws an error when missing true_values", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    pit_sample(predictions = predictions),
    "true_values` or `predictions` missing in function 'pit_sample()"
  )
})

test_that("pit_sample() function throws an error when missing 'predictions'", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

  expect_error(
    pit_sample(predictions = predictions),
    "true_values` or `predictions` missing in function 'pit_sample()"
  )
})


test_that("pit_sample() function works for integer true_values and predictions", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- replicate(10, rpois(10, lambda = 1:10))
  output <- pit_sample(
    true_values = true_values,
    predictions = predictions,
    n_replicates = 56
  )
  expect_equal(
    length(output),
    560
  )
})

test_that("pit_sample() function works for continuous true_values and predictions", {
  true_values <- rnorm(10)
  predictions <- replicate(10, rnorm(10))
  output <- pit_sample(
    true_values = true_values,
    predictions = predictions,
    n_replicates = 56
  )
  expect_equal(
    length(output),
    10
  )
})

test_that("pit function works for continuous integer and quantile data", {
  pit1 <- suppressMessages(pit(example_quantile, by = "model"))
  pit2 <- suppressMessages(pit(example_continuous,
    by = c("model", "target_type")
  ))
  pit3 <- suppressMessages(pit(example_integer,
    by = c("model", "location")
  ))

  expect_equal(names(pit1), c("model", "quantile", "pit_value"))
  expect_equal(names(pit2), c("model", "target_type", "pit_value"))
  expect_equal(names(pit3), c("model", "location", "pit_value"))
})

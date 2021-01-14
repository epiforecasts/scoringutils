test_that("function throws an error when missing true_values",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(bias(predictions = predictions),
                         "true_values or predictions argument missing")
          })

test_that("function throws an error when missing 'predictions'",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(bias(true_values = true_values),
                         "true_values or predictions argument missing")
          })

test_that("function works for integer true_values and predictions",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(10, rpois(10, lambda = 1:10))
            output <- bias(true_values = true_values,
                           predictions = predictions)
            expect_equal(length(output),
                         length(true_values))
            expect_equal(class(output),
                         "numeric")
          })

test_that("function works for continuous true_values and predictions",
          {
            true_values <- rnorm(10)
            predictions <- replicate(10, rnorm(10))
            output <- bias(true_values = true_values,
                           predictions = predictions)
            expect_equal(length(output),
                         length(true_values))
            expect_equal(class(output),
                         "numeric")
          })



test_that("bias works", {
  true_values <- rpois(30, lambda = 1:30)
  predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
  all(scoringutils::bias(true_values, predictions) == scoringutils::bias(true_values, predictions))

  ## continuous forecasts
  true_values <- rnorm(30, mean = 1:30)
  predictions <- replicate(200, rnorm(30, mean = 1:30))

  scoringutils2 <- scoringutils::bias(true_values, predictions)
  scoringutils <- scoringutils::bias(true_values, predictions)

  expect_equal(scoringutils, scoringutils2)
})


test_that("quantile bias works", {
  lower <- c(6341.000, 6329.500, 6087.014, 5703.500,
             5451.000, 5340.500, 4821.996, 4709.000,
             4341.500, 4006.250, 1127.000, 705.500)

  upper <- c(6341.000, 6352.500, 6594.986, 6978.500,
             7231.000, 7341.500, 7860.004, 7973.000,
             8340.500, 8675.750, 11555.000, 11976.500)

  range <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98)

  true_value <- 8062

  scoringutils2 <- scoringutils::quantile_bias(lower = lower, upper = upper,
                                                range = range, true_value = true_value)
  scoringutils <- scoringutils:: quantile_bias(lower = lower, upper = upper,
                                               range = range, true_value = true_value)

  expect_equal(scoringutils, scoringutils2)
})



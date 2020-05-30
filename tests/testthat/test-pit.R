
# ===================================================================== #
# pit
# ===================================================================== #


test_that("function throws an error when missing true_values",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(pit(predictions = predictions),
                         "true_values or predictions argument missing")
          })

test_that("function throws an error when missing 'predictions'",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(pit(predictions = predictions),
                         "true_values or predictions argument missing")
          })


test_that("function works for integer true_values and predictions",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(10, rpois(10, lambda = 1:10))
            output <- pit(true_values = true_values,
                          predictions = predictions)
            expect_equal(length(output),
                         3)
            expect_equal(class(output),
                         "list")
            expect_equal(class(output[[1]]),
                         "numeric")
          })

test_that("function works for continuous true_values and predictions",
          {
            true_values <- rnorm(10)
            predictions <- replicate(10, rnorm(10))
            output <- pit(true_values = true_values,
                          predictions = predictions)
            expect_equal(length(output),
                         3)
            expect_equal(class(output),
                         "list")
            expect_equal(class(output[[1]]),
                         "numeric")
          })




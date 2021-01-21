test_that("function throws an error when missing true_values",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(pit(predictions = predictions),
                         "true_values` or `predictions` missing in function 'pit()")
          })

test_that("function throws an error when missing 'predictions'",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(pit(predictions = predictions),
                         "true_values` or `predictions` missing in function 'pit()")
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




# compare results scoringutils2 with scoringutils

test_that("function works for continuous true_values and predictions",
          {
            ## continuous predictions
            true_values <- rnorm(30, mean = 1:30)
            predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
            scoringutils2 <- scoringutils::pit(true_values, predictions)
            scoringutils <- scoringutils::pit(true_values, predictions)

            expect_equal(scoringutils2$p_value, scoringutils$p_value)
          })








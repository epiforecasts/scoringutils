# ===================================================================== #
# Test Error Handling
# ===================================================================== #

# ===================================================================== #
# Brier_score

test_that("function throws an error when missing true_values",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- replicate(20,
                                     sample(c(0,1), size = 10, replace = TRUE))

            expect_error(brier_score(predictions = predictions))
          })

test_that("function throws an error when missing 'predictions'",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- replicate(20,
                                     sample(c(0,1), size = 10, replace = TRUE))

            expect_error(brier_score(true_values = true_values))
          })

test_that("function works with probabilities as well as predictive samples
          for 'predictions'",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- replicate(20,
                                     sample(c(0,1), size = 10, replace = TRUE))

            expect_equal(class(brier_score(true_values = true_values,
                                           predictions = predictions)),
                         "numeric")

            predictions <- runif(10, min = 0, max = 1)
            expect_equal(class(brier_score(true_values = true_values,
                                           predictions = predictions)),
                         "numeric")
          })



test_that("function throws an error for wrong format of true_value",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- runif(10, min = 0, max = 1)

            expect_error(brier_score(true_values = true_values,
                                     predictions = predictions))
            true_values <- rnorm(10)
            expect_error(brier_score(true_values = true_values,
                                     predictions = predictions))
          })

test_that("function throws an error for wrong format of predictions",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- sample(c(0,1), size = 10, replace = TRUE)

            expect_warning(brier_score(true_values = true_values,
                                       predictions = predictions))

            predictions <- runif(10, min = 0, max = 3)
            expect_error(brier_score(true_values = true_values,
                                     predictions = predictions))

            predictions <- runif(10, min = 0, max = 1)
            expect_error(brier_score(true_values = true_values,
                                     predictions = list(predictions)))

            predictions <- runif(15, min = 0, max = 1)
            expect_error(brier_score(true_values = true_values,
                                     predictions = predictions))
          })

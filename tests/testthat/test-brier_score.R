test_that("function throws an error when missing true_values or predictions",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- replicate(20,
                                     sample(c(0,1), size = 10, replace = TRUE))

            expect_error(brier_score(predictions = predictions),
                         "true_values or predictions argument missing")

            expect_error(brier_score(true_values = true_values),
                         "true_values or predictions argument missing")
          })



test_that("function throws an error for wrong format of true_value",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- runif(10, min = 0, max = 1)

            expect_error(brier_score(true_values = true_values,
                                     predictions = predictions),
                         "elements of true_values should be either zero or one")

            true_values <- rnorm(10)
            expect_error(brier_score(true_values = true_values,
                                     predictions = predictions),
                         "elements of true_values should be either zero or one")
          })

test_that("function throws an error for wrong format of predictions",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- runif(10, min = 0, max = 3)
            expect_error(brier_score(true_values = true_values,
                                     predictions = predictions),
                         "elements of 'predictions' should be probabilites between zero and one")

            predictions <- runif(10, min = 0, max = 1)
            expect_error(brier_score(true_values = true_values,
                                     predictions = list(predictions)),
                         "Mismatch: 'true_values' has length `10`, but 'predictions' has length `1`")

            predictions <- runif(15, min = 0, max = 1)
            expect_error(brier_score(true_values = true_values,
                                     predictions = predictions),
                         "Mismatch: 'true_values' has length `10`, but 'predictions' has length `15`")
          })


test_that("brier_score works", {
  true_values <- sample(c(0,1), size = 30, replace = TRUE)
  predictions <- runif(n = 30, min = 0, max = 1)

  scoringutils2 <- scoringutils::brier_score(true_values, predictions)
  scoringutils <- scoringutils::brier_score(true_values, predictions)
  expect_equal(scoringutils2, scoringutils)
})

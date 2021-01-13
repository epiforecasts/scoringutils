test_that("function throws an error when missing 'predictions'",
          {
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(sharpness(),
                         "predictions argument missing")
          })


test_that("pit() function throws an error when missing true_values",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(pit(predictions = predictions),
                         "true_values` or `predictions` missing in function 'pit()")
          })

test_that("pit() function throws an error when missing 'predictions'",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(50, rpois(n = 10, lambda = 1:10))

            expect_error(pit(predictions = predictions),
                         "true_values` or `predictions` missing in function 'pit()")
          })


test_that("pit() function works for integer true_values and predictions",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- replicate(10, rpois(10, lambda = 1:10))
            output <- pit(true_values = true_values,
                          predictions = predictions,
                          n_replicates = 56)
            expect_equal(length(output),
                         560)
          })

test_that("pit() function works for continuous true_values and predictions",
          {
            true_values <- rnorm(10)
            predictions <- replicate(10, rnorm(10))
            output <- pit(true_values = true_values,
                          predictions = predictions,
                          n_replicates = 56)
            expect_equal(length(output),
                         10)
          })

test_that("pit_df function works for continuous integer and quantile data",
          {
            pit1 <- pit_df(example_quantile, summarise_by = "model")
            pit2 <- pit_df(example_continuous,
                           summarise_by = c("model", "target_type"))
            pit3 <- pit_df(example_integer,
                           summarise_by = c("model", "location"))

            expect_equal(names(pit1), c("model", "quantile", "pit_value"))
            expect_equal(names(pit2), c("model", "target_type", "pit_value"))
            expect_equal(names(pit3), c("model", "location", "pit_value"))
          })








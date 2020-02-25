
# ===================================================================== #
# Test Error Handling
# ===================================================================== #

test_that("function throws an error for wrong input for prediction_type", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- list(replicate(50, rpois(n = 10, lambda = 1:10)))

  expect_error(eval_forecasts(true_values = true_values,
                              predictions = predictions,
                              prediction_type = "wrong",
                              outcome_type = "integer"))
})

test_that("function throws an error for wrong input for outcome_type", {
  true_values <- rpois(10, lambda = 1:10)
  predictions <- list(replicate(50, rpois(n = 10, lambda = 1:10)))

  expect_error(eval_forecasts(true_values = true_values,
                              predictions = predictions,
                              prediction_type = "probabilistic",
                              outcome_type = "wrong"))
})

# ===================================================================== #
# Test combination probabilistic / binary

test_that("function throws an error for wrong format of true_value
          for prediction_type 'probabilistic' and for outcome_type 'binary'",
          {
            true_values <- rpois(10, lambda = 1:10)
            predictions <- runif(10, min = 0, max = 1)

            expect_error(eval_forecasts(true_values = true_values,
                                        predictions = predictions,
                                        prediction_type = "probabilistic",
                                        outcome_type = "binary"))
          })

test_that("function throws an error for wrong format of predictions
          for prediction_type 'probabilistic' and for outcome_type 'binary'",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- rpois(10, lambda = 1)

            expect_error(eval_forecasts(true_values = true_values,
                                        predictions = predictions,
                                        prediction_type = "probabilistic",
                                        outcome_type = "binary"))
          })

test_that("function works for correct format of true_value and predictions
          for prediction_type 'probabilistic' and for outcome_type 'binary'",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- runif(10, min = 0, max = 1)

            expect_equal(class(eval_forecasts(true_values = true_values,
                                              predictions = predictions,
                                              prediction_type = "probabilistic",
                                              outcome_type = "binary")),
                         "data.frame")
          })

test_that("function works for correct format of true_value and prediction matrix
          for prediction_type 'probabilistic' and for outcome_type 'binary'",
          {
            true_values <- sample(c(0,1), size = 10, replace = TRUE)
            predictions <- replicate(20,
                                     sample(c(0,1), size = 10, replace = TRUE))

            expect_equal(class(eval_forecasts(true_values = true_values,
                                              predictions = predictions,
                                              prediction_type = "probabilistic",
                                              outcome_type = "binary")),
                         "data.frame")
          })

# "function throws an error for wrong format of true_values"

# "function throws an error for wrong format of predictions"



# test_that("function throws an error, if prediction_type is 'integer', but
#           predictions are not integers", {
#   expect_equal(class(output),
#                "data.frame")
# })



# ===================================================================== #
# Test Output Format
# ===================================================================== #

true_values <- rpois(100, lambda = 1:100)
predictions <- list(replicate(5000, rpois(n = 100, lambda = 1:100)))
output <- eval_forecasts(true_values = true_values,
                         predictions = predictions,
                         prediction_type = "probabilistic",
                         outcome_type = "integer")


test_that("function outputs a data.frame given only one set of predictions", {
  expect_equal(class(output),
               "data.frame")
})

# ===================================================================== #

output <- eval_forecasts(true_values = true_values,
                         predictions = predictions,
                         prediction_type = "probabilistic",
                         outcome_type = "integer",
                         output = "list")


test_that("function outputs a list given only one set of predictions", {
  expect_equal(class(output),
               "list")
})



# ===================================================================== #

predictions = list(dat1 = replicate(5000, rpois(n = 100, lambda = 1:100)),
                   dat2 = replicate(5000, rpois(n = 100, lambda = 1:100)))

output <- eval_forecasts(true_values = true_values,
                         predictions = predictions,
                         prediction_type = "probabilistic",
                         outcome_type = "integer")

test_that("function outputs a data.frame if given multiple prediction sets", {
  expect_equal(class(output),
               "data.frame")
})


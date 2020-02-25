
# ===================================================================== #
# Test Error Handling
# ===================================================================== #

# "function throws an error for wrong input for prediction_type"

# "function throws an error for wrong input for outcome_type"

# "function throws an error for wrong format of true_values"

# "function throws an error for wrong format of predictions"



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


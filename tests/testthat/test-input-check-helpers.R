test_that("Check equal length works if all arguments have length 1", {
  out <- interval_score(
    observed = 5,
    lower = 4,
    upper = 6,
    interval_range = 95,
    weigh = TRUE,
    separate_results = FALSE
  )
  expect_equal(out, 0.05)
})

test_that("assert_not_null works", {
  test_function <- function(argument = NULL) {
    scoringutils:::assert_not_null("argument" = argument)
    return(paste("Input:", argument))
  }
  out <- test_function("works")
  expect_equal(out, "Input: works")
  expect_error(test_function())
})

test_that("check_quantiles works", {
  expect_null(
    check_quantiles(range = c(0.4, 0.5), quantiles = c(0.4, 0.5))
  )
  expect_error(
    check_quantiles(range = c(0.4, 0.5), quantiles = c(0.3, 0.6)),
    "must be between"
  )
  expect_error(
    check_quantiles(range = c(0.4, 0.5), quantiles = c(0.41, 0.4)),
    "must be increasing."
  )
})

test_that("assure_model_column works", {
  expect_message(
    assure_model_column(example_binary[, model := NULL]),
    "There is no column called `model` in the data."
  )
})

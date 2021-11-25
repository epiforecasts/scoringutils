test_that("check_forecasts() function works", {
  check <- check_forecasts(quantile_example_data)
  expect_s3_class(check, "scoringutils_check")
})


test_that("check_forecasts() function has an error for empty data.frame", {
  expect_error(check_forecasts(data.frame()))
})

test_that("check_forecasts() function returns a warning with NA in the data", {
  check <- check_forecasts(quantile_example_data)
  expect_equal(unlist(check$warnings),
               "Some values for `prediction` are NA in the data provided")
})

test_that("check_forecasts() function returns warnings with NA in the data", {
  example <- data.table::copy(quantile_example_data)
  example[horizon == 7, true_value := NA]
  check <- check_forecasts(example)

  expect_equal(length(check$warnings), 2)
})


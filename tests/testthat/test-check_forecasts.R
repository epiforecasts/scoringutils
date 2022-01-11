test_that("check_forecasts() function works", {
  check <- check_forecasts(example_quantile)
  expect_s3_class(check, "scoringutils_check")
})

test_that("check_forecasts() function has an error for empty data.frame", {
  expect_error(check_forecasts(data.frame()))
})

test_that("check_forecasts() function returns a warning with NA in the data", {
  check <- check_forecasts(example_quantile)
  expect_equal(
    unlist(check$warnings),
    "Some values for `prediction` are NA in the data provided"
  )
})

test_that("check_forecasts() function returns warnings with NA in the data", {
  example <- data.table::copy(example_quantile)
  example[horizon == 2, true_value := NA]
  check <- check_forecasts(example)

  expect_equal(length(check$warnings), 2)
})

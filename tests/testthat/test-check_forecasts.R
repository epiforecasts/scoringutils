test_that("check_forecasts() function works", {
  check <- check_forecasts(example_quantile)
  expect_s3_class(check, "scoringutils_check")
})

test_that("check_forecasts() function has an error for empty data.frame", {
  expect_error(check_forecasts(data.frame()))
})

test_that("check_forecasts() function returns a message with NA in the data", {
  check <- check_forecasts(example_quantile)
  expect_equal(
    unlist(check$messages),
    "Some values for `prediction` are NA in the data provided and the corresponding rows were removed. This may indicate a problem if unexpected."
  )
})

test_that("check_forecasts() function returns messages with NA in the data", {
  example <- data.table::copy(example_quantile)
  example[horizon == 2, true_value := NA]
  check <- check_forecasts(example)

  expect_equal(length(check$messages), 2)
})

test_that("check_forecasts() function throws an error with duplicate forecasts", {
  example <- rbind(example_quantile,
                   example_quantile[1000:1010])

  expect_error(check_forecasts(example))
})

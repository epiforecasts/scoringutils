test_that("check_forecasts() function works", {
  check <- suppressMessages(check_forecasts(example_quantile))
  expect_s3_class(check, "scoringutils_check")
})

test_that("check_forecasts() function has an error for empty data.frame", {
  expect_error(suppressMessages(check_forecasts(data.frame())))
})

test_that("check_forecasts() function returns a message with NA in the data", {
  expect_message(
    { check <- check_forecasts(example_quantile) },
    "\\d+ values for `prediction` are NA"
  )
  expect_match(
    unlist(check$messages),
    "\\d+ values for `prediction` are NA"
  )
})

test_that("check_forecasts() function returns messages with NA in the data", {
  example <- data.table::copy(example_quantile)
  example[horizon == 2, true_value := NA]
  check <- suppressMessages(check_forecasts(example))

  expect_equal(length(check$messages), 2)
})

test_that("check_forecasts() function throws an error with duplicate forecasts", {
  example <- rbind(example_quantile,
                   example_quantile[1000:1010])

  expect_error(suppressMessages(suppressWarnings(check_forecasts(example))))
})

test_that("check_forecasts() function throws an error when no model column is
           present", {
  no_model <- data.table::copy(example_quantile)[, model := NULL]
  expect_error(suppressMessages(suppressWarnings(check_forecasts(no_model))))
})

test_that("check_forecasts() function throws an error when no predictions or
           true values are present", {
  expect_error(suppressMessages(suppressWarnings(check_forecasts(
    data.table::copy(example_quantile)[, prediction := NA]
  ))))
  expect_error(suppressMessages(suppressWarnings(check_forecasts(
    data.table::copy(example_quantile)[, true_value := NA]
  ))))
})

test_that("check_forecasts() function throws an sample/quantile not present", {
  expect_error(suppressMessages(suppressWarnings(check_forecasts(
    data.table::copy(example_quantile)[, quantile := NULL]
  ))))
})

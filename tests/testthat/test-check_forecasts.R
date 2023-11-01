test_that("check_forecasts() function works", {
  check <- suppressMessages(check_forecasts(example_quantile))
  expect_s3_class(check, "scoringutils_check")
})

test_that("check_forecasts() function has an error for empty data.frame", {
  expect_error(suppressMessages(check_forecasts(data.frame())))
})

test_that("check_columns_present() works", {
  expect_equal(
    check_columns_present(example_quantile, c("observed", "predicted"), "nop"),
    "Data needs to have a column called 'nop'"
  )
  expect_equal(
    check_columns_present(example_quantile, "observed", "predicted"),
    TRUE
  )
})

test_that("check_duplicates() works", {
  bad <- rbind(
    example_quantile[1000:1010],
    example_quantile[1000:1010]
  )

  expect_equal(scoringutils:::check_duplicates(bad),
    "There are instances with more than one forecast for the same target. This can't be right and needs to be resolved. Maybe you need to check the unit of a single forecast and add missing columns? Use the function find_duplicates() to identify duplicate rows."
  )
})

test_that("check_forecasts() function returns a message with NA in the data", {
  expect_message(
    { check <- check_forecasts(example_quantile) },
    "\\d+ values for `predicted` are NA"
  )
  expect_match(
    unlist(check$messages),
    "\\d+ values for `predicted` are NA"
  )
})

test_that("check_forecasts() function returns messages with NA in the data", {
  example <- data.table::copy(example_quantile)
  example[horizon == 2, observed := NA]
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
           observed values are present", {
  expect_error(suppressMessages(suppressWarnings(check_forecasts(
    data.table::copy(example_quantile)[, predicted := NA]
  ))))
  expect_error(suppressMessages(suppressWarnings(check_forecasts(
    data.table::copy(example_quantile)[, observed := NA]
  ))))
})

test_that("check_forecasts() function throws an sample/quantile not present", {
  expect_error(suppressMessages(suppressWarnings(check_forecasts(
    data.table::copy(example_quantile)[, quantile := NULL]
  ))))
})

test_that("output of check_forecasts() is accepted as input to score()", {
  check <- suppressMessages(check_forecasts(example_binary))
  expect_no_error(
    score_check <- score(check)
  )
  expect_equal(score_check, suppressMessages(score(example_binary)))
})


test_that("as_forecast() function works", {
  check <- suppressMessages(as_forecast(example_quantile))
  expect_s3_class(check, "forecast_quantile")
})

test_that("as_forecast() function has an error for empty data.frame", {
  expect_error(suppressMessages(as_forecast(data.frame())))
})

test_that("check_columns_present() works", {
  expect_equal(
    check_columns_present(example_quantile, c("observed", "predicted", "nop")),
    "Column 'nop' not found in data"
  )
  expect_true(
    check_columns_present(example_quantile, c("observed", "predicted"))
  )
})

test_that("check_duplicates() works", {
  bad <- rbind(
    example_quantile[1000:1010],
    example_quantile[1000:1010]
  )

  expect_equal(scoringutils:::check_duplicates(bad),
    "There are instances with more than one forecast for the same target. This can't be right and needs to be resolved. Maybe you need to check the unit of a single forecast and add missing columns? Use the function get_duplicate_forecasts() to identify duplicate rows"
  )
})

# test_that("as_forecast() function returns a message with NA in the data", {
#   expect_message(
#     { check <- as_forecast(example_quantile) },
#     "\\d+ values for `predicted` are NA"
#   )
#   expect_match(
#     unlist(check$messages),
#     "\\d+ values for `predicted` are NA"
#   )
# })

# test_that("as_forecast() function returns messages with NA in the data", {
#   example <- data.table::copy(example_quantile)
#   example[horizon == 2, observed := NA]
#   check <- suppressMessages(as_forecast(example))
#
#   expect_equal(length(check$messages), 2)
# })

test_that("as_forecast() function throws an error with duplicate forecasts", {
  example <- rbind(example_quantile,
                   example_quantile[1000:1010])

  expect_error(
    suppressMessages(suppressWarnings(as_forecast(example))),
    "Assertion on 'data' failed: There are instances with more than one forecast for the same target. This can't be right and needs to be resolved. Maybe you need to check the unit of a single forecast and add missing columns? Use the function get_duplicate_forecasts() to identify duplicate rows.", #nolint
    fixed = TRUE
  )
})

test_that("as_forecast() function creates a message when no model column is
           present", {
  no_model <- data.table::copy(example_quantile[model == "EuroCOVIDhub-ensemble"])[, model := NULL][]
  expect_message(
    suppressWarnings(as_forecast(no_model)),
    "There is no column called `model` in the data.scoringutils assumes that all forecasts come from the same model")
})

test_that("as_forecast() function throws an error when no predictions or observed values are present", {
  expect_error(suppressMessages(suppressWarnings(as_forecast(
    data.table::copy(example_quantile)[, predicted := NULL]
  ))),
  "Assertion on 'data' failed: Both columns `observed` and predicted` are needed.")

  expect_error(suppressMessages(suppressWarnings(as_forecast(
    data.table::copy(example_quantile)[, observed := NULL]
  ))),
  "Assertion on 'data' failed: Both columns `observed` and predicted` are needed.")
})

# test_that("as_forecast() function throws an error when no predictions or observed values are present", {
#   expect_error(suppressMessages(suppressWarnings(as_forecast(
#     data.table::copy(example_quantile)[, predicted := NA]
#   ))))
#   expect_error(suppressMessages(suppressWarnings(check_forecasts(
#     data.table::copy(example_quantile)[, observed := NA]
#   ))))
# })

# test_that("as_forecast() function throws an sample/quantile not present", {
#   expect_error(suppressMessages(suppressWarnings(as_forecast(
#     data.table::copy(example_quantile)[, quantile := NULL]
#   ))))
# })

test_that("output of check_forecasts() is accepted as input to score()", {
  check <- suppressMessages(as_forecast(example_binary))
  expect_no_error(
    score_check <- score(na.omit(check))
  )
  expect_equal(score_check, suppressMessages(score(as_forecast(example_binary))))
})


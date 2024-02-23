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

test_that("assert_equal_length works", {
  expect_error(
    assert_equal_length(a = 1:2, b = 1:4, call_levels_up = 0),
    "should have the same length"
  )
  expect_error(
    assert_equal_length(
      a = 1:2, b = 1:4, call_levels_up = 0, one_allowed = FALSE
    ),
    "should have the same length"
  )
})

test_that("check_attribute_conflict works", {
  expect_message(
    check_attribute_conflict(example_binary, "class", "forecast"),
    "looks different from what's expected based on the data."
  )
  expect_no_message(
    check_attribute_conflict(
      example_binary, "class", c("data.table", "data.frame")
    )
  )
})

test_that("assure_model_column works", {
  expect_message(
    assure_model_column(example_binary[, model := NULL]),
    "no column called model"
  )
})

test_that("check_number_per_forecast works", {
  expect_message(
    check_number_per_forecast(
      example_binary, forecast_unit = "location_name"
    ),
    "Some forecasts have different numbers of rows"
  )
})

test_that("check_no_NA_present works", {
  expect_message(
    check_no_NA_present(example_binary, columns = "predicted"),
    "values in column \"predicted\" are NA"
  )
})

test_that("check_duplicates works", {
  example_bin <- rbind(example_binary[1:2, ], example_binary[1:2, ])
  expect_message(
    check_duplicates(example_bin),
    "instances with more than one forecast for the same target"
  )
})

test_that("check_columns_present works", {
  expect_message(
    check_columns_present(example_binary, c("loc1", "loc2", "loc3")),
    "Columns \"loc1\", \"loc2\", and \"loc3\" not found in data."
    )
  expect_message(
    check_columns_present(example_binary, c("loc1")),
    "Column \"loc1\" not found in data."
  )
})

test_that("check_has_attribute works", {
  expect_message(
    check_has_attribute(example_binary, "forecast"),
    "Found no attribute \"forecast\""
  )
})

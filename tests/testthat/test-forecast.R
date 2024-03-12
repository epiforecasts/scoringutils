test_that("Running `as_forecast()` twice returns the same object", {
  ex <- na.omit(example_continuous)

  expect_identical(
    as_forecast(as_forecast(ex)),
    as_forecast(ex)
  )
})

test_that("as_forecast() works as expected", {
  test <- na.omit(data.table::copy(example_quantile))
  expect_s3_class(as_forecast(test), "forecast_quantile")

  # expect error when arguments are not correct
  expect_error(as_forecast(test, observed = 3), "Must be of type 'character'")
  expect_error(as_forecast(test, quantile_level = c("1", "2")), "Must have length 1")
  expect_error(as_forecast(test, observed = "missing"), "Must be a subset of")

  # expect no condition with columns already present
  expect_no_condition(
    as_forecast(test,
      observed = "observed", predicted = "predicted",
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      quantile_level = "quantile_level"
    )
  )

  # additional test with renaming the model column
  test <- na.omit(data.table::copy(example_continuous))
  data.table::setnames(test,
    old = c("observed", "predicted", "sample_id", "model"),
    new = c("obs", "pred", "sample", "mod")
  )
  expect_no_condition(
    as_forecast(test,
      observed = "obs", predicted = "pred", model = "mod",
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      sample_id = "sample"
    )
  )

  # test if desired forecast type does not correspond to inferred one
  test <- na.omit(data.table::copy(example_continuous))
  expect_error(
    as_forecast(test, forecast_type = "quantile"),
    "Forecast type determined by scoringutils based on input"
  )
})

test_that("as_forecast() function works", {
  check <- suppressMessages(as_forecast(example_quantile))
  expect_s3_class(check, "forecast_quantile")
})

test_that("as_forecast() function has an error for empty data.frame", {
  d <- data.frame(observed = numeric(), predicted = numeric(), model = character())

  expect_error(
    as_forecast(d),
    "Assertion on 'data' failed: Must have at least 1 rows, but has 0 rows."
  )
})

test_that("as_forecast() errors if there is both a sample_id and a quantile_level column", {
  example <- data.table::copy(example_quantile)[, sample_id := 1]
  expect_error(
    as_forecast(example),
    "Found columns `quantile_level` and `sample_id`. Only one of these is allowed"
  )
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

test_that("as_forecast() function warns when no model column is present", {
  no_model <- data.table::copy(example_quantile[model == "EuroCOVIDhub-ensemble"])[, model := NULL][]
  expect_warning(
    as_forecast(no_model),
    "There is no column called `model` in the data.")
})

test_that("as_forecast() function throws an error when no predictions or observed values are present", {
  expect_error(suppressMessages(suppressWarnings(as_forecast(
    data.table::copy(example_quantile)[, predicted := NULL]
  ))),
  "Assertion on 'data' failed: Column 'predicted' not found in data.")

  expect_error(suppressMessages(suppressWarnings(as_forecast(
    data.table::copy(example_quantile)[, observed := NULL]
  ))),
  "Assertion on 'data' failed: Column 'observed' not found in data.")

  expect_error(suppressMessages(suppressWarnings(as_forecast(
    data.table::copy(example_quantile)[, c("observed", "predicted") := NULL]
  ))),
  "Assertion on 'data' failed: Columns 'observed', 'predicted' not found in data.")
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

test_that("output of as_forecasts() is accepted as input to score()", {
  check <- suppressMessages(as_forecast(example_binary))
  expect_no_error(
    score_check <- score(na.omit(check))
  )
  expect_equal(score_check, suppressMessages(score(as_forecast(example_binary))))
})

# ==============================================================================
# is_forecast()
# ==============================================================================

test_that("is_forecast() works as expected", {
  ex_binary <- suppressMessages(as_forecast(example_binary))
  ex_point <- suppressMessages(as_forecast(example_point))
  ex_quantile <- suppressMessages(as_forecast(example_quantile))
  ex_continuous <- suppressMessages(as_forecast(example_continuous))

  expect_true(is_forecast(ex_binary))
  expect_true(is_forecast(ex_point))
  expect_true(is_forecast(ex_quantile))
  expect_true(is_forecast(ex_continuous))

  expect_false(is_forecast(1:10))
  expect_false(is_forecast(data.table::as.data.table(example_point)))
  expect_false(is_forecast.forecast_sample(ex_quantile))
  expect_false(is_forecast.forecast_quantile(ex_binary))
})


# ==============================================================================
# validate_forecast()
# ==============================================================================

test_that("validate_forecast() works as expected", {
  # test that by default, `as_forecast()` errors
  expect_error(validate_forecast(data.frame(x = 1:10)),
               "The input needs to be a forecast object.")
})

test_that("validate_forecast.forecast_binary works as expected", {
  test <- na.omit(data.table::copy(example_binary))
  test[, "sample_id" := 1:nrow(test)]

  # error if there is a superficial sample_id column
  expect_error(
    as_forecast(test),
    "Input looks like a binary forecast, but an additional column called `sample_id` or `quantile` was found."
  )

  # expect error if probabilties are not in [0, 1]
  test <- na.omit(data.table::copy(example_binary))
  test[, "predicted" := predicted + 1]
  expect_error(
    as_forecast(test),
    "Input looks like a binary forecast, but found the following issue"
  )
})

test_that("validate_forecast.forecast_point() works as expected", {
  test <- na.omit(data.table::copy(example_point))
  test <- as_forecast(test)

  # expect an error if column is changed to character after initial validation.
  test <- test[, "predicted" := as.character(predicted)]
  expect_error(
    validate_forecast(test),
    "Input looks like a point forecast, but found the following issue"
  )
})

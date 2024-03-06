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

  # test that as_forecast() complains if there is no model column
  test <- na.omit(data.table::copy(example_continuous))[model == "EuroCOVIDhub-ensemble"]
  test <- test[, model := NULL]
  expect_warning(
    as_forecast(test),
    "There is no column called `model` in the data. scoringutils assumes that all forecasts come from the same model"
    )
})


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

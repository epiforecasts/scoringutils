test_that("get_forecast_counts() works as expected", {
  af <- suppressMessages(
    get_forecast_counts(example_quantile,
      by = c("model", "target_type", "target_end_date")
    )
  )
  expect_type(af, "list")
  expect_type(af$target_type, "character")
  expect_type(af$`count`, "integer")
  expect_equal(nrow(af[is.na(`count`)]), 0)
  af <- get_forecast_counts(na.omit(example_quantile), by = "model")
  expect_equal(nrow(af), 4)
  expect_equal(af$`count`, c(256, 256, 128, 247))

  # Ensure the returning object class is exactly same as a data.table.
  expect_s3_class(af, c("data.table", "data.frame"), exact = TRUE)

  # Setting `collapse = c()` means that all quantiles and samples are counted
  af <- get_forecast_counts(
    na.omit(example_quantile),
     by = "model", collapse = c()
  )
  expect_equal(nrow(af), 4)
  expect_equal(af$`count`, c(5888, 5888, 2944, 5681))

  # setting by = NULL, the default, results in by equal to forecast unit
  af <- get_forecast_counts(na.omit(example_quantile))
  expect_equal(nrow(af), 50688)

  # check whether collapsing also works for model-based forecasts
  af <- get_forecast_counts(na.omit(example_integer), by = "model")
  expect_equal(nrow(af), 4)

  af <- get_forecast_counts(na.omit(example_integer), by = "model", collapse = c())
  expect_equal(af$count, c(10240, 10240, 5120, 9880))
})

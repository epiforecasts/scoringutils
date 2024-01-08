test_that("Running `as_forecast()` twice returns the same object", {
  ex <- na.omit(example_continuous)

  expect_identical(
    as_forecast(as_forecast(ex)),
    as_forecast(ex)
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

  expect_true(is_forecast(ex_binary, class = "forecast_binary"))
  expect_false(is_forecast(ex_binary, class = "forecast_quantile"))

  expect_true(is_forecast(
    ex_binary, class = c("forecast_quantile", "forecast_binary")
  ))
  expect_false(is_forecast(
    ex_binary, class = c("forecast_quantile", "forecast_point")
  ))
})

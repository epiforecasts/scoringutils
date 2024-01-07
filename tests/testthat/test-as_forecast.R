test_that("Running `as_forecast()` twice returns the same object", {
  ex <- na.omit(example_continuous)

  expect_identical(
    as_forecast(as_forecast(ex)),
    as_forecast(ex)
  )
})

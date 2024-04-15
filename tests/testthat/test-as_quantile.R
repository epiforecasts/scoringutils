test_that("as_quantile converts sample forecast to quantile forecast", {
  # Create a sample forecast object
  forecast <- as_forecast(example_sample_continuous)
  
  # Convert the sample forecast to a quantile forecast
  quantile_forecast <- as_quantile(forecast)
  
  # Check if the quantile forecast has the correct structure
  expect_s3_class(quantile_forecast, "forecast_quantile")
  expect_equal(length(unique(quantile_forecast$quantile_level)), 99)
})

test_that("as_quantile throws an error for non-forecast input", {
  # Create a non-forecast object
  non_forecast <- data.frame(x = 1:10, y = 11:20)
  
  # Check if as_quantile throws an error for non-forecast input
  expect_error(as_quantile(non_forecast), "The input needs to be a forecast object.")
})

test_that("as_quantile can handle custom quantiles", {
  # Create a sample forecast object
  forecast <- as_forecast(example_sample_continuous)
  
  # Convert the sample forecast to a quantile forecast with custom quantiles
  quantile_forecast <- as_quantile(forecast, quantile_levels = c(0.1, 0.9))
  
  # Check if the quantile forecast has the correct structure
  expect_s3_class(quantile_forecast, "forecast_quantile")
  expect_equal(length(unique(quantile_forecast$quantile_level)), 2)
  expect_equal(unique(quantile_forecast$quantile_level), c(0.1, 0.9))
})
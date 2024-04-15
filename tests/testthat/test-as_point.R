test_that("as_point converts forecast_quantile to forecast_point", {
  # Create a forecast_quantile object
  forecast_quantile <- as_forecast(example_quantile)
  
  # Convert to forecast_point using as_point
  point_forecast <- as_point(forecast_quantile)
  
  # Check if the result is of class forecast_point
  expect_s3_class(point_forecast, "forecast_point")

  # Convert to forecast_point using as_point with custom quantile_level
  point_forecast_0.9 <- as_point(forecast_quantile, quantile_level = 0.9)
  
  # Check if the result is of class forecast_point
  expect_s3_class(point_forecast_0.9, "forecast_point")
  expect_true(!all(point_forecast_0.9 == point_forecast))
})

test_that("as_point converts forecast_sample to forecast_point using quantile approach", {
  # Create a forecast_sample object
  forecast_sample <- as_forecast(example_sample_continuous)
  
  # Convert to forecast_point using as_point
  point_forecast <- as_point(forecast_sample)
  
  # Check if the result is of class forecast_point
  expect_s3_class(point_forecast, "forecast_point")

  # Convert to forecast_point using as_point with custom quantile_level
  point_forecast_0.9 <- as_point(forecast_sample, quantile_level = 0.9)

  # Check if the result is of class forecast_point
  expect_s3_class(point_forecast_0.9, "forecast_point")
  expect_true(!all(point_forecast_0.9 == point_forecast))
})

test_that("as_point converts forecast_sample to forecast_point using custom function", {
  # Create a forecast_sample object
  forecast_sample <- as_forecast(example_sample_continuous)
  
  # Convert to forecast_point using as_point with custom function
  point_forecast <- as_point(forecast_sample, fun = mean)
  
  # Check if the result is of class forecast_point
  expect_s3_class(point_forecast, "forecast_point")
})

test_that("as_point throws an error for non-forecast input", {
  # Create a non-forecast object
  non_forecast <- data.frame(x = 1:10)
  
  # Check if as_point throws an error
  expect_error(as_point(non_forecast))
})
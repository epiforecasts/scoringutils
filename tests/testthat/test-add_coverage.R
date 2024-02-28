ex_coverage <- example_quantile[model == "EuroCOVIDhub-ensemble"]

test_that("add_coverage() works as expected", {
  expect_message(
    cov <- add_coverage(example_quantile),
    "Some rows containing NA values may be removed."
  )

  required_names <- c(
    "interval_range", "interval_coverage", "interval_coverage_deviation",
    "quantile_coverage", "quantile_coverage_deviation"
  )
  expect_equal(colnames(cov), c(colnames(example_quantile), required_names))

  expect_equal(nrow(cov), nrow(example_quantile))

  # check that
})

test_that("add_coverage() outputs an object of class forecast_*", {
  ex <- as_forecast(na.omit(example_quantile))
  cov <- add_coverage(ex)
  expect_s3_class(cov, "forecast_quantile")
})

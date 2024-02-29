ex_coverage <- example_quantile[model == "EuroCOVIDhub-ensemble"]

test_that("get_coverage() works as expected", {
  cov <- get_coverage(example_quantile, by = get_forecast_unit(example_quantile))

  expect_equal(
    sort(colnames(cov)),
    sort(c(get_forecast_unit(example_quantile), c(
      "interval_range", "quantile_level", "interval_coverage", "interval_coverage_deviation",
      "quantile_coverage", "quantile_coverage_deviation"
    )))
  )

  expect_equal(nrow(cov), nrow(na.omit(example_quantile)))
})

test_that("get_coverage() outputs an object of class c('data.table', 'data.frame'", {
  ex <- as_forecast(na.omit(example_quantile))
  cov <- get_coverage(ex)
  expect_s3_class(cov, c("data.table", "data.frame"), exact = TRUE)
})

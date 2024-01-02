ex_coverage <- example_quantile[model == "EuroCOVIDhub-ensemble"]

test_that("add_coverage() works as expected", {
  expect_message(
    cov <- add_coverage(example_quantile),
    "Some rows contain NA values and will be removed in subsequent operations"
  )

  required_names <- c(
    "range", "interval_coverage", "interval_coverage_deviation",
    "quantile_coverage", "quantile_coverage_deviation"
  )
  expect_equal(colnames(cov), c(colnames(example_quantile), required_names))

  expect_equal(nrow(cov), nrow(example_quantile))
})

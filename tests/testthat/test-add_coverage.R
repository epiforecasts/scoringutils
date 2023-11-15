ex_coverage <- example_quantile[model == "EuroCOVIDhub-ensemble"]

test_that("add_coverage() works as expected", {
  expect_no_condition(cov <- add_coverage(example_quantile))

  required_names <- c(
    "range", "interval_coverage", "interval_coverage_deviation",
    "quantile_coverage", "quantile_coverage_deviation"
  )
  expect_equal(colnames(cov), c(colnames(example_quantile), required_names))

  expect_equal(nrow(cov), nrow(example_quantile))
})

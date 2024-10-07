# ==============================================================================
# `get_coverage()`
# ==============================================================================
ex_coverage <- example_quantile[model == "EuroCOVIDhub-ensemble"]

test_that("get_coverage() works as expected", {
  cov <- example_quantile %>%
    get_coverage(by = get_forecast_unit(example_quantile))

  expect_equal(
    sort(colnames(cov)),
    sort(c(get_forecast_unit(example_quantile), c(
      "interval_range", "quantile_level", "interval_coverage", "interval_coverage_deviation",
      "quantile_coverage", "quantile_coverage_deviation"
    )))
  )

  expect_equal(nrow(cov), nrow(na.omit(example_quantile)))

  expect_s3_class(
    cov,
    c("data.table", "data.frame"),
    exact = TRUE
  )
})

test_that("get_coverage() outputs an object of class c('data.table', 'data.frame'", {
  cov <- get_coverage(example_quantile)
  expect_s3_class(cov, c("data.table", "data.frame"), exact = TRUE)
})

test_that("get_coverage() can deal with non-symmetric prediction intervals", {
  # the expected result is that `get_coverage()` just works. However,
  # all interval coverages with missing values should just be `NA`
  test <- data.table::copy(example_quantile)
  test <- test[!quantile_level %in% c(0.2, 0.3, 0.5)]

  expect_no_condition(cov <- get_coverage(test))

  prediction_intervals <- get_range_from_quantile(c(0.2, 0.3, 0.5))

  missing <- cov[interval_range %in% prediction_intervals]
  not_missing <- cov[!interval_range %in% prediction_intervals]

  expect_true(all(is.na(missing$interval_coverage)))
  expect_false(any(is.na(not_missing)))

  # test for a version where values are not missing, but just `NA`
  # since `get_coverage()` calls `na.omit`, the result should be the same.
  test <- data.table::copy(example_quantile)
  test <- test[quantile_level %in% c(0.2, 0.3, 0.5), predicted := NA]
  cov2 <- get_coverage(test)
  expect_equal(cov, cov2)
})


# ==============================================================================
# plot_interval_coverage()
# ==============================================================================
test_that("plot_interval_coverage() works as expected", {
  coverage <- example_quantile %>%
    na.omit() %>%
    as_forecast_quantile() %>%
    get_coverage(by = c("model"))
  p <- plot_interval_coverage(coverage)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_interval_coverage", p))

  # make sure that plot_interval_coverage() doesn't drop column names
  expect_true(all(c(
    "interval_coverage", "interval_coverage_deviation",
    "quantile_coverage", "quantile_coverage_deviation"
  ) %in%
    names(coverage)))
})


# ==============================================================================
# plot_quantile_coverage()
# ==============================================================================
test_that("plot_quantile_coverage() works as expected", {
  coverage <- example_quantile %>%
    na.omit() %>%
    as_forecast_quantile() %>%
    get_coverage(by = c("model", "quantile_level"))

  p <- plot_quantile_coverage(coverage)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_quantile_coverage", p))
})

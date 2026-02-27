# ==============================================================================
# `get_coverage()` # nolint: commented_code_linter
# ==============================================================================
ex_coverage <- example_quantile[model == "EuroCOVIDhub-ensemble"]

test_that("get_coverage() works as expected", {
  cov <- example_quantile |>
    get_coverage(by = get_forecast_unit(example_quantile))

  expect_identical(
    sort(colnames(cov)),
    sort(c(get_forecast_unit(example_quantile), c(
      "interval_range", "quantile_level", "interval_coverage", "interval_coverage_deviation",
      "quantile_coverage", "quantile_coverage_deviation"
    )))
  )

  expect_identical(nrow(cov), nrow(na.omit(example_quantile)))

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

test_that("get_coverage() interval coverage matches interval_coverage() for same data", {
  # Regression guard: both functions independently compute the same bounds check
  fc <- data.table::copy(example_quantile[model == "EuroCOVIDhub-ensemble"])
  fc <- fc[!is.na(predicted)]
  fc_obj <- as_forecast_quantile(fc)

  cov <- get_coverage(fc_obj, by = get_forecast_unit(fc_obj))

  # Compare for 50% interval — get_coverage returns multiple rows per forecast
  # (one per quantile_level), but interval_coverage is the same for all rows
  # with the same interval_range. Take unique per forecast unit + interval_range.
  cov_50 <- unique(cov[interval_range == 50, c(get_forecast_unit(fc_obj),
                                                "interval_range",
                                                "interval_coverage"),
                        with = FALSE])

  # Get matching numeric data for interval_coverage()
  obs <- fc[quantile_level == 0.5]$observed
  pred_mat <- as.matrix(
    data.table::dcast(
      fc, ... ~ quantile_level, value.var = "predicted"
    )[, .SD, .SDcols = as.character(sort(unique(fc$quantile_level)))]
  )
  ql <- sort(unique(fc$quantile_level))

  ic_50 <- interval_coverage(obs, pred_mat, ql, interval_range = 50)
  expect_equal(cov_50$interval_coverage, as.numeric(ic_50))
})

test_that("get_coverage() produces correct interval_coverage for known inputs", {
  # Hand-crafted data with known expected coverage
  dt1 <- data.table::data.table(
    observed = rep(5, 3),
    model = "m1", target_type = "t1",
    target_end_date = as.Date("2020-01-01"), location = "loc1",
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 5, 7)
  )
  dt2 <- data.table::data.table(
    observed = rep(10, 3),
    model = "m1", target_type = "t1",
    target_end_date = as.Date("2020-01-02"), location = "loc1",
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(3, 5, 7)
  )
  dt <- rbind(dt1, dt2)
  fc <- as_forecast_quantile(dt)

  cov <- get_coverage(fc, by = get_forecast_unit(fc))

  # For observed=5, 50% interval [3,7]: TRUE (5 >= 3 and 5 <= 7)
  cov_50_obs5 <- cov[target_end_date == as.Date("2020-01-01") &
                        interval_range == 50]
  # interval_coverage is the same for all quantile_levels in this range
  expect_true(all(cov_50_obs5$interval_coverage == TRUE))

  # For observed=10, 50% interval [3,7]: FALSE (10 > 7)
  cov_50_obs10 <- cov[target_end_date == as.Date("2020-01-02") &
                         interval_range == 50]
  expect_true(all(cov_50_obs10$interval_coverage == FALSE))

  # Quantile coverage for quantile_level=0.5: TRUE for observed=5, FALSE for observed=10
  qcov_obs5 <- cov[target_end_date == as.Date("2020-01-01") &
                      quantile_level == 0.5]
  expect_equal(nrow(qcov_obs5), 1)
  expect_true(as.logical(qcov_obs5$quantile_coverage))

  qcov_obs10 <- cov[target_end_date == as.Date("2020-01-02") &
                       quantile_level == 0.5]
  expect_equal(nrow(qcov_obs10), 1)
  expect_false(as.logical(qcov_obs10$quantile_coverage))
})

test_that("get_coverage() and interval_coverage() agree when observation outside all intervals", {
  dt <- data.table::data.table(
    observed = rep(100, 5),
    model = "m1", target_type = "t1",
    target_end_date = as.Date("2020-01-01"), location = "loc1",
    quantile_level = c(0.1, 0.25, 0.5, 0.75, 0.9),
    predicted = c(1, 3, 5, 7, 9)
  )
  fc <- as_forecast_quantile(dt)
  cov <- get_coverage(fc, by = get_forecast_unit(fc))

  # All interval_coverage should be FALSE
  expect_true(all(cov$interval_coverage == FALSE))

  # interval_coverage() should agree
  pred_mat <- matrix(c(1, 3, 5, 7, 9), nrow = 1)
  ql <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  expect_false(interval_coverage(100, pred_mat, ql, interval_range = 50))
  expect_false(interval_coverage(100, pred_mat, ql, interval_range = 80))
})

test_that("refactored interval coverage produces identical output to original", {
  # Comprehensive regression guard using full example dataset
  cov <- get_coverage(example_quantile, by = get_forecast_unit(example_quantile))
  scores <- score(example_quantile)

  # Compare interval_coverage from get_coverage() for range=50 with score()'s interval_coverage_50
  cov_50 <- cov[interval_range == 50]
  # Merge on forecast unit to compare
  fu <- get_forecast_unit(example_quantile)
  merged <- merge(cov_50, scores, by = fu)
  expect_equal(merged$interval_coverage, as.numeric(merged$interval_coverage_50))

  # Same for range=90
  cov_90 <- cov[interval_range == 90]
  merged_90 <- merge(cov_90, scores, by = fu)
  expect_equal(merged_90$interval_coverage, as.numeric(merged_90$interval_coverage_90))
})

test_that("get_coverage() can deal with non-symmetric prediction intervals", {
  # the expected result is that `get_coverage()` just works. However,
  # all interval coverages with missing values should just be `NA`
  test <- data.table::copy(example_quantile)
  test <- test[!quantile_level %in% c(0.2, 0.3, 0.5)]

  cov <- expect_no_condition(get_coverage(test))

  prediction_intervals <- get_range_from_quantile(c(0.2, 0.3, 0.5))

  missing <- cov[interval_range %in% prediction_intervals]
  not_missing <- cov[!interval_range %in% prediction_intervals]

  expect_true(all(is.na(missing$interval_coverage)))
  expect_false(anyNA(not_missing))

  # test for a version where values are not missing, but just `NA`
  # since `get_coverage()` calls `na.omit`, the result should be the same.
  test <- data.table::copy(example_quantile)
  test <- test[quantile_level %in% c(0.2, 0.3, 0.5), predicted := NA]
  cov2 <- get_coverage(test)
  expect_identical(cov, cov2)
})


# ==============================================================================
# plot_interval_coverage() # nolint: commented_code_linter
# ==============================================================================
test_that("plot_interval_coverage() works as expected", {
  coverage <- example_quantile |>
    na.omit() |>
    as_forecast_quantile() |>
    get_coverage(by = "model")
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
# plot_quantile_coverage() # nolint: commented_code_linter
# ==============================================================================
test_that("plot_quantile_coverage() works as expected", {
  coverage <- example_quantile |>
    na.omit() |>
    as_forecast_quantile() |>
    get_coverage(by = c("model", "quantile_level"))

  p <- plot_quantile_coverage(coverage)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_quantile_coverage", p))
})

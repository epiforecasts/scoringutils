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

test_that("get_coverage() with type = 'interval' returns only interval coverage columns", {
  cov <- get_coverage(example_quantile, by = "model", type = "interval")
  expect_s3_class(cov, c("data.table", "data.frame"), exact = TRUE)
  expect_true("interval_coverage" %in% names(cov))
  expect_true("interval_coverage_deviation" %in% names(cov))
  expect_false("quantile_coverage" %in% names(cov))
  expect_false("quantile_coverage_deviation" %in% names(cov))
  expect_true("interval_range" %in% names(cov))
  expect_true("model" %in% names(cov))
  expect_true(all(cov$interval_coverage >= 0 & cov$interval_coverage <= 1,
                  na.rm = TRUE))
})

test_that("get_coverage() with type = 'quantile' returns only quantile coverage columns", {
  cov <- get_coverage(example_quantile, by = "model", type = "quantile")
  expect_s3_class(cov, c("data.table", "data.frame"), exact = TRUE)
  expect_true("quantile_coverage" %in% names(cov))
  expect_true("quantile_coverage_deviation" %in% names(cov))
  expect_false("interval_coverage" %in% names(cov))
  expect_false("interval_coverage_deviation" %in% names(cov))
  expect_true("quantile_level" %in% names(cov))
  expect_true("model" %in% names(cov))
  expect_true(all(cov$quantile_coverage >= 0 & cov$quantile_coverage <= 1))
  expect_false("interval_range" %in% names(cov))
})

test_that("get_coverage() with type = c('quantile', 'interval') returns both (default behavior)", {
  cov_default <- get_coverage(example_quantile, by = "model")
  cov_explicit <- get_coverage(example_quantile, by = "model",
                               type = c("quantile", "interval"))
  expect_identical(cov_default, cov_explicit)
  expect_true(all(c("interval_coverage", "interval_coverage_deviation",
                     "quantile_coverage", "quantile_coverage_deviation")
                  %in% names(cov_default)))
  expect_true(all(c("quantile_level", "interval_range") %in% names(cov_default)))
})

test_that("get_coverage() with type = 'interval' produces correct coverage values", {
  test_data <- data.table::data.table(
    model = rep("model1", 4),
    target_type = rep("Cases", 4),
    location = rep(c("A", "B"), each = 2),
    target_end_date = as.Date(rep("2021-01-01", 4)),
    forecast_date = as.Date(rep("2020-12-20", 4)),
    quantile_level = rep(c(0.25, 0.75), 2),
    predicted = c(10, 20, 10, 20),
    observed = c(15, 15, 25, 25)
  )
  test_data <- as_forecast_quantile(test_data)
  cov <- get_coverage(test_data, by = get_forecast_unit(test_data),
                      type = "interval")
  # location A: observed=15 inside [10,20] -> coverage=1
  expect_equal(cov[grepl("A", location)]$interval_coverage, 1)
  # location B: observed=25 outside [10,20] -> coverage=0
  expect_equal(cov[grepl("B", location)]$interval_coverage, 0)
  # interval_coverage_deviation = interval_coverage - interval_range/100
  expect_equal(cov$interval_coverage_deviation,
               cov$interval_coverage - cov$interval_range / 100)
  expect_false("quantile_coverage" %in% names(cov))
})

test_that("get_coverage() with type = 'quantile' produces correct coverage values", {
  test_data <- data.table::data.table(
    model = rep("model1", 3),
    target_type = rep("Cases", 3),
    location = rep("A", 3),
    target_end_date = as.Date(rep("2021-01-01", 3)),
    forecast_date = as.Date(rep("2020-12-20", 3)),
    quantile_level = c(0.25, 0.5, 0.75),
    predicted = c(10, 15, 20),
    observed = rep(12, 3)
  )
  test_data <- as_forecast_quantile(test_data)
  cov <- get_coverage(test_data, by = get_forecast_unit(test_data),
                      type = "quantile")
  # 12 <= 10 -> FALSE -> 0
  expect_equal(cov[quantile_level == 0.25]$quantile_coverage, 0)
  # 12 <= 15 -> TRUE -> 1
  expect_equal(cov[quantile_level == 0.5]$quantile_coverage, 1)
  # 12 <= 20 -> TRUE -> 1
  expect_equal(cov[quantile_level == 0.75]$quantile_coverage, 1)
  # quantile_coverage_deviation = quantile_coverage - quantile_level
  expect_equal(cov$quantile_coverage_deviation,
               cov$quantile_coverage - cov$quantile_level)
  expect_false("interval_coverage" %in% names(cov))
})

test_that("get_coverage() errors for invalid type argument", {
  expect_error(get_coverage(example_quantile, type = "invalid"))
  expect_error(get_coverage(example_quantile, type = c("quantile", "invalid")))
  expect_error(get_coverage(example_quantile, type = NULL))
})

test_that("get_coverage() with type = 'interval' works with non-symmetric prediction intervals", {
  test <- data.table::copy(example_quantile)
  test <- test[!quantile_level %in% c(0.2, 0.3, 0.5)]
  cov <- expect_no_condition(get_coverage(test, type = "interval"))

  prediction_intervals <- get_range_from_quantile(c(0.2, 0.3, 0.5))
  missing <- cov[interval_range %in% prediction_intervals]
  not_missing <- cov[!interval_range %in% prediction_intervals]

  expect_true(all(is.na(missing$interval_coverage)))
  expect_false(anyNA(not_missing))
  expect_false("quantile_coverage" %in% names(cov))
})

test_that("get_coverage() with type = 'quantile' summarises correctly with by argument", {
  cov <- get_coverage(example_quantile, by = "model", type = "quantile")
  # should have one row per model + quantile_level
  models <- unique(na.omit(example_quantile)$model)
  quantile_levels <- unique(na.omit(example_quantile)$quantile_level)
  expect_equal(nrow(cov), length(models) * length(quantile_levels))
  expect_false("interval_range" %in% names(cov))
  expect_true(all(cov$quantile_coverage >= 0 & cov$quantile_coverage <= 1))
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

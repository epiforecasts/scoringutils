# Tests to verify NAMESPACE imports are correct and that removing redundant
# `pkg::fn()` qualifiers doesn't cause regressions. See issue #445.

test_that("all functions used without :: prefix are properly imported in NAMESPACE", {
  ns <- asNamespace("scoringutils")

  # data.table functions
  expect_true(exists("as.data.table", envir = ns, inherits = TRUE))
  expect_true(exists("melt", envir = ns, inherits = TRUE))
  expect_true(exists("dcast", envir = ns, inherits = TRUE))
  expect_true(exists("rbindlist", envir = ns, inherits = TRUE))
  expect_true(exists("copy", envir = ns, inherits = TRUE))
  expect_true(exists("data.table", envir = ns, inherits = TRUE))
  expect_true(exists("setnames", envir = ns, inherits = TRUE))
  expect_true(exists("is.data.table", envir = ns, inherits = TRUE))

  # scoringRules functions
  expect_true(exists("logs_sample", envir = ns, inherits = TRUE))
  expect_true(exists("dss_sample", envir = ns, inherits = TRUE))
  expect_true(exists("crps_sample", envir = ns, inherits = TRUE))
  expect_true(exists("rps_probs", envir = ns, inherits = TRUE))

  # cli, purrr, utils
  expect_true(exists("cli_warn", envir = ns, inherits = TRUE))
  expect_true(exists("partial", envir = ns, inherits = TRUE))
  expect_true(exists("tail", envir = ns, inherits = TRUE))
})

test_that("scoringRules wrapper functions resolve to correct implementations", {
  set.seed(42)
  observed <- rpois(10, lambda = 5)
  predicted <- replicate(50, rpois(10, lambda = 5))

  result_logs <- logs_sample(observed, predicted)
  expect_type(result_logs, "double")
  expect_length(result_logs, 10)

  result_dss <- dss_sample(observed, predicted)
  expect_type(result_dss, "double")
  expect_length(result_dss, 10)

  result_crps <- crps_sample(observed, predicted)
  expect_type(result_crps, "double")
  expect_length(result_crps, 10)
})

test_that("get_correlations() produces correct output after namespace cleanup", {
  result <- suppressMessages(
    get_correlations(summarise_scores(
      scores_quantile,
      by = get_forecast_unit(scores_quantile)
    ))
  )
  expect_s3_class(result, c("scores", "data.table", "data.frame"), exact = TRUE)
  expect_true(nrow(result) > 0)
})

test_that("pairwise comparisons produce correct output after namespace cleanup", {
  result <- suppressMessages(
    get_pairwise_comparisons(scores_quantile,
      baseline = "EuroCOVIDhub-baseline"
    )
  )
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expected_cols <- c("model", "compare_against", "mean_scores_ratio",
                     "pval", "adj_pval")
  for (col in expected_cols) {
    expect_true(col %in% names(result),
                info = paste("Missing column:", col))
  }
})

test_that("PIT histogram for sample forecasts works after namespace cleanup", {
  result <- suppressMessages(
    get_pit_histogram(as_forecast_sample(example_sample_continuous))
  )
  expect_s3_class(result, "data.table")
  expect_true("density" %in% names(result))
  expect_true(all(result$density >= 0))
})

test_that("quantile_to_interval conversion works after namespace cleanup", {
  result <- suppressMessages(
    score(as_forecast_quantile(na.omit(example_quantile)))
  )
  expect_s3_class(result, "scores")
  expect_true("wis" %in% names(result))
  expect_true("interval_coverage_50" %in% names(result))
  expect_true("interval_coverage_90" %in% names(result))
})

test_that("get_metrics for quantile forecasts resolves partial() correctly", {
  metrics <- suppressMessages(
    get_metrics(as_forecast_quantile(na.omit(example_quantile)))
  )
  expect_true("interval_coverage_90" %in% names(metrics))
  expect_true(is.function(metrics$interval_coverage_90))
})

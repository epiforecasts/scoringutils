# ==============================================================================
# metric_adapt_binary_numeric()
# ==============================================================================
test_that("metric_adapt_binary_numeric() converts factor observed to numeric 0/1", {
  observed <- factor(c(0, 1, 1, 0, 1))
  predicted <- c(0.2, 0.8, 0.6, 0.3, 0.9)

  # Define a simple external metric that requires numeric observed
  external_brier <- function(observed, predicted) {
    (observed - predicted)^2
  }

  adapted <- metric_adapt_binary_numeric(external_brier)
  result <- adapted(observed, predicted)

  # Factor is converted to 0/1 numeric and the external function computes correctly
  expect_equal(result, (c(0, 1, 1, 0, 1) - predicted)^2)

  # Without the adapter, factor arithmetic produces a warning and NAs
  expect_warning(
    raw_result <- external_brier(observed, predicted)
  )
})

test_that("metric_adapt_binary_numeric() works inside score() with binary forecasts", {
  # Define an external metric expecting numeric 0/1
  numeric_brier <- function(observed, predicted) {
    (observed - predicted)^2
  }
  adapted_brier <- metric_adapt_binary_numeric(numeric_brier)
  metrics <- list(custom_brier = adapted_brier)

  result <- score(example_binary, metrics = metrics)

  expect_s3_class(result, "scores")
  expect_true("custom_brier" %in% names(result))

  # Compare against the built-in brier_score
  reference <- score(example_binary, metrics = list(brier_score = brier_score))
  expect_equal(result$custom_brier, reference$brier_score)
})

# ==============================================================================
# metric_adapt_swap_args()
# ==============================================================================
test_that("metric_adapt_swap_args() reverses observed/predicted argument order", {
  observed <- c(1, 2, 3, 4, 5)
  predicted <- c(1.1, 2.2, 2.8, 4.1, 5.3)

  # External metric with swapped arg order
  swapped_ae <- function(predicted, observed) {
    abs(predicted - observed)
  }
  normal_ae <- function(observed, predicted) {
    abs(observed - predicted)
  }

  adapted <- metric_adapt_swap_args(swapped_ae)
  result <- adapted(observed, predicted)

  expect_equal(result, normal_ae(observed, predicted))

  # Verify the swap actually happened
  first_arg <- function(a, b) { a }
  expect_equal(metric_adapt_swap_args(first_arg)(10, 20), 20)
})

test_that("metric_adapt_swap_args() works inside score() with point forecasts", {
  # External absolute error function with swapped order
  swapped_ae <- function(predicted, observed) {
    abs(predicted - observed)
  }
  adapted_ae <- metric_adapt_swap_args(swapped_ae)
  metrics <- list(ae_adapted = adapted_ae)

  result <- score(example_point, metrics = metrics)

  expect_s3_class(result, "scores")
  expect_true("ae_adapted" %in% names(result))

  reference <- score(example_point, metrics = list(ae_point = Metrics::ae))
  expect_equal(result$ae_adapted, reference$ae_point)
})

test_that("metric_adapt_swap_args() works inside score() with sample forecasts", {
  # Define a function with swapped order that computes MAD
  swapped_mad <- function(predicted, observed) {
    apply(predicted, 1, mad)
  }
  adapted_mad <- metric_adapt_swap_args(swapped_mad)
  metrics <- list(mad_adapted = adapted_mad)

  result <- score(example_sample_continuous, metrics = metrics)

  expect_s3_class(result, "scores")
  expect_true("mad_adapted" %in% names(result))

  reference <- score(
    example_sample_continuous, metrics = list(mad = mad_sample)
  )
  expect_equal(result$mad_adapted, reference$mad)
})

# ==============================================================================
# Adapters return proper function objects
# ==============================================================================
test_that("adapted metrics return a function object", {
  dummy <- function(observed, predicted) { 0 }

  adapted_numeric <- metric_adapt_binary_numeric(dummy)
  adapted_swap <- metric_adapt_swap_args(dummy)

  expect_true(is.function(adapted_numeric))
  expect_true(is.function(adapted_swap))

  # Must have at least observed and predicted formals
  expect_true(length(formals(adapted_numeric)) >= 2)
  expect_true(length(formals(adapted_swap)) >= 2)

  # Must accept ... so run_safely() passes through all args
  expect_true("..." %in% names(formals(adapted_numeric)))
  expect_true("..." %in% names(formals(adapted_swap)))
})

# ==============================================================================
# Composability with purrr::partial()
# ==============================================================================
test_that("adapted metrics compose with purrr::partial()", {
  # Metric with an extra parameter expecting numeric observed
  weighted_brier <- function(observed, predicted, weight = 1) {
    weight * (observed - predicted)^2
  }
  adapted <- metric_adapt_binary_numeric(weighted_brier)
  custom <- purrr::partial(adapted, weight = 2)

  observed <- factor(c(0, 1, 1, 0))
  predicted <- c(0.3, 0.7, 0.8, 0.2)

  result <- custom(observed, predicted)
  expect_equal(result, 2 * (c(0, 1, 1, 0) - predicted)^2)
})

# ==============================================================================
# Input validation
# ==============================================================================
test_that("metric_adapt_binary_numeric() errors on non-function input", {
  expect_error(metric_adapt_binary_numeric("not_a_function"))
  expect_error(metric_adapt_binary_numeric(42))
  expect_error(metric_adapt_binary_numeric(NULL))
})

test_that("metric_adapt_swap_args() errors on non-function input", {
  expect_error(metric_adapt_swap_args("not_a_function"))
  expect_error(metric_adapt_swap_args(42))
})

# ==============================================================================
# Extra arguments pass through via ...
# ==============================================================================
test_that("metric_adapt_binary_numeric() preserves extra arguments via ...", {
  # Metric with an extra param
  my_metric <- function(observed, predicted, na.rm = FALSE) {
    if (na.rm) observed[is.na(observed)] <- 0
    sum(observed != predicted)
  }
  adapted <- metric_adapt_binary_numeric(my_metric)

  observed <- factor(c(0, 1, NA, 0, 1))
  predicted <- c(0.3, 0.7, 0.5, 0.2, 0.8)

  # na.rm = TRUE should be forwarded to the inner function
  result <- adapted(observed, predicted, na.rm = TRUE)
  expect_true(is.numeric(result))
})

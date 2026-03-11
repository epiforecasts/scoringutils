# =============================================================================
# new_scores() # nolint: commented_code_linter
# =============================================================================

test_that("new_scores() works", {
  expect_s3_class(
    new_scores(data.frame(), metrics = ""),
    c("scores", "data.table", "data.frame"),
    exact = TRUE
  )

  expect_error(
    new_scores(data.frame()),
    "missing, with no default"
  )
})

test_that("as_scores() works", {
  expect_s3_class(
    scoringutils:::as_scores(data.frame(wis = 1), metrics = "wis"), # nolint: undesirable_operator_linter
    c("scores", "data.table", "data.frame")
  )
})

test_that("assert_scores() works", {
  expect_error(
    assert_scores(data.frame()),
    "Must inherit from class 'scores'"
  )
})

test_that("Output of `score()` has the class `scores()`", {
  expect_no_condition(assert_scores(scores_point))
  expect_no_condition(assert_scores(scores_binary))
  expect_no_condition(assert_scores(scores_sample_continuous))
  expect_no_condition(assert_scores(scores_quantile))
})

# =============================================================================
# score() # nolint: commented_code_linter
# =============================================================================

# common error handling --------------------------------------------------------
test_that("function throws an error if data is not a forecast object", {
  expect_error(
    score(forecast = NULL),
    "The input needs to be a valid forecast object."
  )
})

test_that("Manipulating scores objects with .[ works as expected", {
  expect_no_condition(scores_point[1:10])

  expect_no_condition(scores_point[, .(model, ae_point)])

  ex <- score(example_quantile)
  expect_no_condition(ex[, extra_col := "something"])
})


# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {
  eval <- scores_sample_continuous

  # [.forecast()` will warn even before score()
  only_nas <- suppressWarnings(
    copy(example_sample_continuous)[, predicted := NA_real_]
  )
  expect_error(
    score(as_forecast_sample(only_nas)),
    "After removing rows with NA values in the data, no forecasts are left."
  )

  expect_gt(
    nrow(eval), 1
  )

  expect_identical(
    nrow(eval),
    887L
  )

  expect_s3_class(eval, c("scores", "data.table", "data.frame"), exact = TRUE)
})

test_that("function throws an error if data is missing", {
  expect_error(suppressMessages(score(forecast = NULL)))
})

test_that("score() works with only one sample", {
  # with only one sample, dss returns NaN and log_score fails
  onesample <- na.omit(example_sample_continuous)[sample_id == 20]
  scoreonesample <- suppressWarnings(score(onesample))
  w <- expect_warning(
    score(onesample),
    "log_score"
  )
  expect_match(conditionMessage(w), "need at least 2 data points")

  # verify that all goes well with two samples
  twosample <- na.omit(example_sample_continuous)[sample_id %in% c(20, 21)]
  expect_no_condition(score(twosample))
})

test_that("score() emits a single batched warning when multiple metrics fail", {
  onesample <- na.omit(example_sample_continuous)[sample_id == 20]
  always_fail <- function(observed, predicted) stop("intentional")
  metrics <- c(
    get_metrics(onesample),
    list("always_fail" = always_fail)
  )
  w <- expect_warning(
    score(onesample, metrics = metrics)
  )
  msg <- conditionMessage(w)
  # both failures mentioned in a single warning

  expect_match(msg, "log_score", fixed = TRUE)
  expect_match(msg, "always_fail", fixed = TRUE)
  expect_match(msg, "intentional", fixed = TRUE)
})


# test nominal case ------------------------------------------------------------
test_that("function produces output for a nominal format case", {
  expect_no_condition(score(as_forecast_nominal(na.omit(example_nominal))))
})


# =============================================================================
# apply_metrics() # nolint: commented_code_linter
# =============================================================================

test_that("apply_metrics() works", {
  dt <- data.table::data.table(x = 1:10)
  scoringutils:::apply_metrics( # nolint: undesirable_operator_linter
    forecast = dt, metrics = list(test = function(x) x + 1),
    dt$x
  )
  expect_equal(dt$test, 2:11) # nolint: expect_identical_linter

  # additional named argument works
  expect_no_condition(
    scoringutils:::apply_metrics( # nolint: undesirable_operator_linter
      forecast = dt, metrics = list(test = function(x) x + 1),
      dt$x, y = dt$test
    )
  )

  # additional unnamed argument does not work
  expect_warning(
    scoringutils:::apply_metrics( # nolint: undesirable_operator_linter
      forecast = dt, metrics = list(test = function(x) x + 1),
      dt$x, dt$test
    )
  )
})

test_that("apply_metrics() emits a single batched warning when multiple metrics fail", {
  dt <- data.table::data.table(x = 1:10)
  fail1 <- function(x) stop("error in fail1")
  fail2 <- function(x) stop("error in fail2")
  good <- function(x) x + 1

  w <- expect_warning(
    scoringutils:::apply_metrics( # nolint: undesirable_operator_linter
      forecast = dt,
      metrics = list("fail1" = fail1, "good" = good, "fail2" = fail2),
      dt$x
    )
  )
  # single warning mentions both failed metrics
  expect_match(conditionMessage(w), "fail1", fixed = TRUE)
  expect_match(conditionMessage(w), "fail2", fixed = TRUE)
  expect_match(conditionMessage(w), "error in fail1", fixed = TRUE)
  expect_match(conditionMessage(w), "error in fail2", fixed = TRUE)
  # successful metric is computed

  expect_true("good" %in% names(dt))
  # failed metrics are not present
  expect_false("fail1" %in% names(dt))
  expect_false("fail2" %in% names(dt))
})

test_that("apply_metrics() emits no warning when all metrics succeed", {
  dt <- data.table::data.table(x = 1:10)
  m1 <- function(x) x + 1
  m2 <- function(x) x * 2
  expect_no_condition(
    scoringutils:::apply_metrics( # nolint: undesirable_operator_linter
      forecast = dt,
      metrics = list("m1" = m1, "m2" = m2),
      dt$x
    )
  )
  expect_true("m1" %in% names(dt))
  expect_true("m2" %in% names(dt))
  expect_equal(dt$m1, 2:11) # nolint: expect_identical_linter
  expect_equal(dt$m2, (1:10) * 2) # nolint: expect_identical_linter
})

test_that("batched warning message includes metric name and error details for each failure", {
  dt <- data.table::data.table(x = 1:5)
  bad_a <- function(x) stop("missing argument foo")
  bad_b <- function(x) stop("division by zero")
  ok <- function(x) x

  w <- expect_warning(
    scoringutils:::apply_metrics( # nolint: undesirable_operator_linter
      forecast = dt,
      metrics = list("bad_a" = bad_a, "ok" = ok, "bad_b" = bad_b),
      dt$x
    )
  )
  msg <- conditionMessage(w)
  expect_match(msg, "bad_a", fixed = TRUE)
  expect_match(msg, "missing argument foo", fixed = TRUE)
  expect_match(msg, "bad_b", fixed = TRUE)
  expect_match(msg, "division by zero", fixed = TRUE)
  # successful metric not mentioned in warning
  expect_false(grepl("\\bok\\b", msg))
})

# attributes
test_that("`[` preserves attributes", {
  test <- data.table::copy(scores_binary)
  class(test) <- c("scores", "data.frame")
  expect_true("metrics" %in% names(attributes(test)))
  expect_true("metrics" %in% names(attributes(test[1:9])))
})


# =============================================================================
# assert_scores() # nolint: commented_code_linter
# =============================================================================
test_that("assert_scores() works", {
  expect_no_condition(assert_scores(scores_binary))
  expect_null(
    assert_scores(scores_binary)
  )
})

# ==============================================================================
# validate_metrics() # nolint: commented_code_linter
# ==============================================================================
test_that("validate_metrics() works as expected", {
  test_fun <- function(x, y, ...) {
    if (hasArg("test")) {
      message("test argument found")
    }
    return(y)
  }
  ## Additional tests for validate_metrics()
  # passing in something that's not a function or a known metric
  expect_warning(
    expect_warning(
      score(as_forecast_binary(na.omit(example_binary)), metrics = list(
        test1 = test_fun, test = test_fun, hi = "hi", "2" = 3
      )),
      "`Metrics` element number 3 is not a valid function"
    ),
    "`Metrics` element number 4 is not a valid function"
  )
})


# ==============================================================================
# run_safely() # nolint: commented_code_linter
# ==============================================================================
test_that("run_safely() works as expected", {
  f <- function(x) {
    x
  }
  expect_identical(run_safely(2, fun = f), 2)
  expect_identical(run_safely(2, y = 3, fun = f), 2)
  # run_safely() no longer warns directly; it returns error info for batching
  result <- run_safely(fun = f, metric_name = "f")
  expect_null(result$result)
  expect_type(result$error, "character")
  expect_match(result$error, "missing, with no default")

  result2 <- run_safely(y = 3, fun = f, metric_name = "f")
  expect_null(result2$result)
})

test_that("run_safely() returns result directly on success", {
  f <- function(x) x + 1
  result <- run_safely(2, fun = f, metric_name = "f")
  expect_identical(result, 3)
})

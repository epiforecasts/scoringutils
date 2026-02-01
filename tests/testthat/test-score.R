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
  expect_warning(
    score(onesample),
    "Computation for `log_score` failed. Error: need at least 2 data points."
  )

  # verify that all goes well with two samples
  twosample <- na.omit(example_sample_continuous)[sample_id %in% c(20, 21)]
  expect_no_condition(score(twosample))
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
  expect_warning(
    run_safely(fun = f, metric_name = "f"),
    'Computation for `f` failed. Error: argument "x" is missing, with no default',
    fixed = TRUE
  )
  expect_null(suppressWarnings(run_safely(y = 3, fun = f, metric_name = "f")))
})

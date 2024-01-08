test_that("get_protected_columns() returns the correct result", {

  data <- example_quantile
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_quantile))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))


  data <- example_binary
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_binary))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))

  data <- example_continuous
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_continuous))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))
})


test_that("run_safely() works as expected", {
  f <- function(x) {x}
  expect_equal(run_safely(2, fun = f), 2)
  expect_equal(run_safely(2, y = 3, fun = f), 2)
  expect_warning(
    run_safely(fun = f),
    'Function execution failed, returning NULL. Error: argument "x" is missing, with no default',
    fixed = TRUE
  )
  expect_equal(suppressWarnings(run_safely(y = 3, fun = f)), NULL)
})


# ==============================================================================
# get score_names
# ==============================================================================

test_that("get_score_names() works as expected", {
  expect_true(
    "brier_score" %in% get_score_names(scores_binary)
  )

  expect_equal(get_score_names(scores_continuous),
               attr(scores_continuous, "score_names"))

  #check that function errors if `error = TRUE` and not otherwise
  expect_error(
    get_score_names(example_quantile, error = TRUE),
    "Object needs an attribute"
  )
  expect_no_condition(
    get_score_names(scores_continuous)
  )

  # expect warning if some column changed
  ex <- data.table::copy(scores_continuous)
  data.table::setnames(ex, old = "crps", new = "changed")
  expect_warning(
    get_score_names(ex),
    "but are no longer column names of the data: `crps`"
  )
})

# test_that("prediction_is_quantile() correctly identifies quantile predictions", {
#   data <- data.frame(
#     predicted = 1:3,
#     quantile = c(0.1, 0.5, 0.9)
#   )
#
#   expect_true(prediction_is_quantile(data))
# })

# test_that("prediction_is_quantile() returns false for non-quantile predictions", {
#   data <- data.frame(
#     predicted = rnorm(5)
#   )
#
#   expect_false(prediction_is_quantile(data))
# })

# test_that("prediction_is_quantile() returns false if quantile column has wrong values", {
#   data <- data.frame(
#     predicted = rnorm(5),
#     quantile = rnorm(5)
#   )
#
#   expect_true(prediction_is_quantile(data))
# })

# test_that("prediction_is_quantile() returns false if quantile column is character", {
#   data <- data.frame(
#     predicted = rep(rnorm(5), 3),
#     quantile = c("A", "B", "C")
#   )
#
#   expect_true(prediction_is_quantile(data))
# })

# test_that("prediction_is_quantile() errors on non-data.frame input", {
#   expect_error(prediction_is_quantile(1:5))
# })
#
# test_that("prediction_is_quantile() handles empty data frame", {
#   data <- data.frame(predicted = numeric(0))
#
#   expect_false(prediction_is_quantile(data))
# })
#
# test_that("prediction_is_quantile() handles NA values", {
#   data <- data.frame(
#     predicted = c(1, NA, 3),
#     quantile = c(0.1, NA, 0.5)
#   )
#
#   expect_true(prediction_is_quantile(data))
# })

# test_that("is_scoringutils_check() is working", {
#   checked <- suppressMessages(validate_forecast(example_binary))
#   expect_true(is_scoringutils_check(checked))
#
#   checked$cleaned_data <- NULL
#   expect_error(is_scoringutils_check(checked))
# })


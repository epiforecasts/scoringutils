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
#   checked <- suppressMessages(validate(example_binary))
#   expect_true(is_scoringutils_check(checked))
#
#   checked$cleaned_data <- NULL
#   expect_error(is_scoringutils_check(checked))
# })


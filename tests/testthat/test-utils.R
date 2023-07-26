test_that("get_protected_columns() returns the correct result", {

  data <- example_quantile
  manual <- protected_columns <- c(
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_quantile))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))


  data <- example_binary
  manual <- protected_columns <- c(
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_binary))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))

  data <- example_continuous
  manual <- protected_columns <- c(
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_continuous))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))
})

test_that("get_prediction_type() correctly identifies quantile predictions", {
  data <- data.frame(
    prediction = 1:3,
    quantile = c(0.1, 0.5, 0.9) 
  )
  
  expect_equal(get_prediction_type(data), "quantile")
})

test_that("get_prediction_type() correctly identifies integer predictions", {
  data <- data.frame(
    prediction = as.integer(1:5)
  )
  
  expect_equal(get_prediction_type(data), "integer")
  
  data <- matrix(as.integer(1:9), nrow = 3)
  expect_equal(get_prediction_type(data), "integer")
})

test_that("get_prediction_type() correctly identifies continuous predictions", {
  data <- data.frame(
    prediction = rnorm(5)
  )
  
  expect_equal(get_prediction_type(data), "continuous") 
})

test_that("works with vector input", {
  predictions <- rnorm(5)
  
  expect_equal(get_prediction_type(predictions), "continuous")
})

test_that("get_prediction_type() returns error on invalid input", {
  suppressWarnings(expect_error(get_prediction_type("foo")))
})

test_that("get_prediction_type() handles NA values across prediction types", {
  # Quantile
  data <- data.frame(
    prediction = c(1, NA, 3),
    quantile = c(0.1, 0.5, 0.9)
  )
  expect_equal(get_prediction_type(data), "quantile")

  # Integer
  data <- data.frame(
    prediction = c(1, NA, 3)
  )
  expect_equal(get_prediction_type(data), "integer")

  # Continuous
  data <- data.frame(
    prediction = c(1.1, NA, 3.2)  
  )
  expect_equal(get_prediction_type(data), "continuous")
  predictions <- c(1.1, NA, 3.5)
  expect_equal(get_prediction_type(predictions), "continuous")
  
  # All NA
  data <- data.frame(prediction = NA)
  expect_error(
    get_prediction_type(data),
    "Input is not numeric and cannot be coerced to numeric"
  )
  expect_error(
    get_prediction_type(NA_real_),
   "Input is not numeric and cannot be coerced to numeric"
  )
})

test_that("prediction_is_quantile() correctly identifies quantile predictions", {
  data <- data.frame(
    prediction = 1:3, 
    quantile = c(0.1, 0.5, 0.9)
  )

  expect_true(prediction_is_quantile(data))
})

test_that("prediction_is_quantile() returns false for non-quantile predictions", {
  data <- data.frame(
    prediction = rnorm(5)
  )
  
  expect_false(prediction_is_quantile(data))
})

test_that("prediction_is_quantile() returns false if quantile column has wrong values", {
  data <- data.frame(
    prediction = rnorm(5),
    quantile = rnorm(5)
  )

  expect_true(prediction_is_quantile(data)) 
})

test_that("prediction_is_quantile() returns false if quantile column is character", {
  data <- data.frame(
    prediction = rep(rnorm(5), 3),
    quantile = c("A", "B", "C")
  )

  expect_true(prediction_is_quantile(data))
})

test_that("prediction_is_quantile() errors on non-data.frame input", {
  expect_error(prediction_is_quantile(1:5))
})

test_that("prediction_is_quantile() handles empty data frame", {
  data <- data.frame(prediction = numeric(0))
  
  expect_false(prediction_is_quantile(data))
})

test_that("prediction_is_quantile() handles NA values", {
  data <- data.frame(
    prediction = c(1, NA, 3),
    quantile = c(0.1, NA, 0.5) 
  )
  
  expect_true(prediction_is_quantile(data))
})

test_that("is_scoringutils_check() is working", {
  checked <- suppressMessages(check_forecasts(example_binary))
  expect_true(is_scoringutils_check(checked))

  checked$cleaned_data <- NULL
  expect_error(is_scoringutils_check(checked))
})


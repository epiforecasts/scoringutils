test_that("get_protected columns returns the correct result", {

  data <- example_quantile
  manual <- protected_columns <- c(
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  auto <- get_protected_columns(data)
  expect_equal(manual, auto)


  data <- example_binary
  manual <- protected_columns <- c(
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  auto <- get_protected_columns(data)
  expect_equal(manual, auto)

  data <- example_continuous
  manual <- protected_columns <- c(
    "prediction", "true_value", "sample", "quantile", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  auto <- get_protected_columns(data)
  expect_equal(manual, auto)

})

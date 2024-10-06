# ==============================================================================
# `get_protected_columns()`
# ==============================================================================
test_that("get_protected_columns() works as expected", {
  expect_equal(
    scoringutils:::get_protected_columns(),
    c(
      "predicted", "observed", "sample_id",
      "quantile_level", "upper", "lower", "pit_value",
      "interval_range", "boundary", "predicted_label", "interval_coverage",
      "interval_coverage_deviation", "quantile_coverage",
      "quantile_coverage_deviation"
    )
  )
})

test_that("get_protected_columns() returns the correct result", {
  data <- example_quantile
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value",
    "range", "boundary",
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_quantile))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))


  data <- example_binary
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value",
    "range", "boundary",
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_binary))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))

  data <- example_sample_continuous
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value",
    "range", "boundary",
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_sample_continuous))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))
})

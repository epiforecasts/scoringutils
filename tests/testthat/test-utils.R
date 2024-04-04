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
# get metrics
# ==============================================================================

test_that("get_metrics() works as expected", {
  expect_true(
    "brier_score" %in% get_metrics(scores_binary)
  )

  expect_equal(get_metrics(scores_continuous),
               attr(scores_continuous, "metrics"))

  #check that function errors if `error = TRUE` and not otherwise
  expect_error(
    get_metrics(example_quantile, error = TRUE),
    "Input needs an attribute"
  )
  expect_no_condition(
    get_metrics(scores_continuous)
  )

  # expect warning if some column changed
  ex <- data.table::copy(scores_continuous)
  data.table::setnames(ex, old = "crps", new = "changed")
  expect_warning(
    get_metrics(ex),
    "scores have been previously computed, but are no longer column names"
  )
})

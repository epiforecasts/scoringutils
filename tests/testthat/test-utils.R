test_that("get_protected_columns() returns the correct result", {

  data <- example_quantile
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_quantile))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))


  data <- example_binary
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value",
    "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )
  manual <- intersect(manual, colnames(example_binary))
  auto <- get_protected_columns(data)
  expect_equal(sort(manual), sort(auto))

  data <- example_continuous
  manual <- protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
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

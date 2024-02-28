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
    "scores have been previously computed, but are no longer column names"
  )
})


# ==============================================================================
# print
# ==============================================================================

test_that("print() works on forecast_* objects", {
  # Check print works on each forecast object
  test_dat <- list(na.omit(example_binary), na.omit(example_quantile),
    na.omit(example_point), na.omit(example_continuous), na.omit(example_integer))
  for (dat in test_dat){
    dat <- as_forecast(dat)
    forecast_type <- get_forecast_type(dat)
    forecast_unit <- get_forecast_unit(dat)

    # Check Forecast type
    expect_output(print(dat), "Forecast type")
    expect_output(print(dat), forecast_type)
    # Check Forecast unit
    expect_output(print(dat), "Forecast unit")
    expect_output(print(dat), pattern = paste(forecast_unit, collapse = " "))

    # Check print.data.table works.
    output_original <- capture.output(print(dat))
    output_test <- capture.output(print(data.table(dat)))
    expect_contains(output_original, output_test)
  }

  # Check Score columns are printed
  dat <- example_quantile %>%
    na.omit %>%
    set_forecast_unit(c("location", "target_end_date",
      "target_type", "horizon", "model")) %>%
    as_forecast() %>%
    add_coverage()

  expect_output(print(dat), "Score columns")
  score_cols <- get_score_names(dat)
  expect_output(print(dat), pattern = paste(score_cols, collapse = " "))
})

test_that("print methods fail gracefully", {
  test <- as_forecast(na.omit(example_quantile))
  test$observed <- NULL

  # message if forecast type can't be computed
  expect_warning(
    expect_message(
      expect_output(
        print(test),
        pattern = "Forecast unit:"
      ),
      "Could not determine forecast type due to error in validation."
    ),
    "Error in validating forecast object:"
  )

  # message if forecast unit can't be computed
  test <- 1:10
  class(test) <- "forecast_point"
  expect_warning(
    expect_message(
      expect_message(
        expect_output(
          print(test),
          pattern = "Forecast unit:"
        ),
        "Could not determine forecast unit."
      ),
      "Could not determine forecast type"
    ),
    "Error in validating forecast object:"
  )
})

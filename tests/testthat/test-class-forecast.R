# ==============================================================================
# as_forecast() # nolint: commented_code_linter
# ==============================================================================
# see tests for each forecast type for more specific tests.


# ==============================================================================
# as_forecast_*.data.frame dispatch and .default error handling
# ==============================================================================

test_that("as_forecast_*.data.frame methods work with data.table input via S3 dispatch", {
  expect_s3_class(
    suppressMessages(as_forecast_binary(na.omit(example_binary))),
    "forecast_binary"
  )
  expect_s3_class(
    as_forecast_quantile(na.omit(example_quantile)),
    "forecast_quantile"
  )
  expect_s3_class(
    as_forecast_sample(na.omit(example_sample_continuous)),
    "forecast_sample"
  )
  expect_s3_class(
    as_forecast_point(na.omit(example_point)),
    "forecast_point"
  )
  expect_s3_class(
    as_forecast_nominal(na.omit(example_nominal)),
    "forecast_nominal"
  )
  expect_s3_class(
    as_forecast_ordinal(na.omit(example_ordinal)),
    "forecast_ordinal"
  )
})

test_that("as_forecast_*.default errors with helpful message for non-data.frame input", {
  vec <- 1:10
  mat <- matrix(1:12, nrow = 3)
  lst <- list(a = 1, b = 2)
  chr <- "not a data.frame"

  expect_error(as_forecast_binary(vec), "data.frame")
  expect_error(as_forecast_binary(mat), "data.frame")
  expect_error(as_forecast_binary(lst), "data.frame")
  expect_error(as_forecast_binary(chr), "data.frame")

  expect_error(as_forecast_sample(vec), "data.frame")
  expect_error(as_forecast_quantile(mat), "data.frame")
})

test_that("as_forecast_*.default errors for matrix input", {
  mat <- matrix(1:20, nrow = 4,
    dimnames = list(NULL, c("observed", "predicted", "model", "date", "sample_id"))
  )
  expect_error(as_forecast_sample(mat), "data.frame")
})

test_that("as_forecast_*.data.frame methods preserve column renaming functionality", {
  set.seed(123)
  df <- data.frame(
    model = rep("m1", 10),
    date = as.Date("2020-01-01") + 1:10,
    obs = 1:10,
    pred = rnorm(10),
    sid = rep(1:2, each = 5)
  )
  result <- as_forecast_sample(df, observed = "obs", predicted = "pred", sample_id = "sid")
  expect_true("observed" %in% colnames(result))
  expect_true("predicted" %in% colnames(result))
  expect_true("sample_id" %in% colnames(result))
})

test_that("as_forecast_*.data.frame methods work with tibble input", {
  skip_if_not_installed("tibble")
  tbl <- tibble::as_tibble(na.omit(as.data.frame(example_binary)))
  result <- expect_no_error(as_forecast_binary(tbl))
  expect_s3_class(result, "forecast_binary")
})


# ==============================================================================
# is_forecast() # nolint: commented_code_linter
# ==============================================================================

test_that("is_forecast() works as expected", {
  expect_true(is_forecast(example_binary))
  expect_true(is_forecast(example_point))
  expect_true(is_forecast(example_quantile))
  expect_true(is_forecast(example_sample_continuous))
  expect_true(is_forecast(example_nominal))

  expect_false(is_forecast(1:10))
  expect_false(is_forecast(data.table::as.data.table(example_point)))
})


# ==============================================================================
# assert_forecast() and assert_forecast_generic() # nolint: commented_code_linter
# ==============================================================================

test_that("assert_forecast() works as expected", {
  # test that by default, `as_forecast()` errors
  expect_error(
    assert_forecast(data.frame(x = 1:10)),
    "The input needs to be a valid forecast object."
  )
})

test_that("assert_forecast_generic() works as expected with a data.frame", {
  expect_error(
    assert_forecast_generic(example_quantile_df),
    "Assertion on 'data' failed: Must be a data.table, not data.frame."
  )
})


# ==============================================================================
# new_forecast() # nolint: commented_code_linter
# ==============================================================================

test_that("new_forecast() works as expected with a data.frame", {
  expect_s3_class(
    new_forecast(example_quantile_df, "quantile"),
    c("forecast_quantile", "data.table", "data.frame")
  )
})


# ==============================================================================
# [.forecast() # nolint: commented_code_linter
# ==============================================================================

test_that("[.forecast() immediately invalidates on change when necessary", {
  test <- na.omit(data.table::copy(example_quantile))

  # For cols; various ways to drop.
  # We use local() to avoid actual deletion in this frame and having to recreate
  # the input multiple times
  expect_warning(
    local(test[, colnames(test) != "observed", with = FALSE]),
    "Error in validating"
  )

  expect_warning(
    local({
      test[, "observed"] <- NULL
    }),
    "Error in validating"
  )

  expect_warning(
    local({
      test$observed <- NULL
    }),
    "Error in validating"
  )

  expect_warning(
    local({
      test[["observed"]] <- NULL
    }),
    "Error in validating"
  )

  # For rows
  expect_warning(
    local({
      test[2, ] <- test[1, ]
    })
  )
})

test_that("[.forecast() doesn't warn on cases where the user likely didn't intend getting a forecast object", {
  test <- as_forecast_quantile(na.omit(example_quantile))

  expect_no_condition(test[, location])
})

test_that("[.forecast() is compatible with data.table syntax", {
  test <- as_forecast_quantile(na.omit(example_quantile))

  expect_no_condition(
    test[location == "DE"]
  )

  expect_no_condition(
    test[
      target_type == "Cases",
      .(location, target_end_date, observed, location_name, forecast_date, quantile_level, predicted, model)
    ]
  )
})


# ==============================================================================
# print.forecast() # nolint: commented_code_linter
# ==============================================================================
test_that("print() works on forecast_* objects", {
  # Check print works on each forecast object
  test_dat <- list(
    example_binary, example_quantile,
    example_point, example_sample_continuous,
    example_sample_discrete
  )
  test_dat <- lapply(test_dat, na.omit)
  for (dat in test_dat) {
    forecast_type <- scoringutils:::get_forecast_type(dat) # nolint: undesirable_operator_linter
    forecast_unit <- get_forecast_unit(dat)

    fn_name <- paste0("as_forecast_", forecast_type)
    fn <- get(fn_name)
    dat <- suppressWarnings(suppressMessages(do.call(fn, list(dat))))

    # Check Forecast type
    expect_snapshot(print(dat))
    expect_snapshot(print(dat))
    # Check Forecast unit
    expect_snapshot(print(dat))
    expect_snapshot(print(dat))

    # Check print.data.table works.
    output_original <- suppressMessages(capture.output(print(dat)))
    output_test <- suppressMessages(capture.output(print(data.table(dat))))
    expect_contains(output_original, output_test)
  }
})

test_that("print() throws the expected messages", {
  test <- data.table::copy(example_point)
  class(test) <- c("point", "forecast", "data.table", "data.frame")

  # note that since introducing a length maximum for validation to be triggered,
  # we don't throw a warning automatically anymore
  suppressMessages(
    expect_message(
      capture.output(print(test)),
      "Could not determine forecast type due to error in validation."
    )
  )

  class(test) <- c("forecast_point", "forecast")
  suppressMessages(
    expect_message(
      capture.output(print(test)),
      "Could not determine forecast unit."
    )
  )
})


# ==============================================================================
# check_number_per_forecast() # nolint: commented_code_linter
# ==============================================================================
test_that("check_number_per_forecast works", {
  expect_identical(
    capture.output(
      check_number_per_forecast(
        example_binary,
        forecast_unit = "location_name"
      )
    ),
    paste(
      "[1] \"Some forecasts have different numbers of rows",
      "(e.g. quantiles or samples). scoringutils found: 224, 215.",
      "This may be a problem (it can potentially distort scores,",
      "making it more difficult to compare them),",
      "so make sure this is intended.\""
    )
  )
  expect_true(
    check_number_per_forecast(
      example_binary
    )
  )
})


# ==============================================================================
# Test removing `NA` values from the data # nolint: commented_code_linter
# ==============================================================================
test_that("removing NA rows from data works as expected", {
  expect_identical(nrow(na.omit(example_quantile)), 20401L)

  ex <- data.frame(observed = c(NA, 1:3), predicted = 1:4)
  expect_identical(nrow(na.omit(ex)), 3L)

  ex$predicted <- c(1:3, NA)
  expect_identical(nrow(na.omit(ex)), 2L)

  # test that attributes and classes are retained
  ex <- as_forecast_sample(na.omit(example_sample_discrete))
  expect_s3_class(
    na.omit(ex),
    c("forecast_sample", "forecast", "data.table", "data.frame"),
    exact = TRUE
  )

  attributes <- attributes(ex)
  expect_identical(
    attributes(na.omit(ex)),
    attributes
  )
})

# ==============================================================================
# quantile_to_interval() # nolint: commented_code_linter
# ==============================================================================

test_that("quantile_to_interval_dataframe() works", {
  quantile <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    observed = 1:10,
    predicted = c(2:11, 4:13),
    quantile_level = rep(c(0.25, 0.75), each = 10),
    stringsAsFactors = FALSE
  )
  long <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    observed = 1:10,
    predicted = c(2:11, 4:13),
    interval_range = 50,
    boundary = rep(c("lower", "upper"), each = 10),
    stringsAsFactors = FALSE
  )
  long2 <- as.data.frame(quantile_to_interval(
    quantile,
    keep_quantile_col = FALSE
  ))
  data.table::setcolorder(long2, names(long))
  # for some reason this is needed to pass the unit tests on gh actions
  long2$boundary <- as.character(long2$boundary)
  long$boundary <- as.character(long$boundary)
  expect_identical(long, as.data.frame(long2))

  # check that it handles NA values
  setDT(quantile)
  quantile[c(1, 3, 11, 13), c("observed", "predicted", "quantile_level") := NA]
  # in this instance, a problem appears because there is an NA value both
  # for the upper and lower bound.

  # the data.table behavior differs before/after v1.16.0
  #  - before, it's a 'message'
  #  - after, it's a 'warning'
  #  - the conditionMessage() also differs
  expected_condition <- tryCatch(
    dcast(data.table(a = c(1, 1), b = 2, c = 3), a ~ b, value.var = "c"),
    condition = identity
  )
  expect_condition(
    quantile_to_interval(
      quantile,
      keep_quantile_col = FALSE,
      format = "wide"
    ),
    class = class(expected_condition)[1L], # testthat 3e requires exactly one class
    regexp = "[Aa]ggregate.*default.*length"
  )
  quantile <- quantile[-c(1, 3), ]
  wide2 <- scoringutils:::quantile_to_interval( # nolint: undesirable_operator_linter
    quantile,
    keep_quantile_col = FALSE,
    format = "wide"
  )
  expect_identical(nrow(wide2), 10L)
  expect_false(("NA") %in% colnames(wide2))
  expect_equal(sum(wide2$lower, na.rm = TRUE), 59) # nolint: expect_identical_linter
})


test_that("sample_to_range_long works", {
  samples <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    observed = 1:10,
    predicted = c(rep(0, 10), 2:11, 3:12, 4:13, rep(100, 10)),
    sample_id = rep(1:5, each = 10),
    stringsAsFactors = FALSE
  )

  long <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    observed = 1:10,
    predicted = c(2:11, 4:13),
    interval_range = 50,
    boundary = rep(c("lower", "upper"), each = 10),
    stringsAsFactors = FALSE
  )

  long2 <- scoringutils:::sample_to_interval_long(as_forecast_sample(samples), # nolint: undesirable_operator_linter
    interval_range = 50,
    keep_quantile_col = FALSE
  )
  long2 <- long2[order(model, boundary, date)]
  data.table::setcolorder(long2, names(long))

  # for some reason this is needed to pass the unit tests on gh actions
  long2$boundary <- as.character(long2$boundary)
  long$boundary <- as.character(long$boundary)

  expect_equal(long, as.data.frame(long2)) # nolint: expect_identical_linter
})

test_that("quantile_to_range works - scalar and vector case", {
  predicted <- 9:1
  quantile <- rev(seq(0.1, 0.9, 0.1))
  observed <- 5

  # check output is produced
  out1 <- quantile_to_interval(observed, predicted, quantile)
  expect_snapshot(out1)

  # check order of predictions doesn't matter
  predicted <- 1:9
  quantile <- seq(0.1, 0.9, 0.1)
  out2 <- quantile_to_interval(observed, predicted, quantile)
  expect_identical(out1, out2)

  # check error if observed is a vector and predicted is a vector as well
  expect_error(
    quantile_to_interval(
      observed = c(1, 2), predicted = c(1, 2), quantile_level = c(0.1, 0.9)
    ),
    "Assertion on 'predicted' failed: Must be of type 'matrix', not 'double'."
  )

  # check NA values are handled gracefully - there should just be NA in the output
  predicted <- c(1:8, NA)
  quantile <- seq(0.1, 0.9, 0.1)
  observed <- 5
  out3 <- quantile_to_interval(observed, predicted, quantile)
  expect_snapshot(out3)

  # check non-symmetrical intervals are handled gracefully
  # result should be newly introduced ranges where one value is NA
  predicted <- 1:9
  quantile <- c(seq(0.1, 0.8, 0.1), 0.95)
  observed <- 5
  out4 <- quantile_to_interval(observed, predicted, quantile)
  expect_snapshot(out4)

  # check function works without a median
  predicted <- 1:8
  quantile <- c(seq(0.1, 0.4, 0.1), seq(0.6, 0.9, 0.1))
  observed <- 5
  expect_no_condition(
    quantile_to_interval(observed, predicted, quantile)
  )

  # check a one-dimensional matrix works fine
  predicted <- matrix(1:9, nrow = 1)
  quantile <- seq(0.1, 0.9, 0.1)
  expect_no_condition(
    quantile_to_interval(observed, predicted, quantile)
  )
})

test_that("quantile_to_range works - matrix case", {
  n <- 5
  N <- 9
  predicted <- matrix(1:45, nrow = n, ncol = N)
  quantile <- seq(0.1, 0.9, 0.1)
  observed <- seq(21, 25, 1)

  # check output is produced
  out1 <- quantile_to_interval(observed, predicted, quantile)
  expect_snapshot(out1)

  # check order of predictions doesn't matter
  predicted <- matrix(
    c(41:45, 36:40, 31:35, 26:30, 21:25, 16:20, 11:15, 6:10, 1:5),
    nrow = n,
    ncol = N
  )
  quantile <- rev(seq(0.1, 0.9, 0.1))
  out2 <- quantile_to_interval(observed, predicted, quantile)
  expect_identical(out1, out2)

  # check NA values are fine
  predicted[1, 1] <- NA
  expect_no_condition(
    quantile_to_interval(observed, predicted, quantile)
  )
})


test_that("quantile_to_interval works - data.frame case", {
  dt <- data.table(
    observed = 5,
    predicted = 1:9,
    quantile_level = seq(0.1, 0.9, 0.1)
  )

  expect_no_condition(
    quantile_to_interval(dt)
  )

  expect_no_condition(
    quantile_to_interval(dt, format = "wide")
  )

  # check that the number of rows after transformation is equal to the number
  # of rows plus the number of medians added (as upper boundary of a 0%
  # prediction interval)
  ex <- example_quantile[!is.na(predicted)]
  n_preds <- nrow(ex)
  n_medians <- nrow(ex[quantile_level == 0.5])
  ex_interval <- quantile_to_interval(ex, keep_quantile_col = TRUE)
  expect_identical(
    nrow(ex_interval),
    n_preds + n_medians
  )

  expect_identical(
    colnames(ex_interval),
    c(colnames(ex), "boundary", "interval_range")
  )

  expect_error(
    quantile_to_interval(x = "not working"),
    "Input must be either a data.frame or a numeric vector."
  )
})


# ==============================================================================
# get_interval_range() # nolint: commented_code_linter
# ==============================================================================

test_that("get_interval_range() returns correct interval ranges for standard quantiles", {
  result <- get_interval_range(c(0.05, 0.25, 0.5, 0.75, 0.95))
  expect_identical(result, c(90, 50, 0, 50, 90))
})

test_that("get_interval_range() handles edge cases correctly", {
  expect_identical(get_interval_range(c(0, 1)), c(100, 100))
  expect_identical(get_interval_range(0.5), 0)
  expect_identical(get_interval_range(c(0.1, 0.9)), c(80, 80))
})

test_that("get_interval_range() is exported and accessible without :::", {
  expect_no_error(get_interval_range(0.25))
  expect_identical(get_interval_range(0.25), 50)
})


# ==============================================================================
# add_interval_range() # nolint: commented_code_linter
# ==============================================================================

test_that("add_interval_range() adds interval_range column to quantile forecast", {
  dt <- data.table::data.table(
    observed = 5,
    predicted = c(1, 3, 5, 7, 9),
    quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95)
  )
  ncol_before <- ncol(dt)
  result <- add_interval_range(dt)
  expect_true("interval_range" %in% colnames(result))
  expect_identical(result$interval_range, c(90, 50, 0, 50, 90))
  expect_identical(ncol(result), ncol_before + 1L)
})

test_that("add_interval_range() works with example_quantile dataset", {
  ex <- na.omit(example_quantile)
  result <- add_interval_range(ex)
  expect_true("interval_range" %in% colnames(result))
  expect_identical(nrow(result), nrow(ex))
  expect_identical(result$interval_range, get_interval_range(result$quantile_level))
})

test_that("add_interval_range() returns a copy (does not modify input in place)", {
  dt <- data.table::data.table(
    observed = 5,
    predicted = c(1, 3, 5, 7, 9),
    quantile_level = c(0.05, 0.25, 0.5, 0.75, 0.95)
  )
  result <- add_interval_range(dt)
  # ensure_data.table() copies, so original dt should be unchanged
  expect_false("interval_range" %in% colnames(dt))
  expect_true("interval_range" %in% colnames(result))
})

test_that("add_interval_range() errors gracefully on non-quantile input", {
  dt <- data.table::data.table(
    observed = 1:5,
    predicted = 2:6,
    sample_id = 1:5
  )
  expect_error(add_interval_range(dt))
})

test_that("internal call sites work correctly after rename", {
  expect_no_error(get_coverage(as_forecast_quantile(example_quantile)))
  expect_no_error(score(as_forecast_quantile(example_quantile)))
})

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
  wide2 <- quantile_to_interval(
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

  long2 <- sample_to_interval_long(as_forecast_sample(samples),
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

test_that("quantile_to_interval() matches lower and upper bounds by interval", {
  # quantile levels in arbitrary order, with one incomplete interval (0.05)
  quantile_level <- c(0.75, 0.05, 0.5, 0.25)
  predicted <- rbind(c(7, 1, 5, 3), c(17, 11, 15, 13))
  out <- quantile_to_interval(c(4, 14), predicted, quantile_level)
  expect_identical(
    out,
    data.table(
      forecast_id = rep(1:2, each = 3),
      observed = rep(c(4, 14), each = 3),
      interval_range = rep(c(0, 50, 90), times = 2),
      lower = c(5, 3, 1, 15, 13, 11),
      upper = c(5, 7, NA, 15, 17, NA),
      key = c("forecast_id", "observed", "interval_range")
    )
  )
})

test_that("quantile_to_interval() errors without lower or upper bounds", {
  expect_error(
    quantile_to_interval(c(1, 2), cbind(1:2, 3:4), c(0.75, 0.9)),
    "No valid forecast intervals found."
  )
  expect_error(
    quantile_to_interval(c(1, 2), cbind(1:2, 3:4), c(0.1, 0.25)),
    "No valid forecast intervals found."
  )
  expect_error(
    quantile_to_interval(
      data.table(observed = 1, predicted = 1:2, quantile_level = c(0.1, 0.25))
    ),
    "No valid forecast intervals found."
  )
})

test_that("quantile_to_interval() handles missing and near-duplicate levels", {
  # a missing quantile level does not form an interval
  predicted <- cbind(1:2, 3:4, 5:6)
  expect_identical(
    quantile_to_interval(c(1, 2), predicted, c(NA, 0.25, 0.75))$interval_range,
    c(50, 50)
  )
  expect_error(
    wis(c(1, 2), predicted, c(NA, 0.25, 0.75)),
    "Not all quantile levels specified form symmetric prediction intervals"
  )
  expect_error(
    wis(c(1, 2), predicted[, 1:2], c(NA, 0.75)),
    "No valid forecast intervals found."
  )
  # 0.1 * 3 and 0.3 are different numbers that describe the same interval
  expect_error(
    quantile_to_interval(1, 1:4, c(0.3, 0.1 * 3, 0.5, 0.7)),
    "Quantile levels must be unique after rounding to 10 digits"
  )
})

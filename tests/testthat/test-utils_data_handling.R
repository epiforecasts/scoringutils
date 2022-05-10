test_that("range_long_to_quantile works", {
  long <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    true_value = 1:10,
    prediction = c(2:11, 4:13),
    range = 50,
    boundary = rep(c("lower", "upper"), each = 10)
  )

  quantile <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    true_value = 1:10,
    prediction = c(2:11, 4:13),
    quantile = rep(c(0.25, 0.75), each = 10)
  )

  quantile2 <- as.data.frame(scoringutils:::range_long_to_quantile(long))
  expect_equal(quantile, quantile2)
})



test_that("quantile_to_range_long works", {
  quantile <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    true_value = 1:10,
    prediction = c(2:11, 4:13),
    quantile = rep(c(0.25, 0.75), each = 10)
  )

  long <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    true_value = 1:10,
    prediction = c(2:11, 4:13),
    range = 50,
    boundary = rep(c("lower", "upper"), each = 10)
  )

  long2 <- as.data.frame(scoringutils:::quantile_to_range_long(quantile,
    keep_quantile_col = FALSE
  ))

  data.table::setcolorder(long2, names(long))

  # for some reason this is needed to pass the unit tests on gh actions
  long2$boundary <- as.character(long2$boundary)
  long$boundary <- as.character(long$boundary)

  expect_equal(long, as.data.frame(long2))
})


test_that("sample_to_quantiles works", {
  samples <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    true_value = 1:10,
    prediction = c(rep(0, 10), 2:11, 3:12, 4:13, rep(100, 10)),
    sample = rep(1:5, each = 10)
  )

  quantile <- data.frame(
    date = rep(as.Date("2020-01-01") + 1:10, each = 2),
    model = "model1",
    true_value = rep(1:10, each = 2),
    quantile = c(0.25, 0.75),
    prediction = rep(2:11, each = 2) + c(0, 2)
  )

  quantile2 <- scoringutils::sample_to_quantile(samples, quantiles = c(0.25, 0.75))

  expect_equal(quantile, as.data.frame(quantile2))

  # Verify that `type` is correctly scoped in sample_to_quantile(), as it is
  # also an argument.
  # If it's not scoped well, the call to `sample_to_quantile()` will fail.
  samples$type <- "test"

  quantile3 <- sample_to_quantile(samples, quantiles = c(0.25, 0.75))
  quantile3$type <- NULL

  expect_identical(
    quantile2,
    quantile3
  )

})



test_that("sample_to_range_long works", {
  samples <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    true_value = 1:10,
    prediction = c(rep(0, 10), 2:11, 3:12, 4:13, rep(100, 10)),
    sample = rep(1:5, each = 10)
  )

  long <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    true_value = 1:10,
    prediction = c(2:11, 4:13),
    range = 50,
    boundary = rep(c("lower", "upper"), each = 10)
  )

  long2 <- scoringutils:::sample_to_range_long(samples,
    range = 50,
    keep_quantile_col = FALSE
  )
  long2 <- long2[order(model, boundary, date)]
  data.table::setcolorder(long2, names(long))

  # for some reason this is needed to pass the unit tests on gh actions
  long2$boundary <- as.character(long2$boundary)
  long$boundary <- as.character(long$boundary)

  expect_equal(long, as.data.frame(long2))
})

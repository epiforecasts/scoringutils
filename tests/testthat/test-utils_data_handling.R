# test equality scoringutils and scorignutils2

test_that("range_long_to_wide works", {
  long <- data.frame(date = as.Date("2020-01-01") + 1:10,
                     model = "model1",
                     true_value = 1:10,
                     prediction = c(2:11, 4:13),
                     range = 50,
                     boundary = rep(c("lower", "upper"), each = 10))

  data.table::setDT(long)
  wide <- scoringutils::quantile_to_wide(long)
  wide2 <- scoringutils2::range_long_to_wide(long)

  expect_equal(wide, wide2)
})



test_that("range_wide_to_long works", {
  wide <- data.frame(date = as.Date("2020-01-01") + 1:10,
                     model = "model1",
                     true_value = 1:10,
                     lower_50 = c(2:11),
                     upper_50 = 4:13)

  data.table::setDT(wide)

  long <- scoringutils::quantile_to_long(wide)
  long2 <- scoringutils2::range_wide_to_long(wide)

  expect_equal(long, long2)
})



test_that("range_long_to_quantile works", {

  long <- data.frame(date = as.Date("2020-01-01") + 1:10,
                     model = "model1",
                     true_value = 1:10,
                     prediction = c(2:11, 4:13),
                     range = 50,
                     boundary = rep(c("lower", "upper"), each = 10))

  quantile <- scoringutils::range_to_quantile(long)
  quantile2 <- scoringutils2::range_long_to_quantile(long)

  expect_equal(quantile, quantile2)
})



test_that("quantile_to_range_long works", {
  quantile <- data.frame(date = as.Date("2020-01-01") + 1:10,
                         model = "model1",
                         true_value = 1:10,
                         prediction = c(2:11, 4:13),
                         quantile = rep(c(0.25, 0.75), each = 10))

  long <- scoringutils::quantile_to_range(quantile)
  long2 <- scoringutils2::quantile_to_range_long(quantile)

  expect_equal(long, long2)
})





test_that("sample_to_quantiles works", {


  samples <- data.frame(date = as.Date("2020-01-01") + 1:10,
                        model = "model1",
                        true_value = 1:10,
                        prediction = c(rep(0, 10), 2:11, 3:12, 4:13, rep(100, 10)),
                        sample = rep(1:5, each = 10))

  quantile2 <- scoringutils2::sample_to_quantile(samples, quantiles = c(0.25, 0.75))
  quantile2 <- quantile2[order(model, quantile, date)]

  quantile <- scoringutils::sample_to_quantile(samples, quantiles = c(0.25, 0.75))
  quantile <- quantile[order(model, quantile, date)]

  expect_equal(quantile, quantile2)
})



test_that("sample_to_range_long works", {
  samples <- data.frame(date = as.Date("2020-01-01") + 1:10,
                        model = "model1",
                        true_value = 1:10,
                        prediction = c(rep(0, 10), 2:11, 3:12, 4:13, rep(100, 10)),
                        sample = rep(1:5, each = 10))

  long2 <- scoringutils2::sample_to_range_long(samples, range = c(50))
  long2 <- long2[order(model, boundary, date)]

  long <- scoringutils::sample_to_range(samples, range = c(50))
  long <- long[order(model, boundary, date)]

  expect_equal(long, long2)
})





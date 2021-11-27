# common error handling --------------------------------------------------------
test_that("function throws an error if data is missing", {
  expect_error(eval_forecasts(data = NULL))
})

test_that("eval_forecasts() warns if column name equals a metric name", {
  data <- data.frame(true_value = rep(1:10, each = 2),
                     prediction = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
                     model = "Model 1",
                     date = as.Date("2020-01-01") + rep(1:10, each = 2),
                     quantile = rep(c(0.1, 0.9), times = 10),
                     bias = 3)

  expect_warning(eval_forecasts(data = data))
})



# test binary case -------------------------------------------------------------
test_that("function produces output for a binary case", {
  binary_example <- data.table::setDT(scoringutils::binary_example_data)
  eval <- eval_forecasts(binary_example[!is.na(prediction)],
                         summarise_by = c("model", "value_desc"),
                         quantiles = c(0.5), sd = TRUE)
  expect_equal(nrow(eval) > 1,
               TRUE)
})


test_that("function produces score for a binary case", {
  binary_example <- data.table::setDT(scoringutils::binary_example_data)
  eval <- eval_forecasts(binary_example[!is.na(prediction)],
                         summarise_by = c("model", "value_desc"),
                         quantiles = c(0.5), sd = TRUE)
  expect_true("brier_score" %in% names(eval))
})




# test quantile case -----------------------------------------------------------
test_that("function produces output for a quantile format case", {
  quantile_example <- data.table::setDT(scoringutils::quantile_example_data)
  eval <- eval_forecasts(quantile_example[!is.na(prediction)],
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  expect_equal(nrow(eval) > 1,
               TRUE)
})

test_that("eval_forecasts() quantile produces desired metrics", {
  data <- data.frame(true_value = rep(1:10, each = 2),
                     prediction = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
                     model = "Model 1",
                     date = as.Date("2020-01-01") + rep(1:10, each = 2),
                     quantile = rep(c(0.1, 0.9), times = 10))

  out <- eval_forecasts(data = data)
  metric_names <- c("dispersion", "underprediction", "overprediction",
                    "bias", "aem", "coverage_deviation")

  expect_true(all(metric_names %in% colnames(out)))
})


test_that("calculation of aem is correct for a quantile format case", {
  quantile_example <- data.table::setDT(scoringutils::quantile_example_data)
  eval <- eval_forecasts(quantile_example[!is.na(prediction)],
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  ae <- quantile_example[quantile == 0.5, ae := abs(true_value - prediction)
  ][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval$aem), sort(ae))
})


test_that("all quantile and range formats yield the same result", {
  quantile_example1 <- data.table::setDT(scoringutils::quantile_example_data)

  quantile_example2 <- data.table::setDT(scoringutils::range_example_data_long)
  quantile_example2 <- range_long_to_quantile(quantile_example2)

  quantile_example3 <- data.table::setDT(scoringutils::range_example_data_semi_wide)
  quantile_example3 <- range_wide_to_long(quantile_example3)
  quantile_example3 <- range_long_to_quantile(quantile_example3)

  wide <- data.table::setDT(scoringutils::range_example_data_wide)
  quantile_example4 <- scoringutils::range_wide_to_long(wide)


  eval1 <- eval_forecasts(quantile_example1[!is.na(prediction)],
                          summarise_by = c("model"),
                          quantiles = c(0.5), sd = TRUE)

  eval2 <- eval_forecasts(quantile_example2[!is.na(prediction)],
                          summarise_by = c("model"),
                          quantiles = c(0.5), sd = TRUE)

  ae <- quantile_example1[quantile == 0.5, ae := abs(true_value - prediction)
  ][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval1$aem), sort(ae))
})

test_that("function produces output even if only some metrics are chosen", {
  range_example_wide <- data.table::setDT(scoringutils::range_example_data_wide)
  range_example <- scoringutils::range_wide_to_long(range_example_wide)
  example <- range_long_to_quantile(range_example)

  eval <- scoringutils::eval_forecasts(example,
                                       summarise_by = c("model", "range"),
                                       metrics = "coverage",
                                       sd = TRUE)

  expect_equal(nrow(eval) > 1,
               TRUE)
})

test_that("WIS is the same with other metrics omitted or included", {
  range_example_wide <- data.table::setDT(scoringutils::range_example_data_wide)
  range_example <- scoringutils::range_wide_to_long(range_example_wide)
  example <- scoringutils::range_long_to_quantile(range_example)

  eval <- scoringutils::eval_forecasts(example,
                                       summarise_by = c("model", "range"),
                                       metrics = "interval_score")

  eval2 <- scoringutils::eval_forecasts(example,
                                        summarise_by = c("model", "range"))

  expect_equal(sum(eval$interval_score),
               sum(eval2$interval_score))
})





# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {
  example <- data.table::setDT(scoringutils::continuous_example_data)
  eval <- eval_forecasts(example[!is.na(prediction)],
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  eval2 <- scoringutils::eval_forecasts(example,
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  data.table::setcolorder(eval2, colnames(eval))
  eval <- eval[order(model)]
  eval2 <- eval2[order(model)]
  all(eval == eval2, na.rm = TRUE)

  expect_equal(nrow(eval) > 1,
               TRUE)
})


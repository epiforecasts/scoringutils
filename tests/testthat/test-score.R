# common error handling --------------------------------------------------------
test_that("function throws an error if data is missing", {
  expect_error(score(data = NULL))
})

test_that("score() warns if column name equals a metric name", {
  data <- data.frame(true_value = rep(1:10, each = 2),
                     prediction = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
                     model = "Model 1",
                     date = as.Date("2020-01-01") + rep(1:10, each = 2),
                     quantile = rep(c(0.1, 0.9), times = 10),
                     bias = 3)

  expect_warning(score(data = data))
})



# test binary case -------------------------------------------------------------
test_that("function produces output for a binary case", {
  binary_example <- data.table::setDT(scoringutils::example_binary)
  eval <- score(binary_example[!is.na(prediction)],
                         summarise_by = c("model", "target_type"))
  expect_equal(nrow(eval) > 1,
               TRUE)
  expect_equal(colnames(eval),
               c("model", "target_type",
                 "brier_score"))
})


test_that("function produces score for a binary case", {
  binary_example <- data.table::setDT(scoringutils::example_binary)
  eval <- score(binary_example[!is.na(prediction)],
                         summarise_by = c("model", "target_type"))
  expect_true("brier_score" %in% names(eval))
})




# test quantile case -----------------------------------------------------------
test_that("function produces output for a quantile format case", {
  quantile_example <- data.table::setDT(scoringutils::example_quantile)
  eval <- score(quantile_example[!is.na(prediction)],
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  expect_equal(nrow(eval) > 1,
               TRUE)
})

test_that("score() quantile produces desired metrics", {
  data <- data.frame(true_value = rep(1:10, each = 2),
                     prediction = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
                     model = "Model 1",
                     date = as.Date("2020-01-01") + rep(1:10, each = 2),
                     quantile = rep(c(0.1, 0.9), times = 10))

  out <- score(data = data)
  metric_names <- c("dispersion", "underprediction", "overprediction",
                    "bias", "aem", "coverage_deviation")

  expect_true(all(metric_names %in% colnames(out)))
})


test_that("calculation of aem is correct for a quantile format case", {
  quantile_example <- data.table::setDT(scoringutils::example_quantile)
  eval <- score(quantile_example[!is.na(prediction)],
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  ae <- quantile_example[quantile == 0.5, ae := abs(true_value - prediction)
  ][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval$aem), sort(ae))
})


test_that("all quantile and range formats yield the same result", {
  quantile_example1 <- data.table::setDT(scoringutils::example_quantile)

  quantile_example2 <- data.table::setDT(scoringutils::example_range_long)
  quantile_example2 <- range_long_to_quantile(quantile_example2)

  quantile_example3 <- data.table::setDT(scoringutils::example_range_semi_wide)
  quantile_example3 <- range_wide_to_long(quantile_example3)
  quantile_example3 <- range_long_to_quantile(quantile_example3)

  wide <- data.table::setDT(scoringutils::example_range_wide)
  quantile_example4 <- scoringutils::range_wide_to_long(wide)


  eval1 <- score(quantile_example1[!is.na(prediction)],
                          summarise_by = c("model"),
                          quantiles = c(0.5), sd = TRUE)

  eval2 <- score(quantile_example2[!is.na(prediction)],
                          summarise_by = c("model"),
                          quantiles = c(0.5), sd = TRUE)

  ae <- quantile_example1[quantile == 0.5, ae := abs(true_value - prediction)
  ][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval1$aem), sort(ae))
})

test_that("function produces output even if only some metrics are chosen", {
  range_example_wide <- data.table::setDT(scoringutils::example_range_wide)
  range_example <- scoringutils::range_wide_to_long(range_example_wide)
  example <- range_long_to_quantile(range_example)

  eval <- scoringutils::score(example,
                                       summarise_by = c("model", "range"),
                                       metrics = "coverage",
                                       sd = TRUE)

  expect_equal(nrow(eval) > 1,
               TRUE)
})

test_that("WIS is the same with other metrics omitted or included", {
  range_example_wide <- data.table::setDT(scoringutils::example_range_wide)
  range_example <- scoringutils::range_wide_to_long(range_example_wide)
  example <- scoringutils::range_long_to_quantile(range_example)

  eval <- scoringutils::score(example,
                                       summarise_by = c("model", "range"),
                                       metrics = "interval_score")

  eval2 <- scoringutils::score(example,
                                        summarise_by = c("model", "range"))

  expect_equal(sum(eval$interval_score),
               sum(eval2$interval_score))
})





# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {
  example <- data.table::setDT(scoringutils::example_continuous)
  eval <- score(example[!is.na(prediction)],
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  eval2 <- scoringutils::score(example,
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  data.table::setcolorder(eval2, colnames(eval))
  eval <- eval[order(model)]
  eval2 <- eval2[order(model)]
  all(eval == eval2, na.rm = TRUE)

  expect_equal(nrow(eval) > 1,
               TRUE)
})


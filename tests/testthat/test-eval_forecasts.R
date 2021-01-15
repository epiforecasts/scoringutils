# common error handling --------------------------------------------------------
test_that("function throws an error if data is missing", {
  expect_error(eval_forecasts(data = NULL))
})



# test binary case -------------------------------------------------------------
test_that("function produces output for a binary case", {
  binary_example <- data.table::setDT(scoringutils::binary_example_data)
  eval <- eval_forecasts(binary_example[!is.na(prediction)],
                         summarise_by = c("model", "value_desc"),
                         quantiles = c(0.5), sd = TRUE,
                         verbose = FALSE)
  expect_equal(nrow(eval) > 1,
               TRUE)
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
  quantile_example3 <- data.table::setDT(scoringutils::range_example_data_semi_wide)

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


# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {
  example <- data.table::setDT(scoringutils::continuous_example_data)
  eval <- eval_forecasts(example[!is.na(prediction)],
                         summarised = TRUE,
                         summarise_by = c("model"),
                         quantiles = c(0.5), sd = TRUE)

  # eval2 <- scoringutils::eval_forecasts(example,
  #                                       summarised = TRUE,
  #                        summarise_by = c("model"),
  #                        quantiles = c(0.5), sd = TRUE)
  #
  # setcolorder(eval2, colnames(eval))
  # eval <- eval[order(model)]
  # eval2 <- eval2[order(model)]
  # all(eval == eval2, na.rm = TRUE)

  expect_equal(nrow(eval) > 1,
               TRUE)
})












#
# # tests that function returns the same results for scoringutils2 and scoringutils1
# test_that("scoringutils and scoringutils2 are the same for a binary case", {
#   binary_example <- data.table::setDT(scoringutils::binary_example_data)
#   eval2 <- scoringutils::eval_forecasts(binary_example,
#                                          summarise_by = c("model", "value_desc"),
#                                          quantiles = c(0.5), sd = TRUE,
#                                          verbose = FALSE)
#
#   eval <- scoringutils::eval_forecasts(binary_example[!is.na(prediction)],
#                                        summarise_by = c("model", "value_desc"),
#                                        quantiles = c(0.5), sd = TRUE,
#                                        verbose = FALSE)
#
#   all(eval == eval2)
#
#   expect_equal(eval, eval2)
# })
#
#
# test_that("scoringutils and scoringutils2 are the same for a continuous case", {
#   example <- data.table::setDT(scoringutils::continuous_example_data)
#   eval2 <- scoringutils::eval_forecasts(example,
#                                          summarise_by = c("model", "value_desc"),
#                                          quantiles = c(0.5), sd = TRUE,
#                                          verbose = FALSE)
#
#   eval <- scoringutils::eval_forecasts(example[!is.na(prediction)],
#                                        summarise_by = c("model", "value_desc"),
#                                        quantiles = c(0.5), sd = TRUE,
#                                        verbose = FALSE)
#
#   eval2 <- eval2[, .SD, .SDcols = names(eval2)[names(eval2) %in% names(eval)]]
#   data.table::setcolorder(eval2, names(eval))
#
#   expect_equal(eval, eval2)
# })
#
#
# test_that("scoringutils and scoringutils2 are the same for an integer case", {
#   set.seed(1)
#   example <- data.table::setDT(scoringutils::integer_example_data)
#   eval2 <- scoringutils::eval_forecasts(example,
#                                          summarise_by = c("model", "value_desc"),
#                                          quantiles = c(0.5), sd = TRUE,
#                                          verbose = FALSE)
#   set.seed(1)
#   eval <- scoringutils::eval_forecasts(example[!is.na(prediction)],
#                                        summarise_by = c("model", "value_desc"),
#                                        quantiles = c(0.5), sd = TRUE,
#                                        verbose = FALSE)
#
#   eval <- eval[order(model)]
#   eval[, c("pit_p_val", "pit_sd", "pit_p_val_0.5") := NULL]
#   eval2 <- eval2[order(model)]
#   eval2 <- eval2[, .SD, .SDcols = names(eval2)[names(eval2) %in% names(eval)]]
#   data.table::setcolorder(eval2, names(eval))
#
#   expect_equal(eval, eval2)
# })
#
#
#
#
# test_that("scoringutils and scoringutils2 are the same for a quantile case", {
#   example <- data.table::setDT(scoringutils::quantile_example_data)
#   eval2 <- scoringutils::eval_forecasts(example,
#                                          summarise_by = c("model", "value_desc"),
#                                          interval_score_arguments = list(count_median_twice = FALSE),
#                                          quantiles = c(0.5), sd = TRUE,
#                                          verbose = FALSE)
#
#   eval <- scoringutils::eval_forecasts(example[!is.na(prediction)],
#                                        summarise_by = c("model", "value_desc"),
#                                        quantiles = c(0.5), sd = TRUE,
#                                        verbose = FALSE)
#
#   eval <- eval[order(model)]
#   eval2 <- eval2[order(model)]
#   eval2 <- eval2[, .SD, .SDcols = names(eval2)[names(eval2) %in% names(eval)]]
#   eval <- eval[, .SD, .SDcols = names(eval)[names(eval) %in% names(eval2)]]
#   data.table::setcolorder(eval2, names(eval))
#
#   expect_equal(eval, eval2)
# })
#
#
# test_that("scoringutils and scoringutils2 are the same for a range format case", {
#   example <- data.table::setDT(scoringutils::range_example_data_long)
#   eval2 <- scoringutils::eval_forecasts(example,
#                                          summarise_by = c("model", "value_desc"),
#                                          interval_score_arguments = list(count_median_twice = FALSE),
#                                          quantiles = c(0.5), sd = TRUE,
#                                          verbose = FALSE)
#
#   eval <- scoringutils::eval_forecasts(example[!is.na(prediction)],
#                                        summarise_by = c("model", "value_desc"),
#                                        quantiles = c(0.5), sd = TRUE,
#                                        verbose = FALSE)
#
#   eval <- eval[order(model)]
#   eval2 <- eval2[order(model)]
#   eval2 <- eval2[, .SD, .SDcols = names(eval2)[names(eval2) %in% names(eval)]]
#   eval <- eval[, .SD, .SDcols = names(eval)[names(eval) %in% names(eval2)]]
#   data.table::setcolorder(eval2, names(eval))
#
#   expect_equal(eval, eval2)
# })



## test for separate truth and forecast data



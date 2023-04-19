# common error handling --------------------------------------------------------
test_that("function throws an error if data is missing", {
  expect_error(suppressMessages(score(data = NULL)))
})

test_that("score() warns if column name equals a metric name", {
  data <- data.frame(
    true_value = rep(1:10, each = 2),
    prediction = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
    model = "Model 1",
    date = as.Date("2020-01-01") + rep(1:10, each = 2),
    quantile = rep(c(0.1, 0.9), times = 10),
    bias = 3
  )

  expect_warning(suppressMessages(score(data = data)))
})



# test binary case -------------------------------------------------------------
test_that("function produces output for a binary case", {
  binary_example <- data.table::setDT(scoringutils::example_binary)
  eval <- suppressMessages(score(binary_example[!is.na(prediction)]))
  eval <- summarise_scores(eval, by = c("model", "target_type"))

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
  expect_equal(
    colnames(eval),
    c(
      "model", "target_type",
      "brier_score",
      "log_score"
    )
  )
})


test_that("function produces score for a binary case", {
  binary_example <- data.table::setDT(scoringutils::example_binary)
  eval <- suppressMessages(score(binary_example[!is.na(prediction)]))
  eval <- summarise_scores(eval, by = c("model", "target_type"))
  expect_true("brier_score" %in% names(eval))
})




# test quantile case -----------------------------------------------------------
test_that("function produces output for a quantile format case", {
  quantile_example <- data.table::setDT(scoringutils::example_quantile)
  eval <- suppressMessages(score(quantile_example[!is.na(prediction)]))

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
})

test_that("score_quantile correctly handles separate results = FALSE", {
  quantile_example <- data.table::setDT(scoringutils::example_quantile)
  eval <- suppressMessages(
    score(
      quantile_example[!is.na(prediction)],
      separate_results = FALSE
    )
  )

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
})


test_that("score() quantile produces desired metrics", {
  data <- data.frame(
    true_value = rep(1:10, each = 2),
    prediction = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
    model = "Model 1",
    date = as.Date("2020-01-01") + rep(1:10, each = 2),
    quantile = rep(c(0.1, 0.9), times = 10)
  )

  out <- suppressMessages(score(data = data))
  metric_names <- c(
    "dispersion", "underprediction", "overprediction",
    "bias", "ae_median", "coverage_deviation"
  )

  expect_true(all(metric_names %in% colnames(out)))
})


test_that("calculation of ae_median is correct for a quantile format case", {
  eval <- suppressMessages(
    score(scoringutils::example_quantile[!is.na(prediction)])
  )

  eval <- summarise_scores(eval,by = "model")

  example <- scoringutils::example_quantile
  ae <- example[quantile == 0.5, ae := abs(true_value - prediction)][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval$ae_median), sort(ae))
})


test_that("all quantile and range formats yield the same result", {
  quantile_example1 <- data.table::setDT(scoringutils::example_quantile)

  eval1 <- suppressMessages(score(quantile_example1[!is.na(prediction)]))
  eval1 <- summarise_scores(eval1, by = "model")

  ae <- quantile_example1[
    quantile == 0.5, ae := abs(true_value - prediction)][
    !is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval1$ae_median), sort(ae))
})

test_that("function produces output even if only some metrics are chosen", {
  example <- scoringutils::example_quantile

  eval <- suppressMessages(score(example, metrics = "coverage"))

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
})

test_that("WIS is the same with other metrics omitted or included", {
  eval <- suppressMessages(score(example_quantile,
    metrics = "interval_score"
  ))

  eval2 <- suppressMessages(score(example_quantile))

  expect_equal(
    sum(eval$interval_score),
    sum(eval2$interval_score)
  )
})





# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {
  example <- data.table::setDT(scoringutils::example_continuous)
  eval <- suppressMessages(score(example[!is.na(prediction)]))

  eval2 <- suppressMessages(score(example))

  data.table::setcolorder(eval2, colnames(eval))
  eval <- eval[order(model)]
  eval2 <- eval2[order(model)]
  all(eval == eval2, na.rm = TRUE)

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
})

test_that(
  "score() can support a sample column when a quantile forecast is used", {
  ex <- example_quantile[!is.na(quantile)][1:200, ]
  ex <- rbind(
    data.table::copy(ex)[, sample := 1],
    ex[, sample := 2]
  )
  scores <- suppressWarnings(score(ex))
  expect_snapshot(summarise_scores(
    summarise_scores(scores, by = "model"), by = "model", 
    fun = signif, digits = 2
  ))
 })

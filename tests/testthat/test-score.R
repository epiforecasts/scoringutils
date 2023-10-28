# common error handling --------------------------------------------------------
test_that("function throws an error if data is missing", {
  expect_error(suppressMessages(score(data = NULL)))
})

# test_that("score() warns if column name equals a metric name", {
#   data <- data.frame(
#     observed = rep(1:10, each = 2),
#     predicted = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
#     model = "Model 1",
#     date = as.Date("2020-01-01") + rep(1:10, each = 2),
#     quantile = rep(c(0.1, 0.9), times = 10),
#     bias = 3
#   )
#
#   expect_warning(suppressMessages(score(data = data)))
# })



# test binary case -------------------------------------------------------------
test_that("function produces output for a binary case", {
  binary_example <- data.table::setDT(scoringutils::example_binary)
  eval <- suppressMessages(score(binary_example[!is.na(predicted)]))
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
  eval <- suppressMessages(score(binary_example[!is.na(predicted)]))
  eval <- summarise_scores(eval, by = c("model", "target_type"))
  expect_true("brier_score" %in% names(eval))
})

test_that("score.scoringutils_binary() errors with only NA values", {
  only_nas <- copy(example_binary)[, predicted := NA_real_]
  expect_error(
    score(only_nas),
    "After removing NA values in `observed` and `predicted`, there were no observations left"
  )
})

test_that("score() gives same result for binary as regular function", {
  binary_example <- data.table::setDT(scoringutils::example_binary[!is.na(predicted)])
  eval <- suppressMessages(score(binary_example))
  manual_eval <- brier_score(
    factor(binary_example$observed),
    binary_example$predicted
  )
  expect_equal(eval$brier_score, manual_eval)
})

test_that(
  "passing additional functions to score binary works handles them", {
    test_fun <- function(x, y, ...) {
      if (hasArg("test")) {
        message("test argument found")
      }
      return(y)
    }

    df <- example_binary[model == "EuroCOVIDhub-ensemble" &
                           target_type == "Cases" & location == "DE"]

    # passing a simple function works
    expect_equal(
      score(df,
            metrics = list("identity" = function(x, y) {return(y)}))$identity,
      df$predicted
    )


    # passing an additional argument that is not part of the function
    # definition works
    expect_equal(
      score(df,
            metrics = list("identity" = function(x, y) {return(y)}),
            additional_arg = "something")$identity,
      df$predicted
    )

    # passing an additional function to one that accepts ... works
    expect_message(
      score(df,
            metrics = list("test_function" = test_fun),
            test = "something"),
      "test argument found"
    )

    # passing an argument that's the same as a named argument
    expect_equal(
      unique(
        score(df,
              metrics = list("test_function" = test_fun),
              y = "something")$test_function
      ),
      "something"
    )


    ## Additional tests for validate_metrics()
    # passing in something that's not a function or a known metric
    expect_warning(
      expect_warning(
        score(df, metrics = list(
          "test1" = test_fun, "test" = test_fun, "hi" = "hi", "2" = 3)
        ),
        "`Metrics` element number 3 is not a valid function"
      ),
      "`Metrics` element number 4 is not a valid function")

    # passing a single named argument for metrics by position
    expect_contains(
      names(score(df, list("hi" = test_fun))),
      "hi")

    # providing an additional, unrelated function argument works
    expect_no_error(
      score(example_binary, unnecessary_argument = "unnecessary")
    )
    expect_no_error(
      score(example_binary, metrics = list("brier_score" = brier_score),
            unnecessary_argument = "unnecessary")
    )
  }
)






# test point case --------------------------------------------------------------
test_that("function produces output for a point case", {
  point_example <- data.table::setDT(scoringutils::example_point)
  eval <- suppressMessages(score(point_example))
  eval <- summarise_scores(eval, by = c("model", "target_type"))

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
  expect_equal(
    colnames(eval),
    c("model", "target_type",names(metrics_point))
  )
})

test_that("Changing metrics names works", {
  metrics_test <- metrics_point
  names(metrics_test)[1] = "just_testing"
  eval <- suppressMessages(score(example_point, metrics = metrics_test))
  eval_summarised <- summarise_scores(eval, by = "model")
  expect_equal(
    colnames(eval_summarised),
    c("model", "just_testing", names(metrics_point)[-1])
  )
})


test_that("score.scoringutils_point() errors with only NA values", {
  only_nas <- copy(example_point)[, predicted := NA_real_]
  expect_error(
    score(only_nas),
    "After removing NA values in `observed` and `predicted`, there were no observations left"
  )
})

# test quantile case -----------------------------------------------------------
test_that("function produces output for a quantile format case", {
  quantile_example <- data.table::setDT(scoringutils::example_quantile)
  eval <- suppressMessages(score(quantile_example[!is.na(predicted)]))

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
})

test_that("score_quantile correctly handles separate results = FALSE", {
  quantile_example <- data.table::setDT(scoringutils::example_quantile)
  eval <- suppressMessages(
    score(
      quantile_example[!is.na(predicted)],
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
    observed = rep(1:10, each = 2),
    predicted = rep(c(-0.3, 0.3), 10) + rep(1:10, each = 2),
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
    score(scoringutils::example_quantile[!is.na(predicted)])
  )

  eval <- summarise_scores(eval,by = "model")

  example <- scoringutils::example_quantile
  ae <- example[quantile == 0.5, ae := abs(observed - predicted)][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval$ae_median), sort(ae))
})


test_that("all quantile and range formats yield the same result", {
  quantile_example1 <- data.table::setDT(scoringutils::example_quantile)

  eval1 <- suppressMessages(score(quantile_example1[!is.na(predicted)]))
  eval1 <- summarise_scores(eval1, by = "model")

  ae <- quantile_example1[
    quantile == 0.5, ae := abs(observed - predicted)][
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


test_that("score.scoringutils_quantile() errors with only NA values", {
  only_nas <- copy(example_quantile)[, predicted := NA_real_]
  expect_error(
    score(only_nas),
    "After removing NA values in `observed` and `predicted`, there were no observations left"
  )
})





# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {
  example <- data.table::setDT(scoringutils::example_continuous)
  eval <- suppressMessages(score(example[!is.na(predicted)]))

  eval2 <- suppressMessages(score(example))

  data.table::setcolorder(eval2, colnames(eval))
  eval <- eval[order(model)]
  eval2 <- eval2[order(model)]
  all(eval == eval2, na.rm = TRUE)

  only_nas <- copy(example_continuous)[, predicted := NA_real_]
  expect_error(
    score(only_nas),
    "After removing NA values in `observed` and `predicted`, there were no observations left"
  )

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )

  expect_equal(
    nrow(eval),
    887
  )
})

test_that("function throws an error if data is missing", {
  expect_error(suppressMessages(score(data = NULL)))
})

# test_that(
#   "score() can support a sample column when a quantile forecast is used", {
#   ex <- example_quantile[!is.na(quantile)][1:200, ]
#   ex <- rbind(
#     data.table::copy(ex)[, sample_id := 1],
#     ex[, sample_id := 2]
#   )
#   scores <- suppressWarnings(score(ex))
#   expect_snapshot(summarise_scores(
#     summarise_scores(scores, by = "model"), by = "model",
#     fun = signif, digits = 2
#   ))
#  })

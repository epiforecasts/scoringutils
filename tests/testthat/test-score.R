# =============================================================================
# new_scores()
# =============================================================================

test_that("new_scores() works", {
  expect_s3_class(
    new_scores(data.frame(), metrics = ""),
    c("scores", "data.table", "data.frame"),
    exact = TRUE
  )

  expect_error(
    new_scores(data.frame()),
    "missing, with no default"
  )
})

test_that("as_scores() works", {
  expect_equal(
    class(scoringutils:::as_scores(data.frame(wis = 1), metrics = "wis")),
    c("scores", "data.table", "data.frame")
  )
})

test_that("validate_scores() works", {
  expect_error(
    validate_scores(data.frame()),
    "Must inherit from class 'scores'"
  )
})

test_that("Output of `score()` has the class `scores()`", {
  expect_no_condition(validate_scores(scores_point))
  expect_no_condition(validate_scores(scores_binary))
  expect_no_condition(validate_scores(scores_sample_continuous))
  expect_no_condition(validate_scores(scores_quantile))
})

# =============================================================================
# score()
# =============================================================================

# common error handling --------------------------------------------------------
test_that("function throws an error if data is not a forecast object", {
  expect_error(
    score(forecast = NULL),
    "The input needs to be a valid forecast object."
  )
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
#   expect_warning(suppressMessages(score(forecast = data)))
# })

test_that("Manipulating scores objects with .[ works as expected", {
  expect_no_condition(scores_point[1:10])

  expect_no_condition(scores_point[, .(model, ae_point)])

  ex <- score(example_quantile)
  expect_no_condition(ex[, extra_col := "something"])
})


# test binary case -------------------------------------------------------------
test_that("function produces output for a binary case", {

  expect_equal(
    names(scores_binary),
    c(get_forecast_unit(example_binary), names(get_metrics(example_binary)))
  )

  eval <- summarise_scores(scores_binary, by = c("model", "target_type"))

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

  expect_true("brier_score" %in% names(eval))

  expect_s3_class(eval, c("scores", "data.table", "data.frame"), exact = TRUE)
})

test_that("score.forecast_binary() errors with only NA values", {
  # [.forecast()` will warn even before score()
  only_nas <- suppressWarnings(
    copy(example_binary)[, predicted := NA_real_]
  )
  expect_error(
    score(only_nas),
    "After removing rows with NA values in the data, no forecasts are left."
  )
})

test_that("score() gives same result for binary as regular function", {
  manual_eval <- brier_score(
    factor(example_binary$observed),
    example_binary$predicted
  )
  expect_equal(scores_binary$brier_score, manual_eval[!is.na(manual_eval)])
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
                           target_type == "Cases" & location == "DE"] %>%
      as_forecast_binary()

    # passing a simple function works
    expect_equal(
      score(df,
            metrics = list("identity" = function(x, y) {return(y)}))$identity,
      df$predicted
    )
  }
)

# test point case --------------------------------------------------------------
test_that("function produces output for a point case", {
  expect_equal(
    names(scores_binary),
    c(get_forecast_unit(example_binary), names(get_metrics(example_binary)))
  )

  eval <- summarise_scores(scores_point, by = c("model", "target_type"))

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
  expect_equal(
    colnames(eval),
    c("model", "target_type", names(get_metrics(example_point)))
  )

  expect_s3_class(eval, c("scores", "data.table", "data.frame"), exact = TRUE)
})

test_that("Changing metrics names works", {
  metrics_test <- get_metrics(example_point)
  names(metrics_test)[1] = "just_testing"
  eval <- suppressMessages(score(as_forecast_point(example_point),
                                 metrics = metrics_test))
  eval_summarised <- summarise_scores(eval, by = "model")
  expect_equal(
    colnames(eval_summarised),
    c("model", "just_testing", names(get_metrics(example_point))[-1])
  )
})


test_that("score.forecast_point() errors with only NA values", {
  # [.forecast()` will warn even before score()
  only_nas <- suppressWarnings(
    copy(example_point)[, predicted := NA_real_]
  )
  expect_error(
    score(only_nas),
    "After removing rows with NA values in the data, no forecasts are left."
  )
})

# test quantile case -----------------------------------------------------------
test_that("score_quantile correctly handles separate results = FALSE", {
  df <- example_quantile[model == "EuroCOVIDhub-ensemble" &
                           target_type == "Cases" & location == "DE"]
  metrics <- get_metrics(example_quantile)
  metrics$wis <- purrr::partial(wis, separate_results = FALSE)
  eval <- score(df[!is.na(predicted)], metrics = metrics)

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )
  expect_true(all(names(get_metrics(example_quantile)) %in% colnames(eval)))

  expect_s3_class(eval, c("scores", "data.table", "data.frame"), exact = TRUE)
})


test_that("score() quantile produces desired metrics", {
  data <- data.frame(
    observed = rep(1:10, each = 3),
    predicted = rep(c(-0.3, 0, 0.3), 10) + rep(1:10, each = 3),
    model = "Model 1",
    date = as.Date("2020-01-01") + rep(1:10, each = 3),
    quantile_level = rep(c(0.1, 0.5, 0.9), times = 10)
  )

  data <-suppressWarnings(suppressMessages(as_forecast_quantile(data)))

  out <- score(forecast = data, metrics = metrics_no_cov)
  metrics <- c(
    "dispersion", "underprediction", "overprediction",
    "bias", "ae_median"
  )

  expect_true(all(metrics %in% colnames(out)))
})


test_that("calculation of ae_median is correct for a quantile format case", {
  eval <- summarise_scores(scores_quantile,by = "model")

  example <- as.data.table(example_quantile)
  ae <- example[quantile_level == 0.5, ae := abs(observed - predicted)][!is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval$ae_median), sort(ae))
})


test_that("all quantile and range formats yield the same result", {
  eval1 <- summarise_scores(scores_quantile, by = "model")

  df <- as.data.table(example_quantile)

  ae <- df[
    quantile_level == 0.5, ae := abs(observed - predicted)][
    !is.na(model), .(mean = mean(ae, na.rm = TRUE)),
    by = "model"
  ]$mean

  expect_equal(sort(eval1$ae_median), sort(ae))
})

test_that("WIS is the same with other metrics omitted or included", {
  eval <- score(example_quantile,
    metrics = list("wis" = wis)
  )

  eval2 <- scores_quantile

  expect_equal(
    sum(eval$wis),
    sum(eval2$wis)
  )
})


test_that("score.forecast_quantile() errors with only NA values", {
  # [.forecast()` will warn even before score()
  only_nas <- suppressWarnings(
    copy(example_quantile)[, predicted := NA_real_]
  )
  expect_error(
    score(only_nas),
    "After removing rows with NA values in the data, no forecasts are left."
  )
})

test_that("score.forecast_quantile() works as expected in edge cases", {
  # only the median
  onlymedian <- example_quantile[quantile_level == 0.5]
  expect_no_condition(
    s <- score(onlymedian, metrics = get_metrics(
      example_quantile,
      exclude = c("interval_coverage_50", "interval_coverage_90")
    ))
  )
  expect_equal(
    s$wis, abs(onlymedian$observed - onlymedian$predicted)
  )

  # only one symmetric interval is present
  oneinterval <- example_quantile[quantile_level %in% c(0.25,0.75)] %>%
    as_forecast_quantile()
  expect_message(
    s <- score(
      oneinterval,
      metrics = get_metrics(
        example_quantile,
        exclude = c("interval_coverage_90", "ae_median")
      )
    ),
    "Median not available"
  )
})


# test integer and continuous case ---------------------------------------------
test_that("function produces output for a continuous format case", {

  eval <- scores_sample_continuous

  # [.forecast()` will warn even before score()
  only_nas <- suppressWarnings(
    copy(example_sample_continuous)[, predicted := NA_real_]
  )
  expect_error(
    score(as_forecast_sample(only_nas)),
    "After removing rows with NA values in the data, no forecasts are left."
  )

  expect_equal(
    nrow(eval) > 1,
    TRUE
  )

  expect_equal(
    nrow(eval),
    887
  )

  expect_s3_class(eval, c("scores", "data.table", "data.frame"), exact = TRUE)
})

test_that("function throws an error if data is missing", {
  expect_error(suppressMessages(score(forecast = NULL)))
})

test_that("score() works with only one sample", {

  # with only one sample, dss returns NaN and log_score fails
  onesample <- na.omit(example_sample_continuous)[sample_id == 20]
  expect_warning(
    scoreonesample <- score(onesample),
    "Computation for `log_score` failed. Error: need at least 2 data points."
  )

  # verify that all goes well with two samples
  twosample <- na.omit(example_sample_continuous)[sample_id %in% c(20, 21)]
  expect_no_condition(score(twosample))
})


# test nominal case ------------------------------------------------------------
test_that("function produces output for a nominal format case", {
  expect_no_condition(score(as_forecast_nominal(na.omit(example_nominal))))
})


# =============================================================================
# apply_metrics()
# =============================================================================

test_that("apply_metrics() works", {

  dt <- data.table::data.table(x = 1:10)
  scoringutils:::apply_metrics(
    forecast = dt, metrics = list("test" = function(x) x + 1),
    dt$x
  )
  expect_equal(dt$test, 2:11)

  # additional named argument works
  expect_no_condition(
    scoringutils:::apply_metrics(
      forecast = dt, metrics = list("test" = function(x) x + 1),
      dt$x, y = dt$test)
  )

  # additional unnamed argument does not work
  expect_warning(
    scoringutils:::apply_metrics(
      forecast = dt, metrics = list("test" = function(x) x + 1),
      dt$x, dt$test)
  )
})

# attributes
test_that("`[` preserves attributes", {
  test <- data.table::copy(scores_binary)
  class(test) <- c("scores", "data.frame")
  expect_true("metrics" %in% names(attributes(test)))
  expect_true("metrics" %in% names(attributes(test[1:9])))
})


# =============================================================================
# validate_scores()
# =============================================================================
test_that("validate_scores() works", {
  expect_no_condition(validate_scores(scores_binary))
  expect_null(
    validate_scores(scores_binary),
  )
})

# ==============================================================================
# score.forecast_quantile()
# ==============================================================================
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



test_that("score() works even if only some quantiles are missing", {

  # only the median is there
  onlymedian <- example_quantile[quantile_level == 0.5]
  expect_no_condition(
    score(onlymedian, metrics = get_metrics(
      example_quantile,
      exclude = c("interval_coverage_50", "interval_coverage_90")
    ))
  )


  # asymmetric intervals
  asymm <- example_quantile[!quantile_level > 0.6]
  expect_warning(
    expect_warning(
      score_a <- score(asymm) %>% summarise_scores(by = "model"),
      "Computation for `interval_coverage_50` failed."
    ),
    "Computation for `interval_coverage_90` failed."
  )

  # check that the result is equal to a case where we discard the entire
  # interval in terms of WIS
  inner <- example_quantile[quantile_level %in% c(0.4, 0.45, 0.5, 0.55, 0.6)]
  score_b <- score(inner, get_metrics(
    inner, exclude = c("interval_coverage_50", "interval_coverage_90")
  )) %>%
    summarise_scores(by = "model")
  expect_equal(
    score_a$wis,
    score_b$wis
  )

  # median is not there, but only in a single model
  test <- data.table::copy(example_quantile)
  test_no_median <- test[model == "epiforecasts-EpiNow2" & !(quantile_level %in% c(0.5)), ]
  test <- rbind(test[model != "epiforecasts-EpiNow2"], test_no_median)

  test <- suppressWarnings(as_forecast_quantile(test))
  expect_message(
    expect_warning(
      score(test),
      "Computation for `ae_median` failed."
    ),
    "interpolating median from the two innermost quantiles"
  )
})

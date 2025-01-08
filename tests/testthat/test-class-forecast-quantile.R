# ==============================================================================
# as_forecast_quantile()
# ==============================================================================

test_that("as_forecast_quantile() works as expected", {
  test <- na.omit(data.table::copy(example_quantile))

  expect_s3_class(
    as_forecast_quantile(test),
    c("forecast_quantile", "forecast", "data.table", "data.frame"),
    exact = TRUE
  )

  # expect error when arguments are not correct
  expect_error(as_forecast_quantile(test, observed = 3), "Must be of type 'character'")
  expect_error(as_forecast_quantile(test, quantile_level = c("1", "2")), "Must have length 1")
  expect_error(as_forecast_quantile(test, observed = "missing"), "Must be a subset of")

  # expect no condition with columns already present
  expect_no_condition(
    as_forecast_quantile(test,
      observed = "observed", predicted = "predicted",
      forecast_unit = c(
        "location", "model", "target_type",
        "target_end_date", "horizon"
      ),
      quantile_level = "quantile_level"
    )
  )
})

test_that("as_forecast_quantile() function works", {
  check <- suppressMessages(as_forecast_quantile(example_quantile))
  expect_s3_class(check, "forecast_quantile")
})

test_that("as_forecast_quantile() errors if there is both a sample_id and a quantile_level column", {
  example <- as.data.table(example_quantile)[, sample_id := 1]
  expect_error(
    as_forecast_quantile(example),
    "Found columns `quantile_level` and `sample_id`. Only one of these is allowed"
  )
})

test_that("as_forecast_quantile() warns if there are different numbers of quantiles", {
  example <- as.data.table(example_quantile)[-1000, ]
  expect_warning(
    w <- as_forecast_quantile(na.omit(example)),
    "Some forecasts have different numbers of rows"
  )
  # printing should work without a warning because printing is silent
  expect_no_condition(w)
})

test_that("as_forecast_quantile() function throws an error with duplicate forecasts", {
  example <- rbind(
    example_quantile,
    example_quantile[1000:1010]
  )

  expect_error(
    suppressMessages(suppressWarnings(as_forecast_quantile(example))),
    "Assertion on 'data' failed: There are instances with more than one forecast for the same target. This can't be right and needs to be resolved. Maybe you need to check the unit of a single forecast and add missing columns? Use the function get_duplicate_forecasts() to identify duplicate rows.", # nolint
    fixed = TRUE
  )
})

test_that("as_forecast_quantile() function throws an error when no predictions or observed values are present", {
  expect_error(
    suppressMessages(suppressWarnings(as_forecast_quantile(
      data.table::copy(example_quantile)[, predicted := NULL]
    ))),
    "Assertion on 'data' failed: Column 'predicted' not found in data."
  )

  expect_error(
    suppressMessages(suppressWarnings(as_forecast_quantile(
      data.table::copy(example_quantile)[, observed := NULL]
    ))),
    "Assertion on 'data' failed: Column 'observed' not found in data."
  )

  expect_error(
    suppressMessages(suppressWarnings(as_forecast_quantile(
      data.table::copy(example_quantile)[, c("observed", "predicted") := NULL]
    ))),
    "Assertion on 'data' failed: Columns 'observed', 'predicted' not found in data."
  )
})

test_that("as_forecast_quantile() works with a data.frame", {
  expect_no_condition(as_forecast_quantile(example_quantile_df))
})

test_that("as_forecast_quantiles works", {
  samples <- data.frame(
    date = as.Date("2020-01-01") + 1:10,
    model = "model1",
    observed = 1:10,
    predicted = c(rep(0, 10), 2:11, 3:12, 4:13, rep(100, 10)),
    sample_id = rep(1:5, each = 10)
  ) %>%
    as_forecast_sample()

  quantile <- data.frame(
    date = rep(as.Date("2020-01-01") + 1:10, each = 2),
    model = "model1",
    observed = rep(1:10, each = 2),
    quantile_level = c(0.25, 0.75),
    predicted = rep(2:11, each = 2) + c(0, 2)
  )

  expect_no_condition(
    as_forecast_quantile(samples, probs = c(0.25, 0.75))
  )

  wrongclass <- as_forecast_sample(samples)
  class(wrongclass) <- c("forecast_point", "data.table", "data.frame")
  expect_error(
    as_forecast_quantile(wrongclass, quantile_level = c(0.25, 0.75)),
    "Assertion on 'quantile_level' failed: Must be of type"
  )


  quantile2 <- as_forecast_quantile(
    as_forecast_sample(samples),
    probs = c(0.25, 0.75)
  )

  expect_equal(quantile, as.data.frame(quantile2))

  # Verify that `type` is correctly scoped in as_forecast_quantile(), as it is
  # also an argument.
  # If it's not scoped well, the call to `as_forecast_quantile()` will fail.
  samples$type <- "test"

  quantile3 <- as_forecast_quantile(
    as_forecast_sample(samples),
    probs = c(0.25, 0.75)
  )
  quantile3$type <- NULL

  expect_identical(
    quantile2,
    quantile3
  )
})

test_that("as_forecast_quantiles issue 557 fix", {
  out <- example_sample_discrete %>%
    na.omit() %>%
    as_forecast_quantile(
      probs = c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
    ) %>%
    score()

  expect_equal(any(is.na(out$interval_coverage_deviation)), FALSE)
})

test_that("as_forecast_quantile doesn't modify column names in place", {
  quantile_data <- data.table(
    my_quantile = c(0.25, 0.5),
    forecast_value = c(1, 2),
    observed_value = c(5, 5)
  )
  pre <- names(quantile_data)

  quantile_forecast <- quantile_data %>%
    as_forecast_quantile(
      predicted = "forecast_value",
      observed = "observed_value",
      quantile_level = "my_quantile"
    )

  post <- names(quantile_data)
  expect_equal(pre, post)
})


# ==============================================================================
# is_forecast_quantile()
# ==============================================================================
test_that("is_forecast_quantile() works as expected", {
  expect_true(is_forecast_quantile(example_quantile))
  expect_false(is_forecast_quantile(example_binary))
  expect_false(is_forecast_quantile(example_point))
  expect_false(is_forecast_quantile(example_sample_continuous))
  expect_false(is_forecast_quantile(example_nominal))
})

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
  metrics <- get_metrics(
    example_quantile,
    exclude = c("overprediction", "underprediction", "dispersion")
  )
  metrics$wis <- purrr::partial(wis, na.rm = TRUE)
  expect_warning(
    expect_warning(
      score_a <- score(asymm, metrics = metrics) %>%
        summarise_scores(by = "model"),
      "Computation for `interval_coverage_50` failed."
    ),
    "Computation for `interval_coverage_90` failed."
  )

  # expect a failure with the regular wis wihtout ma.rm=TRUE
  expect_warning(
    score(asymm, metrics = c(wis = wis)),
    "Not all quantile levels specified form symmetric prediction intervals."
  )

  # check that the result is equal to a case where we discard the entire
  # interval in terms of WIS
  inner <- example_quantile[quantile_level %in% c(0.4, 0.45, 0.5, 0.55, 0.6)]
  score_b <- score(inner, metrics = c(wis = wis)) %>%
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

# ==============================================================================
# get_metrics.forecast_quantile()
# ==============================================================================
test_that("get_metrics.forecast_quantile() works as expected", {
  expect_true(
    is.list(get_metrics(example_quantile))
  )
})


# ==============================================================================
# get_pit_histogram.forecast_quantile()
# ==============================================================================
test_that("get_pit_histogram.forecast_quantile() works as expected", {
  pit_quantile <- get_pit_histogram(example_quantile, by = "model")

  expect_equal(names(pit_quantile), c("model", "density", "bin", "mid"))
  expect_s3_class(pit_quantile, c("data.table", "data.frame"), exact = TRUE)

  # check printing works
  expect_output(print(pit_quantile))
})

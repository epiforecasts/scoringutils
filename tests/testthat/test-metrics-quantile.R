observed <- c(1, -15, 22)
predicted <- rbind(
  c(-1, 0, 1, 2, 3),
  c(-2, 1, 2, 2, 4),
  c(-2, 0, 3, 3, 4)
)
quantile_level <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# covidHubUtils test:
y <- c(1, -15, 22)
forecast_quantiles_matrix <- rbind(
  c(-1, 0, 1, 2, 3),
  c(-2, 1, 2, 2, 4),
  c(-2, 0, 3, 3, 4)
)
forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)


# ==============================================================================
# check_input_quantile()
# ==============================================================================
test_that("check_input_quantile() works as expected", {
  # expect no error if dimensions are ok
  expect_true(
    check_input_quantile(
      1:10, matrix(1:20, nrow = 10),
      quantile_level = c(0.1, 0.9)
    )
  )

  # expect error if dimensions are not ok
  expect_match(
    check_input_quantile(
      1:10, matrix(1:20, nrow = 10),
      quantile_level = seq(0.1, 0.9, length.out = 8)
    ),
    "Assertion on 'predicted' failed: Must have exactly 8 cols, but has 2 cols."
  )
})

# ============================================================================ #
# Input handling ===============================================================
# ============================================================================ #
test_that("Input checking for quantile forecasts works", {
  # everything correct
  expect_no_condition(
    scoringutils:::assert_input_quantile(observed, predicted, quantile_level)
  )

  # quantile_level > 1
  expect_error(
    scoringutils:::assert_input_quantile(observed, predicted, quantile_level + 1),
    "Assertion on 'quantile_level' failed: Element 1 is not <= 1."
  )

  # quantile_level < 0
  expect_error(
    scoringutils:::assert_input_quantile(observed, predicted, quantile_level - 1),
    "Assertion on 'quantile_level' failed: Element 1 is not >= 0."
  )

  # 10 observations, but only 3 forecasts
  expect_error(
    scoringutils:::assert_input_quantile(1:10, predicted, quantile_level),
    "Assertion on 'predicted' failed: Must have exactly 10 rows, but has 3 rows."
  )

  # observed value is a factor
  expect_error(
    scoringutils:::assert_input_quantile(factor(1:10), predicted, quantile_level),
    "Assertion on 'observed' failed: Must be of type 'numeric', not 'factor'."
  )

  # observed is a single number and does not have the same length as predicted
  # There seems to be an issue with the error message: there is one \n to many
  # such that the test fails when executed alone, but works when executed
  # together with others.
  expect_error(
    scoringutils:::assert_input_quantile(1, predicted, quantile_level),
    "Assertion failed. One of the following must apply:\n * check_numeric_vector(predicted): Must be of type 'atomic vector',\n * not 'matrix'\n * check_matrix(predicted): Must have exactly 1 rows, but has 3 rows",
    fixed = TRUE
  )

  # predicted is a vector
  expect_error(
    scoringutils:::assert_input_quantile(observed, as.vector(predicted), quantile_level),
    "Assertion on 'predicted' failed: Must be of type 'matrix', not 'double'."
  )
})


# ============================================================================ #
# wis ==========================================================================
# ============================================================================ #a
test_that("wis works standalone, median only", {
  y <- c(1, -15, 22)
  lower <- upper <- predicted_quantile <- c(1, 2, 3)
  quantile_probs <- 0.5

  actual <- interval_score(y,
                           lower = lower, upper = upper,
                           weigh = TRUE,
                           interval_range = 0
  )

  actual_wis <- wis(
    observed = y,
    predicted = matrix(predicted_quantile),
    quantile_level = quantile_probs,
  )

  expected <- abs(y - lower)

  expect_identical(actual, expected)
})

test_that("`wis()` works within score for median forecast", {
  test_data <- data.frame(
    observed = c(1, -15, 22),
    predicted = 1:3,
    quantile_level = rep(c(0.5), each = 3),
    model = "model1",
    date = 1:3
  ) %>%
    as_forecast_quantile()

  metrics <- get_metrics(example_quantile) %>%
    select_metrics(select = c("ae_median", "wis"))
  metrics$wis <- purrr::partial(metrics$wis, count_median_twice = TRUE)

  eval <- score(test_data, metrics = metrics)
  expect_equal(eval$ae_median, eval$wis)
})

test_that("`wis()` equals `interval_score()`, 1 interval only", {
  y <- c(1, -15, 22)
  lower <- c(0, 1, 0)
  upper <- c(2, 2, 3)
  quantile_probs <- c(0.25, 0.75)
  predicted <- matrix(c(lower, upper), ncol = 2)

  alpha <- 0.5
  expected <- (upper - lower) * (alpha / 2) + c(0, 1 - (-15), 22 - 3)

  actual <- interval_score(
    y,
    lower = lower, upper = upper,
    weigh = TRUE,
    interval_range = 50
  )

  actual_wis <- wis(
    observed = y,
    predicted = predicted,
    quantile_level = quantile_probs,
  )

  expect_identical(actual, expected)
  expect_identical(actual_wis, expected)
})

test_that("wis() works within score for one interval", {
  test_data <- data.frame(
    observed = rep(c(1, -15, 22), times = 2),
    quantile_level = rep(c(0.25, 0.75), each = 3),
    predicted = c(c(0, 1, 0), c(2, 2, 3)),
    model = c("model1"),
    date = rep(1:3, times = 2)
  ) %>%
    as_forecast_quantile()

  metrics <- get_metrics(example_quantile) %>%
    select_metrics(select = c("wis"))
  metrics$wis <- purrr::partial(metrics$wis, count_median_twice = TRUE)

  eval <- score(test_data, metrics = metrics)

  eval <- summarise_scores(eval, by = c("model", "date"))

  lower <- c(0, 1, 0)
  upper <- c(2, 2, 3)
  alpha <- 0.5

  expected <- (upper - lower) * (alpha / 2) + c(0, 1 - (-15), 22 - 3)

  expect_equal(expected, eval$wis)
})

test_that("`wis()` works 1 interval and median", {
  test_data <- data.frame(
    observed = rep(c(1, -15, 22), times = 3),
    quantile_level = rep(c(0.25, 0.5, 0.75), each = 3),
    predicted = c(c(0, 1, 0), c(1, 2, 3), c(2, 2, 3)),
    model = c("model1"),
    date = rep(1:3, times = 3)
  ) %>%
    as_forecast_quantile()

  metrics <- get_metrics(example_quantile, select = c("wis"))
  metrics$wis <- purrr::partial(metrics$wis, count_median_twice = TRUE)

  eval <- score(test_data, metrics = metrics)

  eval <- summarise_scores(eval, by = c("model", "date"))

  y <- c(1, -15, 22)
  quantile <- rbind(c(0, 1, 2), c(1, 2, 2), c(0, 3, 3))
  quantile_probs <- c(0.25, 0.5, 0.75)

  alpha <- 0.5
  expected <- 0.5 * (
    abs(y - quantile[, 2]) +
      (quantile[, 3] - quantile[, 1]) * (alpha / 2) + c(0, 1 - (-15), 22 - 3)
  )

  actual_wis <- wis(
    observed = y,
    predicted = quantile,
    quantile_level = quantile_probs,
    count_median_twice = TRUE
  )

  expect_identical(eval$wis, expected)
  expect_identical(actual_wis, expected)
})

test_that("wis works, 2 intervals and median", {
  test_data <- data.frame(
    observed = rep(c(1, -15, 22), times = 5),
    quantile_level = rep(c(0.1, 0.25, 0.5, 0.75, 0.9), each = 3),
    predicted = c(
      c(-1, -2, -2), c(0, 1, 0), c(1, 2, 3),
      c(2, 2, 3), c(3, 4, 4)
    ),
    model = c("model1"),
    date = rep(1:3, times = 5)
  ) %>%
    as_forecast_quantile()

  metrics <- get_metrics(example_quantile, select = c("wis"))
  metrics$wis <- purrr::partial(metrics$wis, count_median_twice = TRUE)

  eval <- score(test_data, metrics = metrics)

  eval <- summarise_scores(eval, by = c("model", "date"))

  quantile <- forecast_quantiles_matrix
  quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  alpha1 <- 0.2
  alpha2 <- 0.5

  expected <- (1 / 3) * (
    abs(y - quantile[, 3]) +
      (quantile[, 5] - quantile[, 1]) * (alpha1 / 2) + c(0, (-2) - (-15), 22 - 4) +
      (quantile[, 4] - quantile[, 2]) * (alpha2 / 2) + c(0, 1 - (-15), 22 - 3)
  )

  actual_wis <- wis(
    observed = y,
    predicted = quantile,
    quantile_level = quantile_probs,
    count_median_twice = TRUE
  )

  expect_equal(
    as.numeric(eval$wis),
    as.numeric(expected)
  )
  expect_identical(actual_wis, expected)
})

# additional tests from the covidhubutils repo
test_that("wis is correct, median only - test corresponds to covidHubUtils", {
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]

  target_end_dates <- as.Date("2020-01-01") + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(as.Date("2020-01-01"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile_level = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  # make a version that conforms to scoringutils format
  truth_formatted <- data.table::as.data.table(test_truth)
  truth_formatted[, `:=`(model = NULL)]
  data.table::setnames(truth_formatted, old = "value", new = "observed")

  forecasts_formated <- data.table::as.data.table(test_forecasts)
  data.table::setnames(forecasts_formated, old = "value", new = "predicted")

  data_formatted <- merge(forecasts_formated, truth_formatted)

  metrics <- get_metrics(example_quantile, select = c("wis"))
  metrics$wis <- purrr::partial(metrics$wis, count_median_twice = TRUE)

  eval <- score(
    as_forecast_quantile(data_formatted), metrics = metrics
  )

  expected <- abs(y - forecast_quantiles_matrix[, 1])

  actual_wis <- wis(
    observed = y,
    predicted = matrix(forecast_quantiles_matrix),
    quantile_level = 0.5,
    count_median_twice = FALSE
  )

  expect_equal(eval$wis, expected)
  expect_equal(actual_wis, expected)
})

test_that("wis is correct, 1 interval only - test corresponds to covidHubUtils", {
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, c(1, 5), drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[c(1, 5)]

  target_end_dates <- as.Date("2020-01-01") + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(as.Date("2020-01-01"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile_level = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  # make a version that conforms to scoringutils format
  truth_formatted <- data.table::as.data.table(test_truth)
  truth_formatted[, `:=`(model = NULL)]
  data.table::setnames(truth_formatted, old = "value", new = "observed")

  forecasts_formated <- data.table::as.data.table(test_forecasts)
  data.table::setnames(forecasts_formated, old = "value", new = "predicted")

  data_formatted <- merge(forecasts_formated, truth_formatted) %>%
    as_forecast_quantile()

  metrics <- get_metrics(example_quantile, select = c("wis"))

  eval <- suppressMessages(score(data_formatted, metrics = metrics))

  eval <- summarise_scores(eval,
                           by = c(
                             "model", "location", "target_variable",
                             "target_end_date", "forecast_date", "horizon"
                           )
  )

  alpha1 <- 0.2
  expected <- (forecast_quantiles_matrix[, 2] - forecast_quantiles_matrix[, 1]) * (alpha1 / 2) +
    c(0, (-2) - (-15), 22 - 4)

  actual_wis <- wis(
    observed = y,
    predicted = forecast_quantiles_matrix,
    quantile_level = c(0.1, 0.9)
  )

  expect_equal(eval$wis, expected)
  expect_equal(actual_wis, expected)
})

test_that("wis is correct, 2 intervals and median - test corresponds to covidHubUtils", {
  target_end_dates <- as.Date("2020-01-01") + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(as.Date("2020-01-01"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile_level = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  # make a version that conforms to scoringutils format
  truth_formatted <- data.table::as.data.table(test_truth)
  truth_formatted[, `:=`(model = NULL)]
  data.table::setnames(truth_formatted, old = "value", new = "observed")

  forecasts_formated <- data.table::as.data.table(test_forecasts)
  data.table::setnames(forecasts_formated, old = "value", new = "predicted")

  data_formatted <- merge(forecasts_formated, truth_formatted) %>%
    as_forecast_quantile()

  eval <- score(data_formatted, metrics = metrics_no_cov)

  eval <- summarise_scores(eval,
                           by = c(
                             "model", "location", "target_variable",
                             "target_end_date", "forecast_date", "horizon"
                           )
  )

  alpha1 <- 0.2
  alpha2 <- 0.5
  expected <- (1 / 2.5) * (
    0.5 * abs(y - forecast_quantiles_matrix[, 3]) +
      (forecast_quantiles_matrix[, 5] - forecast_quantiles_matrix[, 1]) * (alpha1 / 2) + c(0, (-2) - (-15), 22 - 4) +
      (forecast_quantiles_matrix[, 4] - forecast_quantiles_matrix[, 2]) * (alpha2 / 2) + c(0, 1 - (-15), 22 - 3)
  )

  actual_wis <- wis(
    observed = y,
    predicted = forecast_quantiles_matrix,
    quantile_level = c(0.1, 0.25, 0.5, 0.75, 0.9),
    count_median_twice = FALSE
  )
  expect_equal(eval$wis, expected)
  expect_equal(actual_wis, expected)

  # check whether `count_median_twice = TRUE` also works
  expected2 <- (1 / 3) * (
    abs(y - forecast_quantiles_matrix[, 3]) +
      (forecast_quantiles_matrix[, 5] - forecast_quantiles_matrix[, 1]) * (alpha1 / 2) + c(0, (-2) - (-15), 22 - 4) +
      (forecast_quantiles_matrix[, 4] - forecast_quantiles_matrix[, 2]) * (alpha2 / 2) + c(0, 1 - (-15), 22 - 3)
  )

  actual_wis2 <- wis(
    observed = y,
    predicted = forecast_quantiles_matrix,
    quantile_level = c(0.1, 0.25, 0.5, 0.75, 0.9),
    count_median_twice = TRUE
  )

  metrics <- get_metrics(example_quantile, "wis")
  metrics$wis <- purrr::partial(wis, count_median_twice = TRUE)
  eval2 <- eval <- score(data_formatted, metrics = metrics)
  eval2 <- summarise_scores(eval2,
                           by = c(
                             "model", "location", "target_variable",
                             "target_end_date", "forecast_date", "horizon"
                           )
  )
  expect_equal(eval2$wis, expected2)
  expect_equal(actual_wis2, expected2)
})

test_that("Quantlie score and interval score yield the same result, weigh = FALSE", {

  # calling quantile_score and wis should return the same result if all
  # quantiles form central prediction intervals
  expect_equal(
    quantile_score(observed, predicted, quantile_level),
    wis(observed, predicted, quantile_level)
  )

  observed <- rnorm(10, mean = 1:10)
  alphas <- c(0.1, 0.5, 0.9)

  for (alpha in alphas) {
    lower <- qnorm(alpha / 2, rnorm(10, mean = 1:10))
    upper <- qnorm((1 - alpha / 2), rnorm(10, mean = 11:20))

    w <- FALSE
    is <- interval_score(
      observed = observed,
      lower = lower,
      upper = upper,
      interval_range = (1 - alpha) * 100,
      weigh = w
    )

    wis <- wis(
      observed = observed,
      predicted = cbind(lower, upper),
      quantile_level = c(alpha / 2, 1 - alpha / 2),
      count_median_twice = FALSE,
      weigh = w
    )

    qs_lower <- quantile_score(observed,
                               predicted = matrix(lower),
                               quantile_level = alpha / 2,
                               weigh = w
    )
    qs_upper <- quantile_score(observed,
                               predicted = matrix(upper),
                               quantile_level = 1 - alpha / 2,
                               weigh = w
    )
    expect_equal((qs_lower + qs_upper) / 2, is)
    expect_equal(wis, is)
  }
})


test_that("wis errors when there are no valid forecast intervals", {
  # test that wis throws an error when there are no valid forecast intervals
  # (i.e. the quantiles do not form central prediction intervals)
  observed <- 0.1
  predicted <- c(0.1032978, 0.1078166, 0.1906849, 0.1969254, 0.2017249, 0.2078487, 0.2810377, 0.3062168, 0.4006721, 0.8219655)
  quantile_level <- c(0.600, 0.650, 0.700, 0.750, 0.800, 0.850, 0.900, 0.950, 0.975, 0.990)

  expect_error(
    wis(observed, predicted, quantile_level),
    "No valid forecast intervals found."
  )
})


# ============================================================================ #
# Quantile score ============================================================= #
# ============================================================================ #
test_that("Quantlie score and interval score yield the same result, weigh = TRUE", {
  observed <- rnorm(10, mean = 1:10)
  alphas <- c(0.1, 0.5, 0.9)

  for (alpha in alphas) {
    lower <- qnorm(alpha / 2, rnorm(10, mean = 1:10))
    upper <- qnorm((1 - alpha / 2), rnorm(10, mean = 11:20))

    w <- TRUE
    is <- interval_score(
      observed = observed,
      lower = lower,
      upper = upper,
      interval_range = (1 - alpha) * 100,
      weigh = w
    )

    wis <- wis(
      observed = observed,
      predicted = cbind(lower, upper),
      quantile_level = c(alpha / 2, 1 - alpha / 2),
      count_median_twice = FALSE,
      weigh = w
    )

    qs_lower <- quantile_score(observed,
                               predicted = matrix(lower),
                               quantile_level = alpha / 2,
                               weigh = w
    )
    qs_upper <- quantile_score(observed,
                               predicted = matrix(upper),
                               quantile_level = 1 - alpha / 2,
                               weigh = w
    )
    expect_equal((qs_lower + qs_upper) / 2, is)
    expect_equal(wis, is)
  }
})

test_that("wis works with separate results", {
  wis <- wis(
    observed = y,
    predicted = forecast_quantiles_matrix,
    quantile_level = forecast_quantile_probs,
    separate_results = TRUE
  )
  expect_equal(wis$wis, wis$dispersion + wis$overprediction + wis$underprediction)
})


# ============================================================================ #
# overprediction, underprediction, dispersion ================================ #
# ============================================================================ #
test_that("wis is the sum of overprediction, underprediction, dispersion", {
  wis <- wis(
    observed = y,
    predicted = forecast_quantiles_matrix,
    quantile_level = forecast_quantile_probs
  )

  d <- dispersion_quantile(y, forecast_quantiles_matrix, forecast_quantile_probs)
  o <- overprediction_quantile(y, forecast_quantiles_matrix, forecast_quantile_probs)
  u <- underprediction_quantile(y, forecast_quantiles_matrix, forecast_quantile_probs)

  expect_equal(wis, d + o + u)
})


# ============================================================================ #
# `interval_coverage` =============================================== #
# ============================================================================ #
test_that("interval_coverage works", {
  expect_equal(
    interval_coverage(observed, predicted, quantile_level, interval_range = 50),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("interval_coverage rejects wrong inputs", {
  expect_error(
    interval_coverage(observed, predicted, quantile_level, interval_range = c(50, 0)),
    "Assertion on 'interval_range' failed: Must have length 1."
  )
})

test_that("interval_coverage_quantile throws a warning when a required quantile is not available", {
  dropped_quantile_pred <- predicted[, -4]
  dropped_quantiles <- quantile_level[-4]
  expect_error(
    interval_coverage(
      observed, dropped_quantile_pred, dropped_quantiles, interval_range = 50
    ),
    paste(
      "To compute the interval coverage for an interval range of \"50%\",",
      "the 0.25 and 0.75 quantiles are required"
    )
  )
})


# ============================================================================ #
# `bias_quantile` ============================================================ #
# ============================================================================ #
test_that("bias_quantile() works as expected", {
  predicted <- c(1, 2, 3)
  quantiles <- c(0.1, 0.5, 0.9)
  expect_equal(
    bias_quantile(observed = 2, predicted, quantiles),
    0
  )
  predicted <- c(0, 1, 2)
  quantiles <- c(0.1, 0.5, 0.9)
  expect_equal(
    bias_quantile(observed = 2, predicted, quantiles),
    -0.8
  )

  predicted <- c(
    705.500, 1127.000, 4006.250, 4341.500, 4709.000, 4821.996,
    5340.500, 5451.000, 5703.500, 6087.014, 6329.500, 6341.000,
    6352.500, 6594.986, 6978.500, 7231.000, 7341.500, 7860.004,
    7973.000, 8340.500, 8675.750, 11555.000, 11976.500
  )

  quantile_level <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)

  observed <- 8062
  expect_equal(bias_quantile(observed, predicted, quantile_level), -0.8)
})

test_that("bias_quantile handles matrix input", {
  observed <- seq(10, 0, length.out = 4)
  predicted <- matrix(1:12, ncol = 3)
  quantiles <- c(0.1, 0.5, 0.9)
  expect_equal(
    bias_quantile(observed, predicted, quantiles),
    c(-1.0, -0.8,  0.8,  1.0)
  )
})


test_that("bias_quantile() handles vector that is too long", {
  predicted <- c(NA, 1, 2, 3)
  quantiles <- c(0.1, 0.5, 0.9)

  expect_error(
    bias_quantile(observed = 2, predicted, quantiles),
    "Assertion on 'quantile_level' failed: Must have length 4, but has length 3."
  )
})

test_that("bias_quantile() handles NA values", {
  predicted <- c(NA, 1, 2)
  quantiles <- c(0.1, 0.5, 0.9)
  expect_equal(
    bias_quantile(observed = 2, predicted, quantiles),
    -0.8
  )
  predicted <- c(0, 1, 2)
  quantiles <- c(0.1, 0.5, NA)
  expect_equal(
    bias_quantile(observed = 2, predicted, quantiles),
    -1
  )
  expect_equal(
    bias_quantile(observed = 2, predicted, quantiles, na.rm = FALSE),
    NA_real_
  )
})

test_that("bias_quantile() errors if no predictions", {
  expect_error(
    bias_quantile(observed = 2, numeric(0), numeric(0)),
    "Assertion on 'quantile_level' failed: Must have length >= 1, but has length 0"
  )
})

test_that("bias_quantile() returns correct bias if value below the median", {
  predicted <- c(1, 2, 4, 5)
  quantiles <- c(0.1, 0.3, 0.7, 0.9)
  suppressMessages(
    expect_equal(bias_quantile(observed = 1, predicted, quantiles), 0.8)
  )
})

test_that("bias_quantile() returns correct bias if value above median", {
  predicted <- c(1, 2, 4, 5)
  quantiles <- c(0.1, 0.3, 0.7, 0.9)
  suppressMessages(
    expect_equal(bias_quantile(observed = 5, predicted, quantiles), -0.8)
  )
})

test_that("bias_quantile() returns correct bias if value at the median", {
  predicted <- c(1, 2, 3, 4)
  quantiles <- c(0.1, 0.3, 0.5, 0.7)

  expect_equal(bias_quantile(observed = 3, predicted, quantiles), 0)
})

test_that("bias_quantile() returns 1 if true value below min prediction", {
  predicted <- c(2, 3, 4, 5)
  quantiles <- c(0.1, 0.3, 0.7, 0.9)

  suppressMessages(
    expect_equal(bias_quantile(observed = 1, predicted, quantiles), 1)
  )
})

test_that("bias_quantile() returns -1 if true value above max prediction", {
  predicted <- c(1, 2, 3, 4)
  quantiles <- c(0.1, 0.3, 0.5, 0.7)

  expect_equal(bias_quantile(observed = 6, predicted, quantiles), -1)
})

test_that("bias_quantile(): quantiles must be between 0 and 1", {
  predicted <- 1:4

  # Failing example
  quantiles <- c(-0.1, 0.3, 0.5, 0.8)
  expect_error(
    bias_quantile(observed = 3, predicted, quantiles),
    "Assertion on 'quantile_level' failed: Element 1 is not >= 0."
  )

  # Passing counter example
  quantiles <- c(0.1, 0.3, 0.5, 0.8)
  expect_silent(bias_quantile(observed = 3, predicted, quantiles))
})

test_that("bias_quantile(): quantiles must be increasing", {
  predicted <- 1:4

  # Failing example
  quantiles <- c(0.8, 0.3, 0.5, 0.9)
  expect_error(
    bias_quantile(observed = 3, predicted, quantiles),
    "Predictions must not be decreasing with increasing quantile level"
  )

  # Passing counter example
  quantiles <- c(0.3, 0.5, 0.8, 0.9)
  expect_silent(bias_quantile(observed = 3, predicted, quantiles))
})

test_that("bias_quantile(): predictions must be increasing", {
  predicted <- c(1, 2, 4, 3)
  quantiles <- c(0.1, 0.3, 0.5, 0.9)

  expect_error(
    bias_quantile(observed = 3, predicted, quantiles),
    "Predictions must not be decreasing with increasing quantile level"
  )
  expect_silent(bias_quantile( observed = 3, 1:4, quantiles))
})

test_that("bias_quantile(): quantile levels must be unique", {
  predicted <- 1:4

  # Failing example
  quantiles <- c(0.3, 0.3, 0.5, 0.8)
  expect_error(
    bias_quantile(observed = 3, predicted, quantiles),
    "Assertion on 'quantile_level' failed: Contains duplicated values, position 2."
  )

  # Passing example
  quantiles <- c(0.3, 0.5, 0.8, 0.9)
  expect_silent(bias_quantile(observed = 3, predicted, quantiles))
})

test_that("bias_quantile only produces one message", {
  expect_message(
    bias_quantile(observed, predicted[, -3], quantile_level[-3]),
    "Median not available, interpolating median from the two innermost quantiles in order to compute bias."
  )
})

test_that("bias_quantile() works with point forecasts", {
  predicted <- 1
  observed <- 1
  quantile_level <- 0.5
  expect_equal(bias_quantile(observed, predicted, quantile_level), 0)
})


test_that("bias_quantile() handles cases where median is not available", {
  predicted <- c(1, 10)
  observed <- 15
  quantile_level <- c(0.2, 0.4)

  expect_error(
    bias_quantile(observed, predicted, quantile_level),
    "Assertion on 'quantile_level\\[quantile_leve\\l >= 0.5]' failed: Must have length >= 1, but has length 0."
  )
})

# `interpolate_median` ======================================================= #
test_that("interpolation in `interpolate_median` works", {
  predicted <- c(1, 10)
  observed <- 15
  quantile_level <- c(0.4, 0.6)

  # median is missing, symmetric quantile levels given
  expect_equal(interpolate_median(predicted, quantile_level), mean(predicted))
  quantile_level <- c(0.1, 0.9)
  expect_equal(interpolate_median(predicted, quantile_level), mean(predicted))

  1 + 0.4/0.8 * (10 - 1)

  # asymmetric quantile levels given
  quantile_level <- c(0.3, 0.6)
  expect_equal(interpolate_median(predicted, quantile_level), 1 + 0.2/0.3 * (10 - 1))

  quantile_level <- c(0.2, 0.6)
  expect_equal(interpolate_median(predicted, quantile_level), 1 + 0.3/0.4 * (10 - 1))
})



# ============================================================================ #
# `ae_median_quantile` ======================================================= #
# ============================================================================ #

test_that("ae_median_quantile() works as_expected", {
  observed <- 1:30
  predicted_values <- matrix(2:31)
  expect_equal(
    ae_median_quantile(observed, predicted_values, quantile_level = 0.5),
    as.numeric(predicted_values - observed)
  )

  # using a vector as input for predicted does not work if there are several
  # observed values
  expect_error(
    ae_median_quantile(observed, as.numeric(predicted_values), quantile_level = 0.5),
    "Assertion on 'predicted' failed: Must be of type 'matrix', not 'double'."
  )

  # it does work if there is only a single observed value - in this case
  # the vector gets treated as one forecast - which means that we need to
  # supply multiple quantile levels now, because it assumes that single forecast
  # is represented by several quantiles
  expect_equal(
    ae_median_quantile(observed[1], as.numeric(predicted_values)[1:3], quantile_level = c(0.1, 0.5, 0.7)),
    2
  )

  # test that we get a warning if there are inputs without a 0.5 quantile
  expect_error(
    ae_median_quantile(observed, predicted_values, quantile_level = 0.6),
    'In order to compute the absolute error of the median, '
  )
})



# ============================================================================ #
# quantile_score() =============================================================
# ============================================================================ #
test_that("quantile_score() works regardless of whether input is vector or matrix", {

  observed <- rnorm(1, mean = 2)
  alpha <- seq(0.2, 0.8, 0.2)
  predicted <- rnorm(4, mean = 1:4)


  expect_equal(
    quantile_score(observed, predicted, quantile_level = alpha),
    quantile_score(observed, t(matrix(predicted)), quantile_level = alpha)
  )
})


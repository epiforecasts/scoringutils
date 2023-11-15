metrics_no_cov <- metrics_quantile[!grepl("coverage", names(metrics_quantile))]
metrics_no_cov_no_ae <- metrics_no_cov[!grepl("ae", names(metrics_no_cov))]

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
    quantile = quantile_probs,
  )

  expected <- abs(y - lower)

  expect_identical(actual, expected)
})

test_that("`wis()` works within score for median forecast", {
  test_data <- data.frame(
    observed = c(1, -15, 22),
    predicted = 1:3,
    quantile = rep(c(0.5), each = 3),
    model = "model1",
    date = 1:3
  )
  eval <- score(
    test_data,
    count_median_twice = TRUE, metrics = metrics_no_cov
  )
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
    quantile = quantile_probs,
  )

  expect_identical(actual, expected)
  expect_identical(actual_wis, expected)
})

test_that("wis() works within score for one interval", {
  test_data <- data.frame(
    observed = rep(c(1, -15, 22), times = 2),
    quantile = rep(c(0.25, 0.75), each = 3),
    predicted = c(c(0, 1, 0), c(2, 2, 3)),
    model = c("model1"),
    date = rep(1:3, times = 2)
  )

  eval <- score(
    test_data,
    count_median_twice = TRUE, metrics = list(wis = wis)
  )

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
    quantile = rep(c(0.25, 0.5, 0.75), each = 3),
    predicted = c(c(0, 1, 0), c(1, 2, 3), c(2, 2, 3)),
    model = c("model1"),
    date = rep(1:3, times = 3)
  )

  eval <- score(
    test_data,
    count_median_twice = TRUE, metrics = metrics_no_cov
  )

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
    quantile = quantile_probs,
    count_median_twice = TRUE
  )

  expect_identical(eval$wis, expected)
  expect_identical(actual_wis, expected)
})

# covidHubUtils test:
y <- c(1, -15, 22)
forecast_quantiles_matrix <- rbind(
  c(-1, 0, 1, 2, 3),
  c(-2, 1, 2, 2, 4),
  c(-2, 0, 3, 3, 4)
)
forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

test_that("wis works, 2 intervals and median", {
  test_data <- data.frame(
    observed = rep(c(1, -15, 22), times = 5),
    quantile = rep(c(0.1, 0.25, 0.5, 0.75, 0.9), each = 3),
    predicted = c(
      c(-1, -2, -2), c(0, 1, 0), c(1, 2, 3),
      c(2, 2, 3), c(3, 4, 4)
    ),
    model = c("model1"),
    date = rep(1:3, times = 5)
  )

  eval <- score(
    test_data,
    count_median_twice = TRUE, metrics = metrics_no_cov
  )

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
    quantile = quantile_probs,
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
    quantile = forecast_quantile_probs,
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

  eval <- score(
    data_formatted,
    count_median_twice = FALSE, metrics = metrics_no_cov
  )

  expected <- abs(y - forecast_quantiles_matrix[, 1])

  actual_wis <- wis(
    observed = y,
    predicted = matrix(forecast_quantiles_matrix),
    quantile = 0.5,
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
    quantile = forecast_quantile_probs,
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

  eval <- suppressMessages(score(data_formatted,
                                 count_median_twice = FALSE, metrics = metrics_no_cov_no_ae
  ))

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
    quantile = c(0.1, 0.9),
    count_median_twice = FALSE
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
    quantile = forecast_quantile_probs,
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

  eval <- score(data_formatted,
                count_median_twice = FALSE, metrics = metrics_no_cov
  )

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
    quantile = c(0.1, 0.25, 0.5, 0.75, 0.9),
    count_median_twice = FALSE
  )

  expect_equal(eval$wis, expected)
})

test_that("Quantlie score and interval score yield the same result, weigh = FALSE", {
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
      quantile = c(alpha / 2, 1 - alpha / 2),
      count_median_twice = FALSE,
      weigh = w
    )

    qs_lower <- quantile_score(observed,
                               predicted = lower,
                               quantile = alpha / 2,
                               weigh = w
    )
    qs_upper <- quantile_score(observed,
                               predicted = upper,
                               quantile = 1 - alpha / 2,
                               weigh = w
    )
    expect_equal((qs_lower + qs_upper) / 2, is)
    expect_equal(wis, is)
  }
})

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
      quantile = c(alpha / 2, 1 - alpha / 2),
      count_median_twice = FALSE,
      weigh = w
    )

    qs_lower <- quantile_score(observed,
                               predicted = lower,
                               quantile = alpha / 2,
                               weigh = w
    )
    qs_upper <- quantile_score(observed,
                               predicted = upper,
                               quantile = 1 - alpha / 2,
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
    quantile = forecast_quantile_probs,
    separate_results = TRUE
  )
  expect_equal(wis$wis, wis$dispersion + wis$overprediction + wis$underprediction)
})


# `bias_quantile` ==============================================================
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

  quantile <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)

  observed <- 8062
  expect_equal(bias_quantile(observed, predicted, quantile), -0.8)
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
    "Assertion on 'quantile' failed: Must have length 4, but has length 3."
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
    "Assertion on 'quantile' failed: Must have length >= 1, but has length 0"
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
    "Assertion on 'quantile' failed: Element 1 is not >= 0."
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

test_that("bias_quantile(): quantiles must be unique", {
  predicted <- 1:4

  # Failing example
  quantiles <- c(0.3, 0.3, 0.5, 0.8)
  expect_error(
    bias_quantile(observed = 3, predicted, quantiles),
    "Assertion on 'quantile' failed: Contains duplicated values, position 2."
  )

  # Passing example
  quantiles <- c(0.3, 0.5, 0.8, 0.9)
  expect_silent(bias_quantile(observed = 3, predicted, quantiles))
})
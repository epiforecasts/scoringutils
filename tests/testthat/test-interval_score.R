test_that("wis works, median only", {
  y <- c(1, -15, 22)
  lower <- upper <- c(1, 2, 3)
  quantile_probs <- 0.5

  actual <- interval_score(y,
    lower = lower, upper = upper,
    weigh = TRUE,
    interval_range = 0
  )
  expected <- abs(y - lower)

  expect_identical(actual, expected)
})

test_that("WIS works within score for median forecast", {
  test_data <- data.frame(
    true_value = c(1, -15, 22),
    prediction = 1:3,
    quantile = rep(c(0.5), each = 3),
    model = "model1",
    date = 1:3
  )
  eval <- scoringutils::score(test_data,
    count_median_twice = TRUE
  )
  expect_equal(eval$ae_median, eval$interval_score)
})


test_that("wis works, 1 interval only", {
  y <- c(1, -15, 22)
  lower <- c(0, 1, 0)
  upper <- c(2, 2, 3)
  quantile_probs <- c(0.25, 0.75)

  alpha <- 0.5

  actual <- scoringutils::interval_score(y,
    lower = lower, upper = upper,
    weigh = TRUE,
    interval_range = 50
  )
  expected <- (upper - lower) * (alpha / 2) + c(0, 1 - (-15), 22 - 3)

  expect_identical(actual, expected)
})

test_that("WIS works within score for one interval", {
  test_data <- data.frame(
    true_value = rep(c(1, -15, 22), times = 2),
    quantile = rep(c(0.25, 0.75), each = 3),
    prediction = c(c(0, 1, 0), c(2, 2, 3)),
    model = c("model1"),
    date = rep(1:3, times = 2)
  )

  eval <- scoringutils::score(test_data,
    count_median_twice = TRUE
  )

  eval <- summarise_scores(eval, by = c("model", "date"))

  lower <- c(0, 1, 0)
  upper <- c(2, 2, 3)
  alpha <- 0.5

  expected <- (upper - lower) * (alpha / 2) + c(0, 1 - (-15), 22 - 3)

  expect_equal(expected, eval$interval_score)
})






test_that("wis works, 1 interval and median", {
  test_data <- data.frame(
    true_value = rep(c(1, -15, 22), times = 3),
    quantile = rep(c(0.25, 0.5, 0.75), each = 3),
    prediction = c(c(0, 1, 0), c(1, 2, 3), c(2, 2, 3)),
    model = c("model1"),
    date = rep(1:3, times = 3)
  )

  eval <- scoringutils::score(test_data,
    count_median_twice = TRUE
  )

  eval <- summarise_scores(eval, by = c("model", "date"))

  y <- c(1, -15, 22)
  quantiles <- rbind(c(0, 1, 2), c(1, 2, 2), c(0, 3, 3))
  quantile_probs <- c(0.25, 0.5, 0.75)

  alpha <- 0.5

  expected <- 0.5 * (
    abs(y - quantiles[, 2]) +
      (quantiles[, 3] - quantiles[, 1]) * (alpha / 2) + c(0, 1 - (-15), 22 - 3)
  )

  expect_identical(eval$interval_score, expected)
})


test_that("wis works, 2 intervals and median", {
  test_data <- data.frame(
    true_value = rep(c(1, -15, 22), times = 5),
    quantile = rep(c(0.1, 0.25, 0.5, 0.75, 0.9), each = 3),
    prediction = c(
      c(-1, -2, -2), c(0, 1, 0), c(1, 2, 3),
      c(2, 2, 3), c(3, 4, 4)
    ),
    model = c("model1"),
    date = rep(1:3, times = 5)
  )

  eval <- scoringutils::score(test_data,
    count_median_twice = TRUE
  )

  eval <- summarise_scores(eval, by = c("model", "date"))

  y <- c(1, -15, 22)
  quantiles <- rbind(c(-1, 0, 1, 2, 3), c(-2, 1, 2, 2, 4), c(-2, 0, 3, 3, 4))
  quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  alpha1 <- 0.2
  alpha2 <- 0.5

  expected <- (1 / 3) * (
    abs(y - quantiles[, 3]) +
      (quantiles[, 5] - quantiles[, 1]) * (alpha1 / 2) + c(0, (-2) - (-15), 22 - 4) +
      (quantiles[, 4] - quantiles[, 2]) * (alpha2 / 2) + c(0, 1 - (-15), 22 - 3)
  )

  expect_equal(
    as.numeric(eval$interval_score),
    as.numeric(expected)
  )
})








# additional tests from the covidhubutils repo

test_that("wis is correct, median only - test corresponds to covidHubUtils", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
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
  data.table::setnames(truth_formatted, old = "value", new = "true_value")

  forecasts_formated <- data.table::as.data.table(test_forecasts)
  data.table::setnames(forecasts_formated, old = "value", new = "prediction")

  data_formatted <- merge(forecasts_formated, truth_formatted)

  eval <- scoringutils::score(data_formatted,
    count_median_twice = FALSE
  )

  expected <- abs(y - forecast_quantiles_matrix[, 1])

  expect_equal(eval$interval_score, expected)
})




test_that("wis is correct, 1 interval only - test corresponds to covidHubUtils", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
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
  data.table::setnames(truth_formatted, old = "value", new = "true_value")

  forecasts_formated <- data.table::as.data.table(test_forecasts)
  data.table::setnames(forecasts_formated, old = "value", new = "prediction")

  data_formatted <- merge(forecasts_formated, truth_formatted)

  eval <- scoringutils::score(data_formatted,
    count_median_twice = FALSE
  )

  eval <- summarise_scores(eval,
    by = c(
      "model", "location", "target_variable",
      "target_end_date", "forecast_date", "horizon"
    )
  )

  alpha1 <- 0.2
  expected <- (forecast_quantiles_matrix[, 2] - forecast_quantiles_matrix[, 1]) * (alpha1 / 2) +
    c(0, (-2) - (-15), 22 - 4)

  expect_equal(eval$interval_score, expected)
})


test_that("wis is correct, 2 intervals and median - test corresponds to covidHubUtils", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

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
  data.table::setnames(truth_formatted, old = "value", new = "true_value")

  forecasts_formated <- data.table::as.data.table(test_forecasts)
  data.table::setnames(forecasts_formated, old = "value", new = "prediction")

  data_formatted <- merge(forecasts_formated, truth_formatted)

  eval <- scoringutils::score(data_formatted,
    count_median_twice = FALSE
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

  expect_equal(eval$interval_score, expected)
})





test_that("Quantlie score and interval score yield the same result, weigh = FALSE", {
  true_values <- rnorm(10, mean = 1:10)
  alphas <- c(0.1, 0.5, 0.9)

  for (alpha in alphas) {
    lower <- qnorm(alpha / 2, rnorm(10, mean = 1:10))
    upper <- qnorm((1 - alpha / 2), rnorm(10, mean = 1:10))

    w <- FALSE
    is <- interval_score(
      true_values = true_values,
      lower = lower,
      upper = upper,
      interval_range = (1 - alpha) * 100,
      weigh = w
    )

    qs_lower <- quantile_score(true_values,
      predictions = lower,
      quantiles = alpha / 2,
      weigh = w
    )
    qs_upper <- quantile_score(true_values,
      predictions = upper,
      quantiles = 1 - alpha / 2,
      weigh = w
    )
    expect_equal((qs_lower + qs_upper) / 2, is)
  }
})


test_that("Quantlie score and interval score yield the same result, weigh = TRUE", {
  true_values <- rnorm(10, mean = 1:10)
  alphas <- c(0.1, 0.5, 0.9)

  for (alpha in alphas) {
    lower <- qnorm(alpha / 2, rnorm(10, mean = 1:10))
    upper <- qnorm((1 - alpha / 2), rnorm(10, mean = 1:10))

    w <- TRUE
    is <- interval_score(
      true_values = true_values,
      lower = lower,
      upper = upper,
      interval_range = (1 - alpha) * 100,
      weigh = w
    )

    qs_lower <- quantile_score(true_values,
      predictions = lower,
      quantiles = alpha / 2,
      weigh = w
    )
    qs_upper <- quantile_score(true_values,
      predictions = upper,
      quantiles = 1 - alpha / 2,
      weigh = w
    )
    expect_equal((qs_lower + qs_upper) / 2, is)
  }
})

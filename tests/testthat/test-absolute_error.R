test_that("absolute error (sample based) works", {
  true_values <- rnorm(30, mean = 1:30)
  predicted_values <- rnorm(30, mean = 1:30)

  scoringutils <- scoringutils::ae_median_sample(true_values, predicted_values)

  ae <- abs(true_values - predicted_values)
  expect_equal(ae, scoringutils)
})


# covidHubUtils-tests

test_that("abs error is correct within score, point forecast only", {
  # test is adapted from the package covidHubUtils, https://github.com/reichlab/covidHubUtils/
  y <- c(1, -15, 22)

  target_end_dates <- as.Date("2020-01-01") + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1)
  forecast_horizons <- rep(horizons, times = 1)
  forecast_locations <- rep(locations, times = 1)
  forecast_target_variables <-
    rep(target_variables, times = 1)

  point_forecast <- c(5, 6, 7)

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )


  n_forecasts <- length(point_forecast)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(as.Date("2020-01-01"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("point", 3),
    quantile = NA,
    value = point_forecast,
    stringsAsFactors = FALSE
  )

  # bring in scoringutils format
  truth_scoringutils <- data.table::as.data.table(test_truth)
  fc_scoringutils <- data.table::as.data.table(test_forecasts)
  data.table::setnames(truth_scoringutils, old = "value", new = "true_value")
  data.table::setnames(fc_scoringutils, old = "value", new = "prediction")
  truth_scoringutils[, model := NULL]

  data_scoringutils <- merge_pred_and_obs(
    forecasts = fc_scoringutils,
    observations = truth_scoringutils
  )

  eval <- scoringutils::score(data_scoringutils)

  # actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth)

  expected <- abs(y - point_forecast)

  # expect_equal(actual$abs_error, expected)
  expect_equal(eval$ae_point, expected)
})

test_that("abs error is correct, point and median forecasts different", {
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
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  point_forecast <- c(5, 6, 7)

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(as.Date("2020-01-01"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point", length(point_forecast)), rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA, length(point_forecast)), forecast_quantile_probs),
    value = c(point_forecast, forecast_quantiles),
    stringsAsFactors = FALSE
  )

  # bring in scoringutils format
  truth_scoringutils <- data.table::as.data.table(test_truth)
  fc_scoringutils <- data.table::as.data.table(test_forecasts)
  data.table::setnames(truth_scoringutils, old = "value", new = "true_value")
  data.table::setnames(fc_scoringutils, old = "value", new = "prediction")
  truth_scoringutils[, model := NULL]

  data_scoringutils <- merge_pred_and_obs(
    forecasts = fc_scoringutils,
    observations = truth_scoringutils
  )

  eval <- scoringutils::score(data_scoringutils)

  expected <- abs(y - point_forecast)
  # expect_equal(actual$abs_error, expected)
  expect_equal(eval[!is.na(ae_point)]$ae_point, expected)
})

test_that("abs error is correct, point and median forecasts same", {
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
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  point_forecast <- c(1, 2, 3)

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(as.Date("2020-01-01"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point", length(point_forecast)), rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA, length(point_forecast)), forecast_quantile_probs),
    value = c(point_forecast, forecast_quantiles),
    stringsAsFactors = FALSE
  )


  # bring in scoringutils format
  truth_scoringutils <- data.table::as.data.table(test_truth)
  fc_scoringutils <- data.table::as.data.table(test_forecasts)
  data.table::setnames(truth_scoringutils, old = "value", new = "true_value")
  data.table::setnames(fc_scoringutils, old = "value", new = "prediction")
  truth_scoringutils[, model := NULL]

  data_scoringutils <- merge_pred_and_obs(
    forecasts = fc_scoringutils,
    observations = truth_scoringutils
  )

  eval <- score(data = data_scoringutils)
  eval <- summarise_scores(eval,
    by = c(
      "location", "target_end_date",
      "target_variable", "horizon"
    ),
    na.rm = TRUE
  )


  # actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth)

  expected <- abs(y - point_forecast)

  # expect_equal(actual$abs_error, expected)
  expect_equal(eval$ae_point, expected)
})

as_point <- function(forecast, ...) {
  UseMethod("as_point")
}

as_point.default <- function(forecast, ...) {
  cli_abort(
    c(
      "!" = "The input needs to be a forecast object.",
      "i" = "Please run `as_forecast()` first." # nolint
    )
  )
}

as_point.forecast_quantile <- function(forecast, quantile_level = 0.5, ...) {
  assert_forecast(forecast, verbose = FALSE)
  assert_numeric(quantile_level, lower = 0, upper = 1, len = 1)

  forecast <- forecast[
    quantile_level == target_quantile_level, ,
    env = list(target_quantile_level = quantile_level)
  ]
  forecast[, "quantile_level" := NULL]

  point_forecast <- as_forecast.default(
    forecast, forecast_type = "point", check_forecast_type = FALSE
  )
  return(point_forecast[])
}


as_point.forecast_sample <- function(forecast, quantile_level = 0.5, fun, ...) {
  assert_forecast(forecast, verbose = FALSE)
  if (missing(fun)) {
    quantile_forecast <- as_quantile(forecast, quantile_levels = quantile_level)
    point_forecast <- as_point(quantile_forecast)
  }else {
    sum_forecast <- summarise_score(
      forecast, fun = fun, by = get_forecast_unit(forecast),
      metrics = "predicted"
    )
    point_forecast <- as_forecast.default(
      sum_forecast, forecast_type = "point", check_forecast_type = FALSE
    )
  }
  return(point_forecast[])
}
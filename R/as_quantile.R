as_quantile <- function(forecast, ...) {
  UseMethod("as_quantile")
}

as_quantile.default <- function(forecast, ...) {
  cli_abort(
    c(
      "!" = "The input needs to be a forecast object.",
      "i" = "Please run `as_forecast()` first." # nolint
    )
  )
}

as_quantile.forecast_sample <- function(
  forecast, quantile_levels = seq(from = 0.01, to = 0.99, by = 0.01), ...
) {
  assert_forecast(forecast, verbose = FALSE)

  sum_forecast <- forecast[,
    as.list(quantile(predicted, probs = quantile_levels, na.rm = TRUE)),
    by = c(eval(get_forecast_unit(forecast)), "observed")
  ]

  sum_forecast <- melt(
    sum_forecast,
    measure.vars = paste0(quantile_levels * 100, "%"),
    variable.name = "quantile_level",
    value.name = "predicted"
  )

  sum_forecast[,
   quantile_level := as.numeric(gsub("\\%", "", quantile_level)) / 100
  ]

  quantile_forecast <- as_forecast.default(
    sum_forecast, forecast_type = "quantile", check_forecast_type = FALSE
  )
  return(quantile_forecast[])
}

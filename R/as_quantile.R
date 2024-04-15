#' Convert a forecast object to a quantile object
#'
#' This function is used to convert a forecast object to a quantile object.
#' It dispatches to the appropriate method based on the class of the forecast
#' object.
#'
#' @inheritParams as_point
#'
#' @return A `forecast_quantile` object.
#'
#' @export
#' @keywords check-forecasts
#' @examples
#' as_quantile(as_forecast(example_sample_continuous))
#'
as_quantile <- function(forecast, ...) {
  UseMethod("as_quantile")
}

#' @rdname as_quantile
#' @export
#' @importFrom cli cli_abort
as_quantile.default <- function(forecast, ...) {
  cli_abort(
    c(
      "!" = "The input needs to be a forecast object.",
      "i" = "Please run `as_forecast()` first." # nolint
    )
  )
}

#' Convert a sample forecast to a quantile forecast
#'
#' This function takes a sample forecast and converts it to a quantile forecast
#' sample.
#'
#' @param forecast The `forecast_sample`` object to convert to a
#' `forecast_quantile`.
#'
#' @param quantile_levels A vector of quantile levels. Defaults to
#' 0.01 to 0.99 by 0.01.
#' @inheritParams as_point.forecast_quantile
#'
#' @return A `forecast_quantile` object.
#' @export
#' @keywords check-forecasts
#' @importFrom data.table melt
#'
#' @export
#' @examples
#' as_quantile(as_forecast(example_sample_continuous))
as_quantile.forecast_sample <- function(
  forecast, quantile_levels = seq(from = 0.01, to = 0.99, by = 0.01), ...
) {
  assert_forecast(forecast, verbose = FALSE)
  assert_numeric(quantile_levels, lower = 0, upper = 1)

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
    quantile_level := as.numeric(
      gsub("%", "", quantile_level, fixed = TRUE)
    ) / 100
  ]

  quantile_forecast <- remake_forecast(
    sum_forecast, "forecast_sample", "forecast_quantile"
  )
  return(quantile_forecast)
}

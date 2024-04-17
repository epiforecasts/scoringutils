#' @title Convert a forecast object to a point forecast
#'
#' @description
#' The `as_point` function is used to convert a forecast object to a point
#' forecast. It is a generic function that dispatches the conversion to the
#' appropriate method based on the class of the input forecast object.
#'
#' @param forecast A forecast object of class `forecast_{type}` (a validated
#' `data.table` with predicted and observed values, see [as_forecast()])
#' representing a forecast.
#' @param ... Additional arguments to be passed to the specific method.
#'
#' @return The function returns a point forecast object, which is a specific
#' type of forecast object that represents a single value prediction.
#'
#' @export
#' @keywords check-forecasts
#' @examples
#' as_point(as_forecast(example_quantile))
#' as_point(as_forecast(example_sample_continuous))
as_point <- function(forecast, ...) {
  UseMethod("as_point")
}

#' @rdname as_point
#' @export
#' @importFrom cli cli_abort
as_point.default <- function(forecast, ...) {
  cli_abort(
    c(
      "!" = "The input needs to be a forecast object.",
      "i" = "Please run `as_forecast()` first." # nolint
    )
  )
}

#' Convert a quantile forecast to a point forecast
#'
#' This function takes a quantile forecast and converts it to a point forecast
#' by selecting the forecast corresponding to the specified quantile level.
#' @param forecast The `forecast_quantile` object.
#' @param quantile_level The desired quantile level of the current forecast
#' that should become the point forecast. Defaults to 0.5 (median).
#' @param ... Additional arguments passing inherited from the default method but
#' unused.
#'
#' @return A `forecast_point` object.
#'
#' @export
#' @keywords check-forecasts
#' @examples
#' as_point(as_forecast(example_quantile))
as_point.forecast_quantile <- function(forecast, quantile_level = 0.5, ...) {
  assert_forecast(forecast, verbose = FALSE)
  assert_numeric(quantile_level, lower = 0, upper = 1, len = 1)

  forecast <- forecast[
    quantile_level == target_quantile_level, ,
    env = list(target_quantile_level = quantile_level)
  ]
  forecast[, "quantile_level" := NULL]

  point_forecast <- remake_forecast(
    forecast, "forecast_quantile", "forecast_point", verbose = FALSE
  )
  return(point_forecast)
}

#' Convert a sample based forecast to a point forecast
#'
#' This function converts a forecast object to a point forecast by
#' summarising the forecast using a custom function (for example `mean`).
#'
#' @param forecast The `forecast_sample` object to be converted to a point
#' forecast.
#'
#' @param fun A custom function to summarize the forecast. Defaults to `median`.
#'
#' @param ... Additional arguments passed to `fun`..
#'
#' @return A `forecast_point` object.
#' @export
#' @keywords check-forecasts
#' @examples
#' sample_forecast <- as_forecast(example_sample_continuous)
#'
#' as_point(sample_forecast)
#' as_point(sample_forecast, fun = mean)
as_point.forecast_sample <- function(forecast, fun = median, ...) {
  assert_forecast(forecast, verbose = FALSE)
  sum_forecast <- summarise_scores(
    forecast, fun = fun, by = c(get_forecast_unit(forecast), "observed"),
    metrics = "predicted", ...
  )
  point_forecast <- remake_forecast(
    sum_forecast, "forecast_sample", "forecast_point"
  )
  return(point_forecast)
}

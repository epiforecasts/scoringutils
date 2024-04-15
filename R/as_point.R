#' @title Convert a forecast object to a point forecast
#'
#' @description
#' The `as_point` function is used to convert a forecast object to a point
#' forecast. It is a generic function that dispatches the conversion to the
#' appropriate method based on the class of the input forecast object.
#'
#' @param forecast An object of class `forecast_{type}`,
#' representing a forecast.
#' @param ... Additional arguments to be passed to the specific method.
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
#'
#' @param forecast The `forecast_quantile` object.
#' @param quantile_level The desired quantile level for the point forecast.
#' Defaults to 0.5 (median).
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

  point_forecast <- as_forecast.default(
    forecast, forecast_type = "point", check_forecast_type = FALSE
  )
  return(point_forecast[])
}


#' Convert a sample based forecast to a point forecast
#'
#' This function converts a forecast object to a point forecast by either
#' taking the quantile forecast at a specified quantile level or by summarizing
#' the forecast using a custom function (for example `mean`).
#'
#' @param forecast The `forecast_sample` object to be converted to a point
#' forecast.
#'
#' @param fun A custom function to summarize the forecast. If provided, this
#' function will be used instead of the quantile method. An example could
#' be the `mean`.
#' @inheritParams as_point.forecast_quantile
#' @return A `forecast_point` object.
#' @export
#' @keywords check-forecasts
#' @examples
#' sample_forecast <- as_forecast(example_sample_continuous)
#'
#' # Quantile approach
#' as_point(sample_forecast)
#'
#' # Function approach
#' as_point(sample_forecast, fun = function(...) {mean(..., na.rm = TRUE)})
as_point.forecast_sample <- function(forecast, quantile_level = 0.5, fun, ...) {
  assert_forecast(forecast, verbose = FALSE)
  if (missing(fun)) {
    quantile_forecast <- as_quantile(forecast, quantile_levels = quantile_level)
    point_forecast <- as_point(quantile_forecast)
  } else {
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

  quantile_forecast <- as_forecast.default(
    sum_forecast, forecast_type = "quantile", check_forecast_type = FALSE
  )
  return(quantile_forecast[])
}

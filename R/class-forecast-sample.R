#' @export
#' @rdname assert_forecast
#' @keywords validate-forecast-object
assert_forecast.forecast_sample <- function(
    forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "sample", desired = forecast_type)
  return(invisible(NULL))
}


#' @title Create a `forecast` object for sample-based forecasts
#' @param sample_id (optional) Name of the column in `data` that contains the
#'   sample id. This column will be renamed to "sample_id". Only applicable to
#'   sample-based forecasts.
#' @inheritParams as_forecast
#' @export
#' @family functions to create forecast objects
#' @importFrom cli cli_warn
#' @keywords as_forecast
as_forecast_sample <- function(data,
                               forecast_unit = NULL,
                               observed = NULL,
                               predicted = NULL,
                               sample_id = NULL) {
  assert_character(sample_id, len = 1, null.ok = TRUE)
  assert_subset(sample_id, names(data), empty.ok = TRUE)
  if (!is.null(sample_id)) {
    setnames(data, old = sample_id, new = "sample_id")
  }

  data <- as_forecast_generic(data, forecast_unit, observed, predicted)
  data <- new_forecast(data, "forecast_sample")
  assert_forecast(data)
  return(data)
}


#' @export
#' @rdname is_forecast
is_forecast_sample <- function(x) {
  inherits(x, "forecast_sample") && inherits(x, "forecast")
}


#' @rdname as_forecast_quantile
#' @description
#' When creating a `forecast_quantile` object from a `forecast_sample` object,
#' the quantiles are estimated by computing empircal quantiles from the samples
#' via [quantile()]. Note that empirical quantiles are a biased estimator for
#' the true quantiles in particular in the tails of the distribution and
#' when the number of available samples is low.
#' @param probs A numeric vector of quantile levels for which
#'   quantiles will be computed. Corresponds to the `probs` argument in
#'   [quantile()].
#' @param type Type argument passed down to the quantile function. For more
#'   information, see [quantile()].
#' @importFrom stats quantile
#' @importFrom methods hasArg
#' @importFrom checkmate assert_numeric
#' @export
as_forecast_quantile.forecast_sample <- function(
    data,
    probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
    type = 7,
    ...
) {
  forecast <- copy(data)
  assert_forecast(forecast, verbose = FALSE)
  assert_numeric(probs, min.len = 1)
  reserved_columns <- c("predicted", "sample_id")
  by <- setdiff(colnames(forecast), reserved_columns)

  quantile_level <- unique(
    round(c(probs, 1 - probs), digits = 10)
  )

  forecast <-
    forecast[, .(quantile_level = quantile_level,
                 predicted = quantile(x = predicted, probs = ..probs,
                                      type = ..type, na.rm = TRUE)),
             by = by]

  quantile_forecast <- new_forecast(forecast, "forecast_quantile")
  assert_forecast(quantile_forecast)

  return(quantile_forecast)
}

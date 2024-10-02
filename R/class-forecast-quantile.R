#' @title Create a `forecast` object for quantile-based forecasts
#' @description
#' Create a `forecast` object for quantile-based forecasts. See more information
#' on forecast types and expected input formats by calling `?`[as_forecast()].
#' @param ... Unused
#' @family functions to create forecast objects
#' @inheritParams as_forecast
#' @export
#' @keywords as_forecast transform
as_forecast_quantile <- function(data, ...) {
  UseMethod("as_forecast_quantile")
}


#' @rdname as_forecast_quantile
#' @param quantile_level (optional) Name of the column in `data` that contains
#'   the quantile level of the predicted values. This column will be renamed to
#'   "quantile_level". Only applicable to quantile-based forecasts.
#' @export
#' @importFrom cli cli_warn
as_forecast_quantile.default <- function(data,
                                         forecast_unit = NULL,
                                         observed = NULL,
                                         predicted = NULL,
                                         quantile_level = NULL,
                                         ...) {
  assert_character(quantile_level, len = 1, null.ok = TRUE)
  assert_subset(quantile_level, names(data), empty.ok = TRUE)
  if (!is.null(quantile_level)) {
    setnames(data, old = quantile_level, new = "quantile_level")
  }

  data <- as_forecast_generic(data, forecast_unit, observed, predicted)
  data <- new_forecast(data, "forecast_quantile")
  assert_forecast(data)
  return(data)
}


#' @importFrom stats na.omit
#' @importFrom data.table `:=` as.data.table rbindlist %like% setattr copy
#' @rdname score
#' @export
score.forecast_quantile <- function(forecast, metrics = get_metrics(forecast), ...) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast_unit <- get_forecast_unit(forecast)
  metrics <- validate_metrics(metrics)
  forecast <- as.data.table(forecast)

  # transpose the forecasts that belong to the same forecast unit
  # make sure the quantiles and predictions are ordered in the same way
  f_transposed <- forecast[, .(
    predicted = list(predicted[order(quantile_level)]),
    observed = unique(observed),
    quantile_level = list(sort(quantile_level, na.last = TRUE)),
    scoringutils_quantile_level = toString(sort(quantile_level, na.last = TRUE))
  ), by = forecast_unit]

  # split according to quantile_level lengths and do calculations for different
  # quantile_level lengths separately. The function `wis()` assumes that all
  # forecasts have the same quantile_levels
  f_split <- split(f_transposed, f_transposed$scoringutils_quantile_level)

  split_result <- lapply(f_split, function(forecast) {
    # create a matrix out of the list of predicted values and quantile_levels
    observed <- forecast$observed
    predicted <- do.call(rbind, forecast$predicted)
    quantile_level <- unlist(unique(forecast$quantile_level))
    forecast[, c(
      "observed", "predicted", "quantile_level", "scoringutils_quantile_level"
    ) := NULL]

    forecast <- apply_metrics(
      forecast, metrics,
      observed, predicted, quantile_level
    )
    return(forecast)
  })
  scores <- rbindlist(split_result, fill = TRUE)

  scores <- as_scores(scores, metrics = names(metrics))

  return(scores[])
}

#' @export
#' @rdname assert_forecast
#' @keywords validate-forecast-object
assert_forecast.forecast_quantile <- function(
    forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "quantile", desired = forecast_type)
  assert_numeric(forecast$quantile_level, lower = 0, upper = 1)
  return(invisible(NULL))
}


#' @rdname as_forecast_point
#' @description
#' When converting a `forecast_quantile` object into a `forecast_point` object,
#' the 0.5 quantile is extracted and returned as the point forecast.
#' @export
#' @keywords as_forecast
as_forecast_point.forecast_quantile <- function(data, ...) {
  assert_forecast(data, verbose = FALSE)
  assert_subset(0.5, unique(data$quantile_level))

  # At end of this function, the object will have be turned from a
  # forecast_quantile to a forecast_point and we don't want to validate it as a
  # forecast_point during the conversion process. The correct class is restored
  # at the end.
  data <- as.data.table(data)

  forecast <- data[quantile_level == 0.5]
  forecast[, "quantile_level" := NULL]

  point_forecast <- new_forecast(forecast, "forecast_point")
  return(point_forecast)
}


#' Get default metrics for quantile-based forecasts
#'
#' @description
#' For quantile-based forecasts, the default scoring rules are:
#' - "wis" = [wis()]
#' - "overprediction" = [overprediction_quantile()]
#' - "underprediction" = [underprediction_quantile()]
#' - "dispersion" = [dispersion_quantile()]
#' - "bias" = [bias_quantile()]
#' - "interval_coverage_50" = [interval_coverage()]
#' - "interval_coverage_90" = purrr::partial(
#'      interval_coverage, interval_range = 90
#'    )
#' - "ae_median" = [ae_median_quantile()]
#'
#' Note: The `interval_coverage_90` scoring rule is created by modifying
#' [interval_coverage()], making use of the function [purrr::partial()].
#' This construct allows the function to deal with arbitrary arguments in `...`,
#' while making sure that only those that [interval_coverage()] can
#' accept get passed on to it. `interval_range = 90` is set in the function
#' definition, as passing an argument `interval_range = 90` to [score()] would
#' mean it would also get passed to `interval_coverage_50`.
#' @inheritSection illustration-input-metric-quantile Input format
#' @inheritParams get_metrics.forecast_binary
#' @export
#' @family `get_metrics` functions
#' @keywords handle-metrics
#' @importFrom purrr partial
#' @examples
#' get_metrics(example_quantile, select = "wis")
get_metrics.forecast_quantile <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    wis = wis,
    overprediction = overprediction_quantile,
    underprediction = underprediction_quantile,
    dispersion = dispersion_quantile,
    bias = bias_quantile,
    interval_coverage_50 = interval_coverage,
    interval_coverage_90 = purrr::partial(
      interval_coverage, interval_range = 90
    ),
    ae_median = ae_median_quantile
  )
  select_metrics(all, select, exclude)
}


#' @rdname get_pit
#' @importFrom stats na.omit
#' @importFrom data.table `:=` as.data.table
#' @export
get_pit.forecast_quantile <- function(forecast, by, ...) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast <- as.data.table(forecast)

  forecast[, quantile_coverage := (observed <= predicted)]
  quantile_coverage <-
    forecast[, .(quantile_coverage = mean(quantile_coverage)),
             by = c(unique(c(by, "quantile_level")))]
  quantile_coverage <- quantile_coverage[order(quantile_level),
                                         .(
                                           quantile_level = c(quantile_level, 1),
                                           pit_value = diff(c(0, quantile_coverage, 1))
                                         ),
                                         by = c(get_forecast_unit(quantile_coverage))
  ]
  return(quantile_coverage[])
}

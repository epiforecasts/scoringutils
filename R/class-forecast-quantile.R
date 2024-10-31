#' @title Create a `forecast` object for quantile-based forecasts
#' @inherit as_forecast_doc_template params description
#' @details
#' # Required input
#'
#' The input needs to be a data.frame or similar with the following columns:
#' - `observed`: Column of type `numeric` with observed values.
#' - `predicted`: Column of type `numeric` with predicted values. Predicted
#'    values represent quantiles of the predictive distribution.
#' - `quantile_level`: Column of type `numeric`, denoting the quantile level of
#'    the corresponding predicted value.
#'    Quantile levels must be between 0 and 1.
#'
#' For convenience, we recommend an additional column `model` holding the name
#' of the forecaster or model that produced a prediction, but this is not
#' strictly necessary.
#'
#' See the [example_quantile] data set for an example.
#' @inheritSection forecast_types Forecast unit
#' @param ... Unused
#' @family functions to create forecast objects
#' @returns A `forecast` object of class `forecast_quantile`
#' @export
#' @keywords as_forecast transform
#' @examples
#' as_forecast_quantile(
#'   example_quantile,
#'   predicted = "predicted",
#'   forecast_unit = c("model", "target_type", "target_end_date",
#'                     "horizon", "location")
#' )
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


#' @export
#' @rdname is_forecast
is_forecast_quantile <- function(x) {
  inherits(x, "forecast_quantile") && inherits(x, "forecast")
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
#' @family get_metrics functions
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


#' @rdname get_pit_histogram
#' @importFrom stats na.omit
#' @importFrom data.table `:=` as.data.table
#' @export
get_pit_histogram.forecast_quantile <- function(forecast, num_bins = NULL,
                                                breaks = NULL, by, ...) {
  assert_number(num_bins, lower = 1, null.ok = TRUE)
  assert_numeric(breaks, lower = 0, upper = 1, null.ok = TRUE)
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast <- as.data.table(forecast)
  present_quantiles <- unique(c(0, forecast$quantile_level, 1))
  present_quantiles <- round(present_quantiles, 10)

  if (!is.null(breaks)) {
    quantiles <- unique(c(0, breaks, 1))
  } else if (is.null(num_bins) || num_bins == "auto") {
    quantiles <- present_quantiles
  } else {
    quantiles <- seq(0, 1, 1 / num_bins)
  }
  ## avoid rounding errors
  quantiles <- round(quantiles, 10)
  diffs <- round(diff(quantiles), 10)

  if (length(setdiff(quantiles, present_quantiles)) > 0) {
    cli::cli_warn(
      "Some requested quantiles are missing in the forecast. ",
      "The PIT histogram will be based on the quantiles present in the forecast."
    )
  }

  forecast <- forecast[quantile_level %in% quantiles]
  forecast[, quantile_coverage := (observed <= predicted)]

  quantile_coverage <-
    forecast[, .(quantile_coverage = mean(quantile_coverage)),
             by = c(unique(c(by, "quantile_level")))]

  bins <- sprintf("[%s,%s)", quantiles[-length(quantiles)], quantiles[-1])
  mids <- (quantiles[-length(quantiles)] + quantiles[-1]) / 2

  pit_histogram <- quantile_coverage[
    order(quantile_level),
    .(
      density = diff(c(0, quantile_coverage, 1)) / diffs,
      bin = bins,
      mid = mids
    ),
    by = c(get_forecast_unit(quantile_coverage))
  ]
  return(pit_histogram[])
}


#' Quantile example data
#'
#' A data set with predictions for COVID-19 cases and deaths submitted to the
#' European Forecast Hub.
#'
#' The data was created using the script create-example-data.R in the inst/
#' folder (or the top level folder in a compiled package).
#'
#' @format An object of class `forecast_quantile`
#' (see [as_forecast_quantile()]) with the following columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{observed}{Numeric: observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{quantile_level}{quantile level of the corresponding prediction}
#'   \item{predicted}{predicted value}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#' }
# nolint start
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_quantile"

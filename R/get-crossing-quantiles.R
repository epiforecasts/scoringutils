#' @title Find quantile forecasts with crossing quantiles
#'
#' @description
#' Identify quantile forecasts in which the predicted values decrease with
#' increasing quantile level (also called "quantile crossing"), e.g. where the
#' 0.1 quantile is larger than the 0.2 quantile. Such forecasts do not
#' correspond to a valid predictive distribution. Interval-based metrics
#' such as [wis()] (and its components) and [bias_quantile()] can fail for
#' them, in which case [score()] drops the affected metric columns.
#'
#' The order of the rows does not matter: predictions are compared after
#' sorting by quantile level within each forecast. Missing values in
#' `predicted` or `quantile_level` are ignored (matching the behaviour of
#' [bias_quantile()] with `na.rm = TRUE`). Equal predictions for different
#' quantile levels are allowed.
#'
#' @inheritParams as_forecast_doc_template
#' @param data A data.frame (or a `forecast_quantile` object) with columns
#'   `predicted` and `quantile_level`, as well as any columns that make up the
#'   forecast unit.
#' @returns A data.table with all rows belonging to forecasts with crossing
#'   quantiles, ordered by forecast unit and quantile level.
#' @export
#' @importFrom checkmate assert_data_frame assert_subset assert_numeric
#' @importFrom data.table setorderv rleidv
#' @keywords diagnose-inputs
#' @examples
#' # no crossing quantiles in the example data
#' get_crossing_quantiles(example_quantile)
#'
#' # the 0.5 quantile is larger than the 0.75 quantile
#' df <- data.frame(
#'   model = "model1",
#'   observed = 5,
#'   quantile_level = c(0.25, 0.5, 0.75),
#'   predicted = c(3, 7, 4)
#' )
#' get_crossing_quantiles(df)
get_crossing_quantiles <- function(data, forecast_unit = NULL) {
  assert_data_frame(data)
  data <- ensure_data.table(data)
  if (!is.null(forecast_unit)) {
    data <- set_forecast_unit(data, forecast_unit)
  }
  assert_subset(c("predicted", "quantile_level"), colnames(data))
  assert_numeric(data$predicted, .var.name = "predicted")
  assert_numeric(data$quantile_level, .var.name = "quantile_level")
  forecast_unit <- get_forecast_unit(data)

  # strip the forecast class so that `[.forecast` does not re-validate the
  # object on the modifications below
  data <- as.data.table(data)

  # Sort the complete rows once and compare neighbouring rows, instead of
  # sorting within every forecast. A forecast crosses if a prediction is
  # smaller than the one before it within the same forecast.
  complete <- !is.na(data$predicted) & !is.na(data$quantile_level)
  sorted <- data[complete, c(forecast_unit, "quantile_level", "predicted"),
                 with = FALSE]
  setorderv(sorted, cols = c(forecast_unit, "quantile_level"))
  if (length(forecast_unit) == 0) {
    # without forecast unit columns, all rows form a single forecast
    forecast_id <- rep(1L, nrow(sorted))
  } else {
    forecast_id <- rleidv(sorted, cols = forecast_unit)
  }
  decreasing <- which(diff(sorted$predicted) < 0 & diff(forecast_id) == 0)
  if (length(decreasing) == 0) {
    return(data[0])
  }
  if (length(forecast_unit) == 0) {
    out <- data
  } else {
    crossing <- unique(sorted[decreasing + 1, forecast_unit, with = FALSE])
    out <- data[crossing, on = forecast_unit, nomatch = NULL]
  }
  setorderv(out, cols = c(forecast_unit, "quantile_level"))
  return(out[])
}

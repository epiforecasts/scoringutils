#' @title Find quantile forecasts with crossing quantiles
#'
#' @description
#' Identify quantile forecasts in which the predicted values decrease with
#' increasing quantile level (also called "quantile crossing"), e.g. where the
#' 0.1 quantile is larger than the 0.2 quantile. Such forecasts do not
#' correspond to a valid predictive distribution. Some metrics, e.g.
#' [bias_quantile()], cannot be computed for them.
#'
#' The order of the rows does not matter: predictions are compared after
#' sorting by quantile level within each forecast. Missing values in
#' `predicted` or `quantile_level` are ignored. Equal predictions for
#' different quantile levels are allowed.
#'
#' @inheritParams as_forecast_doc_template
#' @param data A data.frame (or a `forecast_quantile` object) with columns
#'   `predicted` and `quantile_level`, as well as any columns that make up the
#'   forecast unit.
#' @returns A data.table with all rows belonging to forecasts with crossing
#'   quantiles, ordered by forecast unit and quantile level.
#' @export
#' @importFrom checkmate assert_data_frame assert_subset
#' @importFrom data.table setorderv
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
  forecast_unit <- get_forecast_unit(data)

  data <- as.data.table(data)
  data[,
    scoringutils_InternalCrossingCheck := {
      keep <- !is.na(predicted) & !is.na(quantile_level)
      is.unsorted(predicted[keep][order(quantile_level[keep])])
    },
    by = forecast_unit
  ]
  out <- data[(scoringutils_InternalCrossingCheck)]
  out[, scoringutils_InternalCrossingCheck := NULL]
  setorderv(out, cols = c(forecast_unit, "quantile_level"))
  return(out[])
}

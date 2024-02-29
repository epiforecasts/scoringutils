#' @title Get Quantile And Interval Coverage Values For Quantile-Based Forecasts
#'
#' @description Compute interval coverage of central prediction intervals,
#' quantile coverage for predictive quantiles, as well as the deviation between
#' desired and actual coverage to a data.table. Forecasts should be in a
#' quantile format (following the input requirements of `score()`).
#'
#' **Interval coverage**
#'
#' Interval coverage for a given interval range is defined as the proportion of
#' observations that fall within the corresponding central prediction intervals.
#' Central prediction intervals are symmetric around the median and and formed
#' by two quantiles that denote the lower and upper bound. For example, the 50%
#' central prediction interval is the interval between the 0.25 and 0.75
#' quantiles of the predictive distribution.
#'
#' The function `get_coverage()` computes the coverage per central prediction
#' interval. This means that if you set `by` to the unit of a single forecast,
#' interval coverage will always be either `TRUE`
#' (observed value falls within the interval) or `FALSE` (observed value falls
#' outside the interval) and analogously for quantile coverage.
#' Coverage values become meaningful by summarising them across different
#' dimensions, as specified in the `by` argument (thereby returning the
#' proportion of values covered by all prediction intervals/quantiles).
#'
#' **Quantile coverage**
#'
#' Quantile coverage for a given quantile is defined as the proportion of
#' observed values that are smaller than the corresponding predictive quantile.
#' For example, the 0.5 quantile coverage is the proportion of observed values
#' that are smaller than the 0.5 quantile of the predictive distribution.
#' Just as above, for a single observation and the quantile of a single
#' predictive distribution, the value will either be `TRUE` or `FALSE`.
#'
#' **Coverage deviation**
#'
#' The coverage deviation is the difference between the desired coverage
#' (can be either interval or quantile coverage) and the
#' actual coverage. For example, if the desired coverage is 90% and the actual
#' coverage is 80%, the coverage deviation is -0.1.
#'
#' @inheritParams score
#' @return a data.table with the input and columns "interval_coverage",
#' "interval_coverage_deviation", "quantile_coverage",
#' "quantile_coverage_deviation" added.
#' @importFrom data.table setcolorder
#' @importFrom checkmate assert_subset
#' @examples
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   as_forecast() %>%
#'   get_coverage(by = "model")
#' @export
#' @keywords scoring
#' @export
get_coverage <- function(data, by = get_forecast_unit(data)) {
  # input checks ---------------------------------------------------------------
  data <- as_forecast(na.omit(data), forecast_type = "quantile")
  assert_subset(by, names(data))

  # convert to wide interval format and compute interval coverage --------------
  interval_data <- quantile_to_interval(data, format = "wide")
  interval_data[,
                interval_coverage := (observed <= upper) & (observed >= lower)
  ][, c("lower", "upper", "observed") := NULL]
  interval_data[, interval_coverage_deviation :=
                  interval_coverage - interval_range / 100]

  # merge interval range data with original data -------------------------------
  # preparations
  data[, interval_range := get_range_from_quantile(quantile_level)]
  data_cols <- colnames(data) # store so we can reset column order later
  forecast_unit <- get_forecast_unit(data)

  data <- merge(data, interval_data,
                by = unique(c(forecast_unit, "interval_range")))

  # compute quantile coverage and deviation ------------------------------------
  data[, quantile_coverage := observed <= predicted]
  data[, quantile_coverage_deviation := quantile_coverage - quantile_level]

  # summarise coverage values according to `by` and cleanup --------------------
  # reset column order
  new_metrics <- c("interval_coverage", "interval_coverage_deviation",
                   "quantile_coverage", "quantile_coverage_deviation")
  setcolorder(data, unique(c(data_cols, "interval_range", new_metrics)))
  # remove forecast class and convert to regular data.table
  data <- as.data.table(data)
  by <- unique(c(by, "quantile_level", "interval_range"))
  # summarise
  data <- data[, lapply(.SD, mean),
                   by = by,
                   .SDcols = new_metrics
  ]
  return(data[])
}

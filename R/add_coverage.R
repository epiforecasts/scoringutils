#' @title Get Quantile And Interval Coverage Values For Quantile-Based Forecasts
#'
#' @description For a validated forecast object in a quantile-based format
#' (see [as_forecast()] for more information), this function computes
#' - interval coverage of central prediction intervals
#' - quantile coverage for predictive quantiles
#' - the deviation between desired and actual coverage (both for interval and
#' quantile coverage)
#'
#' Coverage values are computed for a specific level of grouping, as specified
#' in the `by` argument. By default, coverage values are computed per model.
#'
#' **Interval coverage**
#'
#' Interval coverage for a given interval range is defined as the proportion of
#' observations that fall within the corresponding central prediction intervals.
#' Central prediction intervals are symmetric around the median and formed
#' by two quantiles that denote the lower and upper bound. For example, the 50%
#' central prediction interval is the interval between the 0.25 and 0.75
#' quantiles of the predictive distribution.
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
#' @return A data.table with columns as specified in `by` and additional
#' columns for the coverage values described above
#' @inheritParams score
#' @param by character vector that denotes the level of grouping for which the
#' coverage values should be computed. By default (`"model"`), one coverage
#' value per model will be returned.
#' @return a data.table with columns "interval_coverage",
#' "interval_coverage_deviation", "quantile_coverage",
#' "quantile_coverage_deviation" and the columns specified in `by`.
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
get_coverage <- function(data, by = "model") {
  # input checks ---------------------------------------------------------------
  data <- as_forecast(na.omit(data), forecast_type = "quantile")

  # remove "quantile_level" and "interval_range" from `by` if present, as these
  # are included anyway
  by <- setdiff(by, c("quantile_level", "interval_range"))
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
  data <- data[, lapply(.SD, mean), by = by, .SDcols = new_metrics]
  return(data[])
}

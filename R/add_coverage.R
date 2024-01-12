#' @title Add Coverage Values to Quantile-Based Forecasts
#'
#' @description Adds interval coverage of central prediction intervals,
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
#' The function `add_coverage()` computes the coverage per central prediction
#' interval, so the interval coverage will always be either `TRUE`
#' (observed value falls within the interval) or `FALSE`  (observed value falls
#' outside the interval). You can summarise the interval coverage values to get
#' the proportion of observations that fall within the central prediction
#' intervals.
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
#' @examples
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   add_coverage()
#' @export
#' @keywords scoring
#' @export
add_coverage <- function(data) {
  stored_attributes <- get_scoringutils_attributes(data)
  data <- as_forecast(data)
  forecast_unit <- get_forecast_unit(data)
  data_cols <- colnames(data) # store so we can reset column order later

  interval_data <- quantile_to_interval(data, format = "wide")
  interval_data[,
    interval_coverage := (observed <= upper) & (observed >= lower)
  ][, c("lower", "upper", "observed") := NULL]

  data[, range := get_range_from_quantile(quantile)]

  data <- merge(data, interval_data, by = unique(c(forecast_unit, "range")))
  data[, interval_coverage_deviation := interval_coverage - range / 100]
  data[, quantile_coverage := observed <= predicted]
  data[, quantile_coverage_deviation := quantile_coverage - quantile]

  # reset column order
  new_rules <- c("interval_coverage", "interval_coverage_deviation",
                   "quantile_coverage", "quantile_coverage_deviation")
  setcolorder(data, unique(c(data_cols, "range", new_rules)))

  # add coverage "rules" to list of stored scoring rules
  # this makes it possible to use `summarise_scores()` later on
  stored_attributes[["score_names"]] <- c(
    stored_attributes[["score_names"]],
    new_rules
  )
  data <- assign_attributes(data, stored_attributes)
  return(data[])
}

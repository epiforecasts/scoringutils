#' @title Add Coverage of Central Prediction Intervals to Forecasts
#'
#' @description Adds a column with the coverage of central prediction intervals
#' to a data.table with forecasts either in a quantile or in a sample-based
#' format (following the input requirements of `score()`).
#'
#' Coverage for a given interval range is defined as the proportion of
#' observations that fall within the corresponding central prediction intervals.
#' Central prediction intervals are symmetric around the median and and formed
#' by two quantiles that denote the lower and upper bound. For example, the 50%
#' central prediction interval is the interval between the 0.25 and 0.75
#' quantiles of the predictive distribution.
#'
#' The coverage values that are added are computed according to the values
#' specified in `by`. If, for example, `by = "model"`, then there will be one
#' coverage value for every model. If `by = c("model", "target_type")`, then
#' there will be one coverage value for every combination of model and target
#' type.
#'
#' @inheritParams score
#' @param by character vector with column names to add the coverage for.
#' @param ranges numeric vector of the ranges of the central prediction intervals
#' for which coverage values shall be added. Ranges should be given as
#' percentages. For example, `ranges = c(50, 90)`
#' will add coverage values for the 50% and 90% central prediction intervals (
#' corresponding to the 0.05, 0.25, 0.75 and 0.95 quantiles of the predictive
#' distribution).
#' @return a data.table with with columns added for the
#' coverage of the central prediction intervals. While the overall data.table
#' is still unsummarised, note that for the coverage columns some level of
#' summary is present according to the value specified in `by`.
#' @examples
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   add_coverage_raw_data(by = c("model", "target_type"))
#' @export
#' @keywords scoring
#' @export
add_coverage_raw_data <- function(data,
                                  by = NULL,
                                  ranges = c(50, 90)) {
  UseMethod("add_coverage_raw_data")
}

#' @description
#' `add_coverage_raw_data.default()` validates the input data,
#' checks the forecast type and calls `add_coverage_raw_data()` again to
#' dispatch to the appropriate method.
#' @rdname add_coverage_raw_data
#' @export
add_coverage_raw_data.default <- function(data,
                                          by = NULL,
                                          ranges = c(50, 90)) {
  data <- validate(data)
  add_coverage_raw_data(data, by = by, ranges = ranges)
}

#' @rdname add_coverage_raw_data
#' @export
add_coverage_raw_data.scoringutils_quantile <- function(data,
                                                        by = NULL,
                                                        ranges = c(50, 90)) {
  stored_attributes <- get_scoringutils_attributes(data)
  data <- remove_na_observed_predicted(data)

  if (is.null(by) && !is.null(stored_attributes[["scoringutils_by"]])) {
    by <- stored_attributes[["scoringutils_by"]]
  } else if (is.null(by)) {
    # Need to check this again.
    # (mentioned in https://github.com/epiforecasts/scoringutils/issues/346)
    by <- get_forecast_unit(data)
  }

  interval_data <- quantile_to_interval(data, format = "wide")
  interval_data[, coverage := ifelse(observed <= upper & observed >= lower, 1, 0)] # nolint
  interval_data[, coverage_deviation := coverage - range / 100]

  summarised <- interval_data[, .(coverage = mean(coverage),
                                  coverage_deviation = mean(coverage_deviation)),
                              by = c(by, "range")][range %in% ranges]

  cast_formula <-
    paste(
      paste(by, collapse = "+"),
      "~",
      "paste0('coverage_', range)"
    )

  coverages <- dcast(
    summarised,
    value.var = "coverage",
    formula = cast_formula
  )

  data_with_coverage <- merge(data, coverages, by = by)
  data_with_coverage <- assign_attributes(
    data_with_coverage, stored_attributes
  )

  return(data_with_coverage[])
}

#' @rdname add_coverage_raw_data
#' @export
add_coverage_raw_data.scoringutils_sample <- function(data,
                                                      by = NULL,
                                                      ranges = c(50, 90)) {
  stored_attributes <- get_scoringutils_attributes(data)
  data <- remove_na_observed_predicted(data)
  if (is.null(by) && !is.null(stored_attributes[["scoringutils_by"]])) {
    by <- stored_attributes[["scoringutils_by"]]
  } else if (is.null(by)) {
    # Need to check this again.
    # (mentioned in https://github.com/epiforecasts/scoringutils/issues/346)
    by <- get_forecast_unit(data)
  }

  lower_quantiles <- (100 - ranges) / 200
  upper_quantiles <- 1 - lower_quantiles
  quantiles <- sort(c(lower_quantiles, upper_quantiles))

  quantile_format <- sample_to_quantile(data, quantiles = quantiles, type = 7)

  coverages <- add_coverage_raw_data.scoringutils_quantile(
    quantile_format,
    by = by,
    ranges = ranges
  )

  coverages <- unique(
    coverages[, c("quantile", "predicted", "observed") := NULL]
  )

  data_with_coverage <- merge(data, coverages, by = by)
  data_with_coverage <- assign_attributes(
    data_with_coverage, stored_attributes
  )
  return(data_with_coverage[])
}

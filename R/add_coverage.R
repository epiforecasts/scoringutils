#' @export
add_coverage_raw_data <- function(data,
                                  by = NULL,
                                  ranges = c(50, 90)) {
  UseMethod("add_coverage_raw_data")
}

#' @export
add_coverage_raw_data.default <- function(data,
                                          by = NULL,
                                          ranges = c(50, 90)) {
  data <- validate(data)
  add_coverage_raw_data(data, by = by, ranges = ranges)
}

#' @export
add_coverage_raw_data.scoringutils_quantile <- function(data,
                                                        by,
                                                        ranges) {
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

#' @export
add_coverage_raw_data.scoringutils_sample <- function(data,
                                                      by,
                                                      ranges) {
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

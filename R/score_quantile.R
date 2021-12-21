score_quantile <- function(data,
                           forecast_unit,
                           metrics,
                           weigh = TRUE,
                           count_median_twice = FALSE,
                           separate_results = TRUE) {

  # make sure to have both quantile as well as range format --------------------
  range_data <- quantile_to_range_long(data,
                                       keep_quantile_col = FALSE)
  # adds the range column to the quantile data set
  quantile_data <- range_long_to_quantile(range_data,
                                          keep_range_col = TRUE)


  # to deal with point forecasts in a quantile format. This in effect adds
  # a third column next to lower and upper after pivoting
  range_data[is.na(range), boundary := "point"]

  range_data <- data.table::dcast(range_data, ... ~ boundary,
                                  value.var = "prediction")

  # if we only score point forecasts, it may be true that there are no columns
  # upper and lower in the data.frame. If so, these need to be added
  if (!all(c("upper", "lower") %in% colnames(range_data))) {
    range_data[, c("upper", "lower") := NA]
  }

  # set up results data.table that will then be modified throughout ------------
  res <- data.table::copy(range_data)

  # calculate scores on range format -------------------------------------------
  if ("interval_score" %in% metrics) {
    # compute separate results if desired
    if (separate_results) {
      outcols <- c("interval_score", "dispersion",
                   "underprediction", "overprediction")
    } else {
      outcols <- "interval_score"
    }
    res <- res[,  eval(outcols) := do.call(scoringutils::interval_score,
                                           list(true_value, lower,
                                                upper,range,
                                                weigh, separate_results = TRUE))]
  }

  # compute coverage for every single observation
  if ("coverage" %in% metrics) {
    res[, coverage := ifelse(true_value <= upper & true_value >= lower, 1, 0)]
    res[, coverage_deviation := coverage - range/100]
  }

  # compute bias
  if ("bias" %in% metrics) {
    res[, bias := quantile_bias(range = range, lower = lower, upper = upper,
                                true_value = unique(true_value)),
        by = forecast_unit]
  }

  # score absolute error for point forecasts
  # these are marked by an NA in range, and a numeric value for point
  if (any(c("ae_point", "aem") %in% metrics)) {
    if ("point" %in% colnames(res)) {
      res[is.na(range) & is.numeric(point),
          ae_point := abs_error(predictions = point, true_value)]
    }
  }

  # calculate scores on quantile format ----------------------------------------
  # compute absolute error of the median
  if ("aem" %in% metrics) {
    quantile_data[, aem := ae_median_quantile(true_value,
                                              prediction,
                                              quantile),
                  by = forecast_unit]
  }

  # compute quantile coverage based on quantile version
  if ("quantile_coverage" %in% metrics) {
    quantile_data[, quantile_coverage := (true_value <= prediction)]
  }

  # merge metrics computed on quantile data (i.e. aem, quantile_coverage) back
  # into metrics computed on range data. One important side effect of this is
  # that it controls whether we count the median twice for the interval score
  # (row is then duplicated) or only once. However, merge only needs to happen
  # if we computed either the interval score or the aem or quantile coverage
  if (any(c("aem", "interval_score", "quantile_coverage") %in% metrics)) {
    # delete unnecessary columns before merging back
    keep_cols <- unique(c(forecast_unit, "quantile", "aem", "quantile_coverage",
                          "boundary", "range"))
    delete_cols <- names(quantile_data)[!(names(quantile_data) %in% keep_cols)]
    quantile_data[, eval(delete_cols) := NULL]

    # duplicate median column before merging if median is to be counted twice
    # if this is false, then the res will have one entry for every quantile,
    # which translates to two rows for every interval, but only one for the median
    if (count_median_twice) {
      median <- quantile_data[quantile == 0.5, ][, boundary := "upper"]
      quantile_data <- data.table::rbindlist(list(quantile_data, median))
    }

    # merge back with other metrics
    merge_cols <- setdiff(keep_cols, c("aem", "quantile_coverage", "quantile",
                                       "boundary"))
    # specify all.x = TRUE as the point forecasts got deleted when
    # going from range to quantile above
    res <- merge(res, quantile_data, by = merge_cols, all.x = TRUE)
  }

  return(res[])
}

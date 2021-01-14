eval_forecasts_quantile <- function(data,
                                    by,
                                    summarise_by,
                                    metrics,
                                    quantiles,
                                    sd,
                                    pit_plots,
                                    interval_score_arguments,
                                    summarised,
                                    verbose) {

  # make sure data is in the correct format ------------------------------------
  # check format
  if ("boundary" %in% names(data)) {
    format <- "range_long_format"
  } else if ("quantile" %in% names(data) & !("range" %in% names(data))) {
    format <- "quantile_format"
  }

  # make sure to have both quantile as well as range format
  if ("quantile" %in% names(data) & !("range" %in% names(data))) {
    data <- scoringutils::quantile_to_range_long(data,
                                                  keep_quantile_col = FALSE)
  }
  quantile_data <- scoringutils::range_long_to_quantile(data,
                                                   keep_range_col = TRUE)


  data <- data.table::dcast(data, ... ~ boundary,
                            value.var = "prediction")

  # update interval_score arguments based on what was provided by user
  interval_score_arguments <- update_list(defaults = list(weigh = TRUE,
                                                          count_median_twice = TRUE,
                                                          separate_results = TRUE),
                                          optional = interval_score_arguments)


  # calculate scores on range format -------------------------------------------
  if ("interval_score" %in% metrics) {
    # compute separate results if desired
    if (interval_score_arguments$separate_results) {
      res <- data[, c("interval_score",
                      "sharpness",
                      "underprediction",
                      "overprediction") := do.call(scoringutils::interval_score,
                                                   c(list(true_value,
                                                          lower,
                                                          upper,
                                                          range),
                                                     interval_score_arguments))]
    } else {
      res <- data[, c("interval_score") := do.call(scoringutils::interval_score,
                                                   c(list(true_value,
                                                          lower,
                                                          upper,
                                                          range),
                                                     interval_score_arguments))]
    }
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
        by = by]
  }


  # calculate scores on quantile format ----------------------------------------
  # compute absolute error of the median
  if ("aem" %in% metrics) {
    quantile_data[, aem := ae_median_quantile(true_value,
                                              prediction,
                                              quantile,
                                              verbose = verbose),
                  by = by]
  }

  # compute quantile coverage based on quantile version
  if ("quantile_coverage" %in% metrics) {
    quantile_data[, quantile_coverage := (true_value <= prediction)]
  }

  # merge only if something was computed
  if (any(c("aem", "quantile_coverage") %in% metrics)) {
    # delete unnecessary columns before merging back
    keep_cols <- unique(c(by, "quantile", "aem", "quantile_coverage",
                          "boundary", "range"))
    delete_cols <- names(quantile_data)[!(names(quantile_data) %in% keep_cols)]
    quantile_data[, eval(delete_cols) := NULL]

    #duplicate median column before merging
    median <- quantile_data[quantile == 0.5, ][, boundary := "upper"]
    quantile_data <- data.table::rbindlist(list(quantile_data, median))

    # merge back with other metrics
    merge_cols <- setdiff(keep_cols, c("aem", "quantile_coverage", "quantile",
                                       "boundary"))
    res <- merge(res, quantile_data, by = merge_cols)
  }


  ############################ pairwise comparisons ############################


  # summarise scores if desired ------------------------------------------------
  if (summarised) {
    # add quantiles for the scores
    if (!is.null(quantiles)) {
      res <- add_quantiles(res,
                           c("interval_score", "coverage",
                             "overprediction", "underprediction",
                             "coverage_deviation", "bias", "sharpness", "aem"),
                           quantiles,
                           by = c(summarise_by))
    }
    # add standard deviation
    if (sd) {
      res <- add_sd(res,
                    varnames = c("interval_score", "bias", "coverage",
                                 "overprediction", "underprediction",
                                 "coverage_deviation", "sharpness", "aem"),
                    by = c(summarise_by))
    }

    # summarise by taking the mean and omitting unnecessary columns
    res <- res[, lapply(.SD, mean, na.rm = TRUE),
               by = c(summarise_by),
               .SDcols = colnames(res) %like%
                 "coverage|bias|sharpness|coverage_deviation|interval_score|overprediction|underprediction|aem"]
  }

  # if neither quantile nor range are in summarise_by, remove coverage and quantile_coverage
  if (!("range" %in% summarise_by | "quantile" %in% summarise_by)) {
    res[, c("coverage", "quantile_coverage") := NULL]
  }

  return(res)
}

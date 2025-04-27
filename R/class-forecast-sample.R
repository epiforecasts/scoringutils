#' @title Create a `forecast` object for sample-based forecasts
#' @inherit as_forecast_doc_template params description
#' @details
#' # Required input
#'
#' The input needs to be a data.frame or similar for the default method
#' with the following columns:
#' - `observed`: Column of type `numeric` with observed values.
#' - `predicted`: Column of type `numeric` with predicted values. Predicted
#'    values represent random samples from the predictive distribution.
#' - `sample_id`: Column of any type with unique identifiers
#'    (unique within a single forecast) for each sample.
#'
#' For convenience, we recommend an additional column `model` holding the name
#' of the forecaster or model that produced a prediction, but this is not
#' strictly necessary.
#'
#' See the [example_sample_continuous] and [example_sample_discrete] data set
#' for an example
#' @inheritSection forecast_types Forecast unit
#' @param ... Unused
#' @family functions to create forecast objects
#' @returns A `forecast` object of class `forecast_sample`
#' @export
#' @keywords as_forecast transform
as_forecast_sample <- function(data, ...) {
  UseMethod("as_forecast_sample")
}


#' @rdname as_forecast_sample
#' @param sample_id (optional) Name of the column in `data` that contains the
#'   sample id. This column will be renamed to "sample_id".
#' @export
#' @importFrom cli cli_warn
as_forecast_sample.default <- function(data,
                                       forecast_unit = NULL,
                                       observed = NULL,
                                       predicted = NULL,
                                       sample_id = NULL,
                                       ...) {
  data <- as_forecast_generic(
    data,
    forecast_unit,
    observed = observed,
    predicted = predicted,
    sample_id = sample_id
  )
  data <- new_forecast(data, "forecast_sample")
  assert_forecast(data)
  return(data)
}


#' @export
#' @rdname assert_forecast
#' @keywords validate-forecast-object
assert_forecast.forecast_sample <- function(
  forecast, forecast_type = NULL, verbose = TRUE, ...
) {
  forecast <- assert_forecast_generic(forecast, verbose)
  assert_forecast_type(forecast, actual = "sample", desired = forecast_type)
  return(invisible(NULL))
}


#' @export
#' @rdname is_forecast
is_forecast_sample <- function(x) {
  inherits(x, "forecast_sample") && inherits(x, "forecast")
}


#' @rdname as_forecast_quantile
#' @details # Converting from `forecast_sample` to `forecast_quantile`
#' When creating a `forecast_quantile` object from a `forecast_sample` object,
#' the quantiles are estimated by computing empircal quantiles from the samples
#' via [quantile()]. Note that empirical quantiles are a biased estimator for
#' the true quantiles in particular in the tails of the distribution and
#' when the number of available samples is low.
#' @param probs A numeric vector of quantile levels for which
#'   quantiles will be computed. Corresponds to the `probs` argument in
#'   [quantile()].
#' @param type Type argument passed down to the quantile function. For more
#'   information, see [quantile()].
#' @importFrom stats quantile
#' @importFrom methods hasArg
#' @importFrom checkmate assert_numeric
#' @export
as_forecast_quantile.forecast_sample <- function(
  data,
  probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
  type = 7,
  ...
) {
  forecast <- copy(data)
  assert_forecast(forecast, verbose = FALSE)
  assert_numeric(probs, min.len = 1)
  reserved_columns <- c("predicted", "sample_id")
  by <- setdiff(colnames(forecast), reserved_columns)

  quantile_level <- unique(
    round(c(probs, 1 - probs), digits = 10)
  )

  forecast <-
    forecast[, .(quantile_level = quantile_level,
                 predicted = quantile(x = predicted, probs = ..probs,
                                      type = ..type, na.rm = TRUE)),
             by = by]

  quantile_forecast <- new_forecast(forecast, "forecast_quantile")
  assert_forecast(quantile_forecast)

  return(quantile_forecast)
}


# #' @importFrom stats na.omit
# #' @importFrom data.table setattr copy
# #' @rdname score
# #' @export
# score.forecast_sample_justatest <- function(forecast, metrics = get_metrics(forecast), ...) {
#   forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
#   forecast_unit <- get_forecast_unit(forecast)
#   metrics <- validate_metrics(metrics)
#   forecast <- as.data.table(forecast)

#   # transpose the forecasts that belong to the same forecast unit
#   f_transposed <- forecast[, .(predicted = list(predicted),
#                                observed = unique(observed),
#                                scoringutils_N = length(list(sample_id))),
#                            by = forecast_unit]

#   if (".scoringutils_group_id" %in% names(f_transposed)) {
#     grouping <- get_grouping(forecast)
#     # metrics are only metrics that take "grouping" as an argument
#     metrics <- metrics[sapply(metrics, function(m) {
#       "grouping" %in% formalArgs(m)
#     })]
#     return(score_forecast_sample_multiv(f_transposed, metrics, grouping, ...))
#   } else {
#     # metrics are only metrics that do not take "grouping" as an argument
#     metrics <- metrics[!sapply(metrics, function(m) {
#       "grouping" %in% formalArgs(m)
#     })]
#     return(score_forecast_sample_univ(f_transposed, metrics, ...))
#   }
# }


#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @rdname score
#' @export
score.forecast_sample <- function(forecast, metrics = get_metrics(forecast), ...) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast_unit <- get_forecast_unit(forecast)
  metrics <- validate_metrics(metrics)
  forecast <- as.data.table(forecast)
  compute_multivariate <- (".scoringutils_group_id" %in% names(forecast))

  # transpose the forecasts that belong to the same forecast unit
  f_transposed <- forecast[, .(
    predicted = list(predicted),
    observed = unique(observed),
    scoringutils_N = length(list(sample_id))
  ),
  by = forecast_unit
  ]

  multivariate_metrics <- metrics[sapply(metrics, function(m) {
    "grouping_id" %in% formalArgs(m)
  })]
  univariate_metrics <- metrics[setdiff(names(metrics), names(multivariate_metrics))]


  f_split <- split(f_transposed, f_transposed$scoringutils_N)

  split_result <- lapply(f_split, function(single_forecast) {

    # create a matrix
    observed <- single_forecast$observed
    predicted <- do.call(rbind, single_forecast$predicted)
    single_forecast[, c("observed", "predicted", "scoringutils_N") := NULL]

    if (!(".scoringutils_group_id" %in% names(single_forecast))) {
      single_forecast <- set_grouping(single_forecast, forecast_unit)
    }
    grouping_id <- single_forecast$.scoringutils_group_id

    univariate_result <- apply_metrics(
      forecast = single_forecast, metrics = univariate_metrics,
      observed = observed, predicted = predicted
    )

    # for multivariate scores, multiple rows collapse to a single score.
    # we therefore create a new data.table, compute scores, and merge back.
    temp_dt <- unique(single_forecast[, .SD, .SDcols = ".scoringutils_group_id"])
    multivariate_result <- apply_metrics(
      forecast = temp_dt, metrics = multivariate_metrics,
      observed = observed, predicted = predicted,
      grouping_id = grouping_id
    )

    result <- merge(univariate_result, multivariate_result, by = ".scoringutils_group_id")
    setcolorder(result, c(setdiff(colnames(result), ".scoringutils_group_id"), ".scoringutils_group_id"))

    return(result)
  })
  scores <- rbindlist(split_result, fill = TRUE)
  scores <- as_scores(scores, metrics = names(metrics))

  # if we're computing multivariate scores, then append "_multiv" to the names of the scores
  if (compute_multivariate && length(multivariate_metrics) > 0) {
    to_rename <- intersect(names(multivariate_metrics), names(scores))
    setnames(scores, to_rename, paste0(to_rename, "_multiv"))
  } else {
    scores[, .scoringutils_group_id := NULL]
  }

  return(scores[])
}


# #' @importFrom stats na.omit
# #' @importFrom data.table setattr copy
# #' @rdname score
# #' @export
# score.forecast_sample_justatest <- function(forecast, metrics = get_metrics(forecast), ...) {
#   forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
#   forecast_unit <- get_forecast_unit(forecast)
#   metrics <- validate_metrics(metrics)
#   forecast <- as.data.table(forecast)

#   # transpose the forecasts that belong to the same forecast unit
#   f_transposed <- forecast[, .(predicted = list(predicted),
#                                observed = unique(observed),
#                                scoringutils_N = length(list(sample_id))),
#                            by = forecast_unit]

#   if (".scoringutils_group_id" %in% names(f_transposed)) {
#     grouping <- get_grouping(forecast)
#     # metrics are only metrics that take "grouping" as an argument
#     metrics <- metrics[sapply(metrics, function(m) {
#       "grouping" %in% formalArgs(m)
#     })]
#     return(score_forecast_sample_multiv(f_transposed, metrics, grouping, ...))
#   } else {
#     # metrics are only metrics that do not take "grouping" as an argument
#     metrics <- metrics[!sapply(metrics, function(m) {
#       "grouping" %in% formalArgs(m)
#     })]
#     return(score_forecast_sample_univ(f_transposed, metrics, ...))
#   }
# }


score_forecast_sample_univ <- function(f_transposed, metrics, ...) {
  # split according to number of samples and do calculations for different
  # sample lengths separately
  f_split <- split(f_transposed, f_transposed$scoringutils_N)

  split_result <- lapply(f_split, function(forecast) {
    # create a matrix
    observed <- forecast$observed
    predicted <- do.call(rbind, forecast$predicted)
    forecast[, c("observed", "predicted", "scoringutils_N") := NULL]

    forecast <- apply_metrics(
      forecast, metrics,
      observed, predicted
    )
    return(forecast)
  })
  scores <- rbindlist(split_result, fill = TRUE)
  scores <- as_scores(scores, metrics = names(metrics))
  return(scores[])
}

# I think I don't actually need this? Because the grouping is handled in scoring function?
# And I can just pass any argument to any function and it will be ignored if it is not needed?
# and then I just have to think about the output and how to name the grouping id there
score_forecast_sample_multiv <- function(f_transposed, metrics, grouping, ...) {
  f_split_multiv <- split(f_transposed, f_transposed$scoringutils_N)

  split_result_multiv <- lapply(f_split_multiv, function(data) {
    observed <- data$observed
    predicted <- do.call(rbind, data$predicted)
    grouping_id <- data$.scoringutils_group_id
    data[, c("observed", "predicted", ".scoringutils_group_id") := NULL]

    result_univ <- apply_metrics(
      data, metrics,
      observed, predicted,
      grouping_id = grouping_id
    )
    return(result_univ)
  })

  scores_multiv <- rbindlist(split_result_multiv, fill = TRUE)
  scores_multiv <- as_scores(scores_multiv, metrics = names(metrics))

  return(scores_multiv[])
}





#' Get default metrics for sample-based forecasts
#'
#' @description
#' For sample-based forecasts, the default scoring rules are:
#' - "crps" = [crps_sample()]
#' - "overprediction" = [overprediction_sample()]
#' - "underprediction" = [underprediction_sample()]
#' - "dispersion" = [dispersion_sample()]
#' - "log_score" = [logs_sample()]
#' - "dss" = [dss_sample()]
#' - "mad" = [mad_sample()]
#' - "bias" = [bias_sample()]
#' - "ae_median" = [ae_median_sample()]
#' - "se_mean" = [se_mean_sample()]
#' @inheritSection illustration-input-metric-sample Input format
#' @inheritParams get_metrics.forecast_binary
#' @export
#' @family get_metrics functions
#' @keywords handle-metrics
#' @examples
#' get_metrics(example_sample_continuous, exclude = "mad")
get_metrics.forecast_sample <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    bias = bias_sample,
    dss = dss_sample,
    crps = crps_sample,
    overprediction = overprediction_sample,
    underprediction = underprediction_sample,
    dispersion = dispersion_sample,
    log_score = logs_sample,
    mad = mad_sample,
    ae_median = ae_median_sample,
    se_mean = se_mean_sample,
    # multivariate scores
    energy_score = energy_score_multivariate
  )
  select_metrics(all, select, exclude)
}


#' @rdname get_pit_histogram
#' @importFrom data.table `:=` as.data.table dcast
#' @importFrom checkmate assert_int assert_numeric
#' @inheritParams pit_histogram_sample
#' @seealso [pit_histogram_sample()]
#' @export
get_pit_histogram.forecast_sample <- function(forecast, num_bins = 10,
                                              breaks = NULL, by, integers = c(
                                                "nonrandom", "random", "ignore"
                                              ), n_replicates = NULL, ...) {
  integers <- match.arg(integers)
  assert_int(num_bins, lower = 1, null.ok = FALSE)
  assert_numeric(breaks, lower = 0, upper = 1, null.ok = TRUE)
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast <- as.data.table(forecast)

  if (is.null(breaks)) {
    quantiles <- seq(0, 1, 1 / num_bins)
  } else {
    quantiles <- unique(c(0, breaks, 1))
  }

  forecast_wide <- data.table::dcast(
    forecast,
    ... ~ paste0("InternalSampl_", sample_id),
    value.var = "predicted"
  )

  bins <- sprintf("[%s,%s)", quantiles[-length(quantiles)], quantiles[-1])
  mids <- (quantiles[-length(quantiles)] + quantiles[-1]) / 2

  pit_histogram <- forecast_wide[, .(
    density = pit_histogram_sample(
      observed = observed,
      predicted = as.matrix(.SD),
      quantiles = quantiles,
      integers = integers,
      n_replicates = n_replicates
    ),
    bin = bins,
    mid = mids
  ),
  by = by,
  .SDcols = grepl("InternalSampl_", names(forecast_wide), fixed = TRUE)
  ]

  return(pit_histogram[])
}


#' Continuous forecast example data
#'
#' A data set with continuous predictions for COVID-19 cases and deaths
#' constructed from data submitted to the European Forecast Hub.
#'
#' The data was created using the script create-example-data.R in the inst/
#' folder (or the top level folder in a compiled package).
#'
#' @format An object of class `forecast_sample` (see [as_forecast_sample()])
#' with the following columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{observed}{observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{predicted}{predicted value}
#'   \item{sample_id}{id for the corresponding sample}
#' }
# nolint start
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_sample_continuous"


#' Discrete forecast example data
#'
#' A data set with integer predictions for COVID-19 cases and deaths
#' constructed from data submitted to the European Forecast Hub.
#'
#' The data was created using the script create-example-data.R in the inst/
#' folder (or the top level folder in a compiled package).
#'
#' @format An object of class `forecast_sample` (see [as_forecast_sample()])
#' with the following columns:
#' \describe{
#'   \item{location}{the country for which a prediction was made}
#'   \item{target_end_date}{the date for which a prediction was made}
#'   \item{target_type}{the target to be predicted (cases or deaths)}
#'   \item{observed}{observed values}
#'   \item{location_name}{name of the country for which a prediction was made}
#'   \item{forecast_date}{the date on which a prediction was made}
#'   \item{model}{name of the model that generated the forecasts}
#'   \item{horizon}{forecast horizon in weeks}
#'   \item{predicted}{predicted value}
#'   \item{sample_id}{id for the corresponding sample}
#' }
# nolint start
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_sample_discrete"


# Add this new function above score.forecast_sample
apply_multivariate_metrics <- function(forecast, metrics, observed, predicted, grouping_id, forecast_unit) {
  # Early return if no metrics or no grouping
  if (length(metrics) == 0 || is.null(grouping_id)) {
    return(NULL)
  }

  # Create group-to-forecast-unit mapping
  group_mapping <- unique(forecast[, c(forecast_unit, ".scoringutils_group_id"), with = FALSE])

  # Calculate metrics directly by group
  metric_names <- names(metrics)
  group_results <- forecast[, lapply(metric_names, function(metric_name) {
    do.call(
      run_safely,
      list(
        observed = observed,
        predicted = predicted,
        grouping_id = .BY[[1]],
        fun = metrics[[metric_name]],
        metric_name = metric_name
      )
    )
  }), by = ".scoringutils_group_id", .SDcols = metric_names]

  # Set column names
  setnames(group_results,
           paste0("V", seq_along(metric_names)),
           metric_names)

  # Skip if no results
  if (nrow(group_results) == 0) {
    return(NULL)
  }

  # Join results back to forecast units and remove grouping ID
  result <- merge(group_mapping, group_results, by = ".scoringutils_group_id")[
    , .SD, .SDcols = c(forecast_unit, metric_names)]

  return(result)
}

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


#' @title Create a `forecast` object for sample-based forecasts
#' @param sample_id (optional) Name of the column in `data` that contains the
#'   sample id. This column will be renamed to "sample_id". Only applicable to
#'   sample-based forecasts.
#' @inheritParams as_forecast
#' @export
#' @family functions to create forecast objects
#' @importFrom cli cli_warn
#' @keywords as_forecast
as_forecast_sample <- function(data,
                               forecast_unit = NULL,
                               observed = NULL,
                               predicted = NULL,
                               sample_id = NULL) {
  assert_character(sample_id, len = 1, null.ok = TRUE)
  assert_subset(sample_id, names(data), empty.ok = TRUE)
  if (!is.null(sample_id)) {
    setnames(data, old = sample_id, new = "sample_id")
  }

  data <- as_forecast_generic(data, forecast_unit, observed, predicted)
  data <- new_forecast(data, "forecast_sample")
  assert_forecast(data)
  return(data)
}


#' @export
#' @rdname is_forecast
is_forecast_sample <- function(x) {
  inherits(x, "forecast_sample") && inherits(x, "forecast")
}


#' @rdname as_forecast_quantile
#' @description
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
#' @family `get_metrics` functions
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
    se_mean = se_mean_sample
  )
  select_metrics(all, select, exclude)
}


#' @importFrom stats na.omit
#' @importFrom data.table setattr copy
#' @rdname score
#' @export
score.forecast_sample <- function(forecast, metrics = get_metrics(forecast), ...) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast_unit <- get_forecast_unit(forecast)
  metrics <- validate_metrics(metrics)
  forecast <- as.data.table(forecast)

  # transpose the forecasts that belong to the same forecast unit
  f_transposed <- forecast[, .(predicted = list(predicted),
                               observed = unique(observed),
                               scoringutils_N = length(list(sample_id))),
                           by = forecast_unit]

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


#' @rdname get_pit
#' @importFrom stats na.omit
#' @importFrom data.table `:=` as.data.table dcast
#' @inheritParams pit_sample n_replicates
#' @export
get_pit.forecast_sample <- function(forecast, by, n_replicates = 100, ...) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast <- as.data.table(forecast)

  # if prediction type is not quantile, calculate PIT values based on samples
  forecast_wide <- data.table::dcast(forecast,
                                     ... ~ paste0("InternalSampl_", sample_id),
                                     value.var = "predicted"
  )

  pit <- forecast_wide[, .(pit_value = pit_sample(
    observed = observed,
    predicted = as.matrix(.SD)
  )),
  by = by,
  .SDcols = grepl("InternalSampl_", names(forecast_wide), fixed = TRUE)
  ]

  return(pit[])
}


#' Continuous forecast example data
#'
#' A data set with continuous predictions for COVID-19 cases and deaths
#' constructed from data submitted to the European Forecast Hub.
#'
#' The data was created using the script create-example-data.R in the inst/
#' folder (or the top level folder in a compiled package).
#'
#' @format An object of class `forecast_sample` (see [as_forecast()]) with the
#' following columns:
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
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
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
#' @format An object of class `forecast_sample` (see [as_forecast()]) with the
#' following columns:
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
#' @source \url{https://github.com/european-modelling-hubs/covid19-forecast-hub-europe/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/}
# nolint end
"example_sample_discrete"

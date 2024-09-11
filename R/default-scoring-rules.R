#' @title Select metrics from a list of functions
#'
#' @description
#' Helper function to return only the scoring rules selected by
#' the user from a list of possible functions.
#'
#' @param metrics A list of scoring functions.
#' @param select A character vector of scoring rules to select from the list. If
#'   `select` is `NULL` (the default), all possible scoring rules are returned.
#' @param exclude A character vector of scoring rules to exclude from the list.
#'   If `select` is not `NULL`, this argument is ignored.
#' @return A list of scoring functions.
#' @keywords handle-metrics
#' @importFrom checkmate assert_subset assert_list
#' @export
#' @examples
#' select_metrics(
#'   metrics = get_metrics(example_binary),
#'   select = "brier_score"
#' )
#' select_metrics(
#'   metrics = get_metrics(example_binary),
#'   exclude = "log_score"
#' )
select_metrics <- function(metrics, select = NULL, exclude = NULL) {
  assert_character(x = c(select, exclude), null.ok = TRUE)
  assert_list(metrics, names = "named")
  allowed <- names(metrics)

  if (is.null(select) && is.null(exclude)) {
    return(metrics)
  }
  if (is.null(select)) {
    assert_subset(exclude, allowed)
    select <- allowed[!allowed %in% exclude]
    return(metrics[select])
  }
  assert_subset(select, allowed)
  return(metrics[select])
}

#' @title Get metrics available for scoring a forecast object
#' `get_metrics()` is a generic function with methods for all forecast types.
#' Called on a `forecast` object (see [as_forecast()], it returns a list of
#' functions that can be used for scoring.
#' @inheritParams score
#' @param select A character vector of scoring rules to select from the list. If
#'   `select` is `NULL` (the default), all possible scoring rules are returned.
#' @param exclude A character vector of scoring rules to exclude from the list.
#'   If `select` is not `NULL`, this argument is ignored.
#' @return A list of scoring functions.
#' @export
get_metrics <- function(forecast, select = NULL, exclude = NULL) {
  UseMethod("get_metrics")
}

#' @rdname get_metrics
#' @description
#' For binary forecasts, the default scoring rules are:
#' - "brier_score" = [brier_score()]
#' - "log_score" = [logs_binary()]
#' @inheritSection illustration-input-metric-binary-point Input format
#' @export
#' @examples
#' get_metrics(example_binary)
#' get_metrics(example_binary, select = "brier_score")
#' get_metrics(example_binary, exclude = "log_score")
get_metrics.forecast_binary <- function(forecast, select = NULL, exclude = NULL) {
  all <- list(
    brier_score = brier_score,
    log_score = logs_binary
  )
  select_metrics(all, select, exclude)
}

#' @rdname get_metrics
#' @description
#' For nominal forecasts, the default scoring rule is:
#' - "log_score" = [logs_nominal()]
#' @export
#' @examples
#' get_metrics(example_nominal)
get_metrics.forecast_nominal <- function(forecast, select = NULL, exclude = NULL) {
  all <- list(
    log_score = logs_nominal
  )
  select_metrics(all, select, exclude)
}

#' @rdname get_metrics
#' @description
#' For point forecasts, the default scoring rules are:
#' - "ae_point" = [ae()][Metrics::ae()]
#' - "se_point" = [se()][Metrics::se()]
#' - "ape" = [ape()][Metrics::ape()]
#' @inheritSection illustration-input-metric-binary-point Input format
#' @export
#' @examples
#' get_metrics(example_point, select = "ape")
get_metrics.forecast_point <- function(forecast, select = NULL, exclude = NULL) {
  all <- list(
    ae_point = Metrics::ae,
    se_point = Metrics::se,
    ape = Metrics::ape
  )
  select_metrics(all, select, exclude)
}

#' @rdname get_metrics
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
#' @export
#' @examples
#' get_metrics(example_sample_continuous, exclude = "mad")
get_metrics.forecast_sample <- function(forecast, select = NULL, exclude = NULL) {
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

#' @rdname get_metrics
#' @description
#' For quantile-based forecasts, the default scoring rules are:
#' - "wis" = [wis]
#' - "overprediction" = [overprediction_quantile()]
#' - "underprediction" = [underprediction_quantile()]
#' - "dispersion" = [dispersion_quantile()]
#' - "bias" = [bias_quantile()]
#' - "interval_coverage_50" = [interval_coverage()]
#' - "interval_coverage_90" = purrr::partial(
#'      interval_coverage, interval_range = 90
#'    )
#' - "interval_coverage_deviation" = [interval_coverage_deviation()],
#' - "ae_median" = [ae_median_quantile()]
#'
#' Note: The `interval_coverage_90` scoring rule is created by modifying
#' [interval_coverage()], making use of the function [purrr::partial()].
#' This construct allows the function to deal with arbitrary arguments in `...`,
#' while making sure that only those that [interval_coverage()] can
#' accept get passed on to it. `interval_range = 90` is set in the function
#' definition, as passing an argument `interval_range = 90` to [score()] would
#' mean it would also get passed to `interval_coverage_50`.
#' @inheritSection illustration-input-metric-quantile Input format
#' @export
#' @importFrom purrr partial
#' @examples
#' get_metrics(example_quantile, select = "wis")
get_metrics.forecast_quantile <- function(forecast, select = NULL, exclude = NULL) {
  all <- list(
    wis = wis,
    overprediction = overprediction_quantile,
    underprediction = underprediction_quantile,
    dispersion = dispersion_quantile,
    bias = bias_quantile,
    interval_coverage_50 = interval_coverage,
    interval_coverage_90 = purrr::partial(
      interval_coverage, interval_range = 90
    ),
    interval_coverage_deviation = interval_coverage_deviation,
    ae_median = ae_median_quantile
  )
  select_metrics(all, select, exclude)
}

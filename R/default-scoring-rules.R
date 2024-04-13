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
#' @return A list of scoring rules.
#' @keywords metric
#' @importFrom checkmate assert_subset assert_list
#' @export
#' @examples
#' select_metrics(
#'   metrics = metrics_binary(),
#'   select = "brier_score"
#' )
#' select_metrics(
#'   metrics = metrics_binary(),
#'   exclude = "log_score"
#' )
select_metrics <- function(metrics, select = NULL, exclude = NULL) {
  assert_character(x = c(select, exclude), null.ok = TRUE)
  assert_list(metrics, names = "named")
  allowed <- names(metrics)

  if (is.null(select) && is.null(exclude)) {
    return(metrics)
  } else if (is.null(select)) {
    assert_subset(exclude, allowed)
    select <- allowed[!allowed %in% exclude]
    return(metrics[select])
  } else {
    assert_subset(select, allowed)
    return(metrics[select])
  }
}

#' Customises a metric function with additional arguments.
#'
#' @description
#' This function takes a metric function and additional arguments, and returns
#' a new function that includes the additional arguments when calling the
#' original metric function.
#'
#' @param metric The metric function to be customised.
#' @param ... Additional arguments to be included when calling the metric
#'   function.
#'
#' @return A customised metric function.
#' @keywords metric
#'
#' @export
#' @examples
#' # Create a customised metric function
#' custom_metric <- customise_metric(mean, na.rm = TRUE)
#'
#' # Use the customised metric function
#' values <- c(1, 2, NA, 4, 5)
#' custom_metric(values)
customise_metric <- function(metric, ...) {
  assert_function(metric)
  dots <- list(...)
  customised_metric <- function(...) {
    do.call(metric, c(list(...), dots))
  }
  return(customised_metric)
}


#' @rdname customise_metric
#' @keywords metric
#' @export
customize_metric <- customise_metric


#' @title Default metrics and scoring rules for binary forecasts
#' @description
#' Helper function that returns a named list of default
#' scoring rules suitable for binary forecasts.
#'
#' The default scoring rules are:
#' - "brier_score" = [brier_score()]
#' - "log_score" = [logs_binary()]
#' @inherit select_metrics params return
#' @export
#' @keywords metric
#' @examples
#' metrics_binary()
#' metrics_binary(select = "brier_score")
#' metrics_binary(exclude = "log_score")
metrics_binary <- function(select = NULL, exclude = NULL) {
  all <- list(
    brier_score = brier_score,
    log_score = logs_binary
  )
  selected <- select_metrics(all, select, exclude)
  return(selected)
}


#' @title Default metrics and scoring rules for point forecasts
#' @description
#' Helper function that returns a named list of default
#' scoring rules suitable for point forecasts.
#'
#' The default scoring rules are:
#' - "ae_point" = [ae()][Metrics::ae()]
#' - "se_point" = [se()][Metrics::se()]
#' - "ape" = [ape()][Metrics::ape()]
#' @inherit select_metrics params return
#' @export
#' @keywords metric
#' @examples
#' metrics_point()
#' metrics_point(select = "ape")
metrics_point <- function(select = NULL, exclude = NULL) {
  all <- list(
    ae_point = Metrics::ae,
    se_point = Metrics::se,
    ape = Metrics::ape
  )
  selected <- select_metrics(all, select, exclude)
  return(selected)
}


#' @title Default metrics and scoring rules sample-based forecasts
#' @description
#' Helper function that returns a named list of default
#' scoring rules suitable for forecasts in a sample-based format.
#'
#' The default scoring rules are:
#' - "crps" = [crps_sample()]
#' - "log_score" = [logs_sample()]
#' - "dss" = [dss_sample()]
#' - "mad" = [mad_sample()]
#' - "bias" = [bias_sample()]
#' - "ae_median" = [ae_median_sample()]
#' - "se_mean" = [se_mean_sample()]
#' @inherit select_metrics params return
#' @export
#' @keywords metric
#' @examples
#' metrics_sample()
#' metrics_sample(select = "mad")
metrics_sample <- function(select = NULL, exclude = NULL) {
  all <- list(
    bias = bias_sample,
    dss = dss_sample,
    crps = crps_sample,
    log_score = logs_sample,
    mad = mad_sample,
    ae_median = ae_median_sample,
    se_mean = se_mean_sample
  )
  selected <- select_metrics(all, select, exclude)
  return(selected)
}


#' @title Default metrics and scoring rules for quantile-based forecasts
#' @description
#' Helper function that returns a named list of default
#' scoring rules suitable for forecasts in a quantile-based format.
#'
#' The default scoring rules are:
#' - "wis" = [wis]
#' - "overprediction" = [overprediction()]
#' - "underprediction" = [underprediction()]
#' - "dispersion" = [dispersion()]
#' - "bias" = [bias_quantile()]
#' - "interval_coverage_50" = [interval_coverage()]
#' - "interval_coverage_90" = function(...) \{
#'      run_safely(..., interval_range = 90, fun = [interval_coverage])
#'   \}
#' - "interval_coverage_deviation" = [interval_coverage_deviation()],
#' - "ae_median" = [ae_median_quantile()]
#'
#' Note: The `interval_coverage_90` scoring rule is created as a wrapper around
#' [interval_coverage()], making use of the function [run_safely()].
#' This construct allows the function to deal with arbitrary arguments in `...`,
#' while making sure that only those that [interval_coverage()] can
#' accept get passed on to it. `interval_range = 90` is set in the function
#' definition, as passing an argument `interval_range = 90` to [score()] would
#' mean it would also get passed to `interval_coverage_50`.
#' @inherit select_metrics params return
#' @export
#' @keywords metric
#' @examples
#' metrics_quantile()
#' metrics_quantile(select = "wis")
metrics_quantile <- function(select = NULL, exclude = NULL) {
  all <- list(
    wis = wis,
    overprediction = overprediction,
    underprediction = underprediction,
    dispersion = dispersion,
    bias = bias_quantile,
    interval_coverage_50 = interval_coverage,
    interval_coverage_90 = customise_metric(
      interval_coverage, interval_range = 90
    ),
    interval_coverage_deviation = interval_coverage_deviation,
    ae_median = ae_median_quantile
  )
  selected <- select_metrics(all, select, exclude)
  return(selected)
}

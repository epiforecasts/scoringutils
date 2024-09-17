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
#'   exclude = "neg_log_score"
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

#' Get metrics
#'
#' @description
#' Generic function to to obtain default metrics availble for scoring or metrics
#' that were used for scoring.
#'
#' - If called on `forecast` object it returns a list of functions that can be
#' used for scoring.
#' - If called on a `scores` object (see [score()]), it returns a character vector
#' with the names of the metrics that were used for scoring.
#'
#' See the documentation for the actual methods in the `See Also` section below
#' for more details. Alternatively call `?get_metrics.<forecast_type>` or
#' `?get_metrics.scores`.
#'
#' @param x A `forecast` or `scores` object.
#' @param ... Additional arguments passed to the method.
#' @details
#' See [as_forecast()] for more information on `forecast` objects and [score()]
#' for more information on `scores` objects.
#'
#' @family `get_metrics` functions
#' @keywords handle-metrics
#' @export
get_metrics <- function(x, ...) {
  UseMethod("get_metrics")
}


#' Get default metrics for binary forecasts
#'
#' @description
#' For binary forecasts, the default scoring rules are:
#' - "brier_score" = [brier_score()]
#' - "neg_log_score" = [logs_binary()]
#' @inheritSection illustration-input-metric-binary-point Input format
#' @param x A forecast object (a validated data.table with predicted and
#'   observed values, see [as_forecast()]).
#' @param select A character vector of scoring rules to select from the list. If
#'   `select` is `NULL` (the default), all possible scoring rules are returned.
#' @param exclude A character vector of scoring rules to exclude from the list.
#'   If `select` is not `NULL`, this argument is ignored.
#' @param ... unused
#' @return A list of scoring functions.
#' @export
#' @family `get_metrics` functions
#' @keywords handle-metrics
#' @examples
#' get_metrics(example_binary)
#' get_metrics(example_binary, select = "brier_score")
#' get_metrics(example_binary, exclude = "neg_log_score")
get_metrics.forecast_binary <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    brier_score = brier_score,
    neg_log_score = logs_binary
  )
  select_metrics(all, select, exclude)
}


#' Get default metrics for nominal forecasts
#' @inheritParams get_metrics.forecast_binary
#' @description
#' For nominal forecasts, the default scoring rule is:
#' - "neg_log_score" = [logs_nominal()]
#' @export
#' @family `get_metrics` functions
#' @keywords handle-metrics
#' @examples
#' get_metrics(example_nominal)
get_metrics.forecast_nominal <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    neg_log_score = logs_nominal
  )
  select_metrics(all, select, exclude)
}


#' Get default metrics for point forecasts
#'
#' @description
#' For point forecasts, the default scoring rules are:
#' - "ae_point" = [ae()][Metrics::ae()]
#' - "se_point" = [se()][Metrics::se()]
#' - "ape" = [ape()][Metrics::ape()]
#'
#' A note of caution: Every scoring rule for a point forecast
#' is implicitly minimised by a specific aspect of the predictive distribution
#' (see Gneiting, 2011).
#'
#' The mean squared error, for example, is only a meaningful scoring rule if
#' the forecaster actually reported the mean of their predictive distribution
#' as a point forecast. If the forecaster reported the median, then the mean
#' absolute error would be the appropriate scoring rule. If the scoring rule
#' and the predictive task do not align, the results will be misleading.
#'
#' Failure to respect this correspondence can lead to grossly misleading
#' results! Consider the example in the section below.
#' @inheritSection illustration-input-metric-binary-point Input format
#' @inheritParams get_metrics.forecast_binary
#' @export
#' @family `get_metrics` functions
#' @keywords handle-metrics
#' @examples
#' get_metrics(example_point, select = "ape")
#'
#' library(magrittr)
#' set.seed(123)
#' n <- 500
#' observed <- rnorm(n, 5, 4)^2
#'
#' predicted_mu <- mean(observed)
#' predicted_not_mu <- predicted_mu - rnorm(n, 10, 2)
#'
#' df <- data.frame(
#'   model = rep(c("perfect", "bad"), each = n),
#'   predicted = c(rep(predicted_mu, n), predicted_not_mu),
#'   observed = rep(observed, 2),
#'   id = rep(1:n, 2)
#' ) %>%
#'   as_forecast_point()
#' score(df) %>%
#'   summarise_scores()
#' @references
#' Making and Evaluating Point Forecasts, Gneiting, Tilmann, 2011,
#' Journal of the American Statistical Association.
get_metrics.forecast_point <- function(x, select = NULL, exclude = NULL, ...) {
  all <- list(
    ae_point = Metrics::ae,
    se_point = Metrics::se,
    ape = Metrics::ape
  )
  select_metrics(all, select, exclude)
}


#' Get default metrics for sample-based forecasts
#'
#' @description
#' For sample-based forecasts, the default scoring rules are:
#' - "crps" = [crps_sample()]
#' - "overprediction" = [overprediction_sample()]
#' - "underprediction" = [underprediction_sample()]
#' - "dispersion" = [dispersion_sample()]
#' - "neg_log_score" = [logs_sample()]
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
    neg_log_score = logs_sample,
    mad = mad_sample,
    ae_median = ae_median_sample,
    se_mean = se_mean_sample
  )
  select_metrics(all, select, exclude)
}


#' Get default metrics for quantile-based forecasts
#'
#' @description
#' For quantile-based forecasts, the default scoring rules are:
#' - "wis" = [wis()]
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
#' @inheritParams get_metrics.forecast_binary
#' @export
#' @family `get_metrics` functions
#' @keywords handle-metrics
#' @importFrom purrr partial
#' @examples
#' get_metrics(example_quantile, select = "wis")
get_metrics.forecast_quantile <- function(x, select = NULL, exclude = NULL, ...) {
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

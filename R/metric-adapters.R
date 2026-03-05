#' @title Adapt external metrics for use with scoringutils
#'
#' @description
#' These adapter functions wrap external scoring functions so they can be
#' used with [score()]. They handle common incompatibilities between
#' scoringutils' calling conventions and those of external packages.
#'
#' - `metric_adapt_binary_numeric()` wraps a metric that expects numeric
#'   0/1 `observed` values, converting the factor input that scoringutils
#'   uses for binary forecasts.
#'
#' - `metric_adapt_swap_args()` wraps a metric that takes
#'   `function(predicted, observed)` instead of the
#'   `function(observed, predicted)` convention used by scoringutils.
#'
#' The returned functions can be passed directly to [score()] via the
#' `metrics` argument, and are compatible with [purrr::partial()] for
#' further customization.
#'
#' @param fun A function to wrap. For `metric_adapt_binary_numeric()`,
#'   `fun` should expect numeric `observed` (0/1) as its first argument.
#'   For `metric_adapt_swap_args()`, `fun` should expect `predicted` as its
#'   first argument and `observed` as its second.
#'
#' @returns A function with formals `(observed, predicted, ...)` that can be
#'   used as a metric in [score()].
#'
#' @keywords handle-metrics
#' @importFrom checkmate assert_function
#' @examples
#' # Wrap a metric that expects numeric 0/1 observed values
#' numeric_brier <- function(observed, predicted) {
#'   (observed - predicted)^2
#' }
#' adapted_brier <- metric_adapt_binary_numeric(numeric_brier)
#'
#' # Use inside score()
#' score(example_binary,
#'       metrics = list(custom_brier = adapted_brier))
#'
#' # Wrap a metric that takes (predicted, observed) order
#' swapped_ae <- function(predicted, observed) {
#'   abs(predicted - observed)
#' }
#' adapted_ae <- metric_adapt_swap_args(swapped_ae)
#' score(example_point,
#'       metrics = list(custom_ae = adapted_ae))
#' @name metric-adapters
NULL


#' @rdname metric-adapters
#' @export
metric_adapt_binary_numeric <- function(fun) {
  assert_function(fun)
  adapted <- function(observed, predicted, ...) {
    observed <- as.numeric(observed) - 1
    fun(observed, predicted, ...)
  }
  return(adapted)
}


#' @rdname metric-adapters
#' @export
metric_adapt_swap_args <- function(fun) {
  assert_function(fun)
  adapted <- function(observed, predicted, ...) {
    fun(predicted, observed, ...)
  }
  return(adapted)
}

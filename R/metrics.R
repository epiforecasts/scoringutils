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
#' @returns A list of scoring functions.
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

#' Get metrics
#'
#' @description
#' Generic function to to obtain default metrics available for scoring or metrics
#' that were used for scoring.
#'
#' - If called on a `forecast` object it returns a list of functions that can be
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
#'
#' @family get_metrics functions
#' @keywords handle-metrics
#' @export
get_metrics <- function(x, ...) {
  UseMethod("get_metrics")
}

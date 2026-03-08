#' @title Variogram score for multivariate point forecasts
#' @description
#' Compute the variogram score for multivariate point forecasts,
#' treating each point forecast as a single-sample ensemble.
#' This is a thin wrapper around
#' [variogram_score_multivariate()] with `w = NULL`.
#'
#' See [variogram_score_multivariate()] for details on the
#' variogram score and its parameters.
#' @inheritParams variogram_score_multivariate
#' @inherit variogram_score_multivariate return references
#' @param predicted Numeric matrix with one column, where each row
#'   corresponds to a target within a multivariate group.
#' @importFrom checkmate assert_numeric
#' @export
#' @keywords metric
# nolint start: object_name_linter
variogram_score_multivariate_point <- function(
    observed, predicted, mv_group_id,
    w_vs = NULL, p = 0.5
) {
  assert_numeric(observed, min.len = 1)
  assert_numeric(as.vector(predicted), min.len = 1)
  assert_numeric(mv_group_id, len = length(observed))
  variogram_score_multivariate(
    observed = observed,
    predicted = predicted,
    mv_group_id = mv_group_id,
    w = NULL,
    w_vs = w_vs,
    p = p
  )
}
# nolint end

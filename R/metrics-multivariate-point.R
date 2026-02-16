#' @title Variogram score for multivariate point forecasts
#' @description
#' Compute the variogram score
#' (see \link[scoringRules:vs_sample]{scoringRules::vs_sample})
#' for each group defined by `mv_group_id`, treating each point
#' forecast as a single-sample ensemble.
#' @inheritParams ae_median_sample
#' @inheritParams assert_input_multivariate_sample
#' @param w_vs Numeric matrix of weights for the variogram score.
#'   See [scoringRules::vs_sample()] for details.
#' @param p Numeric, order of the variogram score.
#'   Defaults to 0.5. See [scoringRules::vs_sample()] for details.
#' @importFrom scoringRules vs_sample
#' @importFrom checkmate assert_numeric
#' @export
#' @keywords metric
variogram_score_multivariate_point <- function(observed, predicted, mv_group_id, w_vs = NULL, p = 0.5) { # nolint: object_name_linter line_length_linter
  assert_numeric(observed, min.len = 1)
  assert_numeric(as.vector(predicted), min.len = 1)
  assert_numeric(mv_group_id, len = length(observed))
  unique_groups <- unique(mv_group_id)

  vs <- vapply(unique_groups, function(group) {
    idx <- which(mv_group_id == group)
    scoringRules::vs_sample(
      y = observed[idx],
      dat = predicted[idx, , drop = FALSE],
      w_vs = w_vs,
      p = p
    )
  }, numeric(1))

  names(vs) <- unique_groups
  return(vs)
}

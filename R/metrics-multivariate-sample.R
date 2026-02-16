# These metrics are metrics for the `forecast_sample` class, but accept an
# additional `mv_group_id` argument.
# ==============================================================================


#' @title Assert that inputs are correct for sample-based forecast
#' @description
#' Function assesses whether the inputs correspond to the requirements for
#' scoring sample-based forecasts.
#' @param predicted Input to be checked. Should be a numeric nxN matrix of
#'   predictive samples, n (number of rows) being the number of data points and
#'   N (number of columns) the number of samples per forecast.
#'   If `observed` is just a single number, then predicted values can just be a
#'   vector of size N.
#' @param mv_group_id Numeric vector of length n with ids indicating the
#'   grouping of predicted values. Conceptually, each row of the `predicted`
#'   matrix _could_ be seen as a separate (univariate) forecast.
#'   The grouping id then groups several of those forecasts together, treating
#'   them as a single multivariate forecast.
#' @importFrom scoringRules es_sample
#' @inherit document_assert_functions params return
#' @keywords internal_input_check
assert_input_multivariate_sample <- function(observed, predicted, mv_group_id) { # nolint
  assert_input_sample(observed, predicted)
  assert_numeric(mv_group_id, len = length(observed))
  return(invisible(NULL))
}


#' @title Energy score for multivariate forecasts
#' @description
#' Compute the multivariate energy score
#' (see \link[scoringRules:es_sample]{scoringRules::es_sample})
#' for each group defined by `mv_group_id`.
#' @inheritParams ae_median_sample
#' @inheritParams assert_input_multivariate_sample
#' @inherit scoringRules::es_sample params
#' @keywords internal_input_check
#' @export
energy_score_multivariate <- function(observed, predicted, mv_group_id, w = NULL) {
  assert_input_multivariate_sample(observed, predicted, mv_group_id)
  unique_groups <- unique(mv_group_id)

  energy_score <- vapply(unique_groups, function(group) {
    idx <- which(mv_group_id == group)
    es_sample(y = observed[idx], dat = predicted[idx, , drop = FALSE], w = w)
  }, numeric(1))

  names(energy_score) <- unique_groups
  return(energy_score)
}


#' Variogram score for multivariate forecasts
#'
#' @description
#' Compute the variogram score for multivariate forecasts.
#' The variogram score (Scheuerer and Hamill, 2015) evaluates the
#' dependence structure of multivariate forecasts by comparing
#' predicted pairwise differences against observed pairwise
#' differences.
#'
#' The score is computed using
#' [scoringRules::vs_sample()].
#'
#' @inheritParams energy_score_multivariate
#' @param w_vs Optional non-negative weight matrix. If not `NULL`,
#'   must be a square matrix with dimensions equal to the number
#'   of targets within each multivariate group.
#' @param p Numeric, order of the variogram score.
#'   Typical choices are 0.5 (default, more robust) and 1.
#' @return A named numeric vector of scores, one per multivariate
#'   group. Lower values are better.
#' @references
#' Scheuerer, M. and Hamill, T.M. (2015). Variogram-Based
#' Proper Scoring Rules for Probabilistic Forecasts of
#' Multivariate Quantities. *Monthly Weather Review*, 143,
#' 1321-1334.
#' @importFrom scoringRules vs_sample
#' @export
#' @keywords metric
variogram_score_multivariate <- function(
  observed, predicted, mv_group_id,
  w = NULL, w_vs = NULL, p = 0.5
) {
  assert_input_multivariate_sample(
    observed, predicted, mv_group_id
  )
  unique_groups <- unique(mv_group_id)

  variogram_score <- vapply(
    unique_groups, function(group) {
      idx <- which(mv_group_id == group)
      vs_sample(
        y = observed[idx],
        dat = predicted[idx, , drop = FALSE],
        w = w, w_vs = w_vs, p = p
      )
    }, numeric(1)
  )

  names(variogram_score) <- unique_groups
  return(variogram_score)
}

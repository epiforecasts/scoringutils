# These metrics are metrics for the `forecast_sample` class, but accept an
# additional `grouping_id` argument.
#' @title Assert that inputs are correct for sample-based forecast
#' @description
#' Function assesses whether the inputs correspond to the requirements for
#' scoring sample-based forecasts.
#' @param predicted Input to be checked. Should be a numeric nxN matrix of
#'   predictive samples, n (number of rows) being the number of data points and
#'   N (number of columns) the number of samples per forecast.
#'   If `observed` is just a single number, then predicted values can just be a
#'   vector of size N.
#' @param grouping_id Numeric vector of length n with ids indicating the
#'   grouping of predicted values.
#' @importFrom scoringRules es_sample
#' @inherit document_assert_functions params return
#' @keywords internal_input_check
assert_input_sample_multivariate <- function(observed, predicted, grouping_id) {
  assert_input_sample(observed, predicted)
  assert_numeric(grouping_id, len = length(observed))
  return(invisible(NULL))
}

#' @title Energy score for multivariate forecasts
#'
#' Compute the multivariate energy score (see )
#' @inheritParams ae_median_sample
#' @inherit scoringRules::es_sample params
#' @keywords internal_input_check
#' @export
energy_score_multivariate <- function(observed, predicted, grouping_id, w = NULL) {
  assert_input_sample_multivariate(observed, predicted, grouping_id)
  unique_groups <- unique(grouping_id)

  energy_score <- vapply(unique_groups, function(group_id) {
    idx <- which(grouping_id == group_id)
    es_sample(y = observed[idx], dat = predicted[idx, , drop = FALSE], w = w)
  }, numeric(1))

  names(energy_score) <- unique_groups
  return(energy_score)
}

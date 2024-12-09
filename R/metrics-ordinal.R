#' @title Assert that inputs are correct for ordinal forecasts
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring ordinal forecasts.
#' @param observed Input to be checked. Should be an ordered factor of length n
#'   with N levels holding the observed values. n is the number of observations
#'   and N is the number of possible outcomes the observed values can assume.
#' @param predicted Input to be checked. Should be nxN matrix of predicted
#'   probabilities, n (number of rows) being the number of data points and N
#'   (number of columns) the number of possible outcomes the observed values
#'   can assume.
#'   If `observed` is just a single number, then predicted can just be a
#'   vector of size N.
#'   Values represent the probability that the corresponding value
#'   in `observed` will be equal to factor level referenced in `predicted_label`.
#' @param predicted_label Ordered factor of length N with N levels, where N is
#'   the number of possible outcomes the observed values can assume.
#' @importFrom checkmate assert_factor assert_numeric assert_set_equal
#' @inherit document_assert_functions return
#' @keywords internal_input_check
assert_input_ordinal <- function(observed, predicted, predicted_label) {
  assert_input_categorical(observed, predicted, predicted_label, ordered = TRUE)

  if (!identical(levels(predicted_label), levels(observed))) {
    cli_abort(
      "Levels of `predicted_label` and `observed` must be identical
      and in the same order. Found levels {.val {levels(predicted_label)}}
      and {.val {levels(observed)}}."
    )
  }
  return(invisible(NULL))
}


#' Ranked Probability Score for ordinal outcomes
#'
#' @description
#' The Ranked Probability Score (RPS) measures the difference between the predicted
#' and observed cumulative distribution functions. It is a proper scoring rule that
#' takes the ordering of categories into account. Small values are better
#' (best is zero, worst is N - 1 where N is the number of categories).
#' @param observed A factor of length n with N levels holding the observed
#'   values.
#' @param predicted nxN matrix of predictive probabilities, n (number of rows)
#'   being the number of observations and N (number of columns) the number of
#'   possible outcomes.
#' @param predicted_label A factor of length N, denoting the outcome that the
#'   probabilities in `predicted` correspond to.
#' @returns A numeric vector of size n with ranked probability scores
#' @inheritSection illustration-input-metric-nominal Input format
#' @importFrom methods hasArg
#' @importFrom scoringRules rps_probs
#' @export
#' @keywords metric
#' @family scoring functions
#' @examples
#' factor_levels <- c("one", "two", "three")
#' predicted_label <- factor(factor_levels, levels = factor_levels, ordered = TRUE)
#' observed <- factor(c("three", "three", "two"), levels = factor_levels, ordered = TRUE)
#' predicted <- matrix(
#'   c(0.8, 0.1, 0.1,
#'     0.1, 0.2, 0.7,
#'     0.4, 0.4, 0.2),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#' rps_ordinal(observed, predicted, predicted_label)
rps_ordinal <- function(observed, predicted, predicted_label) {
  assert_input_ordinal(observed, predicted, predicted_label)
  n <- length(observed)
  if (n == 1) {
    predicted <- matrix(predicted, nrow = 1)
  }

  # Reorder the predicted matrix columns to match the natural ordering
  correct_order <- as.numeric(predicted_label)
  ordered_predicted <- predicted[, correct_order]

  rps <- scoringRules::rps_probs(as.numeric(observed), ordered_predicted)
  return(rps)
}

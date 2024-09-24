#' Log score for nominal outcomes
#'
#' @description
#' **Log score for nominal outcomes**
#'
#' The Log Score is the negative logarithm of the probability
#' assigned to the observed value. It is a proper scoring rule. Small values
#' are better (best is zero, worst is infinity).
#' @param observed A factor of length n with N levels holding the observed
#'   values.
#' @param predicted nxN matrix of predictive probabilities, n (number of rows)
#'   being the number of observations and N (number of columns) the number of
#'   possible outcomes.
#' @param predicted_label A factor of length N, denoting the outcome that the
#'   probabilities in `predicted` correspond to.
#' @return A numeric vector of size n with log scores
#' @importFrom methods hasArg
#' @export
#' @keywords metric
#' @rdname scoring-functions-nominal
#' @family log score functions
#' @examples
#' factor_levels <- c("one", "two", "three")
#' predicted_label <- factor(c("one", "two", "three"), levels = factor_levels)
#' observed <- factor(c("one", "three", "two"), levels = factor_levels)
#' predicted <- matrix(c(0.8, 0.1, 0.4,
#'                       0.1, 0.2, 0.4,
#'                       0.1, 0.7, 0.2),
#'                     nrow = 3)
#' logs_nominal(observed, predicted, predicted_label)
logs_nominal <- function(observed, predicted, predicted_label) {
  assert_input_nominal(observed, predicted, predicted_label)
  n <- length(observed)
  if (n == 1) {
    predicted <- matrix(predicted, nrow = 1)
  }
  observed_indices <- as.numeric(observed)
  pred_for_observed <- predicted[cbind(1:n, observed_indices)]
  logs <- -log(pred_for_observed)
  return(logs)
}

#' @title Assert that inputs are correct for binary forecast
#' @description
#' Function assesses whether the inputs correspond to the
#' requirements for scoring binary forecasts.
#' @param observed Input to be checked. Should be a factor of length n with
#'   exactly two levels, holding the observed values.
#'   The highest factor level is assumed to be the reference level. This means
#'   that `predicted` represents the probability that the observed value is
#'   equal to the highest factor level.
#' @param predicted Input to be checked. `predicted` should be a vector of
#'   length n, holding probabilities. Alternatively, `predicted` can be a matrix
#'   of size n x 1. Values represent the probability that
#'   the corresponding value in `observed` will be equal to the highest
#'   available factor level.
#' @importFrom checkmate assert assert_factor
#' @inherit document_assert_functions return
#' @keywords internal_input_check
assert_input_binary <- function(observed, predicted) {
  assert_factor(observed, n.levels = 2, min.len = 1)
  assert_numeric(predicted, lower = 0, upper = 1)
  assert_dims_ok_point(observed, predicted)
  return(invisible(NULL))
}


#' @title Check that inputs are correct for binary forecast
#' @inherit assert_input_binary params description
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_input_binary <- function(observed, predicted) {
  result <- check_try(assert_input_binary(observed, predicted))
  return(result)
}


#' Metrics for binary outcomes
#'
#' @details
#'
#' The functions require users to provide observed values as a factor in order
#' to distinguish its input from the input format required for scoring point
#' forecasts. Internally, however, factors will be converted to numeric values.
#' A factor `observed = factor(c(0, 1, 1, 0, 1)` with two levels (`0` and `1`)
#' would internally be coerced to a numeric vector (in this case this would
#' result in the numeric vector `c(1, 2, 2, 1, 1)`). After subtracting 1, the
#' resulting vector (`c(0, 1, 1, 0)` in this case) is used for internal
#' calculations. All predictions are assumed represent the probability that the
#' outcome is equal of the last/highest factor level (in this case that the
#' outcome is equal to 1).
#'
#' You could alternatively also provide a vector like
#' `observed = factor(c("a", "b", "b", "a"))` (with two levels, `a` and `b`),
#' which would result in exactly the same internal representation. Probabilities
#' then represent the probability that the outcome is equal to "b".
#' If you want your predictions to be probabilities that the outcome is "a",
#' then you could of course make `observed` a factor with levels swapped, i.e.
#' `observed = factor(c("a", "b", "b", "a"), levels = c("b", "a"))`
#'
#' @param observed A factor of length n with exactly two levels, holding
#'   the observed values.
#'   The highest factor level is assumed to be the reference level. This means
#'   that `predicted` represents the probability that the observed value is
#'   equal to the highest factor level.
#' @param predicted A numeric vector of length n, holding probabilities.
#'   Values represent the probability that the corresponding outcome is equal to
#'   the highest level of the factor `observed`.
#' @examples
#' observed <- factor(sample(c(0, 1), size = 30, replace = TRUE))
#' predicted <- runif(n = 30, min = 0, max = 1)
#'
#' brier_score(observed, predicted)
#' logs_binary(observed, predicted)
#' @inheritSection illustration-input-metric-binary-point Input format
#' @name scoring-functions-binary
NULL


#' @description
#' **Brier score**
#'
#' The Brier Score is the mean squared error between the probabilistic
#' prediction and the observed outcome. The Brier score is a proper scoring
#' rule. Small values are better (best is 0, the worst is 1).
#'
#' \deqn{
#'   \textrm{Brier\_Score} = (\textrm{prediction} - \textrm{outcome})^2,
#' }{
#'   Brier_Score = (prediction - outcome)²,
#' } where \eqn{\textrm{outcome} \in \{0, 1\}}{outcome in {0, 1}}, and
#' \eqn{\textrm{prediction} \in [0, 1]}{prediction in [0, 1]} represents
#' the probability that the outcome is equal to 1.
#' @returns A numeric vector of size n with the Brier scores
#' @keywords metric
#' @export
#' @rdname scoring-functions-binary
brier_score <- function(observed, predicted) {
  assert_input_binary(observed, predicted)

  observed <- as.numeric(observed) - 1
  brierscore <- (observed - predicted)^2
  return(brierscore)
}


#' Log score for binary outcomes
#'
#' @description
#' **Log score for binary outcomes**
#'
#' The Log Score is the negative logarithm of the probability
#' assigned to the observed value. It is a proper scoring rule. Small values
#' are better (best is zero, worst is infinity).
#'
#' @returns A numeric vector of size n with log scores
#' @importFrom methods hasArg
#' @export
#' @keywords metric
#' @family log score functions
#' @rdname scoring-functions-binary
logs_binary <- function(observed, predicted) {
  assert_input_binary(observed, predicted)
  observed <- as.numeric(observed) - 1
  logs <- -log(1 - abs(observed - predicted))
  return(logs)
}

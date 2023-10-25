#' Brier Score
#'
#' @description
#' Computes the Brier Score for probabilistic forecasts of binary outcomes.
#'
#' @details
#' The Brier score is a proper score rule that assesses the accuracy of
#' probabilistic binary predictions. The outcomes can be either 0 or 1,
#' the predictions must be a probability that the observed outcome will be 1.
#'
#' The Brier Score is then computed as the mean squared error between the
#' probabilistic prediction and the observed outcome.
#'
#' \deqn{
#'   \textrm{Brier\_Score} = \frac{1}{N} \sum_{t = 1}^{n} (\textrm{prediction}_t -
#'   \textrm{outcome}_t)^2
#' }{
#'   Brier_Score = 1/N sum_{t = 1}^{n} (prediction_t - outcome_t)²
#' }
#'
#' The function requires users to provide observed values as a factor in order
#' to distinguish its input from the input format required for scoring point
#' forecasts. Internally, however, factors will be converted to numeric values.
#' A factor `observed = factor(c(0, 1, 1, 0, 1)` with two levels (`0` and `1`)
#' would internally be coerced to a numeric vector (in this case this would
#' result in the numeric vector c(1, 2, 2, 1, 1)). After subtracting 1, the
#' resulting vector (`c(0, 1, 1, 0)` in this case) is used for internal
#' calculations. All predictions are assumed represent the probability that the
#' outcome is equal of the highest factor level (in this case that the
#' outcome is equal to 1).
#' You could alternatively also provide a vector like
#' `observed = c("a", "b", "b", "a")` (with two levels, `a` and `b`),
#' which would result in exactly the same internal representation. Probabilities
#' then represent the probability that the outcome is equal to "b".
#'
#' @param observed A factor of length n with exactly two levels, holding
#' the observed values.
#' The highest factor level is assumed to be the reference level. This means
#' that `predicted` represents the probability that the observed value is
#' equal to the highest factor level.
#' @param predicted A numeric vector of length n, holding probabilities.
#' Values represent the probability that the corresponding outcome is equal to
#' the highest level of the factor `observed`.
#' @return A numeric value with the Brier Score, i.e. the mean squared
#' error of the given probability forecasts
#' @export
#'
#' @examples
#' observed <- factor(sample(c(0, 1), size = 30, replace = TRUE))
#' predicted <- runif(n = 30, min = 0, max = 1)
#'
#' brier_score(observed, predicted)
#' @keywords metric

brier_score <- function(observed, predicted) {
  assert_input_binary(observed, predicted)

  observed <- as.numeric(observed) - 1
  brierscore <- (observed - predicted)^2
  return(brierscore)
}


#' Log Score for Binary outcomes
#'
#' @description
#' Computes the Log Score for probabilistic forecasts of binary outcomes.
#'
#' @details
#' The Log Score is a proper score rule suited to assessing the accuracy of
#' probabilistic binary predictions. The outcomes can be either 0 or 1,
#' the predictions must be a probability that the true outcome will be 1.
#'
#' The Log Score is then computed as the negative logarithm of the probability
#' assigned to the true outcome. Reporting the negative logarithm means that
#' smaller values are better.
#'
#' The function requires users to provide observed values as a factor in order
#' to distinguish its input from the input format required for scoring point
#' forecasts. Internally, however, factors will be converted to numeric values.
#' A factor `observed = factor(c(0, 1, 1, 0, 1)` with two levels (`0` and `1`)
#' would internally be coerced to a numeric vector (in this case this would
#' result in the numeric vector c(1, 2, 2, 1, 1)). After subtracting 1, the
#' resulting vector (`c(0, 1, 1, 0)` in this case) is used for internal
#' calculations. All predictions are assumed represent the probability that the
#' outcome is equal of the highest factor level (in this case that the
#' outcome is equal to 1).
#' You could alternatively also provide a vector like
#' `observed = c("a", "b", "b", "a")` (with two levels, `a` and `b`),
#' which would result in exactly the same internal representation. Probabilities
#' then represent the probability that the outcome is equal to "b".
#'
#' @inheritParams brier_score
#' @return A numeric vector with log scores
#' @importFrom methods hasArg
#' @export
#' @keywords metric
#'
#' @examples
#' observed <- factor(sample(c(0, 1), size = 30, replace = TRUE))
#' predicted <- runif(n = 30, min = 0, max = 1)

#' logs_binary(observed, predicted)
logs_binary <- function(observed, predicted) {
  assert_input_binary(observed, predicted)
  observed <- as.numeric(observed) - 1
  logs <- -log(ifelse(observed == 1, predicted, 1 - predicted))
  return(logs)
}

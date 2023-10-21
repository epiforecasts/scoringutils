#' Brier Score
#'
#' @description
#' Computes the Brier Score for probabilistic forecasts of binary outcomes.
#'
#' @details
#' The Brier score is a proper score rule that assesses the accuracy of
#' probabilistic binary predictions. The outcomes can be either 0 or 1,
#' the predictions must be a probability that the true outcome will be 1.
#'
#' The Brier Score is then computed as the mean squared error between the
#' probabilistic prediction and the true outcome.
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
#' A factor `true_value = factor(c(0, 1, 1, 0, 1)` with two levels (`0` and `1`)
#' would internally be coerced to a numeric vector (in this case this would
#' result in the numeric vector c(1, 2, 2, 1, 1)). After subtracting 1, the
#' resulting vector (`c(0, 1, 1, 0)` in this case) is used for internal
#' calculations. All predictions are assumed represent the probability that the
#' outcome is equal of the highest factor level (in this case that the
#' outcome is equal to 1).
#' You could alternatively also provide a vector like
#' `true_value = c("a", "b", "b", "a")` (with two levels, `a` and `b`),
#' which would result in exactly the same internal representation. Probabilities
#' then represent the probability that the outcome is equal to "b".
#'
#' @param true_values A factor of length n with exactly two levels, holding
#' the observed values.
#' The highest factor level is assumed to be the reference level. This means
#' that `predictions` represents the probability that the observed value is
#' equal to the highest factor level.
#' @param predictions A numeric vector of length n, holding probabilities.
#' Values represent the probability that the corresponding outcome is equal to
#' the highest level of the factor `true_value`.
#' @return A numeric value with the Brier Score, i.e. the mean squared
#' error of the given probability forecasts
#' @export
#'
#' @examples
#' true_values <- factor(sample(c(0, 1), size = 30, replace = TRUE))
#' predictions <- runif(n = 30, min = 0, max = 1)
#'
#' brier_score(true_values, predictions)
#' @keywords metric

brier_score <- function(true_values, predictions) {
  check_input_binary(true_values, predictions)

  true_values <- as.numeric(true_values) - 1
  brierscore <- (true_values - predictions)^2
  return(brierscore)
}

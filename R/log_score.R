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
#' @inheritParams brier_score
#' @return A numeric vector with log scores
#' @importFrom methods hasArg
#' @export
#' @keywords metric
#'
#' @examples
#' true_values <- factor(sample(c(0, 1), size = 30, replace = TRUE))
#' predictions <- runif(n = 30, min = 0, max = 1)

#' logs_binary(true_values, predictions)
logs_binary <- function(true_values, predictions) {
  check_input_binary(true_values, predictions)
  true_values <- as.numeric(true_values) - 1
  logs <- -log(ifelse(true_values == 1, predictions, 1 - predictions))
  return(logs)
}

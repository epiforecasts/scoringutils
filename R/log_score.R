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
#' @param true_values A vector with the true observed values of size n
#' @param predictions A vector with a predicted probability
#' that true_value = 1.
#' @return A numeric value with the Log Score, i.e. the mean squared
#' error of the given probability forecasts
#' @importFrom methods hasArg
#' @export
#'
#' @examples
#' true_values <- sample(c(0, 1), size = 30, replace = TRUE)
#' predictions <- runif(n = 30, min = 0, max = 1)
#'
#' logs_binary(true_values, predictions)
logs_binary <- function(true_values, predictions) {

  # ============== Error handling ==============

  if (!all(c(methods::hasArg("true_values"), methods::hasArg("predictions")))) {
    stop("true_values or predictions argument missing")
  }

  if (!all(true_values %in% c(0, 1))) {
    stop("elements of true_values should be either zero or one")
  }

  n <- length(true_values)

  if (length(predictions) != n) {
    msg <- sprintf(
      "Mismatch: 'true_values' has length `%s`, but 'predictions' has length `%s`.",
      n, length(predictions)
    )
    stop(msg)
  }

  if (max(predictions) > 1 | min(predictions) < 0) {
    stop("elements of 'predictions' should be probabilites between zero and one")
  }
  # ============================================

  logs <- -(sum(log(ifelse(true_values == 1, predictions, 1 - predictions)))) / n
  return(logs)
}

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
#' @inheritParams brier_score
#' @return A numeric value with the Log Score, i.e. the mean squared
#' error of the given probability forecasts
#' @importFrom methods hasArg
#' @export
#' @keywords metric
#'
#' @examples
#' true_values <- sample(c(0, 1), size = 30, replace = TRUE)
#' predictions <- runif(n = 30, min = 0, max = 1)

#' logs_binary(true_values, predictions)
logs_binary <- function(true_values, predictions) {
  check_true_values(true_values, type = "binary")
  check_predictions(predictions, true_values, type = "binary")

  logs <- -log(ifelse(true_values == 1, predictions, 1 - predictions))
  return(logs)
}

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
#' @param true_values A vector with the true observed values of size n with
#' all values equal to either 0 or 1
#' @param predictions A vector with a predicted probability
#' that true_value = 1.
#' @return A numeric value with the Brier Score, i.e. the mean squared
#' error of the given probability forecasts
#' @export
#'
#' @examples
#' true_values <- sample(c(0, 1), size = 30, replace = TRUE)
#' predictions <- runif(n = 30, min = 0, max = 1)
#'
#' brier_score(true_values, predictions)
#' @keywords metric

brier_score <- function(true_values, predictions) {
  check_true_values(true_values, type = "binary")
  check_predictions(predictions, true_values, type = "binary")

  brierscore <- (true_values - predictions)^2
  return(brierscore)
}

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
#' Brier_Score = \frac{1}{N} \sum_{t = 1}^{n} (prediction_t - outcome_t)^2
#' }
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions A vector with a predicted probability
#' that true_value = 1.
#' @return A numeric value with the Brier Score, i.e. the mean squared
#' error of the given probability forecasts
#' @export
#'
#' @examples
#' true_values <- sample(c(0,1), size = 30, replace = TRUE)
#' predictions <- runif(n = 30, min = 0, max = 1)
#'
#' brier_score(true_values, predictions)
#'

brier_score <- function (true_values, predictions) {

  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  if (!all(true_values %in% c(0,1))) {
      stop("elements of true_values should be either zero or one")
  }

  n <- length(true_values)

  if (length(predictions) != n) {
    msg <- sprintf("Mismatch: 'true_values' has length `%s`, but 'predictions' has length `%s`.",
                   n, length(predictions))
    stop(msg)
  }

  if (max(predictions) > 1 | min(predictions) < 0) {
    stop("elements of 'predictions' should be probabilites between zero and one")
  }
  # ============================================

  brierscore <- (sum((true_values - predictions)^2) ) / n
  return(brierscore)
}



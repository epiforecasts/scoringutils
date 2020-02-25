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
#' @param predictions A vector with probability forecasts for true_value = 1
#' @return mean squared error of the forecasts (numeric)
#' @export
#'
#' @examples
#' true_values <- sample(c(0,1), size = 30, replace = TRUE)
#' predictions <- runif(n = 30, min = 0, max = 1)
#'
#' Brier_score(true_values, predictions)
#'

Brier_score <- function (true_values, predictions) {

  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  if (!is.integer(true_values)) {
    if (max(true_values) > 1 | min(true_values < 0)) {
      stop("elements of true_values should be either zero or one")
    }
    warning("The true_values provided are not integers. Don't trust the results.
            Maybe you want to score continuous predictions instead?")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (is.matrix(predictions)) {
    predictions <- rowMeans(predictions)
  }

  if (!is.vector(predictions)) {
    stop("'predictions' should be a vector with probabilites
         or a matrix with predictive samples equal to zero or one")
  }
  if (length(predictions) != n) {
    msg = cat("'predictions' must have n elements (or n rows), ",
              "where n is the number of true_values to predict. ")
    stop(msg)
  }
  if (is.integer(predictions)) {
    warning("Your predictions are all integers. Maybe you wanted to supply
            probabilites or a matrix of predictive samples?")
  }

  # ============================================

  n <- length(true_values)
  bs <- (sum((true_values - predictions)^2) ) / n
  return(bs)
}



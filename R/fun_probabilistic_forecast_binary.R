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
#' brier_score(true_values, predictions)
#'

brier_score <- function (true_values, predictions) {

  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  if (max(true_values) > 1 | min(true_values < 0)) {
      stop("elements of true_values should be either zero or one")
  }

  if (!all.equal(true_values, as.integer(true_values))){
    stop("The true_values provided are not integers.
         Maybe you want to score continuous predictions instead?")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (is.matrix(predictions)) {
    if (max(predictions) > 1 | min(predictions < 0)) {
      stop("elements of predictive samples should be either zero or one")
    }

    if (!all.equal(as.vector(predictions), as.integer(predictions))){
      stop("The predictive samples provided are not integers.
           Maybe you want to score continuous predictions instead?")
    }
    predictions <- rowMeans(predictions)
  }

  if (!is.vector(predictions)) {
    stop("'predictions' should be a vector with probabilites
         or a matrix with predictive samples equal to zero or one")
  }
  if (max(predictions) > 1 | min(predictions < 0)) {
    stop("elements of 'predictions' should be between zero and one")
  }
  if (length(predictions) != n) {
    msg = cat("'predictions' must have n elements (or n rows), ",
              "where n is the number of true_values to predict. ")
    stop(msg)
  }
  if (all.equal(predictions, as.integer(predictions)) == TRUE) {
    warning("Your probabilities are all integers. Maybe you wanted to supply
            probabilites or a matrix of predictive samples?")
  }

  # ============================================

  bs <- (sum((true_values - predictions)^2) ) / n
  return(bs)
}



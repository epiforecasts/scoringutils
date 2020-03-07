#' @title LogS
#'
#' @description
#' Wrapper around the \code{\link[scoringRules]{logs_sample}} function from the
#' \code{scoringRules} package.
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return vector with the scoring values
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' logs(true_values, predictions)
#' @export


logs <- function(true_values, predictions) {

  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    stop("'predictions' should be a matrix")
  }
  if (nrow(predictions) != n) {
    msg = cat("matrix 'predictions' must have n rows, ",
              "where n is the number of true_values to predict. ")
    stop(msg)
  }

  # ============================================

  scoringRules::logs_sample(y = true_values,
                            dat = predictions)
}


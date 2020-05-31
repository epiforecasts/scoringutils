#' @title Determines bias of forecasts
#'
#' @description
#' Determines bias from predictive Monte-Carlo samples. The function
#' automatically recognises, whether forecasts are continuous or
#' integer valued and adapts the Bias function accordingly.
#'
#' @details
#' For continuous forecasts, Bias is measured as
#'
#' \deqn{
#' B_t (P_t, x_t) = 1 - 2 * (P_t (x_t))
#' }
#'
#' where \eqn{P_t} is the empirical cumulative distribution function of the
#' prediction for the true value \eqn{x_t}. Computationally, \eqn{P_t (x_t)} is
#' just calculated as the fraction of predictive samples for \eqn{x_t}
#' that are smaller than \eqn{x_t}.
#'
#' For integer valued forecasts, Bias is measured as
#'
#' \deqn{
#' B_t (P_t, x_t) = 1 - (P_t (x_t) + P_t (x_t + 1))
#' }
#'
#' to adjust for the integer nature of the forecasts.
#'
#' In both cases, Bias can assume values between
#' -1 and 1 and is 0 ideally.
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return vector of length n with the biases of the predictive samples with
#' respect to the true values.
#' @author Nikos Bosse \email{nikosbosse@gmail.com}
#' @examples
#'
#' ## integer valued forecasts
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' bias(true_values, predictions)
#'
#' ## continuous forecasts
#' true_values <- rnorm(30, mean = 1:30)
#' predictions <- replicate(200, rnorm(30, mean = 1:30))
#' bias(true_values, predictions)
#'
#'
#' @export
#' @references
#' The integer valued Bias function is discussed in
#' Assessing the performance of real-time epidemic forecasts: A case study of Ebola in the Western Area region of Sierra Leone, 2014-15
#' Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, et al. (2019) Assessing the performance of real-time epidemic forecasts: A case study of Ebola in the Western Area region of Sierra Leone, 2014-15. PLOS Computational Biology 15(2): e1006785. https://doi.org/10.1371/journal.pcbi.1006785


bias <- function(true_values, predictions) {

  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    msg <- sprintf("'predictions' should be a matrix. Instead `%s` was found",
                   class(predictions[1]))
    stop(msg)
  }
  if (nrow(predictions) != n) {

    msg <- sprintf("Mismatch: The true values provided have length `%s`, but 'predictions' has `%s` rows.",
                   n, nrow(predictions))
    stop(msg)
  }

  # ============================================

  ## check whether continuous or integer
  if (all.equal(as.vector(predictions), as.integer(predictions)) != TRUE) {
    continuous_predictions <- TRUE
  } else {
    continuous_predictions <- FALSE
  }

  n_pred <- ncol(predictions)

  # empirical cdf
  P_x <- vapply(seq_along(true_values),
                function(i) {
                  sum(predictions[i,] <= true_values[i]) / n_pred
                },
                .0)

  if (continuous_predictions) {
    res <- 1 - 2 * P_x
    return(res)
  } else {
    # for integer case also calculate empirical cdf for (y-1)
    P_xm1 <- vapply(seq_along(true_values),
                    function(i) {
                      sum(predictions[i,] <= true_values[i] - 1) / n_pred
                    },
                    .0)

    res <- 1 - (P_x + P_xm1)
    return(res)
  }
}

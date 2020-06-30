#' @title LogS
#'
#' @description
#' Wrapper around the \code{\link[scoringRules:scores_sample_univ]{logs_sample}}
#' function from the
#' \code{scoringRules} package. Used to score continuous predictions.
#' While the Log Score is in theory also applicable
#' to integer forecasts, the problem lies in the implementation: The Log Score
#' needs a kernel density estimation, which is not well defined with
#' integer-valued Monte Carlo Samples. The Log Score can be used for specific
#' integer valued probabiliy distributions. See the scoringRules package for
#' more details.
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return vector with the scoring values
#' @importFrom scoringRules logs_sample
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' logs(true_values, predictions)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts withscoringRules, https://arxiv.org/pdf/1709.04743.pdf


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
    msg <- sprintf("'predictions' should be a matrix. Instead `%s` was found",
                   class(predictions[1]))
    stop(msg)
  }
  if (nrow(predictions) != n) {
    msg <- sprintf("Mismatch: 'true_values' has length `%s`, but 'predictions' has `%s` rows.",
                   n, nrow(predictions))
    stop(msg)
  }

  # ============================================

  scoringRules::logs_sample(y = true_values,
                            dat = predictions)
}



#' @title Dawid-Sebastiani Score
#'
#' @description
#' Wrapper around the \code{\link[scoringRules:scores_sample_univ]{dss_sample}}
#' function from the
#' \code{scoringRules} package.
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return vector with scoring values
#' @importFrom scoringRules dss_sample
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' dss(true_values, predictions)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts withscoringRules, https://arxiv.org/pdf/1709.04743.pdf

dss <- function(true_values, predictions) {

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
    msg <- sprintf("Mismatch: 'true_values' has length `%s`, but 'predictions' has `%s` rows.",
                   n, nrow(predictions))
    stop(msg)
  }
  # ============================================

  scoringRules::dss_sample(y = true_values,
                           dat = predictions)
}






#' @title Ranked Probability Score
#'
#' @description
#' Wrapper around the \code{\link[scoringRules:scores_sample_univ]{crps_sample}}
#' function from the
#' \code{scoringRules} package. Can be used for continuous as well as integer
#' valued forecasts
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return vector with the scoring values
#' @importFrom scoringRules crps_sample
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' crps(true_values, predictions)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts withscoringRules, https://arxiv.org/pdf/1709.04743.pdf

crps <- function(true_values, predictions) {

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
    msg <- sprintf("Mismatch: 'true_values' has length `%s`, but 'predictions' has `%s` rows.",
                   n, nrow(predictions))
    stop(msg)
  }
  # ============================================

  scoringRules::crps_sample(y = true_values,
                            dat = predictions)
}

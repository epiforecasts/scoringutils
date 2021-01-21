

#' @title Absolute Error of the Median (Sample-based Version)
#'
#' @description
#' Absolute error of the median calculated as
#'
#' \deqn{
#'   abs(true_value - median_prediction)
#' }
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples. Alternatively, predictions can just be a vector
#' of size n
#' @return vector with the scoring values
#' @importFrom stats median
#' @examples
#' true_values <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' ae_median_sample(true_values, predicted_values)
#' @export


ae_median_sample <- function(true_values, predictions) {

  median_predictions <- apply(as.matrix(predictions),
                              MARGIN = 1, # rowwise
                              FUN = median)

  ae_median <- abs(true_values - median_predictions)

  return(ae_median)
}





#' @title Absolute Error of the Median (Quantile-based Version)
#'
#' @description
#' Absolute error of the median calculated as
#'
#' \deqn{
#'   abs(true_value - median_prediction)
#' }
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions numeric vector with predictions, corresponding to the
#' quantiles in a second vector, `quantiles`.
#' @param quantiles numeric vector that denotes the quantile for the values
#' in `predictions`. Only those predictions where `quantiles == 0.5` will
#' be kept. If `quantiles` is `NULL`, then all `predictions` and
#' `true_values` will be used (this is then the same as `absolute_error()`)
#' @param verbose logical, return a warning is something unexpected happens
#' @return vector with the scoring values
#' @importFrom stats median
#' @examples
#' true_values <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' ae_median_quantile(true_values, predicted_values, quantiles = 0.5)
#' @export
ae_median_quantile <- function(true_values, predictions, quantiles = NULL,
                               verbose = TRUE) {
  if (!is.null(quantiles)) {
    if (!any(quantiles == 0.5) && !any(is.na(quantiles))) {
      return(NA_real_)
      if (verbose) {
        warning("in order to compute the absolute error of the median, `0.5` must be among the quantiles given. Maybe you want to use `abs_error()`?")
      }
    }
    true_values <- true_values[quantiles == 0.5]
    predictions <- predictions[quantiles == 0.5]
  }
  abs_error_median <- abs(true_values - predictions)
  return(abs_error_median)
}



#' @title Absolute Error
#'
#' @description
#' Caclulate absolute error as
#'
#' \deqn{
#'   abs(true_value - prediction)
#' }
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions numeric vector with predictions, corresponding to the
#' quantiles in a second vector, `quantiles`.
#' @return vector with the absolute error
#' @examples
#' true_values <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' abs_error(true_values, predicted_values)
#' @export


abs_error <- function(true_values, predictions) {
  return(abs(true_values - predictions))
}

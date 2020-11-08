#' @title Mean Squared Error
#'
#' @description
#' Mean Squared Error MSE of point forecasts.
#' Calculuated as
#'
#' \deqn{
#'   mean((true_values - predicted_values)^2)
#' }
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions A vector with predicted values of size n
#' @return vector with the scoring values
#' @examples
#' true_values <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' mse(true_values, predicted_values)
#' @export

mse <- function(true_values, predictions) {
  mse <- mean((true_values - predictions)^2)
  return(mse)
}




#' @title Absolute Error of the Median
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
#' ae_median(true_values, predicted_values)
#' @export


ae_median <- function(true_values, predictions) {

  median_predictions <- apply(as.matrix(predictions),
                              MARGIN = 1,
                              FUN = median)

  ae_median <- abs(true_values - median_predictions)

  return(ae_median)
}


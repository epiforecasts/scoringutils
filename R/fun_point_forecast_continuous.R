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

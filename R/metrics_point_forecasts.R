#' @title Mean Squared Error
#'
#' @description
#' Mean Squared Error MSE of point forecasts.
#' Calculated as
#'
#' \deqn{
#'   mean((true_values - predicted_values)^2)
#' }
#'
#' @param predictions A vector with predicted values of size n
#' @return vector with the scoring values
#' @inheritParams ae_median_sample
#' @export
#' @keywords metric
#' @examples
#' true_values <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' mse(true_values, predicted_values)

mse <- function(true_values, predictions) {
  mse <- mean((true_values - predictions)^2)
  return(mse)
}

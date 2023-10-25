#' @title Absolute Error
#'
#' @description
#' Calculate absolute error as
#'
#' \deqn{
#'   \textrm{abs}(\textrm{observed} - \textrm{median\_prediction})
#' }{
#'   abs(observed - prediction)
#' }
#'
#' @return vector with the absolute error
#' @inheritParams ae_median_quantile
#' @seealso [ae_median_sample()], [ae_median_quantile()]
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' abs_error(observed, predicted_values)
#' @export
#' @keywords metric

abs_error <- function(observed, predicted) {
  return(abs(observed - predicted))
}


#' @title Squared Error
#'
#' @description
#' Squared Error SE calculated as
#'
#' \deqn{
#'   (\textrm{observed} - \textrm{predicted})^2
#' }{
#'   (observed - predicted)^2
#' }
#'
#' @param predicted A vector with predicted values of size n
#' @return vector with the scoring values
#' @inheritParams ae_median_sample
#' @export
#' @keywords metric
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' squared_error(observed, predicted_values)

squared_error <- function(observed, predicted) {
  assert_input_point(observed, predicted)
  se <- (observed - predicted)^2
  return(se)
}

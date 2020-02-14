#' Brier Score
#'
#' @description Missing
#'
#' @param true_values A vector with the true observed values of size n
#' @param samples A vector with probability for true_value = 1
#' @return list
#' @export
#'
#' @examples
#'

Brier_score <- function (true_values, predictions) {
  n <- length(true_values)
  bs <- (sum((true_values - predictions)^2) ) / n
  return(bs)
}



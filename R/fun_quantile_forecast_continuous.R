#' @title Interval Score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions, following Gneiting
#' and Raftery (2007). Smaller values are better. The user can either specify
#' a interval range in percentage terms, i.e. interval_range = 90 (percent)
#' prediction intervals, or
#' a decimal alpha value, i.e. alpha = 0.1, both corresponding
#' to the 95% and 5% quantiles. No specific distribution is assumed,
#' but the range has to be symmetric (i.e you can't use the 0.1 quantile
#' as the lower bound and the 0.7 quantile as the upper).
#'
#' @param interval_range the range of the prediction intervals. i.e. if you're
#' forecasting the 0.05 and 0.95 quantile, the interval_range would be 90.
#' Can be either a single number or a vector of size n, if the range changes.
#' @param lower vector of size n with the lower quantile of the given range
#' @param upper vector of size n with the upper quantile of the given range
#' @param true_values A vector with the true observed values of size n
#' @param alpha alternative to specifying an interval_range. You can give a
#' decimal value like 0.1, which means that you have specified the 0.05 and
#' 0.95 quantiles as lower and upper
#' @return vector with the scoring values
#' @examples
#' true_values <- rnorm(30, mean = 1:30)
#' interval_range = 90
#' alpha = 0.1
#' lower = qnorm(alpha/2, rnorm(30, mean = 1:30))
#' upper = qnorm((1- alpha/2), rnorm(30, mean = 1:30))
#'
#' interval_score(interval_range = interval_range,
#'                true_values = true_values,
#'                lower = lower,
#'                upper = upper)
#' @export

interval_score <- function(interval_range = NULL,
                           lower,
                           upper,
                           true_values,
                           alpha = NULL) {

  if(!is.null(interval_range)) {
    if (interval_range < 1) {
      message(paste(interval_range),
              "% interval was provided. Are you sure that's right?", sep = "")
    }
    alpha = (100 - interval_range) / 100
  }

  score = (upper - lower) +
    2/alpha * (lower - true_values) * (true_values < lower) +
    2/alpha * (true_values - upper) * (true_values > upper)

  return(score)
}



#' @title Interval Score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions, following Gneiting
#' and Raftery (2007). Smaller values are better.
#'
#' The score is computed as
#'
#' \deqn{
#' score = (upper - lower) + 2/alpha * (lower - true_value) *
#' 1(true_values < lower) + 2/alpha * (true_value - upper) *
#' 1(true_value > upper)
#' }
#' where $1()$ is the indicator function and alpha is the decimal value that
#' indicates how much is outside the prediction interval.
#' To improve usability, the user is asked to provide an interval range in
#' percentage terms, i.e. interval_range = 90 (percent) for a 90 percent
#' prediction interval. Correspondingly, the user would have to provide the
#' 5\% and 95\% quantiles (the corresponding alpha would then be 0.1).
#' No specific distribution is assumed,
#' but the range has to be symmetric (i.e you can't use the 0.1 quantile
#' as the lower bound and the 0.7 quantile as the upper).
#'
#' The interval score is a proper scoring rule that scores a quantile forecast
#'
#' @param true_values A vector with the true observed values of size n
#' @param lower vector of size n with the lower quantile of the given range
#' @param upper vector of size n with the upper quantile of the given range
#' @param interval_range the range of the prediction intervals. i.e. if you're
#' forecasting the 0.05 and 0.95 quantile, the interval_range would be 90.
#' Can be either a single number or a vector of size n, if the range changes
#' for different forecasts to be scored. This corresponds to (100-alpha)/100
#' in Gneiting and Raftery (2007). Internally, the range will be transformed
#' to alpha.
#' @param weigh if TRUE, weigh the score by alpha / 4, so it can be averaged
#' into an interval score that, in the limit, corresponds to CRPS. Default:
#' FALSE.
#' @param separate_results if TRUE (default is FALSE), then the separate parts
#' of the interval score (sharpness, penalties for over- and under-prediction
#' get returned as separate elements of a list). If you want a `data.frame`
#' instead, simply call `as.data.frmae()` on the output.
#' @return vector with the scoring values, or a list with separate entries if
#' \code{separate_results} is TRUE.
#' @examples
#' true_values <- rnorm(30, mean = 1:30)
#' interval_range = rep(90, 30)
#' alpha = (100 - interval_range) / 100
#' lower = qnorm(alpha/2, rnorm(30, mean = 1:30))
#' upper = qnorm((1- alpha/2), rnorm(30, mean = 1:30))
#'
#' interval_score(true_values = true_values,
#'                lower = lower,
#'                upper = upper,
#'                interval_range = interval_range)
#'
#' interval_score(true_values = c(true_values, NA),
#'                lower = c(lower, NA),
#'                upper = c(NA, upper),
#'                separate_results = TRUE,
#'                interval_range = 90)
#' @export
#' @references Strictly Proper Scoring Rules, Prediction,and Estimation,
#' Tilmann Gneiting and Adrian E. Raftery, 2007, Journal of the American
#' Statistical Association, Volume 102, 2007 - Issue 477
#'
#' Evaluating epidemic forecasts in an interval format,
#' Johannes Bracher, Evan L. Ray, Tilmann Gneiting and Nicholas G. Reich,
#' <arXiv:2005.12881v1>
#'
#' Bracher J, Ray E, Gneiting T, Reich, N (2020) Evaluating epidemic forecasts
#' in an interval format. \url{https://arxiv.org/abs/2005.12881}
#'


interval_score <- function(true_values,
                           lower,
                           upper,
                           interval_range,
                           weigh = TRUE,
                           separate_results = FALSE) {

  # error handling - not sure how I can make this better
  present <- c(methods::hasArg("true_values"), methods::hasArg("lower"),
               methods::hasArg("upper"), methods::hasArg("interval_range"))
  if (!all(present)) {
    stop("need all arguments 'true_values', 'lower', 'upper' and 'interval_range' in function 'interval_score()'")
  }
  check_not_null(true_values = true_values, lower = lower, upper = upper,
                 interval_range = interval_range)
  check_equal_length(true_values, lower, interval_range, upper)

  # calculate alpha from the interval range
  alpha <- (100 - interval_range) / 100

  # calculate three components of WIS
  sharpness <- (upper - lower)
  overprediction <- 2/alpha * (lower - true_values) * (true_values < lower)
  underprediction <- 2/alpha * (true_values - upper) * (true_values > upper)

  if (weigh) {
    sharpness <- sharpness * alpha / 2
    underprediction <- underprediction * alpha / 2
    overprediction <- overprediction * alpha / 2
  }

  score <- sharpness + underprediction + overprediction

  if (separate_results) {
    return(list(interval_score = score,
                sharpness = sharpness,
                underprediction = underprediction,
                overprediction = overprediction))
  } else {
    return(score)
  }
}



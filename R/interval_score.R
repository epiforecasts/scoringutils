#' @title Interval Score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions, following Gneiting
#' and Raftery (2007). Smaller values are better.
#'
#' The score is computed as
#'
#' \deqn{
#' \textrm{score} = (\textrm{upper} - \textrm{lower}) + \frac{2}{\alpha}(\textrm{lower}
#'  - \textrm{true\_value}) *
#' \mathbf{1}(\textrm{true\_value} < \textrm{lower}) +
#' \frac{2}{\alpha}(\textrm{true\_value} - \textrm{upper}) *
#' \mathbf{1}(\textrm{true\_value} > \textrm{upper})
#' }{
#' score = (upper - lower) + 2/alpha * (lower - true_value) *
#' 1(true_values < lower) + 2/alpha * (true_value - upper) *
#' 1(true_value > upper)
#' }
#' where \eqn{\mathbf{1}()}{1()} is the indicator function and
#' indicates how much is outside the prediction interval.
#' \eqn{\alpha}{alpha} is the decimal value that indicates how much is outside
#' the prediction interval.
#'
#' To improve usability, the user is asked to provide an interval range in
#' percentage terms, i.e. interval_range = 90 (percent) for a 90 percent
#' prediction interval. Correspondingly, the user would have to provide the
#' 5% and 95% quantiles (the corresponding alpha would then be 0.1).
#' No specific distribution is assumed,
#' but the range has to be symmetric (i.e you can't use the 0.1 quantile
#' as the lower bound and the 0.7 quantile as the upper).
#' Non-symmetric quantiles can be scored using the function [quantile_score()].
#'
#' @param lower vector of size n with the prediction for the lower quantile
#' of the given range
#' @param upper vector of size n with the prediction for the upper quantile
#' of the given range
#' @param interval_range the range of the prediction intervals. i.e. if you're
#' forecasting the 0.05 and 0.95 quantile, the interval_range would be 90.
#' Can be either a single number or a vector of size n, if the range changes
#' for different forecasts to be scored. This corresponds to (100-alpha)/100
#' in Gneiting and Raftery (2007). Internally, the range will be transformed
#' to alpha.
#' @param weigh if TRUE, weigh the score by alpha / 2, so it can be averaged
#' into an interval score that, in the limit, corresponds to CRPS. Alpha is the
#' decimal value that  represents how much is outside a central prediction
#' interval (e.g. for a 90 percent central prediction interval, alpha is 0.1)
#' Default: `TRUE`.
#' @param separate_results if `TRUE` (default is `FALSE`), then the separate
#' parts of the interval score (dispersion penalty, penalties for over- and
#' under-prediction get returned as separate elements of a list). If you want a
#' `data.frame` instead, simply call [as.data.frame()] on the output.
#' @return vector with the scoring values, or a list with separate entries if
#' `separate_results` is `TRUE`.
#' @importFrom rlang warn
#' @inheritParams ae_median_sample
#' @examples
#' true_values <- rnorm(30, mean = 1:30)
#' interval_range <- rep(90, 30)
#' alpha <- (100 - interval_range) / 100
#' lower <- qnorm(alpha / 2, rnorm(30, mean = 1:30))
#' upper <- qnorm((1 - alpha / 2), rnorm(30, mean = 1:30))
#'
#' interval_score(
#'   true_values = true_values,
#'   lower = lower,
#'   upper = upper,
#'   interval_range = interval_range
#' )
#'
#' # gives a warning, as the interval_range should likely be 50 instead of 0.5
#' interval_score(true_value = 4, upper = 2, lower = 8, interval_range = 0.5)
#'
#' # example with missing values and separate results
#' interval_score(
#'   true_values = c(true_values, NA),
#'   lower = c(lower, NA),
#'   upper = c(NA, upper),
#'   separate_results = TRUE,
#'   interval_range = 90
#' )
#' @export
#' @keywords metric
#' @references Strictly Proper Scoring Rules, Prediction,and Estimation,
#' Tilmann Gneiting and Adrian E. Raftery, 2007, Journal of the American
#' Statistical Association, Volume 102, 2007 - Issue 477
#'
#' Evaluating epidemic forecasts in an interval format,
#' Johannes Bracher, Evan L. Ray, Tilmann Gneiting and Nicholas G. Reich,
#' <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618> # nolint
#'

interval_score <- function(true_values,
                           lower,
                           upper,
                           interval_range,
                           weigh = TRUE,
                           separate_results = FALSE) {

  # error handling - not sure how I can make this better
  present <- c(
    methods::hasArg("true_values"), methods::hasArg("lower"),
    methods::hasArg("upper"), methods::hasArg("interval_range")
  )
  if (!all(present)) {
    stop(
      "need all arguments 'true_values', 'lower', 'upper' and 'interval_range' in function 'interval_score()'" # nolint
    )
  }
  check_not_null(
    true_values = true_values, lower = lower, upper = upper,
    interval_range = interval_range
  )
  check_equal_length(true_values, lower, interval_range, upper)

  if (any(interval_range < 0, na.rm = TRUE)) {
    stop("interval ranges must be positive")
  }
  if (any(interval_range > 0 & interval_range < 1, na.rm = TRUE)) {
    msg <- paste("Found interval ranges between 0 and 1. Are you sure that's right?",
                 "An interval range of 0.5 e.g. implies a (49.75%, 50.25%) prediction interval.",
                 "If you want to score a (25%, 75%) prediction interval, set interval_range = 50.")
    rlang::warn(message = msg, .frequency = "once", .frequency_id = "small_interval_range")
  }

  # calculate alpha from the interval range
  alpha <- (100 - interval_range) / 100

  # calculate three components of WIS
  dispersion <- (upper - lower)
  overprediction <-
    2 / alpha * (lower - true_values) * as.numeric(true_values < lower)
  underprediction <-
    2 / alpha * (true_values - upper) * as.numeric(true_values > upper)

  if (weigh) {
    dispersion <- dispersion * alpha / 2
    underprediction <- underprediction * alpha / 2
    overprediction <- overprediction * alpha / 2
  }

  score <- dispersion + underprediction + overprediction

  if (separate_results) {
    return(list(
      interval_score = score,
      dispersion = dispersion,
      underprediction = underprediction,
      overprediction = overprediction
    ))
  } else {
    return(score)
  }
}

#' @title Quantile Score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions. Smaller values are better.
#' The quantile score is
#' closely related to the Interval score (see [interval_score()]) and is
#' the quantile equivalent that works with single quantiles instead of
#' central prediction intervals.
#'
#' @param quantiles vector of size n with the quantile values of the
#' corresponding predictions.
#' @param weigh if TRUE, weigh the score by alpha / 2, so it can be averaged
#' into an interval score that, in the limit, corresponds to CRPS. Alpha is the
#' value that corresponds to the (alpha/2) or (1 - alpha/2) quantiles provided
#' and will be computed from the quantile. Alpha is the decimal value that
#' represents how much is outside a central prediction interval (E.g. for a
#' 90 percent central prediction interval, alpha is 0.1). Default: `TRUE`.
#' @return vector with the scoring values
#' @inheritParams interval_score
#' @inheritParams ae_median_sample
#' @examples
#' true_values <- rnorm(10, mean = 1:10)
#' alpha <- 0.5
#'
#' lower <- qnorm(alpha / 2, rnorm(10, mean = 1:10))
#' upper <- qnorm((1 - alpha / 2), rnorm(10, mean = 1:10))
#'
#' qs_lower <- quantile_score(true_values,
#'   predictions = lower,
#'   quantiles = alpha / 2
#' )
#' qs_upper <- quantile_score(true_values,
#'   predictions = upper,
#'   quantiles = 1 - alpha / 2
#' )
#' interval_score <- (qs_lower + qs_upper) / 2
#' @export
#' @keywords metric
#' @references Strictly Proper Scoring Rules, Prediction,and Estimation,
#' Tilmann Gneiting and Adrian E. Raftery, 2007, Journal of the American
#' Statistical Association, Volume 102, 2007 - Issue 477
#'
#' Evaluating epidemic forecasts in an interval format,
#' Johannes Bracher, Evan L. Ray, Tilmann Gneiting and Nicholas G. Reich,
#' <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618>
#

quantile_score <- function(true_values,
                           predictions,
                           quantiles,
                           weigh = TRUE) {

  # get central prediction interval which corresponds to given quantiles
  central_interval <- abs(0.5 - quantiles) * 2
  alpha <- 1 - central_interval

  # compute score - this is the version explained in the SI of Bracher et. al.
  error <- abs(predictions - true_values)
  score <- 2 * ifelse(
    true_values <= predictions, 1 - quantiles, quantiles
  ) * error

  # adapt score such that mean of unweighted quantile scores corresponds to
  # unweighted interval score of the corresponding prediction interval
  score <- 2 * score / alpha

  if (weigh) {
    score <- score * alpha / 2
  }

  return(score)
}

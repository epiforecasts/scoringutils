# NOTE: the interval range format is only used internally.



#' @title Assert that inputs are correct for interval-based forecast
#' @description
#' Function assesses whether the inputs correspond to the
#' requirements for scoring interval-based forecasts.
#' @param lower Input to be checked. Should be a numeric vector of size n that
#'   holds the predicted value for the lower bounds of the prediction intervals.
#' @param upper Input to be checked. Should be a numeric vector of size n that
#'   holds the predicted value for the upper bounds of the prediction intervals.
#' @param interval_range Input to be checked. Should be a vector of size n that
#'   denotes the interval range in percent. E.g. a value of 50 denotes a
#'   (25%, 75%) prediction interval.
#' @importFrom cli cli_warn cli_abort
#' @inherit document_assert_functions params return
#' @keywords internal_input_check
assert_input_interval <- function(observed, lower, upper, interval_range) {

  assert(check_numeric_vector(observed, min.len = 1))
  n <- length(observed)
  assert(check_numeric_vector(lower, len = n))
  assert(check_numeric_vector(upper, len = n))
  assert(
    check_numeric_vector(interval_range, len = 1, lower = 0, upper = 100),
    check_numeric_vector(interval_range, len = n, lower = 0, upper = 100)
  )

  diff <- upper - lower
  diff <- diff[!is.na(diff)]
  if (any(diff < 0)) {
    cli_abort(
      c(
        "!" = "All values in `upper` need to be greater than or equal to
        the corresponding values in `lower`"
      )
    )
  }
  if (any(interval_range > 0 & interval_range < 1, na.rm = TRUE)) {
    #nolint start: keyword_quote_linter
    cli_warn(
      c(
        "!" = "Found interval ranges between 0 and 1. Are you sure that's
        right? An interval range of 0.5 e.g. implies a (49.75%, 50.25%)
        prediction interval.",
        "i" = "If you want to score a (25%, 75%) prediction interval, set
        `interval_range = 50`."
      ),
      .frequency = "once",
      .frequency_id = "small_interval_range"
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @title Check that inputs are correct for interval-based forecast
#' @inherit assert_input_interval params description
#' @inherit check_input_sample return description
#' @keywords internal_input_check
check_input_interval <- function(observed, lower, upper, interval_range) {
  result <- check_try(
    assert_input_interval(observed, lower, upper, interval_range)
  )
  return(result)
}


#' @title Interval score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions, following Gneiting
#' and Raftery (2007). Smaller values are better.
#'
#' The score is computed as
#'
#' \deqn{
#' \textrm{score} = (\textrm{upper} - \textrm{lower}) + \frac{2}{\alpha}(\textrm{lower}
#'  - \textrm{observed}) *
#' \mathbf{1}(\textrm{observed} < \textrm{lower}) +
#' \frac{2}{\alpha}(\textrm{observed} - \textrm{upper}) *
#' \mathbf{1}(\textrm{observed} > \textrm{upper})
#' }{
#' score = (upper - lower) + 2/alpha * (lower - observed) *
#' 1(observed < lower) + 2/alpha * (observed - upper) *
#' 1(observed > upper)
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
#' No specific distribution is assumed, but the interval has to be symmetric
#' around the median (i.e you can't use the 0.1 quantile
#' as the lower bound and the 0.7 quantile as the upper bound).
#' Non-symmetric quantiles can be scored using the function [quantile_score()].
#'
#' @param lower Vector of size n with the prediction for the lower quantile
#'   of the given interval range.
#' @param upper Vector of size n with the prediction for the upper quantile
#'   of the given interval range.
#' @param interval_range Numeric vector (either a single number or a vector of
#'   size n) with the range of the prediction intervals. For example, if you're
#'   forecasting the 0.05 and 0.95 quantile, the interval range would be 90.
#'   The interval range corresponds to \eqn{(100-\alpha)/100}, where
#'   \eqn{\alpha}{alpha} is the decimal value that indicates how much is outside
#'   the prediction interval (see e.g. Gneiting and Raftery (2007)).
#' @param separate_results Logical. If `TRUE` (default is `FALSE`), then the
#'   separate parts of the interval score (dispersion penalty, penalties for
#'   over- and under-prediction get returned as separate elements of a list).
#'   If you want a `data.frame` instead, simply call [as.data.frame()] on the
#'   output.
#' @param weigh Logical. If `TRUE` (the default), weigh the score by
#'   \eqn{\alpha / 2}, so it can be averaged into an interval score that, in
#'   the limit (for an increasing number of equally spaced quantiles/prediction
#'   intervals), corresponds
#'   to the CRPS. \eqn{\alpha} is the value that corresponds to the
#'   (\eqn{\alpha/2}) or (\eqn{1 - \alpha/2}), i.e. it is the decimal
#'   value that represents how much is outside a central prediction interval
#'   (E.g. for a 90 percent central prediction interval, alpha is 0.1).
#' @return
#' Vector with the scoring values, or a list with separate entries if
#' `separate_results` is `TRUE`.
#' @inheritParams ae_median_sample
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' interval_range <- rep(90, 30)
#' alpha <- (100 - interval_range) / 100
#' lower <- qnorm(alpha / 2, rnorm(30, mean = 1:30))
#' upper <- qnorm((1 - alpha / 2), rnorm(30, mean = 11:40))
#'
#' scoringutils:::interval_score(
#'   observed = observed,
#'   lower = lower,
#'   upper = upper,
#'   interval_range = interval_range
#' )
#'
#' # gives a warning, as the interval_range should likely be 50 instead of 0.5
#' scoringutils:::interval_score(
#'   observed = 4, upper = 8, lower = 2, interval_range = 0.5
#' )
#'
#' # example with missing values and separate results
#' scoringutils:::interval_score(
#'   observed = c(observed, NA),
#'   lower = c(lower, NA),
#'   upper = c(NA, upper),
#'   separate_results = TRUE,
#'   interval_range = 90
#' )
#' @keywords metric
#' @references Strictly Proper Scoring Rules, Prediction,and Estimation,
#' Tilmann Gneiting and Adrian E. Raftery, 2007, Journal of the American
#' Statistical Association, Volume 102, 2007 - Issue 477
#'
#' Evaluating epidemic forecasts in an interval format,
#' Johannes Bracher, Evan L. Ray, Tilmann Gneiting and Nicholas G. Reich,
#' <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618> # nolint

interval_score <- function(observed,
                           lower,
                           upper,
                           interval_range,
                           weigh = TRUE,
                           separate_results = FALSE) {

  assert_input_interval(observed, lower, upper, interval_range)

  # calculate alpha from the interval range
  alpha <- (100 - interval_range) / 100

  # calculate three components of WIS
  dispersion <- (upper - lower)
  overprediction <-
    2 / alpha * (lower - observed) * as.numeric(observed < lower)
  underprediction <-
    2 / alpha * (observed - upper) * as.numeric(observed > upper)

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

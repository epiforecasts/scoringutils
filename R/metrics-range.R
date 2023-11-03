################################################################################
# Metrics with a one-to-one relationship between input and score
################################################################################

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
#' observed <- rnorm(30, mean = 1:30)
#' interval_range <- rep(90, 30)
#' alpha <- (100 - interval_range) / 100
#' lower <- qnorm(alpha / 2, rnorm(30, mean = 1:30))
#' upper <- qnorm((1 - alpha / 2), rnorm(30, mean = 1:30))
#'
#' interval_score(
#'   observed = observed,
#'   lower = lower,
#'   upper = upper,
#'   interval_range = interval_range
#' )
#'
#' # gives a warning, as the interval_range should likely be 50 instead of 0.5
#' interval_score(observed = 4, upper = 2, lower = 8, interval_range = 0.5)
#'
#' # example with missing values and separate results
#' interval_score(
#'   observed = c(observed, NA),
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


################################################################################
# Metrics with a many-to-one relationship between input and score
################################################################################

#' @title Determines Bias of Quantile Forecasts based on the range of the
#' prediction intervals
#'
#' @description
#' Determines bias from quantile forecasts based on the range of the
#' prediction intervals. For an increasing number of quantiles this measure
#' converges against the sample based bias version for integer and continuous
#' forecasts.
#'
#' @details
#' For quantile forecasts, bias is measured as
#'
#' \deqn{
#' B_t = (1 - 2 \cdot \max \{i | q_{t,i} \in Q_t \land q_{t,i} \leq x_t\})
#' \mathbf{1}( x_t \leq q_{t, 0.5}) \\
#' + (1 - 2 \cdot \min \{i | q_{t,i} \in Q_t \land q_{t,i} \geq x_t\})
#'  \mathbf{1}( x_t \geq q_{t, 0.5}),
#' }{
#' B_t = (1 - 2 * max(i | q_{t,i} in Q_t and q_{t,i} <= x_t\))
#' 1( x_t <= q_{t, 0.5}) + (1 - 2 * min(i | q_{t,i} in Q_t and q_{t,i} >= x_t))
#'  1( x_t >= q_{t, 0.5}),
#' }
#'
#' where \eqn{Q_t} is the set of quantiles that form the predictive
#' distribution at time \eqn{t}. They represent our
#' belief about what the later observed value \eqn{x_t} will be. For
#' consistency, we define
#' \eqn{Q_t} such that it always includes the element
#' \eqn{q_{t, 0} = - \infty} and \eqn{q_{t,1} = \infty}.
#' \eqn{\mathbf{1}()}{1()} is the indicator function that is \eqn{1} if the
#' condition is satisfied and $0$ otherwise. In clearer terms, \eqn{B_t} is
#' defined as the maximum percentile rank for which the corresponding quantile
#' is still below the observed value, if the observed value is smaller than the
#' median of the predictive distribution. If the observed value is above the
#' median of the predictive distribution, then $B_t$ is the minimum percentile
#' rank for which the corresponding quantile is still larger than the true
#' value. If the observed value is exactly the median, both terms cancel out and
#' \eqn{B_t} is zero. For a large enough number of quantiles, the
#' percentile rank will equal the proportion of predictive samples below the
#' observed value, and this metric coincides with the one for
#' continuous forecasts.
#'
#' Bias can assume values between
#' -1 and 1 and is 0 ideally.
#' @param lower vector of length corresponding to the number of central
#' prediction intervals that holds predictions for the lower bounds of a
#' prediction interval
#' @param upper vector of length corresponding to the number of central
#' prediction intervals that holds predictions for the upper bounds of a
#' prediction interval
#' @param range vector of corresponding size with information about the width
#' of the central prediction interval
#' @param observed a single observed value
#' @return scalar with the quantile bias for a single quantile prediction
#' @seealso bias_quantile bias_sample
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @examples
#'
#' lower <- c(
#'   6341.000, 6329.500, 6087.014, 5703.500,
#'   5451.000, 5340.500, 4821.996, 4709.000,
#'   4341.500, 4006.250, 1127.000, 705.500
#' )
#'
#' upper <- c(
#'   6341.000, 6352.500, 6594.986, 6978.500,
#'   7231.000, 7341.500, 7860.004, 7973.000,
#'   8340.500, 8675.750, 11555.000, 11976.500
#' )
#'
#' range <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98)
#'
#' observed <- 8062
#'
#' bias_range(
#'   lower = lower, upper = upper,
#'   range = range, observed = observed
#' )
#' @export
#' @keywords metric

bias_range <- function(lower, upper, range, observed) {

  if (anyNA(range)) {
    if (is.na(range[1]) && !any(range[-1] == 0)) {
      range[1] <- 0
    }
    range <- range[!is.na(range)]
    lower <- lower[!is.na(range)]
    upper <- upper[!is.na(range)]
  }

  if (length(range) > 1 && !all(diff(range) > 0)) {
    stop("Range must be increasing")
  }

  if (length(lower) != length(upper) || length(range) != length(lower)) {
    stop("Inputs must have same length")
  }

  check_quantiles(range, name = "range", range = c(0, 100))

  # Convert range to quantiles
  quantile <- c(
    rev(abs(100 - range) / (2 * 100)),
    abs(100 + range[range != 0]) / (2 * 100)
  )

  # Combine predictions
  upper_without_median <- upper[range != 0]
  predicted <- c(rev(lower), upper_without_median)

  # Call bias_quantile
  bias <- bias_quantile(observed, predicted, quantile)

  return(bias)
}

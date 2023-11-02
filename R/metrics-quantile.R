#' @title Quantile Score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions. Smaller values are better.
#' The quantile score is
#' closely related to the Interval score (see [interval_score()]) and is
#' the quantile equivalent that works with single quantiles instead of
#' central prediction intervals.
#'
#' @param quantile vector of size n with the quantile levels of the
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
#' observed <- rnorm(10, mean = 1:10)
#' alpha <- 0.5
#'
#' lower <- qnorm(alpha / 2, rnorm(10, mean = 1:10))
#' upper <- qnorm((1 - alpha / 2), rnorm(10, mean = 1:10))
#'
#' qs_lower <- quantile_score(observed,
#'   predicted = lower,
#'   quantile = alpha / 2
#' )
#' qs_upper <- quantile_score(observed,
#'   predicted = upper,
#'   quantile = 1 - alpha / 2
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

quantile_score <- function(observed,
                           predicted,
                           quantile,
                           weigh = TRUE) {

  # get central prediction interval which corresponds to given quantiles
  central_interval <- abs(0.5 - quantile) * 2
  alpha <- 1 - central_interval

  # compute score - this is the version explained in the SI of Bracher et. al.
  error <- abs(predicted - observed)
  score <- 2 * ifelse(
    observed <= predicted, 1 - quantile, quantile
  ) * error

  # adapt score such that mean of unweighted quantile scores corresponds to
  # unweighted interval score of the corresponding prediction interval
  score <- 2 * score / alpha

  if (weigh) {
    score <- score * alpha / 2
  }

  return(score)
}


#' @title Absolute Error of the Median (Quantile-based Version)
#'
#' @description
#' Absolute error of the median calculated as
#'
#' \deqn{
#'   \textrm{abs}(\textrm{observed} - \textrm{prediction})
#' }{
#'   abs(observed - median_prediction)
#' }
#'
#' The function was created for internal use within [score()], but can also
#' used as a standalone function.
#'
#' @param predicted numeric vector with predictions, corresponding to the
#' quantiles in a second vector, `quantiles`.
#' @param quantiles numeric vector that denotes the quantile for the values
#' in `predicted`. Only those predictions where `quantiles == 0.5` will
#' be kept. If `quantiles` is `NULL`, then all `predicted` and
#' `observed` will be used (this is then the same as [abs_error()])
#' @return vector with the scoring values
#' @seealso [ae_median_sample()], [abs_error()]
#' @importFrom stats median
#' @inheritParams ae_median_sample
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' ae_median_quantile(observed, predicted_values, quantiles = 0.5)
#' @export
#' @keywords metric

ae_median_quantile <- function(observed, predicted, quantiles = NULL) {
  if (!is.null(quantiles)) {
    if (!any(quantiles == 0.5) && !anyNA(quantiles)) {
      return(NA_real_)
      warning(
        "in order to compute the absolute error of the median, `0.5` must be ",
        "among the quantiles given. Maybe you want to use `abs_error()`?"
      )
    }
    observed <- observed[quantiles == 0.5]
    predicted <- predicted[quantiles == 0.5]
  }
  abs_error_median <- abs(observed - predicted)
  return(abs_error_median)
}



#' @title Determines Bias of Quantile Forecasts
#'
#' @description
#' Determines bias from quantile forecasts. For an increasing number of
#' quantiles this measure converges against the sample based bias version
#' for integer and continuous forecasts.
#'
#' @details
#' For quantile forecasts, bias is measured as
#'
#' \deqn{
#' B_t = (1 - 2 \cdot \max \{i | q_{t,i} \in Q_t \land q_{t,i} \leq x_t\})
#'  \mathbf{1}( x_t \leq q_{t, 0.5}) \\
#' + (1 - 2 \cdot \min \{i | q_{t,i} \in Q_t \land q_{t,i} \geq x_t\})
#'  1( x_t \geq q_{t, 0.5}),}
#'
#' where \eqn{Q_t} is the set of quantiles that form the predictive
#' distribution at time \eqn{t}. They represent our
#' belief about what the observed value $x_t$ will be. For consistency, we define
#' \eqn{Q_t} such that it always includes the element
#' \eqn{q_{t, 0} = - \infty} and \eqn{q_{t,1} = \infty}.
#' \eqn{1()} is the indicator function that is \eqn{1} if the
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
#' -1 and 1 and is 0 ideally (i.e. unbiased).
#' @param predicted vector of length corresponding to the number of quantiles
#' that holds predictions
#' @param quantile vector of corresponding size with the quantile levels for
#' which predictions were made. If this does not contain the median (0.5) then
#' the median is imputed as being the mean of the two innermost quantiles.
#' @inheritParams bias_range
#' @return scalar with the quantile bias for a single quantile prediction
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @examples
#'
#' predicted <- c(
#'   705.500, 1127.000, 4006.250, 4341.500, 4709.000, 4821.996,
#'   5340.500, 5451.000, 5703.500, 6087.014, 6329.500, 6341.000,
#'   6352.500, 6594.986, 6978.500, 7231.000, 7341.500, 7860.004,
#'   7973.000, 8340.500, 8675.750, 11555.000, 11976.500
#' )
#'
#' quantile <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
#'
#' observed <- 8062
#'
#' bias_quantile(observed, predicted, quantile)
#' @export
#' @keywords metric

bias_quantile <- function(observed, predicted, quantile) {
  # check that predictions and quantile have the same length
  if (!length(predicted) == length(quantile)) {
    stop("`predicted` and `quantile` must have the same length")
  }

  if (anyNA(predicted)) {
    quantile <- quantile[!is.na(predicted)]
    predicted <- predicted[!is.na(predicted)]
  }

  if (anyNA(quantile)) {
    quantile <- quantile[!is.na(quantile)]
    predicted <- predicted[!is.na(quantile)]
  }

  # if there is no input, return NA
  if (length(quantile) == 0 || length(predicted) == 0) {
    return(NA_real_)
  }

  check_quantiles(quantile)

  if (!all(diff(predicted) >= 0)) {
    stop("predictions must be increasing with quantiles")
  }

  if (0.5 %in% quantile) {
    median_prediction <- predicted[quantile == 0.5]
  } else {
    # if median is not available, compute as mean of two innermost quantiles
    message(
      "Median not available, computing as mean of two innermost quantiles",
      " in order to compute bias."
    )
    median_prediction <-
      0.5 * predicted[quantile == max(quantile[quantile < 0.5])] +
      0.5 * predicted[quantile == min(quantile[quantile > 0.5])]
  }

  if (observed == median_prediction) {
    bias <- 0
    return(bias)
  } else if (observed < median_prediction) {
    if (observed < min(predicted)) {
      bias <- 1
    } else {
      q <- max(quantile[predicted <= observed])
      bias <- 1 - 2 * q
    }
  } else if (observed > median_prediction) {
    if (observed > max(predicted)) {
      bias <- -1
    } else {
      q <- min(quantile[predicted >= observed])
      bias <- 1 - 2 * q
    }
  }
  return(bias)
}
#' @title Determines bias of forecasts
#'
#' @description
#' Determines bias from predictive Monte-Carlo samples. The function
#' automatically recognises, whether forecasts are continuous or
#' integer valued and adapts the Bias function accordingly.
#'
#' @details
#' For continuous forecasts, Bias is measured as
#'
#' \deqn{
#' B_t (P_t, x_t) = 1 - 2 * (P_t (x_t))
#' }
#'
#' where \eqn{P_t} is the empirical cumulative distribution function of the
#' prediction for the observed value \eqn{x_t}. Computationally, \eqn{P_t (x_t)} is
#' just calculated as the fraction of predictive samples for \eqn{x_t}
#' that are smaller than \eqn{x_t}.
#'
#' For integer valued forecasts, Bias is measured as
#'
#' \deqn{
#' B_t (P_t, x_t) = 1 - (P_t (x_t) + P_t (x_t + 1))
#' }
#'
#' to adjust for the integer nature of the forecasts.
#'
#' In both cases, Bias can assume values between
#' -1 and 1 and is 0 ideally.
#'
#' @return vector of length n with the biases of the predictive samples with
#' respect to the observed values.
#' @inheritParams ae_median_sample
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @examples
#'
#' ## integer valued forecasts
#' observed <- rpois(30, lambda = 1:30)
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' bias_sample(observed, predicted)
#'
#' ## continuous forecasts
#' observed <- rnorm(30, mean = 1:30)
#' predicted <- replicate(200, rnorm(30, mean = 1:30))
#' bias_sample(observed, predicted)
#' @export
#' @references
#' The integer valued Bias function is discussed in
#' Assessing the performance of real-time epidemic forecasts: A case study of
#' Ebola in the Western Area region of Sierra Leone, 2014-15 Funk S, Camacho A,
#' Kucharski AJ, Lowe R, Eggo RM, et al. (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15. PLOS Computational Biology 15(2): e1006785.
#' \doi{10.1371/journal.pcbi.1006785}
#' @keywords metric

bias_sample <- function(observed, predicted) {

  assert_input_sample(observed, predicted)
  prediction_type <- get_prediction_type(predicted)

  # empirical cdf
  n_pred <- ncol(predicted)
  p_x <- rowSums(predicted <= observed) / n_pred

  if (prediction_type == "continuous") {
    res <- 1 - 2 * p_x
    return(res)
  } else {
    # for integer case also calculate empirical cdf for (y-1)
    p_xm1 <- rowSums(predicted <= (observed - 1)) / n_pred

    res <- 1 - (p_x + p_xm1)
    return(res)
  }
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

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
#' prediction for the true value \eqn{x_t}. Computationally, \eqn{P_t (x_t)} is
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
#' respect to the true values.
#' @inheritParams ae_median_sample
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @examples
#'
#' ## integer valued forecasts
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' bias_sample(true_values, predictions)
#'
#' ## continuous forecasts
#' true_values <- rnorm(30, mean = 1:30)
#' predictions <- replicate(200, rnorm(30, mean = 1:30))
#' bias_sample(true_values, predictions)
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

bias_sample <- function(true_values, predictions) {

  # check inputs
  check_true_values(true_values)
  check_predictions(predictions, true_values, class = "matrix")
  prediction_type <- get_prediction_type(predictions)

  # empirical cdf
  n_pred <- ncol(predictions)
  p_x <- rowSums(predictions <= true_values) / n_pred

  if (prediction_type == "continuous") {
    res <- 1 - 2 * p_x
    return(res)
  } else {
    # for integer case also calculate empirical cdf for (y-1)
    p_xm1 <- rowSums(predictions <= (true_values - 1)) / n_pred

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
#' belief about what the true value \eqn{x_t} will be. For consistency, we
#' define
#' \eqn{Q_t} such that it always includes the element
#' \eqn{q_{t, 0} = - \infty} and \eqn{q_{t,1} = \infty}.
#' \eqn{\mathbf{1}()}{1()} is the indicator function that is \eqn{1} if the
#' condition is satisfied and $0$ otherwise. In clearer terms, \eqn{B_t} is
#' defined as the maximum percentile rank for which the corresponding quantile
#' is still below the true value, if the true value is smaller than the
#' median of the predictive distribution. If the true value is above the
#' median of the predictive distribution, then $B_t$ is the minimum percentile
#' rank for which the corresponding quantile is still larger than the true
#' value. If the true value is exactly the median, both terms cancel out and
#' \eqn{B_t} is zero. For a large enough number of quantiles, the
#' percentile rank will equal the proportion of predictive samples below the
#' observed true value, and this metric coincides with the one for
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
#' @param true_value a single true value
#' @return scalar with the quantile bias for a single quantile prediction
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
#' true_value <- 8062
#'
#' bias_range(
#'   lower = lower, upper = upper,
#'   range = range, true_value = true_value
#' )
#' @export
#' @keywords metric

bias_range <- function(range, lower, upper,
                       true_value) {
  lower_predictions <- lower
  upper_predictions <- upper

  if (anyNA(upper) | anyNA(lower)) {
    range <- range[!is.na(upper) & !is.na(lower)]
    lower_predictions <- lower[!is.na(lower) & !is.na(upper)]
    upper_predictions <- upper[!is.na(lower) & !is.na(upper)]

    # deal with the point forecast case where inputs may be NA
    if (length(range) == 0 |
        length(lower_predictions) == 0 |
        length(upper_predictions) == 0
      ) {
      return(NA_real_)
    }
  }

  # convert range to quantiles
  lower_quantiles <- abs(100 - range) / (2 * 100)
  upper_quantiles <- abs(100 + range) / (2 * 100)

  # handling if median is not available - compute median as mean between two
  # inner most quantiles
  if (0 %in% range) {
    median_prediction <- upper_predictions[range == 0]
  } else {
    median_prediction <-
      0.5 * upper_predictions[which.min(range)] +
      0.5 * lower_predictions[which.min(range)]
  }


  if (true_value == median_prediction) {
    bias <- 0
    return(bias)
  } else if (true_value < min(lower_predictions)) {
    lower <- 0
    bias <- 1 - 2 * lower
    return(bias)
  } else if (true_value > max(upper_predictions)) {
    upper <- 1
    bias <- 1 - 2 * upper
    return(bias)
  } else if (any(lower_predictions >= true_value)) {
    lower <- max(lower_quantiles[lower_predictions <= true_value])
    bias <- 1 - 2 * lower
    return(bias)
  } else if (any(upper_predictions <= true_value)) {
    upper <- min(upper_quantiles[upper_predictions >= true_value])
    bias <- 1 - 2 * upper
    return(bias)
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
#'  1( x_t \leq q_{t, 0.5}) \\
#' + (1 - 2 \cdot \min \{i | q_{t,i} \in Q_t \land q_{t,i} \geq x_t\})
#'  1( x_t \geq q_{t, 0.5}),}
#'
#' where \eqn{Q_t} is the set of quantiles that form the predictive
#' distribution at time \eqn{t}. They represent our
#' belief about what the true value $x_t$ will be. For consistency, we define
#' \eqn{Q_t} such that it always includes the element
#' \eqn{q_{t, 0} = - \infty$ and $q_{t,1} = \infty}.
#' \eqn{1()} is the indicator function that is \eqn{1} if the
#' condition is satisfied and $0$ otherwise. In clearer terms, \eqn{B_t} is
#' defined as the maximum percentile rank for which the corresponding quantile
#' is still below the true value, if the true value is smaller than the
#' median of the predictive distribution. If the true value is above the
#' median of the predictive distribution, then $B_t$ is the minimum percentile
#' rank for which the corresponding quantile is still larger than the true
#' value. If the true value is exactly the median, both terms cancel out and
#' \eqn{B_t} is zero. For a large enough number of quantiles, the
#' percentile rank will equal the proportion of predictive samples below the
#' observed true value, and this metric coincides with the one for
#' continuous forecasts.
#'
#' Bias can assume values between
#' -1 and 1 and is 0 ideally.
#' @param predictions vector of length corresponding to the number of quantiles
#' that holds predictions
#' @param quantiles vector of corresponding size with the quantiles for which
#' predictions were made
#' @inheritParams bias_range
#' @return scalar with the quantile bias for a single quantile prediction
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @examples
#'
#' predictions <- c(
#'   705.500, 1127.000, 4006.250, 4341.500, 4709.000, 4821.996,
#'   5340.500, 5451.000, 5703.500, 6087.014, 6329.500, 6341.000,
#'   6352.500, 6594.986, 6978.500, 7231.000, 7341.500, 7860.004,
#'   7973.000, 8340.500, 8675.750, 11555.000, 11976.500
#' )
#'
#' quantiles <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
#'
#' true_value <- 8062
#'
#' bias_quantile(predictions, quantiles, true_value = true_value)
#' @export
#' @keywords metric

bias_quantile <- function(predictions, quantiles, true_value) {
  # check that predictions and quantiles have the same length
  if (!length(predictions) == length(quantiles)) {
    stop("predictions and quantiles must have the same length")
  }

  if (anyNA(predictions)) {
    quantiles <- quantiles[!is.na(predictions)]
    predictions <- predictions[!is.na(predictions)]
  }
  # if there is no input, return NA
  if (length(quantiles) == 0 | length(predictions) == 0) {
    return(NA_real_)
  }

  if (0.5 %in% quantiles) {
    median_prediction <- predictions[quantiles == 0.5]
  } else {
    # if median is not available, compute as mean of two innermost quantiles
    median_prediction <-
      0.5 * predictions[quantiles == max(quantiles[quantiles < 0.5])] +
      0.5 * predictions[quantiles == min(quantiles[quantiles > 0.5])]
  }

  if (true_value == median_prediction) {
    bias <- 0
    return(bias)
  } else if (true_value < median_prediction) {
    if (true_value < min(predictions)) {
      bias <- 1
    } else {
      q <- max(quantiles[predictions <= true_value])
      bias <- 1 - 2 * q
    }
  } else if (true_value > median_prediction) {
    if (true_value > max(predictions)) {
      bias <- -1
    } else {
      q <- min(quantiles[predictions >= true_value])
      bias <- 1 - 2 * q
    }
  }
  return(bias)
}

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
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return vector of length n with the biases of the predictive samples with
#' respect to the true values.
#' @author Nikos Bosse \email{nikosbosse@gmail.com}
#' @examples
#'
#' ## integer valued forecasts
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' bias(true_values, predictions)
#'
#' ## continuous forecasts
#' true_values <- rnorm(30, mean = 1:30)
#' predictions <- replicate(200, rnorm(30, mean = 1:30))
#' bias(true_values, predictions)
#'
#'
#' @export
#' @references
#' The integer valued Bias function is discussed in
#' Assessing the performance of real-time epidemic forecasts: A case study of Ebola in the Western Area region of Sierra Leone, 2014-15
#' Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, et al. (2019) Assessing the performance of real-time epidemic forecasts: A case study of Ebola in the Western Area region of Sierra Leone, 2014-15. PLOS Computational Biology 15(2): e1006785. https://doi.org/10.1371/journal.pcbi.1006785


bias <- function(true_values, predictions) {

  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    msg <- sprintf("'predictions' should be a matrix. Instead `%s` was found",
                   class(predictions[1]))
    stop(msg)
  }
  if (nrow(predictions) != n) {

    msg <- sprintf("Mismatch: The true values provided have length `%s`, but 'predictions' has `%s` rows.",
                   n, nrow(predictions))
    stop(msg)
  }

  # ============================================

  ## check whether continuous or integer
  if (all.equal(as.vector(predictions), as.integer(predictions)) != TRUE) {
    continuous_predictions <- TRUE
  } else {
    continuous_predictions <- FALSE
  }

  n_pred <- ncol(predictions)

  # empirical cdf
  P_x <- vapply(seq_along(true_values),
                function(i) {
                  sum(predictions[i,] <= true_values[i]) / n_pred
                },
                .0)

  if (continuous_predictions) {
    res <- 1 - 2 * P_x
    return(res)
  } else {
    # for integer case also calculate empirical cdf for (y-1)
    P_xm1 <- vapply(seq_along(true_values),
                    function(i) {
                      sum(predictions[i,] <= true_values[i] - 1) / n_pred
                    },
                    .0)

    res <- 1 - (P_x + P_xm1)
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
#' B_t = (1 - 2 \cdot \max \{i | q_{t,i} \in Q_t \land q_{t,i} \leq x_t\}) 1( x_t \leq q_{t, 0.5}) \\
#' + (1 - 2 \cdot \min \{i | q_{t,i} \in Q_t \land q_{t,i} \geq x_t\}) 1( x_t \geq q_{t, 0.5}),}
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
#' @author Nikos Bosse \email{nikosbosse@gmail.com}
#' @examples
#'
#' lower <- c(6341.000, 6329.500, 6087.014, 5703.500,
#'            5451.000, 5340.500, 4821.996, 4709.000,
#'            4341.500, 4006.250, 1127.000, 705.500)
#'
#' upper <- c(6341.000, 6352.500, 6594.986, 6978.500,
#'            7231.000, 7341.500, 7860.004, 7973.000,
#'            8340.500, 8675.750, 11555.000, 11976.500)
#'
#' range <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98)
#'
#' true_value <- 8062
#'
#' quantile_bias(lower = lower, upper = upper,
#'               range = range, true_value = true_value)
#'
#' @export
#'

quantile_bias <- function(range, lower, upper,
                          true_value) {

  lower_predictions <- lower
  upper_predictions <- upper

  if(any(is.na(upper)) | any(is.na(lower))) {

    range <- range[!is.na(upper) & !is.na(lower)]
    lower_predictions <- lower[!is.na(lower) & !is.na(upper)]
    upper_predictions <- upper[!is.na(lower) & !is.na(upper)]

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
      0.5 * upper_predictions[range == min(range)] +
      0.5 * lower_predictions[range == min(range)]
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
  } else if (any(upper_predictions <= true_value)){
    upper <- min(upper_quantiles[upper_predictions >= true_value])
    bias <- 1 - 2 * upper
    return(bias)
  }
}

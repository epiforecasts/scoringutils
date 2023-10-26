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


#' @title Absolute Error of the Median (Sample-based Version)
#'
#' @description
#' Absolute error of the median calculated as
#'
#' \deqn{%
#'   \textrm{abs}(\textrm{observevd} - \textrm{median\_prediction})
#' }{%
#'   abs(observed - median_prediction)
#' }
#'
#' @param observed A vector with the observed values of size n
#' @param predicted nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the number of Monte
#' Carlo samples. Alternatively, `predicted` can just be a vector of size n.
#' @return vector with the scoring values
#' @seealso [ae_median_quantile()], [abs_error()]
#' @importFrom stats median
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' ae_median_sample(observed, predicted_values)
#' @export
#' @keywords metric

ae_median_sample <- function(observed, predicted) {
  median_predictions <- apply(
    as.matrix(predicted), MARGIN = 1, FUN = median # this is rowwise
  )

  ae_median <- abs(observed - median_predictions)

  return(ae_median)
}


#' @title Squared Error of the Mean (Sample-based Version)
#'
#' @description
#' Squared error of the mean calculated as
#'
#' \deqn{
#'   \textrm{mean}(\textrm{observed} - \textrm{prediction})^2
#' }{
#'   mean(observed - mean_prediction)^2
#' }
#'
#' @param observed A vector with the observed values of size n
#' @param predicted nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the number of Monte
#' Carlo samples. Alternatively, `predicted` can just be a vector of size n.
#' @return vector with the scoring values
#' @seealso [squared_error()]
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' se_mean_sample(observed, predicted_values)
#' @export
#' @keywords metric

se_mean_sample <- function(observed, predicted) {
  mean_predictions <- rowMeans(as.matrix(predicted))
  se_mean <- (observed - mean_predictions)^2

  return(se_mean)
}


#' @title Logarithmic score
#'
#' @description
#' Wrapper around the [`logs_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package. Used to score continuous predictions.
#' While the Log Score is in theory also applicable
#' to integer forecasts, the problem lies in the implementation: The Log Score
#' needs a kernel density estimation, which is not well defined with
#' integer-valued Monte Carlo Samples. The Log Score can be used for specific
#' integer valued probability distributions. See the scoringRules package for
#' more details.
#' @inheritParams ae_median_sample
#' @param ... additional arguments passed to
#' [logs_sample()][scoringRules::logs_sample()] from the scoringRules package.
#' @return vector with the scoring values
#' @importFrom scoringRules logs_sample
#' @examples
#' observed <- rpois(30, lambda = 1:30)
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' logs_sample(observed, predicted)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

logs_sample <- function(observed, predicted, ...) {
  assert_input_sample(observed, predicted)
  scoringRules::logs_sample(
    y = observed,
    dat = predicted,
    ...
  )
}

#' @title Dawid-Sebastiani Score
#'
#' @description
#' Wrapper around the [`dss_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package.
#' @inheritParams logs_sample
#' @param ... additional arguments passed to
#' [dss_sample()][scoringRules::dss_sample()] from the scoringRules package.
#' @return vector with scoring values
#' @importFrom scoringRules dss_sample
#' @examples
#' observed <- rpois(30, lambda = 1:30)
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' dss_sample(observed, predicted)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

dss_sample <- function(observed, predicted, ...) {
  assert_input_sample(observed, predicted)

  scoringRules::dss_sample(
    y = observed,
    dat = predicted,
    ...
  )
}

#' @title Ranked Probability Score
#'
#' @description
#' Wrapper around the [`crps_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package. Can be used for continuous as well as integer
#' valued forecasts
#' @inheritParams logs_sample
#' @param ... additional arguments passed to
#' [crps_sample()][scoringRules::crps_sample()] from the scoringRules package.
#' @return vector with the scoring values
#' @importFrom scoringRules crps_sample
#' @examples
#' observed <- rpois(30, lambda = 1:30)
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' crps_sample(observed, predicted)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

crps_sample <- function(observed, predicted, ...) {
  assert_input_sample(observed, predicted)

  scoringRules::crps_sample(
    y = observed,
    dat = predicted,
    ...
  )
}


#' @title Determine dispersion of a probabilistic forecast
#' @details
#' Sharpness is the ability of the model to generate predictions within a
#' narrow range and dispersion is the lack thereof.
#' It is a data-independent measure, and is purely a feature
#' of the forecasts themselves.
#'
#' Dispersion of predictive samples corresponding to one single observed value is
#' measured as the normalised median of the absolute deviation from
#' the median of the predictive samples. For details, see [mad()][stats::mad()]
#' and the explanations given in Funk et al. (2019)
#'
#' @inheritParams ae_median_sample
#' @param observed place holder, argument will be ignored and exists only for
#' consistency with other scoring functions. The output does not depend on
#' any observed values.
#' @param ... additional arguments passed to [mad()][stats::mad()].
#' @importFrom stats mad
#' @return vector with dispersion values
#'
#' @references
#' Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, Edmunds WJ (2019)
#' Assessing the performance of real-time epidemic forecasts: A case study of
#' Ebola in the Western Area region of Sierra Leone, 2014-15.
#' PLoS Comput Biol 15(2): e1006785. \doi{10.1371/journal.pcbi.1006785}
#'
#' @export
#' @examples
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' mad_sample(predicted = predicted)
#' @keywords metric

mad_sample <- function(observed = NULL, predicted, ...) {

  assert_input_sample(rep(NA_real_, nrow(predicted)), predicted)

  sharpness <- apply(predicted, MARGIN = 1, mad, ...)
  return(sharpness)
}


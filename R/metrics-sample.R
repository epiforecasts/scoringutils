#' @title Determine bias of forecasts
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
#' @return
#' Numeric vector of length n with the biases of the predictive samples with
#' respect to the observed values.
#' @inheritParams ae_median_sample
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
  prediction_type <- get_type(predicted)

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


#' @title Absolute error of the median (sample-based version)
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
#' @param observed A vector with observed values of size n
#' @param predicted nxN matrix of predictive samples, n (number of rows) being
#'   the number of data points and N (number of columns) the number of Monte
#'   Carlo samples. Alternatively, `predicted` can just be a vector of size n.
#' @return vector with the scoring values
#' @seealso [ae_median_quantile()]
#' @importFrom stats median
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- matrix(rnorm(30, mean = 1:30))
#' ae_median_sample(observed, predicted_values)
#' @export
#' @keywords metric

ae_median_sample <- function(observed, predicted) {
  assert_input_sample(observed, predicted)
  median_predictions <- apply(
    as.matrix(predicted), MARGIN = 1, FUN = median # this is row-wise
  )
  ae_median <- abs(observed - median_predictions)
  return(ae_median)
}


#' @title Squared error of the mean (sample-based version)
#'
#' @description
#' Squared error of the mean calculated as
#'
#' \deqn{
#'   \textrm{mean}(\textrm{observed} - \textrm{mean prediction})^2
#' }{
#'   mean(observed - mean prediction)^2
#' }
#' The mean prediction is calculated as the mean of the predictive samples.
#' @inheritParams ae_median_sample
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- matrix(rnorm(30, mean = 1:30))
#' se_mean_sample(observed, predicted_values)
#' @export
#' @keywords metric

se_mean_sample <- function(observed, predicted) {
  assert_input_sample(observed, predicted)
  mean_predictions <- rowMeans(as.matrix(predicted))
  se_mean <- (observed - mean_predictions)^2

  return(se_mean)
}


#' @title Logarithmic score (sample-based version)
#'
#' @description
#' This function is a wrapper around the
#' [`logs_sample()`][scoringRules::scores_sample_univ] function from the
#' \pkg{scoringRules} package.
#'
#' The function should be used to score continuous predictions only.
#' While the Log Score is in theory also applicable
#' to discrete forecasts, the problem lies in the implementation: The Log score
#' needs a kernel density estimation, which is not well defined with
#' integer-valued Monte Carlo Samples. The Log score can be used for specific
#' discrete probability distributions. See the scoringRules package for
#' more details.
#' @inheritParams ae_median_sample
#' @param ... Additional arguments passed to
#' [logs_sample()][scoringRules::logs_sample()] from the scoringRules package.
#' @return Vector with scores.
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

#' @title Dawid-Sebastiani score
#'
#' @description
#' Wrapper around the [`dss_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package.
#' @inheritParams logs_sample
#' @param ... Additional arguments passed to
#' [dss_sample()][scoringRules::dss_sample()] from the scoringRules package.
#' @return Vector with scores.
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

#' @title (Continuous) ranked probability score
#'
#' @description
#' Wrapper around the [`crps_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package. Can be used for continuous as well as integer
#' valued forecasts
#'
#' The Continuous ranked probability score (CRPS) can be interpreted as the sum
#' of three components: overprediction,  underprediction and dispersion.
#' "Dispersion" is defined as the CRPS of the median forecast $m$. If an
#' observation $y$ is greater than $m$ then overpredictoin is defined as the
#' CRPS of the forecast for $y$ minus the dispersion component, and
#' underprediction is zero. If, on the other hand, $y<m$ then underprediction
#' is defined as the CRPS of the forecast for $y$ minus the dispersion
#' component, and overprediction is zero.
#'
#' The overprediction, underprediction and dispersion components correspond to
#' those of the [wis()].
#'
#' @inheritParams logs_sample
#' @param separate_results Logical. If `TRUE` (default is `FALSE`), then the
#'   separate parts of the CRPS (dispersion penalty, penalties for
#'   over- and under-prediction) get returned as separate elements of a list.
#'   If you want a `data.frame` instead, simply call [as.data.frame()] on the
#'   output.
#' @param ... Additional arguments passed to
#' [crps_sample()][scoringRules::crps_sample()] from the scoringRules package.
#' @return Vector with scores.
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

crps_sample <- function(observed, predicted, separate_results = FALSE, ...) {
  assert_input_sample(observed, predicted)

  crps <- scoringRules::crps_sample(
    y = observed,
    dat = predicted,
    ...
  )

  if (separate_results) {
    medians <- apply(predicted, 1, median)
    dispersion <- scoringRules::crps_sample(
      y = medians,
      dat = predicted,
      ...
    )
    overprediction <- rep(0, length(observed))
    underprediction <- rep(0, length(observed))

    overprediction[observed < medians] <- scoringRules::crps_sample(
      y = observed[observed < medians],
      dat = predicted[observed < medians, , drop = FALSE],
      ...
    )
    underprediction[observed > medians] <- scoringRules::crps_sample(
      y = observed[observed > medians],
      dat = predicted[observed > medians, , drop = FALSE],
      ...
    )

    overprediction[overprediction > 0] <-
      overprediction[overprediction > 0] - dispersion[overprediction > 0]
    underprediction[underprediction > 0] <-
      underprediction[underprediction > 0] - dispersion[underprediction > 0]

    return(list(
      crps = crps,
      dispersion = dispersion,
      underprediction = underprediction,
      overprediction = overprediction
    ))
  } else {
    return(crps)
  }
}

#' @return
#' `dispersion_sample()`: a numeric vector with dispersion values (one per
#' observation).
#' @param ... Additional arguments passed on to `crps_sample()` from functions
#'   `overprediction_sample()`, `underprediction_sample()` and
#' `dispersion_sample()`.
#' @export
#' @rdname crps_sample
#' @keywords metric
dispersion_sample <- function(observed, predicted, ...) {
  crps <- crps_sample(observed, predicted, separate_results = TRUE, ...)
  return(crps$dispersion)
}

#' @return
#' `overprediction_quantile()`: a numeric vector with overprediction values
#' (one per observation).
#' @export
#' @rdname crps_sample
#' @keywords metric
overprediction_sample <- function(observed, predicted, ...) {
  crps <- crps_sample(observed, predicted, separate_results = TRUE, ...)
  return(crps$overprediction)
}

#' @return
#' `underprediction_quantile()`: a numeric vector with underprediction values (one per
#' observation).
#' @export
#' @rdname crps_sample
#' @keywords metric
underprediction_sample <- function(observed, predicted, ...) {
  crps <- crps_sample(observed, predicted, separate_results = TRUE, ...)
  return(crps$underprediction)
}

#' @title Determine dispersion of a probabilistic forecast
#' @description
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
#' @param observed Place holder, argument will be ignored and exists only for
#' consistency with other scoring functions. The output does not depend on
#' any observed values.
#' @param ... Additional arguments passed to [mad()][stats::mad()].
#' @importFrom stats mad
#' @return Vector with dispersion values.
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

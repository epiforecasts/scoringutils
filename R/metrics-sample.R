#' @title Assert that inputs are correct for sample-based forecast
#' @description
#' Function assesses whether the inputs correspond to the requirements for
#' scoring sample-based forecasts.
#' @param predicted Input to be checked. Should be a numeric nxN matrix of
#'   predictive samples, n (number of rows) being the number of data points and
#'   N (number of columns) the number of samples per forecast.
#'   If `observed` is just a single number, then predicted values can just be a
#'   vector of size N.
#' @importFrom checkmate assert assert_numeric check_matrix assert_matrix
#' @inherit document_assert_functions params return
#' @keywords internal_input_check
assert_input_sample <- function(observed, predicted) {
  assert_numeric(observed, min.len = 1)
  n_obs <- length(observed)

  if (n_obs == 1) {
    assert(
      # allow one of two options
      check_numeric_vector(predicted, min.len = 1),
      check_matrix(predicted, mode = "numeric", nrows = n_obs)
    )
  } else {
    assert_matrix(predicted, mode = "numeric", nrows = n_obs)
  }
  return(invisible(NULL))
}

#' @title Check that inputs are correct for sample-based forecast
#' @inherit assert_input_sample params description
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_input_sample <- function(observed, predicted) {
  result <- check_try(assert_input_sample(observed, predicted))
  return(result)
}


#' @title Determine bias of forecasts
#'
#' @description
#' Determines bias from predictive Monte-Carlo samples. The function
#' automatically recognises whether forecasts are continuous or
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
#' @inheritSection illustration-input-metric-sample Input format
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
#' \deqn{
#'   |\text{observed} - \text{median prediction}|
#' }
#' where the median prediction is calculated as the median of the predictive
#' samples.
#'
#' @param observed A vector with observed values of size n
#' @param predicted nxN matrix of predictive samples, n (number of rows) being
#'   the number of data points and N (number of columns) the number of Monte
#'   Carlo samples. Alternatively, `predicted` can just be a vector of size n.
#' @inheritSection illustration-input-metric-sample Input format
#' @returns Numeric vector of length n with the absolute errors of the median.
#' @seealso [ae_median_quantile()]
#' @importFrom stats median
#' @keywords metric
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- matrix(rnorm(30, mean = 1:30))
#' ae_median_sample(observed, predicted_values)
#' @export
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
#' @inheritSection illustration-input-metric-sample Input format
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
#' The log score is the negative logarithm of the predictive density evaluated
#' at the observed value.
#'
#' The function should be used to score continuous predictions only.
#' While the Log Score is in theory also applicable
#' to discrete forecasts, the problem lies in the implementation: The function
#' uses a kernel density estimation, which is not well defined with
#' integer-valued Monte Carlo Samples.
#' See the scoringRules package for more details and alternatives, e.g.
#' calculating scores for specific discrete probability distributions.
#' @inheritParams ae_median_sample
#' @param ... Additional arguments passed to
#' [logs_sample()][scoringRules::logs_sample()] from the scoringRules package.
#' @inheritSection illustration-input-metric-sample Input format
#' @returns Vector with scores.
#' @importFrom scoringRules logs_sample
#' @family log score functions
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
#' @inheritSection illustration-input-metric-sample Input format
#' @returns Vector with scores.
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
#' @inheritSection illustration-input-metric-sample Input format
#' @returns Vector with scores.
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
    if (is.null(dim(predicted))) {
      ## if `predicted` is a vector convert to matrix
      dim(predicted) <- c(1, length(predicted))
    }
    medians <- apply(predicted, 1, median)
    dispersion <- scoringRules::crps_sample(
      y = medians,
      dat = predicted,
      ...
    )
    difference <- crps - dispersion

    overprediction <- fcase(observed < medians, difference, default = 0)
    underprediction <- fcase(observed > medians, difference, default = 0)

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
#' @inheritSection illustration-input-metric-sample Input format
#' @returns Vector with dispersion values.
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


#' @title Probability integral transformation for counts
#'
#' @description Uses a Probability integral transformation (PIT) (or a
#' randomised PIT for integer forecasts) to
#' assess the calibration of predictive Monte Carlo samples.
#'
#' @details
#' Calibration or reliability of forecasts is the ability of a model to
#' correctly identify its own uncertainty in making predictions. In a model
#' with perfect calibration, the observed data at each time point look as if
#' they came from the predictive probability distribution at that time.
#'
#' Equivalently, one can inspect the probability integral transform of the
#' predictive distribution at time t,
#'
#' \deqn{
#' u_t = F_t (x_t)
#' }
#'
#' where \eqn{x_t} is the observed data point at time \eqn{t \textrm{ in } t_1,
#' …, t_n}{t in t_1, …, t_n}, n being the number of forecasts, and \eqn{F_t} is
#' the (continuous) predictive cumulative probability distribution at time t. If
#' the true probability distribution of outcomes at time t is \eqn{G_t} then the
#' forecasts \eqn{F_t} are said to be ideal if \eqn{F_t = G_t} at all times t.
#' In that case, the probabilities \eqn{u_t} are distributed uniformly.
#'
#' In the case of discrete nonnegative outcomes such as incidence counts,
#' the PIT is no longer uniform even when forecasts are ideal.
#' In that case two methods are available ase described by Czado et al. (2007).
#'
#' By default, a nonrandomised PIT is calculated using the conditional
#' cumulative distribution function
#' \deqn{
#'   F(u) =
#'   \begin{cases}
#'     0 & \text{if } v < P_t(k_t - 1) \\
#'     (v - P_t(k_t - 1)) / (P_t(k_t) - P_t(k_t - 1)) & \text{if } P_t(k_t - 1) \leq v < P_t(k_t) \\
#'     1 & \text{if } v \geq P_t(k_t)
#'   \end{cases}
#' }
#'
#' where \eqn{k_t} is the observed count, \eqn{P_t(x)} is the predictive
#' cumulative probability of observing incidence \eqn{k} at time \eqn{t} and
#' \eqn{P_t (-1) = 0} by definition.
#' Values of the PIT histogram are then created by averaging over the \eqn{n}
#' predictions,
#'
#' \deqn{
#'    \bar{F}(u) = \frac{i = 1}{n} \sum_{i=1}^{n} F^{(i)}(u)
#' }
#'
#' And calculating the value at each bin between quantile \eqn{q_i} and quantile
#' \eqn{q_{i + 1}} as
#'
#' \deqn{
#'    \bar{F}(q_i) - \bar{F}(q_{i + 1})
#' }
#'
#' Alternatively, a randomised PIT can be used instead. In this case, the PIT is
#' \deqn{
#'   u_t = P_t(k_t) + v * (P_t(k_t) - P_t(k_t - 1))
#' }
#'
#' where \eqn{v} is standard uniform and independent of \eqn{k}. The values of
#' the PIT histogram are then calculated by binning the \eqn{u_t} values as above.
#'
#' @param quantiles A vector of quantiles between which to calculate the PIT.
#' @param integers How to handle integer forecasts (count data). This is based
#'   on methods described Czado et al. (2007). If "nonrandom" (default) the
#'   function will use the non-randomised PIT method. If "random", will use the
#'   randomised PIT method. If "ignore", will treat integer forecasts as if they
#'   were continuous.
#' @param n_replicates The number of draws for the randomised PIT for discrete
#'   predictions. Will be ignored if forecasts are continuous or `integers` is
#'   not set to `random`.
#' @inheritParams ae_median_sample
#' @inheritParams get_pit_histogram
#' @returns A vector with PIT histogram densities for the bins corresponding
#'   to the given quantiles.
#' @seealso [get_pit_histogram()]
#' @importFrom stats runif
#' @importFrom data.table fcase
#' @importFrom cli cli_warn cli_abort
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' ## continuous predictions
#' observed <- rnorm(20, mean = 1:20)
#' predicted <- replicate(100, rnorm(n = 20, mean = 1:20))
#' pit <- pit_histogram_sample(observed, predicted, quantiles = seq(0, 1, 0.1))
#'
#' ## integer predictions
#' observed <- rpois(20, lambda = 1:20)
#' predicted <- replicate(100, rpois(n = 20, lambda = 1:20))
#' pit <- pit_histogram_sample(observed, predicted, quantiles = seq(0, 1, 0.1))
#'
#' ## integer predictions, randomised PIT
#' observed <- rpois(20, lambda = 1:20)
#' predicted <- replicate(100, rpois(n = 20, lambda = 1:20))
#' pit <- pit_histogram_sample(
#'   observed, predicted, quantiles = seq(0, 1, 0.1),
#'   integers = "random", n_replicates = 30
#' )
#' @export
#' @references
#' Claudia Czado, Tilmann Gneiting Leonhard Held (2009) Predictive model
#' assessment for count data. Biometrika, 96(4), 633-648.
#
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, \doi{10.1371/journal.pcbi.1006785}
#' @keywords metric
pit_histogram_sample <- function(observed,
                                 predicted,
                                 quantiles,
                                 integers = c("nonrandom", "random", "ignore"),
                                 n_replicates = NULL) {
  assert_input_sample(observed = observed, predicted = predicted)
  integers <- match.arg(integers)
  assert_number(
    n_replicates, null.ok = (integers != "random"),
    .var.name = paste("n_replicates with `integers` = ", integers)
  )
  if (is.vector(predicted)) {
    predicted <- matrix(predicted, nrow = 1)
  }

  if (integers != "random" && !is.null(n_replicates)) {
    cli::cli_warn("`n_replicates` is ignored when `integers` is not `random`")
  }

  # calculate PIT-values -------------------------------------------------------
  n_pred <- ncol(predicted)

  # calculate empirical cumulative distribution function as
  # Portion of (y_observed <= y_predicted)
  p_x <- rowSums(predicted <= observed) / n_pred

  # PIT calculation is different for integer and continuous predictions
  predicted <- round(predicted)
  if (get_type(predicted) == "integer" && integers != "ignore") {
    p_xm1 <- rowSums(predicted <= (observed - 1)) / n_pred
    if (integers == "random") {
      pit_values <- as.vector(
        replicate(n_replicates, p_xm1 + runif(1) * (p_x - p_xm1))
      )
    } else {
      f_bar <- function(u) {
        f <- fcase(
          u <= p_xm1, 0,
          u >= p_x, 1,
          default = (u - p_xm1) / (p_x - p_xm1)
        )
        mean(f)
      }
      pit_histogram <- diff(vapply(quantiles, f_bar, numeric(1))) /
        diff(quantiles)
    }
  } else {
    pit_values <- p_x
  }

  if (integers != "nonrandom") {
    pit_histogram <- hist(pit_values, breaks = quantiles, plot = FALSE)$density
  }

  return(pit_histogram)
}

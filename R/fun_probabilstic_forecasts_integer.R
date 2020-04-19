#' @title randomised Probability Integral Transformation
#'
#' @description Uses a (randomised) Probability Integral Transformation (PIT) to
#' assess the calibration of predictive Monte Carlo samples. Returns a
#' randomised PIT histogram and p-values resulting from an Anderson-Darling
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
#' where \eqn{x_t} is the observed data point at time \eqn{t in t_1, …, t_n},
#' n being the number of forecasts, and $F_t$ is the (continuous) predictive
#' cumulative probability distribution at time t. If the true probability
#' distribution of outcomes at time t is \eqn{G_t} then the forecasts eqn{F_t} are
#' said to be ideal if eqn{F_t = G_t} at all times t. In that case, the
#' probabilities ut are distributed uniformly.
#'
#' In the case of discrete outcomes such as the incidence counts that were
#' forecast here, the PIT is no longer uniform even when forecasts are ideal.
#' In that case a randomised PIT can be used instead:
#' \deqn{
#' u_t = P_t(k_t) + v * (P_t(k_t) - P_t(k_t - 1) )
#' }
#'
#' where \eqn{k_t} is the observed count, \eqn{P_t(x)} is the predictive
#' cumulative probability of observing incidence k at time t,
#' eqn{P_t (-1) = 0} by definition and v is standard uniform and independent
#' of k. If \eqn{P_t} is the true cumulative
#' probability distribution, then \eqn{u_t} is standard uniform.
#'
#' The resulting values \eqn{u_t} are then tested for uniformity using the
#' Anderson-Darling test.
#'
#' As a rule of thumb, there is no evidence to suggest a forecasting model is
#' miscalibrated if the p-value found was greater than a threshold of p >= 0.1,
#' some evidence that it was miscalibrated if 0.01 < p < 0.1, and good
#' evidence that it was miscalibrated if p <= 0.01.
#' In this context it should be noted, though, that uniformity of the
#' (randomised) PIT is a necessary but not sufficient condition of calibration.
#'
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param n_replicates the number of tests to perform,
#' each time re-randomising the PIT
#' @param plot logical. If TRUE, a histogram of the PIT values will be returned
#' as well
#' @param num_bins the number of bins in the PIT histogram (provided plot == TRUE)
#' If not given, the square root of n will be used
#' @return a list with the following components:
#' \itemize{
#' \item \code{p_values}: p-values from the
#' Anderson-Darling test on the \code{n_replicate} replicates of the randomised
#' PIT
#' \item \code{hist_PIT} a ggplot object with the PIT histogram. Only returned
#' if \code{plot == TRUE}. Call
#' \code{plot(PIT(...)$hist_PIT)} to display the histogram.
#' \item \code{calibration}: mean and standard deviation of the p-values of the
#' \code{n_replicates} Anderson-Darling tests. This can be used as a summary
#' to assess calibration of the predictions.
#' }
#'
#' @importFrom stats runif sd
#' @importFrom ggplot2 ggplot aes geom_histogram
#' @importFrom goftest ad.test
#'
#' @examples
#' true_values <- rpois(100, lambda = 1:100)
#' predictions <- replicate(5000, rpois(n = 100, lambda = 1:100))
#' pit <- pit(true_values, predictions)
#' plot(pit$hist_PIT)
#'
#' @export
#' @references
#' Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, Edmunds WJ (2019)
#' Assessing the performance of real-time epidemic forecasts: A case study of
#' Ebola in the Western Area region of Sierra Leone, 2014-15.
#' PLoS Comput Biol 15(2): e1006785.
#' \url{https://doi.org/10.1371/journal.pcbi.1006785}
#'
#' Gneiting, T., Balabdaoui, F. and Raftery, A.E. (2007), Probabilistic
#' forecasts, calibration and sharpness. Journal of the Royal Statistical
#' Society: Series B (Statistical Methodology), 69: 243-268.
#' \url{https://doi.org/10.1111/j.1467-9868.2007.00587.x}
#'
#' Dawid, A. (1984). Present Position and Potential Developments: Some Personal
#' Views: Statistical Theory: The Prequential Approach. Journal of the Royal
#' Statistical Society. Series A (General), 147(2), 278-292.
#' doi:10.2307/2981683
#'
#' @seealso
#' Czado, C., Gneiting, T. and Held, L. (2009),
#' Predictive Model Assessment for Count Data. Biometrics, 65:
#' 1254-1261. \url{doi:10.1111/j.1541-0420.2009.01191.x}
#'
#'

pit_int <- function(true_values,
                    predictions,
                    n_replicates = 20,
                    plot = TRUE,
                    num_bins = NULL) {


  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  if (all.equal(true_values, as.integer(true_values)) != TRUE) {
    warning("The true_values provided are not integers. Don't trust the results.
            Maybe you want to score continuous predictions instead?")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    stop("'predictions' should be a matrix")
  }
  if (nrow(predictions) != n) {
    msg = cat("matrix 'predictions' must have n rows, ",
              "where n is the number of true_values to predict. ")
    stop(msg)
  }
  if (all.equal(as.vector(predictions), as.integer(predictions)) != TRUE) {
    warning("predictions provided are not integers. Don't trust the results.
        Maybe you want to score continuous predictions instead?")
  }

  # ============================================

  n_pred <- ncol(predictions)

  # calculate emipirical cumulative distribution function as
  # Portion of (y_true <= y_predicted)
  P_x <- vapply(seq_along(true_values),
                function(i) {
                  sum(predictions[i,] <= true_values[i]) / n_pred
                },
                .0)

  # epirical cdf for (y-1). Only used for for integer-valued
  # predictions and true-values.
  P_xm1 <- vapply(seq_along(true_values),
                  function(i) {
                    sum(predictions[i,] <= true_values[i] - 1) / n_pred
                  },
                  .0)

  u <- replicate(n_replicates, P_xm1 + runif(n) * (P_x - P_xm1))

  p_values <- apply(
    u,
    MARGIN = 2,
    FUN = function (x) {
      goftest::ad.test(x)$p.value
    }
  )

  calibration <- data.frame(mean = mean(p_values),
                            sd = sd(p_values))

  out <- list(p_values = p_values,
              calibration = calibration,
              u = u)

  if (plot == TRUE) {
    hist_PIT <- scoringutils::hist_PIT(rowMeans(u), num_bins = num_bins)
    out$hist_PIT = hist_PIT
  }

  return(out)
}





#' @title Determines sharpness of a probabilistic forecast
#' @details
#' Sharpness is the ability of the model to generate predictions within a
#' narrow range. It is a data-independent measure, and is purely a feature
#' of the forecasts themselves.
#'
#' Shaprness of predictive samples corresponding to one single true value is
#' measured as the normalised median of the absolute deviation from
#' the median of the predictive samples. For details, see \link[stats]{mad}
#'
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @importFrom stats mad
#' @return vector with sharpness values
#'
#' @references
#' Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, Edmunds WJ (2019)
#' Assessing the performance of real-time epidemic forecasts: A case study of
#' Ebola in the Western Area region of Sierra Leone, 2014-15.
#' PLoS Comput Biol 15(2): e1006785.
#' \url{https://doi.org/10.1371/journal.pcbi.1006785}
#'
#' @export
#' @examples
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' sharpness(predictions)

sharpness <- function (predictions) {

  # ============== Error handling ==============

  if (missing(predictions)) {
    stop("predictions argument missing")
  }

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }

  if (!is.matrix(predictions)) {
    stop("'predictions' should be a matrix")
  }


  # ============================================

  sharpness <- apply(predictions, MARGIN = 1, mad)
  return(sharpness)
}




#' @title Dawid-Sebastiani Score
#'
#' @description
#' Wrapper around the \code{\link[scoringRules]{dss_sample}} function from the
#' \code{scoringRules} package.
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return vector with scoring values
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' dss(true_values, predictions)
#' @export


dss <- function(true_values, predictions) {
  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  if (all.equal(true_values, as.integer(true_values)) != TRUE) {
    warning("The true_values provided are not integers. Don't trust the results.
            Maybe you want to score continuous predictions instead?")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    stop("'predictions' should be a matrix")
  }
  if (nrow(predictions) != n) {
    msg = cat("matrix 'predictions' must have n rows, ",
              "where n is the number of true_values to predict. ")
    stop(msg)
  }
  if (all.equal(as.vector(predictions), as.integer(predictions)) != TRUE) {
    warning("predictions provided are not integers. Don't trust the results.
        Maybe you want to score continuous predictions instead?")
  }

  # ============================================

  scoringRules::dss_sample(y = true_values,
                           dat = predictions)
}



#' @title Ranked Probability Score
#'
#' @description
#' Wrapper around the \code{\link[scoringRules]{crps_sample}} function from the
#' \code{scoringRules} package. Can be used for continuous as well as integer
#' valued forecasts
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return vector with the scoring values
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' crps(true_values, predictions)
#' @export


crps <- function(true_values, predictions) {

  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    stop("'predictions' should be a matrix")
  }
  if (nrow(predictions) != n) {
    msg = cat("matrix 'predictions' must have n rows, ",
              "where n is the number of true_values to predict. ")
    stop(msg)
  }

    # ============================================

  scoringRules::crps_sample(y = true_values,
                           dat = predictions)
}






#' @title Error Handling Probabilistic Integer Forecasts
#'
#' @description
#' Does internal error handling
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @return updated predictions
#' @author Nikos Bosse \email{nikosbosse@gmail.com}
#' @export


error_handling_prob_int <- function(true_values, predictions) {
  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  if (all.equal(true_values, as.integer(true_values)) != TRUE) {
    warning("The true_values provided are not integers. Don't trust the results.
            Maybe you want to score continuous predictions instead?")
  }

  n <- length(true_values)

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    stop("'predictions' should be a matrix")
  }
  if (nrow(predictions) != n) {
    msg = cat("matrix 'predictions' must have n rows, ",
              "where n is the number of true_values to predict. ")
    stop(msg)
  }
  if (all.equal(as.vector(predictions), as.integer(predictions)) != TRUE) {
    warning("predictions provided are not integers. Don't trust the results.
        Maybe you want to score continuous predictions instead?")
  }

  return(predictions)
}








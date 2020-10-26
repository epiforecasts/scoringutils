#' @title Probability Integral Transformation
#'
#' @description Uses a Probability Integral Transformation (PIT) (or a
#' randomised PIT for integer forecasts) to
#' assess the calibration of predictive Monte Carlo samples. Returns a
#' p-values resulting from an Anderson-Darling test for uniformity
#' of the (randomised) PIT as well as a PIT histogram if specified.
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
#' In the case of discrete outcomes such as incidence counts,
#' the PIT is no longer uniform even when forecasts are ideal.
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
#' The function checks whether integer or continuous forecasts were provided.
#' It then applies the (randomised) probability integral and tests
#' the values \eqn{u_t} for uniformity using the
#' Anderson-Darling test.
#'
#' As a rule of thumb, there is no evidence to suggest a forecasting model is
#' miscalibrated if the p-value found was greater than a threshold of p >= 0.1,
#' some evidence that it was miscalibrated if 0.01 < p < 0.1, and good
#' evidence that it was miscalibrated if p <= 0.01.
#' In this context it should be noted, though, that uniformity of the
#' PIT is a necessary but not sufficient condition of calibration.
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param plot logical. If TRUE, a histogram of the PIT values will be returned
#' as well
#' @param num_bins the number of bins in the PIT histogram (if plot == TRUE)
#' If not given, the square root of n will be used
#' @param n_replicates the number of tests to perform,
#' each time re-randomising the PIT
#' @param full_output return all individual p_values and computed u_t values
#' for the randomised PIT. Usually not needed.
#' @param verbose if TRUE (default is FALSE) more error messages are printed.
#' Usually, this should not be needed, but may help with debugging.
#' @return a list with the following components:
#' \itemize{
#' \item \code{p_value}: p-value of the Anderson-Darling test on the
#' PIT values. In case of integer forecasts, this will be the mean p_value
#' from the `n_replicates` replicates
#' \item \code{sd}: standard deviation of the p_value returned. In case of
#' continuous forecasts, this will be NA as there is only one p_value returned.
#' \item \code{hist_PIT} a plot object with the PIT histogram. Only returned
#' if \code{plot == TRUE}. Call
#' \code{plot(PIT(...)$hist_PIT)} to display the histogram.
#' \item \code{p_values}: all p_values generated from the Anderson-Darling tests on the
#' randomised PIT. Only returned for integer forecasts
#' and if \code{full_output = TRUE}
#' \item \code{u}: the u_t values internally computed. Only returned if
#' \code{full_output = TRUE}
#' }
#' @importFrom goftest ad.test
#' @importFrom stats runif sd
#' @examples
#'
#' ## continuous predictions
#' true_values <- rnorm(30, mean = 1:30)
#' predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
#' pit(true_values, predictions)
#'
#' ## integer predictions
#' true_values <- rpois(100, lambda = 1:100)
#' predictions <- replicate(5000, rpois(n = 100, lambda = 1:100))
#' pit(true_values, predictions,  n_replicates = 5)
#'
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, <doi:10.1371/journal.pcbi.1006785>


pit <- function(true_values,
                predictions,
                plot = TRUE,
                full_output = FALSE,
                n_replicates = 20,
                num_bins = NULL,
                verbose = FALSE) {



  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  ## check whether continuous or integer
  if (all.equal(as.vector(predictions), as.integer(predictions)) != TRUE) {
    continuous_predictions <- TRUE
  } else {
    continuous_predictions <- FALSE
  }

  n <- length(true_values)


  if (n == 1) {
    if (verbose) {
      message("you need more than one observation to assess uniformity of the PIT")
    }
    out <- list(p_value = NA,
                sd = NA)

    if (full_output) {
      out <- list(p_values = NA,
                  calibration = NA,
                  u = NA)
    }
  }

  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    msg <- sprintf("'predictions' should be a matrix. Instead `%s` was found",
                   class(predictions[1]))
    stop(msg)
  }
  if (nrow(predictions) != n) {

    msg <- sprintf("Mismatch: 'true_values' has length `%s`, but 'predictions' has `%s` rows.",
                   n, nrow(predictions))
    stop(msg)
  }

  # ============================================

  n_pred <- ncol(predictions)

  # calculate emipirical cumulative distribution function as
  # Portion of (y_true <= y_predicted)
  P_x <- vapply(seq_along(true_values),
                function(i) {
                  sum(predictions[i, ] <= true_values[i]) / n_pred
                },
                .0)

  if (continuous_predictions) {
    p_value <- goftest::ad.test(P_x)$p.value

    out <- list(p_value = p_value,
                sd = NA)

    if (plot) {
      hist_PIT <- hist_PIT(P_x, num_bins = num_bins, caption = p_value)
      out$hist_PIT = hist_PIT
    }

    if(full_output) {
      out$u <- P_x
    }

  } else {
    # empirical cdf for (y-1) for integer-valued predictions
    P_xm1 <- vapply(seq_along(true_values),
                    function(i) {
                      sum(predictions[i,] <= true_values[i] - 1) / n_pred
                    },
                    .0)

    # do n_replicates times for randomised PIT
    u <- replicate(n_replicates, P_xm1 + stats::runif(n) * (P_x - P_xm1))

    p_values <- apply(
      u,
      MARGIN = 2,
      FUN = function (x) {
        goftest::ad.test(x)$p.value
      }
    )

    out <- list(p_value = mean(p_values),
                sd = stats::sd(p_values))

    if (full_output) {
      out$u <- u
      out$p_values <- p_values
      out <- list(p_values = p_values,
                  calibration = calibration,
                  u = u)
    }


    if (plot) {
      hist_PIT <- hist_PIT(rowMeans(u), num_bins = num_bins,
                           caption = mean(p_values))
      out$hist_PIT = hist_PIT
    }
  }

  return(out)
}






#' @title PIT Histogram
#'
#' @description
#' Make a simple histogram of the probability integral transformed values to
#' visually check whether a uniform distribution seems likely.
#'
#' @param PIT_samples A vector with the PIT values of size n
#' @param num_bins the number of bins in the PIT histogram.
#' @param caption provide a caption that gets passed to the plot
#' If not given, the square root of n will be used
#' @return vector with the scoring values
#' @importFrom ggplot2 ggplot aes xlab ylab geom_histogram stat


hist_PIT <- function(PIT_samples,
                     num_bins = NULL,
                     caption = NULL) {

  if (is.null(num_bins)) {
    n <- length(PIT_samples)
    num_bins = round(sqrt(n))
  }

  hist_PIT <- ggplot2::ggplot(data = data.frame(x = PIT_samples),
                  ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = stat(count) / sum(count)),
                            breaks = seq(0, 1, length.out = num_bins + 1),
                            colour = "grey") +
    ggplot2::xlab("PIT") +
    ggplot2::ylab("Frequency") +
    ggplot2::labs(caption = paste0("p-value of Andersen-Darling test for uniformity: ",
                                   round(caption, 3)))

  return(hist_PIT)
}



#' @title PIT Histogram Quantile
#'
#' @description
#' Make a simple histogram of the probability integral transformed values to
#' visually check whether a uniform distribution seems likely.
#'
#' @param PIT_samples A vector with the PIT values of size n
#' @param num_bins the number of bins in the PIT histogram.
#' @param caption provide a caption that gets passed to the plot
#' If not given, the square root of n will be used
#' @return vector with the scoring values
#' @importFrom ggplot2 ggplot aes xlab ylab geom_histogram stat


hist_PIT_quantile <- function(PIT_samples,
                     num_bins = NULL,
                     caption = NULL) {

  if (is.null(num_bins)) {
    n <- length(PIT_samples)
    num_bins = round(sqrt(n))
  }

  hist_PIT <- ggplot2::ggplot(data = data.frame(x = PIT_samples),
                              ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = stat(count) / sum(count)),
                            breaks = seq(0, 1, length.out = num_bins + 1),
                            colour = "grey") +
    ggplot2::xlab("PIT") +
    ggplot2::ylab("Frequency") +
    ggplot2::labs()

  return(hist_PIT)
}


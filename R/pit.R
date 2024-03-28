#' @title Probability integral transformation (sample-based version)
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
#' In the case of discrete outcomes such as incidence counts,
#' the PIT is no longer uniform even when forecasts are ideal.
#' In that case a randomised PIT can be used instead:
#' \deqn{
#' u_t = P_t(k_t) + v * (P_t(k_t) - P_t(k_t - 1) )
#' }
#'
#' where \eqn{k_t} is the observed count, \eqn{P_t(x)} is the predictive
#' cumulative probability of observing incidence k at time t,
#' \eqn{P_t (-1) = 0} by definition and v is standard uniform and independent
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
#' evidence that it was miscalibrated if p <= 0.01. However, the AD-p-values
#' may be overly strict and there actual usefulness may be questionable.
#' In this context it should be noted, though, that uniformity of the
#' PIT is a necessary but not sufficient condition of calibration.
#'
#' @param n_replicates The number of draws for the randomised PIT for
#'   discrete predictions. Will be ignored if forecasts are continuous.
#' @inheritParams ae_median_sample
#' @return A vector with PIT-values. For continuous forecasts, the vector will
#'   correspond to the length of `observed`. For integer forecasts, a
#'   randomised PIT will be returned of length
#'   `length(observed) * n_replicates`.
#' @seealso [get_pit()]
#' @importFrom stats runif
#' @importFrom cli cli_abort cli_inform
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' ## continuous predictions
#' observed <- rnorm(20, mean = 1:20)
#' predicted <- replicate(100, rnorm(n = 20, mean = 1:20))
#' pit <- pit_sample(observed, predicted)
#' plot_pit(pit)
#'
#' ## integer predictions
#' observed <- rpois(20, lambda = 1:20)
#' predicted <- replicate(100, rpois(n = 20, lambda = 1:20))
#' pit <- pit_sample(observed, predicted, n_replicates = 30)
#' plot_pit(pit)
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, \doi{10.1371/journal.pcbi.1006785}
#' @keywords metric

pit_sample <- function(observed,
                       predicted,
                       n_replicates = 100) {
  assert_input_sample(observed = observed, predicted = predicted)
  assert_number(n_replicates)
  if (is.vector(predicted)) {
    predicted <- matrix(predicted, nrow = 1)
  }

  # calculate PIT-values -------------------------------------------------------
  n_pred <- ncol(predicted)

  # calculate emipirical cumulative distribution function as
  # Portion of (y_observed <= y_predicted)
  p_x <- rowSums(predicted <= observed) / n_pred

  # PIT calculation is different for integer and continuous predictions
  if (get_type(predicted) == "integer") {
    p_xm1 <- rowSums(predicted <= (observed - 1)) / n_pred
    pit_values <- as.vector(
      replicate(n_replicates, p_xm1 + runif(1) * (p_x - p_xm1))
    )
  } else {
    pit_values <- p_x
  }
  return(pit_values)
}

#' @title Probability integral transformation (data.frame version)
#'
#' @description
#' Compute the Probability Integral Transformation (PIT) for
#' validated forecast objects.
#'
#' @inherit score params
#' @param by Character vector with the columns according to which the
#' PIT values shall be grouped. If you e.g. have the columns 'model' and
#' 'location' in the data and want to have a PIT histogram for
#' every model and location, specify `by = c("model", "location")`.
#' @inheritParams pit_sample
#' @return A data.table with PIT values according to the grouping specified in
#' `by`.
#' @examples
#' result <- get_pit(as_forecast(example_continuous), by = "model")
#' plot_pit(result)
#'
#' # example with quantile data
#' result <- get_pit(as_forecast(example_quantile), by = "model")
#' plot_pit(result)
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, \doi{10.1371/journal.pcbi.1006785}
#' @keywords scoring

get_pit <- function(data,
                    by,
                    n_replicates = 100) {

  data <- copy(data)
  suppressWarnings(suppressMessages(validate_forecast(data)))
  data <- na.omit(data)
  forecast_type <- get_forecast_type(data)

  if (forecast_type == "quantile") {
    data[, quantile_coverage := (observed <= predicted)]
    quantile_coverage <- data[, .(quantile_coverage = mean(quantile_coverage)),
                              by = c(unique(c(by, "quantile_level")))]
    quantile_coverage <- quantile_coverage[order(quantile_level),
      .(
        quantile_level = c(quantile_level, 1),
        pit_value = diff(c(0, quantile_coverage, 1))
      ),
      by = c(get_forecast_unit(quantile_coverage))
    ]
    return(as.data.table(quantile_coverage)[])
  }

  # if prediction type is not quantile, calculate PIT values based on samples
  data_wide <- data.table::dcast(data,
    ... ~ paste0("InternalSampl_", sample_id),
    value.var = "predicted"
  )

  pit <- data_wide[, .(pit_value = pit_sample(
    observed = observed,
    predicted = as.matrix(.SD)
  )),
  by = by,
  .SDcols = grepl("InternalSampl_", names(data_wide), fixed = TRUE)
  ]

  return(pit[])
}

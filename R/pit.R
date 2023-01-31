#' @title Probability Integral Transformation (sample-based version)
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
#' @param n_replicates the number of draws for the randomised PIT for
#' integer predictions.
#' @inheritParams ae_median_sample
#' @return A vector with PIT-values. For continuous forecasts, the vector will
#' correspond to the length of `true_values`. For integer forecasts, a
#' randomised PIT will be returned of length
#' `length(true_values) * n_replicates`
#' @seealso [pit()]
#' @importFrom stats runif
#' @examples
#' data.table::setDTthreads(1) # only needed to avoid issues on CRAN
#'
#' ## continuous predictions
#' true_values <- rnorm(20, mean = 1:20)
#' predictions <- replicate(100, rnorm(n = 20, mean = 1:20))
#' pit <- pit_sample(true_values, predictions)
#' plot_pit(pit)
#'
#' ## integer predictions
#' true_values <- rpois(50, lambda = 1:50)
#' predictions <- replicate(2000, rpois(n = 50, lambda = 1:50))
#' pit <- pit_sample(true_values, predictions, n_replicates = 30)
#' plot_pit(pit)
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, \doi{10.1371/journal.pcbi.1006785}
#' @keywords metric

pit_sample <- function(true_values,
                       predictions,
                       n_replicates = 100) {

  # error handling--------------------------------------------------------------
  # check al arguments are provided
  # this could be integrated into check_not_null
  if (missing("true_values") | missing("predictions")) {
    stop("`true_values` or `predictions` missing in function 'pit_sample()'")
  }
  check_not_null(true_values = true_values, predictions = predictions)

  # check if there is more than one observation
  n <- length(true_values)
  if (n == 1) {
    message(
      "you need more than one observation to assess uniformity of the PIT"
    )
    return(NA)
  }

  # check and handle format of predictions
  if (is.data.frame(predictions)) {
    predictions <- as.matrix(predictions)
  }
  if (!is.matrix(predictions)) {
    msg <- sprintf(
      "'predictions' should be a matrix. Instead `%s` was found",
      class(predictions)[1]
    )
    stop(msg)
  }
  if (nrow(predictions) != n) {
    msg <- sprintf(
      "Mismatch: 'true_values' has length `%s`, but 'predictions' has `%s` rows.",
      n, nrow(predictions)
    )
    stop(msg)
  }

  # check data type ------------------------------------------------------------
  # check whether continuous or integer
  if (!isTRUE(all.equal(as.vector(predictions), as.integer(predictions)))) {
    continuous_predictions <- TRUE
  } else {
    continuous_predictions <- FALSE
  }

  # calculate PIT-values -------------------------------------------------------
  n_pred <- ncol(predictions)

  # calculate emipirical cumulative distribution function as
  # Portion of (y_true <= y_predicted)
  p_x <- rowSums(predictions <= true_values) / n_pred

  # calculate PIT for continuous predictions case
  if (continuous_predictions) {
    pit_values <- p_x
  } else {
    p_xm1 <- rowSums(predictions <= (true_values - 1)) / n_pred
    pit_values <- as.vector(
      replicate(n_replicates, p_xm1 + runif(1) * (p_x - p_xm1))
    )
  }
  return(pit_values)
}

#' @title Probability Integral Transformation (data.frame Format)
#'
#' @description Wrapper around `pit()` for use in data.frames
#'
#' @details
#' see [pit()]
#'
#' @param data a data.frame with the following columns: `true_value`,
#' `prediction`, `sample`.
#' @param by Character vector with the columns according to which the
#' PIT values shall be grouped. If you e.g. have the columns 'model' and
#' 'location' in the data and want to have a PIT histogram for
#' every model and location, specify `by = c("model", "location")`.
#' @inheritParams pit_sample
#' @return a data.table with PIT values according to the grouping specified in
#' `by`
#' @examples
#' result <- pit(example_continuous, by = "model")
#' plot_pit(result)
#'
#' # example with quantile data
#' result <- pit(example_quantile, by = "model")
#' plot_pit(result)
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, \doi{10.1371/journal.pcbi.1006785}
#' @keywords scoring

pit <- function(data,
                by,
                n_replicates = 100) {

  check_data <- check_forecasts(data)

  data <- check_data$cleaned_data
  prediction_type <- check_data$prediction_type

  # if prediction type is quantile, simply extract coverage values from
  # score and returned a list with named vectors
  if (prediction_type == "quantile") {
    coverage <-
      score(data, metrics = "quantile_coverage")

    coverage <- summarise_scores(coverage,
      by = unique(c(by, "quantile"))
    )

    coverage <- coverage[order(quantile),
      .(
        quantile = c(quantile, 1),
        pit_value = diff(c(0, quantile_coverage, 1))
      ),
      by = c(get_forecast_unit(coverage))
    ]

    return(coverage[])
  }

  # if prediction type is not quantile, calculate PIT values based on samples
  data_wide <- data.table::dcast(data,
    ... ~ paste("InternalSampl_", sample, sep = ""),
    value.var = "prediction"
  )

  pit <- data_wide[, .("pit_value" = pit_sample(
    true_values = true_value,
    predictions = as.matrix(.SD)
  )),
  by = by,
  .SDcols = grepl("InternalSampl_", names(data_wide))
  ]

  return(pit[])
}

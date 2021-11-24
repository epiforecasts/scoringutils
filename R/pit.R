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
#' evidence that it was miscalibrated if p <= 0.01. However, the AD-p-values
#' may be overly strict and there actual usefulness may be questionable.
#' In this context it should be noted, though, that uniformity of the
#' PIT is a necessary but not sufficient condition of calibration.
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions nxN matrix of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param n_replicates the number of draws for the ranomised PIT for
#' integer predictions.
#' @return A vector with PIT-values. For continuous forecasts, the vector will
#' correspond to the length of `true_values`. For integer forecasts, a
#' randomised PIT will be returned of length
#' `length(true_values) * n_replicates`
#' @importFrom stats runif
#' @examples
#' library(scoringutils)
#' ## continuous predictions
#' true_values <- rnorm(30, mean = 1:30)
#' predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
#' pit <- pit(true_values, predictions)
#' hist_PIT(pit)
#'
#' ## integer predictions
#' true_values <- rpois(100, lambda = 1:100)
#' predictions <- replicate(5000, rpois(n = 100, lambda = 1:100))
#' pit <- pit(true_values, predictions,  n_replicates = 50)
#' hist_PIT(pit)
#'
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, <doi:10.1371/journal.pcbi.1006785>

pit <- function(true_values,
                predictions,
                n_replicates = 100) {

  # error handling--------------------------------------------------------------
  # check al arguments are provided
  if (!all(c(methods::hasArg("true_values"), methods::hasArg("predictions")))) {
    stop("`true_values` or `predictions` missing in function 'pit()'")
  }
  check_not_null(true_values = true_values, predictions = predictions)

  # check if there is more than one observation
  n <- length(true_values)
  if (n == 1) {
    message("you need more than one observation to assess uniformity of the PIT")
    return(NA)
  }

  # check and handle format of predictions
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
  P_x <- rowSums(predictions <= true_values) / n_pred

  # calculate PIT for continuous predictions case
  if (continuous_predictions) {
    pit_values <- P_x
  } else {
    P_xm1 <- rowSums(predictions <= (true_values - 1)) / n_pred
    pit_values <- as.vector(replicate(n_replicates, P_xm1 + runif(1) * (P_x - P_xm1)))
  }
  return(pit_values)
}




#' @title PIT Histogram
#'
#' @description
#' Make a simple histogram of the probability integral transformed values to
#' visually check whether a uniform distribution seems likely.
#'
#' @param PIT_samples A vector with the PIT values of size n
#' @param num_bins the number of bins in the PIT histogram.
#' @return vector with the scoring values
#' @examples
#' library(scoringutils)
#' true_values <- rnorm(30, mean = 1:30)
#' predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
#' pit <- pit(true_values, predictions)
#' hist_PIT(pit)
#'
#' @importFrom ggplot2 ggplot aes xlab ylab geom_histogram stat theme_light
#' @export


hist_PIT <- function(PIT_samples,
                     num_bins = NULL) {

  single_PIT_hist <- function(PIT_samples,
                              num_bins) {
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
      ggplot2::theme_light()

    return(hist_PIT)
  }

  if (is.list(PIT_samples) && !is.data.frame(PIT_samples)) {
    out <- lapply(PIT_samples, single_PIT_hist, num_bins = num_bins)
  } else {
    out <- single_PIT_hist(PIT_samples, num_bins)
  }
  return(out)
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
#' @param summarise_by Character vector with the columns according to which the
#' PIT values shall be grouped. If you e.g. have the columns 'model' and
#' 'location' in the data and want to have a PIT histogram for
#' every model and location, specify `summarise_by = c("model", "location")`.
#' @inheritParams pit
#' @param include_full Boolean (default is `FALSE`). Whether or not to also
#' return PIT values for the full data set without any grouping.
#' @return a named list with PIT values according to the grouping specified in
#' `summarised_by`
#' @examples
#' example <- scoringutils::continuous_example_data
#' result <- pit_df(example, summarise_by = "model")
#' hist_PIT(result)
#'
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, <doi:10.1371/journal.pcbi.1006785>

pit_df <- function(data,
                   summarise_by,
                   n_replicates = 100,
                   include_full = FALSE) {

  data <- check_clean_data(data, verbose = FALSE)

  # reformat data.table to wide format for PIT
  data_wide <- data.table::dcast(data,
                                 ... ~ paste("InternalSampl_", sample, sep = ""),
                                 value.var = "prediction")

  # implementation idea for a data.table version. Probably only makes sense
  # in a non-randomised version of the integer PIT
  # could have a return = c("Data.table", "list") argument to distinguish
  # data_wide[, "pit_values" := list(pit(true_value, as.matrix(.SD),
  #                                      n_replicates = n_replicates)),
  #           .SDcols = names(data_wide)[grepl("InternalSampl_", names(data_wide))],
  #           by = summarise_by]
  # sample_names <- names(data_wide)[grepl("InternalSampl_", names(data_wide))]
  # data <- data.table::melt(data_wide,
  #                          measure.vars = sample_names,
  #                          variable.name = "sample",
  #                          value.name = "prediction")
  # data[, sample := as.integer(gsub(pattern = "InternalSampl_", replacement = "",
  #                                  x = sample))]

  # include the full data set once in addition to the subsets.
  if (include_full) {
    split_data <- list(data_wide)
  } else {
    split_data <- list()
  }

  split_data <- c(split_data, split(data_wide, by = summarise_by))

  pit_values <- lapply(split_data,
                       FUN = function(data) {
                         true_values <- data$true_value
                         samplecols <- names(data)[grepl("InternalSampl_", names(data))]
                         predictions <- as.matrix(data[, ..samplecols])
                         return(pit(true_values, predictions, n_replicates))
                       })
  return(pit_values)
}


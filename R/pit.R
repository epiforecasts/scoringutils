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
#' @param pit either a vector with the PIT values of size n, or a data.frame as
#' produced by [pit_df()]
#' @param num_bins the number of bins in the PIT histogram, default is "auto".
#' When `num_bins == "auto"`, [hist_PIT()] will either display 10 bins, or it
#' will display a bin for each available quantile in case you passed in data in
#' a quantile-based format.
#' You can control the number of bins by supplying a number. This is fine for
#' sample-based pit histograms, but may fail for quantile-based formats. In this
#' case it is preferred to supply explicit breaks points using the `breaks`
#' argument.
#' @param breaks numeric vector with the break points for the bins in the
#' PIT histogram. This is preferred when creating a PIT histogram based on
#' quantile-based data. Default is `NULL` and breaks will be determined by
#' `num_bins`.
#' @importFrom stats as.formula
#' @importFrom ggplot2 geom_col
#' @return vector with the scoring values
#' @examples
#' library(scoringutils)
#'
#' # PIT histogram in vector based format
#' true_values <- rnorm(30, mean = 1:30)
#' predictions <- replicate(200, rnorm(n = 30, mean = 1:30))
#' pit <- pit(true_values, predictions)
#' hist_PIT(pit)
#'
#' # quantile-based pit
#' pit <- pit_df(example_quantile, summarise_by = c("model"))
#' hist_PIT(pit, breaks = seq(0.1, 1, 0.1))
#'
#' # sample-based pit
#' pit <- pit_df(example_integer, summarise_by = c("model"))
#' hist_PIT(pit)
#'
#' @importFrom ggplot2 ggplot aes xlab ylab geom_histogram stat theme_light
#' @export


hist_PIT <- function(pit,
                     num_bins = "auto",
                     breaks = NULL) {

  if ("quantile" %in% names(pit)) {
    type <- "quantile-based"
  } else {
    type <- "sample-based"
  }

  # use breaks if explicitly given, otherwise assign based on number of bins
  if (!is.null(breaks)) {
    plot_quantiles <- breaks
  } else if (is.null(num_bins) | num_bins == "auto") {
    # automatically set number of bins
    if (type == "sample-based") {
      num_bins <- 10
      width <- 1/num_bins
      plot_quantiles <- seq(width, 1, width)
    }
    if (type == "quantile-based") {
      plot_quantiles <- unique(pit$quantile)
    }
  } else {
    # if num_bins is explicitly given
    width <- 1/num_bins
    plot_quantiles <- seq(width, 1, width)
  }

  # function for data.frames
  if(is.data.frame(pit)) {

    facet_cols <- get_unit_of_forecast(pit)
    formula <- as.formula(paste("~", paste(facet_cols, collapse = "+")))

    # quantile version
    if (type == "quantile-based") {

      if (num_bins == "auto") {
      } else {
        width <- 1/num_bins
        plot_quantiles <- seq(width, 1, width)
      }

      if (!is.null(breaks)) {
        plot_quantiles <- breaks
      }

      hist <- ggplot(data = pit[quantile %in% plot_quantiles],
                     aes(x = quantile, y = pit_value)) +
        geom_col(position = "dodge") +
        facet_wrap(formula)

    }

    if (type == "sample-based") {
      hist <- ggplot2::ggplot(data = pit,
                      aes(x = pit_value)) +
        ggplot2::geom_histogram(aes(y = stat(count) / sum(count)),
                                breaks = plot_quantiles,
                                colour = "grey") +
        facet_wrap(formula)
    }

  } else {
    # non data.frame version
    hist <- ggplot(data = data.frame(x = pit),
                   aes(x = x)) +
      geom_histogram(aes(y = stat(count) / sum(count)),
                     breaks = plot_quantiles,
                     colour = "grey")
  }

  hist <- hist +
    xlab("PIT") +
    ylab("Frequency") +
    theme_light()

  return(hist)
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
#' @return a data.table with PIT values according to the grouping specified in
#' `summarise_by`
#' @examples
#' example <- scoringutils::example_continuous
#' result <- pit_df(example, summarise_by = "model")
#' hist_PIT(result)
#'
#' # example with quantile data
#' result <- pit_df(example_quantile, summarise_by = "model")
#' hist_PIT(result)
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, <doi:10.1371/journal.pcbi.1006785>

pit_df <- function(data,
                   summarise_by,
                   n_replicates = 100) {

  # clean data by removing NA values
  data <- check_clean_data(data, verbose = FALSE)

  # get prediction type
  prediction_type <- get_prediction_type(data)

  # if prediction type is quantile, simply extract coverage values from
  # score and returned a list with named vectors
  if (prediction_type == "quantile") {
    coverage <-
      score(data,
                     summarise_by = unique(c(summarise_by, "quantile")),
                     metrics = "quantile_coverage")

    coverage <- coverage[order(quantile),
                         .(quantile = c(quantile, 1),
                           pit_value = diff(c(0, quantile_coverage, 1))),
                         by = c(get_unit_of_forecast(coverage))]

    return(coverage)
  }

  # if prediction type is not quantile, calculate PIT values based on samples
  data_wide <- data.table::dcast(data,
                                 ... ~ paste("InternalSampl_", sample, sep = ""),
                                 value.var = "prediction")

  pit <- data_wide[, .("pit_value" = pit(true_values = true_value,
                                         predictions = as.matrix(.SD))),
                   by = summarise_by,
                   .SDcols = grepl("InternalSampl_", names(data_wide))]

  return(pit)
}


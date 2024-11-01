#' @title Probability integral transformation histogram
#'
#' @description
#' Generate a Probability Integral Transformation (PIT) histogram for
#' validated forecast objects.
#'
#' See the examples for how to plot the result of this function.
#'
#' @inherit score params
#' @param num_bins The number of bins in the PIT histogram. For sample-based
#'   forecasts, the default is 10 bins. For quantile-based forecasts, the
#'   default is one bin for each available quantile.
#'   You can control the number of bins by supplying a number. This is fine for
#'   sample-based pit histograms, but may fail for quantile-based formats. In
#'   this case it is preferred to supply explicit breaks points using the
#'   `breaks` argument.
#' @param breaks Numeric vector with the break points for the bins in the
#'   PIT histogram. This is preferred when creating a PIT histogram based on
#'   quantile-based data. Default is `NULL` and breaks will be determined by
#'   `num_bins`. If `breaks` is used, `num_bins` will be ignored.
#'   0 and 1 will always be added as left and right bounds, respectively.
#' @param by Character vector with the columns according to which the
#'   PIT values shall be grouped. If you e.g. have the columns 'model' and
#'   'location' in the input data and want to have a PIT histogram for
#' every model and location, specify `by = c("model", "location")`.
#' @inheritParams pit_histogram_sample
#' @returns A data.table with density values for each bin in the PIT histogram.
#' @examples
#' library("ggplot2")
#'
#' result <- get_pit_histogram(example_sample_continuous, by = "model")
#' ggplot(result,  aes(x = mid, y = density)) +
#'   geom_col() +
#'   facet_wrap(. ~ model) +
#'   labs(x = "Quantile", "Density")
#'
#' # example with quantile data
#' result <- get_pit_histogram(example_quantile, by = "model")
#' ggplot(result,  aes(x = mid, y = density)) +
#'   geom_col() +
#'   facet_wrap(. ~ model) +
#'   labs(x = "Quantile", "Density")
#' @export
#' @keywords scoring
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, \doi{10.1371/journal.pcbi.1006785}
get_pit_histogram <- function(forecast, num_bins, breaks, by,
                              ...) {
  UseMethod("get_pit_histogram")
}


#' @rdname get_pit_histogram
#' @importFrom cli cli_abort
#' @export
get_pit_histogram.default <- function(forecast, num_bins, breaks, by, ...) {
  cli_abort(c(
    "!" = "The input needs to be a valid forecast object represented as quantiles or samples." # nolint
  ))
}

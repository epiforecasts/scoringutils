#' @title Probability integral transformation (data.frame version)
#'
#' @description
#' Compute the Probability Integral Transformation (PIT) for
#' validated forecast objects.
#'
#' @inherit score params
#' @param by Character vector with the columns according to which the
#' PIT values shall be grouped. If you e.g. have the columns 'model' and
#' 'location' in the input data and want to have a PIT histogram for
#' every model and location, specify `by = c("model", "location")`.
#' @inheritParams pit_sample
#' @return A data.table with PIT values according to the grouping specified in
#' `by`.
#' @examples
#' example <- as_forecast_sample(example_sample_continuous)
#' result <- get_pit(example, by = "model")
#' plot_pit(result)
#'
#' # example with quantile data
#' example <- as_forecast_quantile(example_quantile)
#' result <- get_pit(example, by = "model")
#' plot_pit(result)
#' @export
#' @references
#' Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
#' Rosalind M. Eggo, W. John Edmunds (2019) Assessing the performance of
#' real-time epidemic forecasts: A case study of Ebola in the Western Area
#' region of Sierra Leone, 2014-15, \doi{10.1371/journal.pcbi.1006785}
#' @keywords scoring

#' @keywords scoring
#' @export
get_pit <- function(forecast, by, ...) {
  UseMethod("get_pit")
}

#' @rdname get_pit
#' @importFrom cli cli_abort
#' @export
get_pit.default <- function(forecast, by, ...) {
  cli_abort(c(
    "!" = "The input needs to be a valid forecast object represented as quantiles or samples." # nolint
  ))
}

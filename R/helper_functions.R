#' @title PIT Histogram
#'
#' @description
#' Make a simple histogram of the probability integral transformed values to
#' visually check whether a uniform distribution seems likely.
#'
#' @param PIT_samples A vector with the PIT values of size n
#' @param num_bins the number of bins in the PIT histogram.
#' If not given, the square root of n will be used
#' @return vector with the scoring values
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' logs(true_values, predictions)
#' @export


hist_PIT <- function(PIT_samples, num_bins = NULL) {

  if (is.null(num_bins)) {
    n <- length(PIT_samples)
    num_bins = round(sqrt(n))
  }

  PIT <- PIT_samples
  hist_PIT <- ggplot(as.data.frame(PIT),
                     aes(x = PIT)) + geom_histogram(color = 'darkblue',
                                                    fill = 'lightblue',
                                                    bins = num_bins)
}


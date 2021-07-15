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
#' PLoS Comput Biol 15(2): e1006785. <doi:10.1371/journal.pcbi.1006785>
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

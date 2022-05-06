#' @title Determine dispersion of a probabilistic forecast
#' @details
#' Sharpness is the ability of the model to generate predictions within a
#' narrow range and dispersion is the lack thereof.
#' It is a data-independent measure, and is purely a feature
#' of the forecasts themselves.
#'
#' Dispersion of predictive samples corresponding to one single true value is
#' measured as the normalised median of the absolute deviation from
#' the median of the predictive samples. For details, see [mad()][stats::mad()]
#' and the explanations given in Funk et al. (2019)
#'
#' @inheritParams ae_median_sample
#' @importFrom stats mad
#' @return vector with dispersion values
#'
#' @references
#' Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, Edmunds WJ (2019)
#' Assessing the performance of real-time epidemic forecasts: A case study of
#' Ebola in the Western Area region of Sierra Leone, 2014-15.
#' PLoS Comput Biol 15(2): e1006785. \doi{10.1371/journal.pcbi.1006785}
#'
#' @export
#' @examples
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' mad_sample(predictions)
#' @keywords metric

mad_sample <- function(predictions) {
  check_predictions(predictions, class = "matrix")

  sharpness <- apply(predictions, MARGIN = 1, mad)
  return(sharpness)
}

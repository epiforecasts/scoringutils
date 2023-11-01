#' @title Determine dispersion of a probabilistic forecast
#' @details
#' Sharpness is the ability of the model to generate predictions within a
#' narrow range and dispersion is the lack thereof.
#' It is a data-independent measure, and is purely a feature
#' of the forecasts themselves.
#'
#' Dispersion of predictive samples corresponding to one single observed value is
#' measured as the normalised median of the absolute deviation from
#' the median of the predictive samples. For details, see [mad()][stats::mad()]
#' and the explanations given in Funk et al. (2019)
#'
#' @inheritParams ae_median_sample
#' @param observed place holder, argument will be ignored and exists only for
#' consistency with other scoring functions. The output does not depend on
#' any observed values.
#' @param ... additional arguments passed to [mad()][stats::mad()].
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
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' mad_sample(predicted = predicted)
#' @keywords metric

mad_sample <- function(observed = NULL, predicted, ...) {

  if(!is.null(observed)) {
    stop("`observed` argument was provided, but should be NULL.",
         "Please assign the `predicted` argument explicitly.")
  }
  observed <- rep(NA_real_, nrow(predicted))
  check_input_sample(observed, predicted)

  sharpness <- apply(predicted, MARGIN = 1, mad, ...)
  return(sharpness)
}

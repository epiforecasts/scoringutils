#' @title Logarithmic score
#'
#' @description
#' Wrapper around the [`logs_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package. Used to score continuous predictions.
#' While the Log Score is in theory also applicable
#' to integer forecasts, the problem lies in the implementation: The Log Score
#' needs a kernel density estimation, which is not well defined with
#' integer-valued Monte Carlo Samples. The Log Score can be used for specific
#' integer valued probability distributions. See the scoringRules package for
#' more details.
#' @inheritParams ae_median_sample
#' @param ... additional arguments passed to
#' [logs_sample()][scoringRules::logs_sample()] from the scoringRules package.
#' @return vector with the scoring values
#' @importFrom scoringRules logs_sample
#' @examples
#' observed <- rpois(30, lambda = 1:30)
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' logs_sample(observed, predicted)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

logs_sample <- function(observed, predicted, ...) {
  check_input_sample(observed, predicted)
  scoringRules::logs_sample(
    y = observed,
    dat = predicted,
    ...
  )
}

#' @title Dawid-Sebastiani Score
#'
#' @description
#' Wrapper around the [`dss_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package.
#' @inheritParams logs_sample
#' @param ... additional arguments passed to
#' [dss_sample()][scoringRules::dss_sample()] from the scoringRules package.
#' @return vector with scoring values
#' @importFrom scoringRules dss_sample
#' @examples
#' observed <- rpois(30, lambda = 1:30)
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' dss_sample(observed, predicted)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

dss_sample <- function(observed, predicted, ...) {
  check_input_sample(observed, predicted)

  scoringRules::dss_sample(
    y = observed,
    dat = predicted,
    ...
  )
}

#' @title Ranked Probability Score
#'
#' @description
#' Wrapper around the [`crps_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package. Can be used for continuous as well as integer
#' valued forecasts
#' @inheritParams logs_sample
#' @param ... additional arguments passed to
#' [crps_sample()][scoringRules::crps_sample()] from the scoringRules package.
#' @return vector with the scoring values
#' @importFrom scoringRules crps_sample
#' @examples
#' observed <- rpois(30, lambda = 1:30)
#' predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
#' crps_sample(observed, predicted)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

crps_sample <- function(observed, predicted, ...) {
  check_input_sample(observed, predicted)

  scoringRules::crps_sample(
    y = observed,
    dat = predicted,
    ...
  )
}

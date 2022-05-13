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
#' @return vector with the scoring values
#' @importFrom scoringRules logs_sample
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' logs_sample(true_values, predictions)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

logs_sample <- function(true_values, predictions) {
  check_true_values(true_values)
  check_predictions(predictions, true_values,
    class = "matrix"
  )

  scoringRules::logs_sample(
    y = true_values,
    dat = predictions
  )
}

#' @title Dawid-Sebastiani Score
#'
#' @description
#' Wrapper around the [`dss_sample()`][scoringRules::scores_sample_univ]
#' function from the
#' \pkg{scoringRules} package.
#' @inheritParams logs_sample
#' @return vector with scoring values
#' @importFrom scoringRules dss_sample
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' dss_sample(true_values, predictions)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

dss_sample <- function(true_values, predictions) {
  check_true_values(true_values)
  check_predictions(predictions, true_values,
    class = "matrix"
  )

  scoringRules::dss_sample(
    y = true_values,
    dat = predictions
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
#' @return vector with the scoring values
#' @importFrom scoringRules crps_sample
#' @examples
#' true_values <- rpois(30, lambda = 1:30)
#' predictions <- replicate(200, rpois(n = 30, lambda = 1:30))
#' crps_sample(true_values, predictions)
#' @export
#' @references
#' Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating Probabilistic
#' Forecasts with scoringRules, <https://www.jstatsoft.org/article/view/v090i12>
#' @keywords metric

crps_sample <- function(true_values, predictions) {

  # check inputs
  check_true_values(true_values)
  check_predictions(predictions, true_values,
    class = "matrix"
  )

  scoringRules::crps_sample(
    y = true_values,
    dat = predictions
  )
}

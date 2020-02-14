#' Applies forecast assessments and scores to the forecasts
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of nxN matrices of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param prediction_type probabilitic or point
#' @param outcome_type integer or continuous or binary
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.

#' @metrics vector of names of the metrics
#'
#'
#' @return either a data.frame or list of data frames with the forecast scores
#' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
#' @export


eval_forecasts <- function(true_values,
                           predictions,
                           prediction_type = "probabilistic",
                           outcome_type = "integer",
                           metrics = c(),
                           output = "df") {


  res <- list()

  # apply PIT function to true_values and the different predictive_samples
  # provided. Get the p_values for the replications of the randomized PIT
  # with Anderson-Darling Test and return mean and sd of those p-values
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringutils::PIT(true_values = true_values,
                                    samples = x)$p_values
                },
                true_values = true_values)

  res$PIT_AD_calibration <- data.frame(mean = colMeans(tmp),
                                       sd = apply(tmp, MARGIN=2, FUN=sd))

  # sharpness
  tmp <- sapply(predictions,
                function(x) {
                  scoringutils::sharpness(x)
                })

  res$sharpness <- data.frame(mean = colMeans(tmp),
                              sd = apply(tmp, MARGIN=2, FUN=sd))


  # bias
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringutils::bias(samples = x,
                                     true_values = true_values)
                },
                true_values = true_values)

  res$bias <- data.frame(mean = colMeans(tmp),
                              sd = apply(tmp, MARGIN=2, FUN=sd))

  # DSS
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringRules::dss_sample(dat = x,
                                           y = true_values)
                },
                true_values = true_values)

  res$DSS <- data.frame(mean = colMeans(tmp),
                         sd = apply(tmp, MARGIN=2, FUN=sd))


  # CRPS
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringRules::crps_sample(dat = x,
                                            y = true_values)
                },
                true_values = true_values)

  res$CRPS <- data.frame(mean = colMeans(tmp),
                         sd = apply(tmp, MARGIN=2, FUN=sd))


  if (output == "df") {
    return (do.call("rbind", res))
  } else {
    return (res)
  }
}

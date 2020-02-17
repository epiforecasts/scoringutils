#' Assess forecasts
#'
#' @description The function \code{eval_forecasts} is a wrapper that provides
#' an interface to the lower-level functions
#' \enumerate{
#'   \item \code{\link{eval_forecasts_prob_int}}
#'   \item \code{\link{eval_forecasts_prob_cont}}
#'   \item \code{\link{eval_forecasts_prob_bin}}
#'
#'   \item \code{\link{eval_forecasts_point_int}}
#'   \item \code{\link{eval_forecasts_point_cont}}
#'   \item \code{\link{eval_forecasts_point_bin}}
#' }
#'
#' To assess the goodness of probabilistic or point forecasts to continues,
#' integer-valued or binary variables.
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of appropriate predictions. Every
#' item in the list corresponds to the predictions made by one model.
#' Appropriate dimensions and input types are:
#'
#' \itemize{
#'   \item for probabilistic integer and continuous forecasts: a matrix or
#'   data.frame of size nxN of predictive samples. #' n (number of rows) being
#'   the number of observed values to predict and N (number of columns) the
#'   number of Monte Carlo samples
#'   \item for probabilistic binary forecasts: a vector of length n that gives
#'   the probability that the corresponding element in \code{true_values}
#'   will be equal to one.
#'   \item for all point estimates: a vector of size n with predictions for the
#'   corresponding entries of \code{true_values}}
#' @param prediction_type Type of the prediction made. Can be eitehr
#' "probabilitic" or "point" for point predictions.
#' @param outcome_type type of the variable to be predicted. Can be either
#' "integer" or "continuous" or "binary".
#' @param metrics what metrics to include. Currently not used, as all metrics
#' are displayed
#' @param output "df" returns a single data.frame with the prediction results.
#' Rownames of the data.frame correspond to the metric applied for the scoring.
#' \code{mean} and \code{sd} are the mean and standard deviations of the scores
#' achieved by the predictions for every single value of \code{true_values}.
#' Only in the case of the \code{\link{PIT}}, \code{mean} and \code{sd} return
#' the mean and standard deviation of the Replicates of the Randomized PIT.
#' If everything else than "df" is specified, the above results are returned
#' as a list of data.frames for the different metrics.
#' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
#' @references Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, Edmunds WJ
#' (2019) Assessing the performance of real-time epidemic forecasts: A
#' case study of Ebola in the Western Area region of Sierra Leone, 2014-15.
#' PLoS Comput Biol 15(2): e1006785.
#' https://doi.org/10.1371/journal.pcbi.1006785
#' @export
#'

eval_forecasts <- function(true_values,
                           predictions,
                           prediction_type = "probabilistic",
                           outcome_type = "integer",
                           metrics = NULL,
                           output = "df") {

  ## Error handling


  if (prediction_type == "probabilistic") {

    if (outcome_type == "integer") {
      res <- eval_forecasts_prob_int(true_values,
                                     predictions,
                                     prediction_type = prediction_type,
                                     outcome_type = outcome_type,
                                     metrics = metrics,
                                     output = output)
      return(res)

    } else if (outcome_type == "binary") {
      res <- eval_forecasts_prob_bin(true_values,
                                     predictions,
                                     prediction_type = prediction_type,
                                     outcome_type = outcome_type,
                                     metrics = metrics,
                                     output = output)
      return(res)

    } else if (outcome_type == "continuous") {

    } else {
      error("outcome_type must be either 'integer', 'continuous' or 'binary'")
    }


  } else if (prediction_type == "point") {

    if (outcome_type == "integer") {

    } else if (outcome_type == "binary") {

    } else if (outcome_type == "continuous") {

    } else {
      error("outcome_type must be either 'integer', 'continuous' or 'binary'")
    }

  } else {
    error("prediction_type must be either 'probabilistic' or 'point'")
  }
}


#' Applies forecast assessments and scores to probabilistic forecasts of
#' integers
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of nxN matrices of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param prediction_type probabilitic or point
#' @param outcome_type integer or continuous or binary
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#'
#'
#' @return either a data.frame or list of data frames with the forecast scores
#' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
#' @export
<<<<<<< HEAD
#'
=======
#' 
#' ## Example: Probabilistic predictions for integer values
#' true_values <- rpois(100, lambda = 1:100)
#
#' predictions <- replicate(5000, rpois(n = 100, lambda = 1:100))
#
#
#' predictions <- list(dat1 = replicate(5000, rpois(n = 100, lambda = 1:100)),
#                    dat2 = replicate(5000, rpois(n = 100, lambda = 1:100)))
#
#
#' eval_forecasts(true_values, predictions)
>>>>>>> f8e1524afe7f12d5c42adfcea27a46c947969024



eval_forecasts_prob_int <- function(true_values,
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



#' Applies forecast assessments and scores to probabilistic forecasts of
#' binary outcomes
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of nxN matrices of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param prediction_type probabilitic or point
#' @param outcome_type integer or continuous or binary
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#'
#'
#' @return either a data.frame or list of data frames with the forecast scores
#' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
#' @export
#'



eval_forecasts_prob_bin <- function(true_values,
                                    predictions,
                                    prediction_type = "probabilistic",
                                    outcome_type = "integer",
                                    metrics = c(),
                                    output = "df") {


  res <- list()

  # Brier Score
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringutils::Brier_score(true_values = true_values,
                                            samples = x)
                },
                true_values = true_values)

  res$Brier_Score <- data.frame(mean = colMeans(tmp),
                                sd = apply(tmp, MARGIN=2, FUN=sd))



  if (output == "df") {
    return (do.call("rbind", res))
  } else {
    return (res)
  }
}



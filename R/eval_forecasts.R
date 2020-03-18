#' @title Evaluate forecasts
#'
#' @description The function \code{eval_forecasts} is a wrapper that provides
#' an interface to lower-level functions. It can be used to assess the goodness
#' of probabilistic or point forecasts to continues, integer-valued or
#' binary variables. The lower-level functions accessed are:
#'
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
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of appropriate predictions. Every
#' item in the list corresponds to the predictions made by one model.
#' Appropriate dimensions and input types are:
#'
#' \itemize{
#'   \item for probabilistic integer and continuous forecasts: a matrix or
#'   data.frame of size nxN of predictive samples. n (number of rows) being
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
#' @param output specify the format of the output. Can be either "df" (returns
#' a single data.frame) or anything else (returns a list of data.frames)
#'
#' @return  output option "df" returns a single data.frame with the prediction
#' results.
#' Rownames of the data.frame correspond to the metric applied for the scoring.
#' \code{mean} and \code{sd} are the mean and standard deviations of the scores
#' achieved by the predictions for every single value of \code{true_values}.
#' Only in the case of the \code{\link{pit}}, \code{mean} and \code{sd} return
#' the mean and standard deviation of the Replicates of the Randomised PIT.
#' If everything else than "df" is specified, the above results are returned
#' as a list of data.frames for the different metrics.
#' @author Nikos Bosse \email{nikosbosse@gmail.com}
#' @references Funk S, Camacho A, Kucharski AJ, Lowe R, Eggo RM, Edmunds WJ
#' (2019) Assessing the performance of real-time epidemic forecasts: A
#' case study of Ebola in the Western Area region of Sierra Leone, 2014-15.
#' PLoS Comput Biol 15(2): e1006785.
#' \url{https://doi.org/10.1371/journal.pcbi.1006785}
#' @export
#'

eval_forecasts <- function(true_values,
                           predictions,
                           prediction_type = "probabilistic",
                           outcome_type = "integer",
                           metrics = NULL,
                           output = "df") {

  if (prediction_type == "probabilistic") {

    if (outcome_type == "integer") {
      res <- eval_forecasts_prob_int(true_values = true_values,
                                     predictions = predictions,
                                     metrics = metrics,
                                     output = output)
      return(res)

    } else if (outcome_type == "binary") {
      res <- eval_forecasts_prob_bin(true_values,
                                     predictions,
                                     metrics = metrics,
                                     output = output)
      return(res)

    } else if (outcome_type == "continuous") {
      res <- eval_forecasts_prob_cont(true_values,
                                      predictions,
                                      metrics = metrics,
                                      output = output)
      return(res)
    } else {
      stop("outcome_type must be either 'integer', 'continuous' or 'binary'")
    }


  } else if (prediction_type == "point") {

    if (outcome_type == "integer") {

    } else if (outcome_type == "binary") {

    } else if (outcome_type == "continuous") {

    } else {
      stop("outcome_type must be either 'integer', 'continuous' or 'binary'")
    }

  } else {
    stop("prediction_type must be either 'probabilistic' or 'point'")
  }
  stop ("end of function reached")
}

# ====================================================== #
# ============= probabilistic + integer ================ #
# ====================================================== #

#' @title Applies forecast assessments and scores to probabilistic
#' forecasts of integers
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of nxN matrices of predictive samples,
#' n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#' @return either a data.frame or list of data frames with the forecast scores
#' @export
#'
#' @examples
#' ## Example: Probabilistic predictions for integer values
#' true_values <- rpois(100, lambda = 1:100)
#
#' predictions <- replicate(5000, rpois(n = 100, lambda = 1:100))
#
#
#' predictions <- list(dat1 = replicate(5000, rpois(n = 100, lambda = 1:100)),
#'                     dat2 = replicate(5000, rpois(n = 100, lambda = 1:100)))
#
#
#' eval_forecasts(true_values = true_values, predictions = predictions)
#'




eval_forecasts_prob_int <- function(true_values,
                           predictions,
                           metrics = c(),
                           output = "df") {



  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  if (!all.equal(true_values, as.integer(true_values)) == TRUE) {
    warning("The true_values provided are not integers. Don't trust the results.
            Maybe you want to score continuous predictions instead?")
  }

  n <- length(true_values)

  if (!is.list(predictions)) {
    if (is.matrix(predictions)) {
      predictions <- list(predictions)
    }
    else {
      stop("predictions argument should be a list of matrices")
    }
  }

  if (is.data.frame(predictions)) {
    predictions <- list(as.matrix(predictions))
  } else {
    for (i in 1:length(predictions)) {
      if (is.data.frame(predictions[[i]])) {
        predictions[[i]] <- as.matrix(predictions[[i]])
      }
      if (!is.matrix(predictions[[i]])) {
        stop("'predictions' should be a list of matrices")
      }
      if (nrow(predictions[[i]]) != n) {
        msg = cat("all matrices in list 'predictions' must have n rows, ",
                  "where n is the number of true_values to predict. ",
                  "Dimension mismatch in list element ", as.character(i))
        stop(msg)
      }
      if (!all.equal(as.vector(predictions[[i]]),
                     as.integer(predictions[[i]])) == TRUE) {
        warning("predictions provided are not integers. Don't trust the results.
            Maybe you want to score continuous predictions instead?")
      }
    }
  }

  # ============================================

  res <- list()

  # apply PIT function to true_values and the different predictive_samples
  # provided. Get the p_values for the replications of the randomised PIT
  # with Anderson-Darling Test and return mean and sd of those p-values
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringutils::pit_int(true_values = true_values,
                                        predictions = x)$p_values
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
                  scoringutils::bias_int(predictions = x,
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

# ====================================================== #
# ============ probabilistic + continuous ============== #
# ====================================================== #

#' @title Applies forecast assessments and scores to probabilistic
#' forecasts of continuous outcomes
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of nxN matrices of predictive samples,
#' n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#'
#'
#' @return either a data.frame or list of data frames with the forecast scores
#' @export

eval_forecasts_prob_cont <- function(true_values,
                                     predictions,
                                     metrics = c(),
                                     output = "df") {


  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }


  n <- length(true_values)

  if (!is.list(predictions)) {
    if (is.matrix(predictions)) {
      predictions <- list(predictions)
    }
    else {
      stop("predictions argument should be a list of matrices")
    }
  }

  if (is.data.frame(predictions)) {
    predictions <- list(as.matrix(predictions))
  } else {
    for (i in 1:length(predictions)) {
      if (is.data.frame(predictions[[i]])) {
        predictions[[i]] <- as.matrix(predictions[[i]])
      }
      if (!is.matrix(predictions[[i]])) {
        stop("'predictions' should be a list of matrices")
      }
      if (nrow(predictions[[i]]) != n) {
        msg = cat("all matrices in list 'predictions' must have n rows, ",
                  "where n is the number of true_values to predict. ",
                  "Dimension mismatch in list element ", as.character(i))
        stop(msg)
      }
    }
  }

  # ============================================

  res <- list()


  # CRPS
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringRules::crps_sample(dat = x,
                                            y = true_values)
                },
                true_values = true_values)

  res$CRPS <- data.frame(mean = colMeans(tmp),
                         sd = apply(tmp, MARGIN=2, FUN=sd))


  # CRPS
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringRules::logs_sample(dat = x,
                                            y = true_values)
                },
                true_values = true_values)

  res$logs <- data.frame(mean = colMeans(tmp),
                         sd = apply(tmp, MARGIN=2, FUN=sd))





  if (output == "df") {
    return (do.call("rbind", res))
  } else {
    return (res)
  }


}


# ====================================================== #
# ============= probabilistic + binary ================= #
# ====================================================== #


#' @title Applies forecast assessments and scores to probabilistic
#' forecasts of binary outcomes
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of either
#'
#' \itemize{
#' \item vectors of size n containing probabilities that the corresponding
#' entries of \code{true_values} will be equal to one. Or
#' \item nxN matrices of predictive samples, n (number of rows) being
#' the number of data points and N (number of columns) the
#' number of Monte Carlo samples. As these represent binary outcomes, these
#' matrices should only contain zeros and ones. Internally, this matrices will
#' be converted to probability estimates by averaging over
#' the rows of the matrices.
#' }
#'
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#'
#'
#' @return either a data.frame or list of data frames with the forecast scores
#' @export
#'



eval_forecasts_prob_bin <- function(true_values,
                                    predictions,
                                    metrics = c(),
                                    output = "df") {


  # ============== Error handling ==============

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  if (max(true_values > 1) | min(true_values) < 0){
    stop("elements of 'true_values' should be equal to either zero or one")
  }

  if (!all.equal(true_values, as.integer(true_values))) {
    stop("The true_values provided are not integers.
         Maybe you want to score continuous predictions instead?")
  }

  n <- length(true_values)

  if (!is.list(predictions)) {
    if (is.matrix(predictions) | is.vector(predictions)) {
      predictions <- list(predictions)
    }
    else {
      stop("predictions argument should be a list of vectors (or matrices)")
    }
  }

  if (is.data.frame(predictions)) {
    stop("predictions argument should be a list of vectors (or matrices)")
  }
  for (i in 1:length(predictions)) {
    if (is.data.frame(predictions[[i]])) {
      predictions[[i]] <- as.matrix(predictions[[i]])
    }
    if (!(is.matrix(predictions[[i]]) | is.vector(predictions[[i]])) ) {
      msg = cat("'predictions' should be a list of vectors or matrices. ",
                "This is not the case for element ", as.character(i))
      stop(msg)
    }
  }


  # ============================================

  res <- list()

  # Brier Score
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringutils::brier_score(true_values = true_values,
                                            predictions = x)
                },
                true_values = true_values)

  if (length(tmp) > 1) {
    res$Brier_Score <- data.frame(mean = mean(tmp),
                                  sd = sd(tmp))
  } else {
    res$Brier_Score <- data.frame(Brier_Score = tmp)
  }

  if (output == "df") {
    return (do.call("rbind", res))
  } else {
    return (res)
  }
}



# ====================================================== #
# ============ point prediction + integer ============== #
# ====================================================== #

#' @title Applies forecast assessments and scores to point predictinos
#' of integer outcomes
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of vectors of predicted values of size n that
#' correspond to the elements in true_values.
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#'
#'
#' @return either a data.frame or list of data frames with the forecast scores
#' @export

eval_forecasts_point_int <- function(true_values,
                                     predictions,
                                     metrics = c(),
                                     output = "df") {

}


# ====================================================== #
# ========== point prediction + continuous ============= #
# ====================================================== #

#' @title Applies forecast assessments and scores to point predictinos of
#' continuous outcomes
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of vectors of predicted values of size n that
#' correspond to the elements in true_values.
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#'
#'
#' @return either a data.frame or list of data frames with the forecast scores
#' @export

eval_forecasts_point_cont <- function(true_values,
                                      predictions,
                                      metrics = c(),
                                      output = "df") {

}


# ====================================================== #
# ============ point prediction + binary =============== #
# ====================================================== #

#' @title Applies forecast assessments and scores to point predictinos of
#' binary outcomes
#'
#' @param true_values A vector with the true observed values of size n
#' @param predictions a list of vectors of predicted values of size n that
#' correspond to the elements in true_values. This should be a vector containing
#' only zeros and ones. If you want to make predictions using a probability,
#' use \code{\link{eval_forecasts_prob_bin}}
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#'
#'
#' @return either a data.frame or list of data frames with the forecast scores
#' @export

eval_forecasts_point_bin <- function(true_values,
                                     predictions,
                                     metrics = c(),
                                     output = "df") {

}


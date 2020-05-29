library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)

## Brier Score
true_values <- sample(c(0,1), size = 31, replace = TRUE)
model1 <- runif(n = 30, min = 0, max = 1)
model2 <- runif(n = 30, min = 0, max = 0.8)
id <- 1:30

data <- data.table(true_values,
                   model1,
                   model2,
                   id)
data <- data.table::melt(data, id.vars = c("id", "true_values"),
                 measure.vars = c("model1", "model2"),
                 variable.name = "model",
                 value.name = "predictions")
#
# data <- data.frame(true_values,
#                    model1,
#                    model2,
#                    id) %>%
#   tidyr::pivot_longer(c(model1, model2), names_to = "model",
#                       values_to = "predictions")

## quantile score
true_values <- rnorm(30, mean = 1:30)
id <- 1:30

data <- data.table(true_values,
                   id,
                   model = rep(c("model1", "model2"), each = 30))
data[model == "model1",
     c("lower_90", "lower_50", "lower_0",
       "upper_0", "upper_50", "upper_90") := as.list(stats::qnorm(p = c(0.05, 0.25, 0.5,
                                                                0.5, 0.75, 0.95),
                                                          mean = id)),
     by = id]
data[model == "model2",
     c("lower_90", "lower_50", "lower_0",
       "upper_0", "upper_50", "upper_90") := as.list(stats::qnorm(p = c(0.05, 0.25, 0.5,
                                                                        0.5, 0.75, 0.95),
                                                                  mean = id + 0.1)),
     by = id]


# model1 <- stats::qnorm(p = rep(c(0.05, 0.25, 0.5, 0.5, 0.75, 0.95), each = 30),
#                        mean = 1:30) %>%
#   matrix(ncol = 6) %>%
#   data.frame(id,
#              true_values,
#              model = "model1")
#
# model2 <- stats::qnorm(p = rep(c(0.05, 0.25, 0.5, 0.5, 0.75, 0.95), each = 30),
#                        mean = 1.1:30.1) %>%
#   matrix(ncol = 6) %>%
#   data.frame(id,
#              true_values,
#              model = "model2")
#
# data <- rbind(model1, model2) %>%
#   dplyr::rename_at(c("X1", "X2", "X3", "X4", "X5", "X6"),
#                    ~ c("lower_90", "lower_50", "lower_0",
#                        "upper_0", "upper_50", "upper_90"))

## integer predictions
true_values <- rpois(30, lambda = 1:30)
id <- 1:30

data <- data.table(true_values,
                   id,
                   model = rep(c("model1", "model2"), each = 30))

data <- rbind(data[model == "model1", .(model = "model1",
                                        true_values = true_values,
                                        sample = 1:50,
                                        predictions = rpois(50, lambda = id)), by = id],
              data[model == "model2", .(model = "model2",
                                        true_values = true_values,
                                        sample = 1:50,
                                        predictions = rpois(50, lambda = id + 0.5)), by = id])


# model1 <- replicate(200, rpois(n = 30, lambda = 1:30)) %>%
#   data.frame(true_values,
#              id,
#              model = "model1") %>%
#   tidyr::pivot_longer(cols = c(-true_values, -id, -model),
#                       values_to = "predictions",
#                       names_to = "sample") %>%
#   dplyr::mutate(sample = as.numeric(substring(sample, 2)))
#
# model2 <- replicate(200, rpois(n = 30, lambda = 1.1:30.1)) %>%
#   data.frame(true_values,
#              id,
#              model = "model2") %>%
#   tidyr::pivot_longer(cols = c(-true_values, -id, -model),
#                       values_to = "predictions",
#                       names_to = "sample") %>%
#   dplyr::mutate(sample = as.numeric(substring(sample, 2)))
#
# data <- rbind(model1, model2)


## continuous predictions
true_values <- rnorm(30)
id <- 1:30

data <- data.table(true_values,
                   id,
                   model = rep(c("model1", "model2"), each = 30))

data <- rbind(data[model == "model1", .(model = "model1",
                                        true_values = true_values,
                                        sample = 1:50,
                                        predictions = rnorm(50)), by = id],
              data[model == "model2", .(model = "model2",
                                        true_values = true_values,
                                        sample = 1:50,
                                        predictions = rnorm(50, mean = 0.2)), by = id])

# model1 <- replicate(200, rnorm(30)) %>%
#   data.frame(true_values,
#              id,
#              model = "model1") %>%
#   tidyr::pivot_longer(cols = c(-true_values, -id, -model),
#                       values_to = "predictions",
#                       names_to = "sample") %>%
#   dplyr::mutate(sample = as.numeric(substring(sample, 2)))
#
# model2 <- replicate(200, rnorm(30, mean = 0.1)) %>%
#   data.frame(true_values,
#              id,
#              model = "model2") %>%
#   tidyr::pivot_longer(cols = c(-true_values, -id, -model),
#                       values_to = "predictions",
#                       names_to = "sample") %>%
#   dplyr::mutate(sample = as.numeric(substring(sample, 2)))
#
# data <- rbind(model1, model2)





eval_forecasts <- function(data,
                           metrics = c(),
                           output = "df",
                           summarised = TRUE) {



  n <- length(true_values)

  ## check if predictions are integer, continuous, etc.

  if (any(grepl("lower", names(data)))) {
    prediction_type <- "quantile"
  } else if (all.equal(data$predictions, as.integer(data$predictions)) == TRUE) {
    prediction_type <- "integer"
  } else {
    prediction_type <- "continuous"
  }

  if (all.equal(data$true_values, as.integer(data$true_values)) == TRUE) {
    if (all(true_values %in% c(0,1))) {
      target_type = "binary"
    } else {
      target_type = "integer"
    }
  } else {
    target_type = "continuous"
  }

  # ============================================

  models <- unique(data$model)
  n <- length(unique(data$id))

  res <- list()

  # Brier Score
  if (target_type == "binary") {

    data[, score := scoringutils::brier_score(true_values, predictions),
         by = .(model, id)]
    data[, metric := "Brier Score"]

    # res$brier_score <- data %>%
    #   dplyr::group_by(model) %>%
    #   dplyr::mutate("score" = brier_score(true_values, predictions),
    #                 "metric" = "Brier Score") %>%
    #   dplyr::ungroup()
    return(res)
  }

  # interval score
  if (prediction_type == "quantile") {

    # get column names
    colnames <- colnames(data)
    ranges <- names[grepl("lower", colnames) | grepl("upper", colnames)]

    data <- data.table::melt(data,
                     id.vars = c("id", "true_values", "model"),
                     measure.vars = ranges,
                     variable.name = "range",
                     value.name = "predictions")
    data[, type := gsub("_.*", "", range)]
    data[, range := as.numeric(gsub("^.*?_","", interval))]
    data <- data.table::dcast(data, id + true_values + range + model ~ type, value.var = "predictions")
    data[, score := scoringutils::interval_score(true_values, lower, upper, range)]

    # # get interval ranges to score
    # res$interval_score <- data %>%
    #   tidyr::pivot_longer(cols = c(dplyr::starts_with("lower"),
    #                                dplyr::starts_with("upper")),
    #                       values_to = "predictions", names_to = "interval") %>%
    #   dplyr::mutate(type = gsub("_.*", "", interval),
    #                 interval = as.numeric(gsub("^.*?_","", interval))) %>%
    #   tidyr::pivot_wider(names_from = type, values_from = predictions) %>%
    #   dplyr::mutate(score = scoringutils::interval_score(true_values, lower,
    #                                                      upper, interval)) %>%
    #   tidyr::pivot_wider(names_from = interval, names_prefix = "score_",
    #                      values_from = score)

    return(res)
  }

  # calibration
  # todo


  data[, score := pit(unique(true_values),
                                    t(predictions),
                                    plot = FALSE)$calibration,
       by = .(model)]

  dat <- data.table::dcast(data, model + id + true_values ~ sample,
                    value.var = "predictions")[, id := NULL]

  dat[, c("score", "sd") := pit(true_values,
                   as.matrix(.SD),
                   plot = FALSE), .SDcols = !c("model", "true_values"), by = model]


  res <- dat[, .(score = unique(score),
          sd = unique(sd),
          model = unique(model),
          metric = "PIT calibration"), by = model ]


  # res$PIT_calibration <- data %>%
  #   pivot_wider(values_from = predictions, names_from = sample,
  #               names_prefix = "sample_internal") %>%
  #   dplyr::group_by(model) %>%
  #   dplyr::group_map(~ scoringutils::pit(true_values = true_values,
  #                                        predictions = as.matrix(dplyr::select(., dplyr::starts_with("sample_internal"))),
  #                                        plot = FALSE)$calibration) %>%
  #
  #   magrittr::set_names(models) %>%
  #   dplyr::bind_rows() %>%
  #   tidyr::pivot_longer(cols = dplyr::everything(),
  #                       names_to = "model", values_to = "score") %>%
  #   dplyr::mutate(metric = "PIT calibration")
  #
  # # add sd columns in case of continuous forecasts
  # if (!("sd" %in% names(res$PIT_AD_calibration))) {
  #   res$PIT_AD_calibration$sd <- NA
  # }

  # sharpness

  data[, score := scoringutils::sharpness(t(predictions)), by = .(id, model)]
  data[, metric := "sharpness"]

  # res$sharpness <- data %>%
  #   pivot_wider(values_from = predictions, names_from = sample,
  #               names_prefix = "sample_internal") %>%
  #   dplyr::group_by(model) %>%
  #   dplyr::group_map(~ scoringutils::sharpness(predictions = as.matrix(dplyr::select(., dplyr::starts_with("sample_internal"))))) %>%
  #   magrittr::set_names(models) %>%
  #   dplyr::bind_rows() %>%
  #   tidyr::pivot_longer(cols = dplyr::everything(),
  #                       names_to = "model", values_to = "score") %>%
  #   dplyr::mutate(metric = "sharpness")

  # bias
  data[, score := scoringutils::bias(unique(true_values),
                                     t(predictions)), by = .(id, model)]
  data[, metric := "bias"]


  # tmp <- sapply(predictions,
  #               function(x, true_values) {
  #                 scoringutils::bias(predictions = x,
  #                                    true_values = true_values)
  #               },
  #               true_values = true_values)
  #
  # res$bias <- data.frame(mean = colMeans(tmp),
  #                        sd = apply(tmp, MARGIN=2, FUN=sd),
  #                        model = models,
  #                        metric = "bias")

  # DSS
  data[, score := scoringutils::dss(unique(true_values),
                                    t(predictions)), by = .(id, model)]
  data[, metric := "DSS"]


  # tmp <- sapply(predictions,
  #               function(x, true_values) {
  #                 scoringRules::dss_sample(dat = x,
  #                                          y = true_values)
  #               },
  #               true_values = true_values)
  #
  # res$DSS <- data.frame(mean = colMeans(tmp),
  #                       sd = apply(tmp, MARGIN=2, FUN=sd),
  #                       model = models,
  #                       metric = "DSS")


  # CRPS
  data[, score := scoringutils::crps(unique(true_values),
                                    t(predictions)), by = .(id, model)]
  data[, metric := "CRPS"]


  # tmp <- sapply(predictions,
  #               function(x, true_values) {
  #                 scoringRules::crps_sample(dat = x,
  #                                           y = true_values)
  #               },
  #               true_values = true_values)
  #
  # res$CRPS <- data.frame(mean = colMeans(tmp),
  #                        sd = apply(tmp, MARGIN=2, FUN=sd),
  #                        model = models,
  #                        metric = "CRPS")


  # Log Score
  data[, score := scoringutils::logs(unique(true_values),
                                     t(predictions)), by = .(id, model)]
  data[, metric := "Log Score"]

  if (output == "df") {
    return (do.call("rbind", res))
  } else {
    return (res)
  }
}












eval_forecasts_prob_int <- function(true_values,
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
      if (!all.equal(as.vector(predictions[[i]]),
                     as.integer(predictions[[i]])) == TRUE) {
        warning("predictions provided are not integers. Don't trust the results.
            Maybe you want to score continuous predictions instead?")
      }
    }
  }

  # ============================================


  # = check if predictions are integer, continuous, etc.
  ## check whether continuous or integer
  if (all.equal(as.vector(predictions), as.integer(predictions)) != TRUE) {
    prediction_type <- "integer"
  } else {
    prediction_type <- "integer"
  }

  if (all.equal(as.vector(true_values), as.integer(true_values)) != TRUE) {
    if (all(true_values %in% c(0,1))) {
      target_type = "binary"
    } else {
      target_type = "integer"
    }
  } else {
    target_type = "continuous"
  }

  # ============================================


  models <- 1:length(predictions)

  res <- list()

  # Brier Score
  if (target_type == "binary") {
    tmp <- lapply(predictions,
                  function(x, true_values) {
                    scoringutils::brier_score(true_values = true_values,
                                              predictions = x)
                  },
                  true_values = true_values)
    res$brier_score <- data.frame(score = do.call(c, tmp),
                                  model = models,
                                  metric = "Brier Score")

    return(res)
  }


  # apply PIT function to get p-values of Anderson-Darling Test
  tmp <- lapply(predictions,
                function(x, true_values) {
                  scoringutils::pit(true_values = true_values,
                                    predictions = x,
                                    plot = FALSE)$calibration
                },
                true_values = true_values)

  res$PIT_AD_calibration <- data.frame(do.call(rbind, tmp),
                                       model = models,
                                       metric = "PIT_calibration")
  # add sd columns in case of continuous forecasts
  if (!("sd" %in% names(res$PIT_AD_calibration))) {
    res$PIT_AD_calibration$sd <- NA
  }

  # sharpness
  tmp <- sapply(predictions,
                function(x) {
                  scoringutils::sharpness(x)
                })

  res$sharpness <- data.frame(mean = colMeans(tmp),
                              sd = apply(tmp, MARGIN=2, FUN=sd),
                              model = models,
                              metric = "sharpness")


  # bias
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringutils::bias(predictions = x,
                                     true_values = true_values)
                },
                true_values = true_values)

  res$bias <- data.frame(mean = colMeans(tmp),
                         sd = apply(tmp, MARGIN=2, FUN=sd),
                         model = models,
                         metric = "bias")

  # DSS
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringRules::dss_sample(dat = x,
                                           y = true_values)
                },
                true_values = true_values)

  res$DSS <- data.frame(mean = colMeans(tmp),
                        sd = apply(tmp, MARGIN=2, FUN=sd),
                        model = models,
                        metric = "DSS")


  # CRPS
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringRules::crps_sample(dat = x,
                                            y = true_values)
                },
                true_values = true_values)

  res$CRPS <- data.frame(mean = colMeans(tmp),
                         sd = apply(tmp, MARGIN=2, FUN=sd),
                         model = models,
                         metric = "CRPS")


  if (output == "df") {
    return (do.call("rbind", res))
  } else {
    return (res)
  }
}














#' @title Evaluate forecasts
#'
#' @description The function \code{eval_forecasts} is a wrapper that provides
#' an interface to lower-level functions. It can be used to assess the goodness
#' of probabilistic, quantile or point forecasts to continuous, integer-valued or
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
#' @param predictions a list of nxN matrices of predictive samples. Every list
#' element is a matrix with the predictive samples generated from one model.
#' n (number of rows) of those matrices is the number of data points and N
#' (number of columns) the
#' number of Monte Carlo samples
#' @param metrics what metrics to include. currently not used
#' @param output "df" returns a data.frame, everything else returns a list.
#' @return either a data.frame or list of data frames with the forecast scores
#' @export
#'
#' @examples
#' ## Example: Probabilistic predictions for integer values
#' true_values <- rpois(100, lambda = 1:100)
#'
#' predictions <- replicate(5000, rpois(n = 100, lambda = 1:100))
#'
#'
#' predictions <- list(dat1 = replicate(5000, rpois(n = 100, lambda = 1:100)),
#'                     dat2 = replicate(5000, rpois(n = 100, lambda = 1:100)))
#'
#' eval_forecasts_prob_int(true_values = true_values, predictions = predictions)
#'




eval_forecasts_prob_int <- function(true_values,
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
      if (!all.equal(as.vector(predictions[[i]]),
                     as.integer(predictions[[i]])) == TRUE) {
        warning("predictions provided are not integers. Don't trust the results.
            Maybe you want to score continuous predictions instead?")
      }
    }
  }

  # ============================================

  models <- 1:length(predictions)

  res <- list()

  # apply PIT function to true_values and the different predictive_samples
  # provided. Get the p_values for the replications of the randomised PIT
  # with Anderson-Darling Test and return mean and sd of those p-values
  tmp <- lapply(predictions,
                function(x, true_values) {
                  scoringutils::pit(true_values = true_values,
                                        predictions = x)$calibration
                },
                true_values = true_values)

  res$PIT_AD_calibration <- data.frame(do.call(rbind, tmp),
                                       model = models,
                                       metric = "PIT_calibration")
  # add sd columns in case of continuous forecasts
  if (!("sd" %in% names(res$PIT_AD_calibration))) {
    res$PIT_AD_calibration$sd <- NA
  }

  # sharpness
  tmp <- sapply(predictions,
                function(x) {
                  scoringutils::sharpness(x)
                })

  res$sharpness <- data.frame(mean = colMeans(tmp),
                              sd = apply(tmp, MARGIN=2, FUN=sd),
                              model = models,
                              metric = "sharpness")


  # bias
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringutils::bias(predictions = x,
                                     true_values = true_values)
                },
                true_values = true_values)

  res$bias <- data.frame(mean = colMeans(tmp),
                         sd = apply(tmp, MARGIN=2, FUN=sd),
                         model = models,
                         metric = "bias")

  # DSS
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringRules::dss_sample(dat = x,
                                           y = true_values)
                },
                true_values = true_values)

  res$DSS <- data.frame(mean = colMeans(tmp),
                        sd = apply(tmp, MARGIN=2, FUN=sd),
                        model = models,
                        metric = "DSS")


  # CRPS
  tmp <- sapply(predictions,
                function(x, true_values) {
                  scoringRules::crps_sample(dat = x,
                                            y = true_values)
                },
                true_values = true_values)

  res$CRPS <- data.frame(mean = colMeans(tmp),
                         sd = apply(tmp, MARGIN=2, FUN=sd),
                         model = models,
                         metric = "CRPS")


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


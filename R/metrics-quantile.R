################################################################################
# Metrics with a many-to-one relationship between input and score
################################################################################

#' Weighted Interval Score
#' @inheritParams interval_score
#' @param predicted vector of size n with the predicted values
#' @param quantile vector with quantile levels of size N
#' @param count_median_twice if TRUE, count the median twice in the score
#' @param na.rm if TRUE, ignore NA values when computing the score
#' @importFrom stats weighted.mean
#' @export
wis <- function(observed,
                predicted,
                quantile,
                separate_results = FALSE,
                weigh = TRUE,
                count_median_twice = FALSE,
                na.rm = TRUE) {
  assert_input_quantile(observed, predicted, quantile)
  reformatted <- quantile_to_interval(observed, predicted, quantile)

  if (separate_results) {
    cols <- c("wis", "dispersion", "underprediction", "overprediction")
  } else {
    cols <- "wis"
  }

  reformatted[, eval(cols) := do.call(
    interval_score,
    list(observed = observed,
         lower = lower,
         upper = upper,
         interval_range = range,
         weigh = weigh,
         separate_results = separate_results
    )
  )]

  if (!count_median_twice) {
    reformatted[, weight := ifelse(range == 0, 0.5, 1)]
  } else {
    reformatted[, weight := 1]
  }

  # summarise results by forecast_id
  reformatted <- reformatted[
    , lapply(.SD, weighted.mean, na.rm = na.rm, w = weight),
    by = c("forecast_id"),
    .SDcols = colnames(reformatted) %like% paste(cols, collapse = "|")
  ]

  if (separate_results) {
    return(list(
      wis = reformatted$wis,
      dispersion = reformatted$dispersion,
      underprediction = reformatted$underprediction,
      overprediction = reformatted$overprediction
    ))
  } else {
    return(reformatted$wis)
  }
}


#' @title Interval Coverage (For Quantile-Based Forecasts)
#' @description Check whether the observed value is within a given central
#' prediction interval. The prediction interval is defined by a lower and an
#' upper bound formed by a pair of predictive quantiles. For example, a 50%
#' prediction interval is formed by the 0.25 and 0.75 quantiles of the
#' predictive distribution.
#' @inheritParams wis
#' @param range A single number with the range of the prediction interval in
#' percent (e.g. 50 for a 50% prediction interval) for which you want to compute
#' coverage.
#' @importFrom checkmate assert_number
#' @return A vector of length n with TRUE if the observed value is within the
#' corresponding prediction interval and FALSE otherwise.
#' @name interval_coverage
#' @export
#' @examples
#' observed <- c(1, -15, 22)
#' predicted <- rbind(
#'   c(-1, 0, 1, 2, 3),
#'   c(-2, 1, 2, 2, 4),
#'    c(-2, 0, 3, 3, 4)
#' )
#' quantile <- c(0.1, 0.25, 0.5, 0.75, 0.9)
#' interval_coverage_quantile(observed, predicted, quantile)
interval_coverage_quantile <- function(observed, predicted, quantile, range = 50) {
  assert_input_quantile(observed, predicted, quantile)
  assert_number(range)
  necessary_quantiles <- c((100 - range) / 2, 100 - (100 - range) / 2) / 100
  if (!all(necessary_quantiles %in% quantile)) {
    rlang::warn(
      "To compute the coverage for a range of ", range, "%, the quantiles ",
      necessary_quantiles, " are required. Returnting `NA`.")
    return(NA)
  }
  r <- range
  reformatted <- quantile_to_interval(observed, predicted, quantile)
  reformatted <- reformatted[range %in% r]
  reformatted[, coverage := ifelse(
    observed >= lower & observed <= upper, TRUE, FALSE
  )]
  return(reformatted$coverage)
}


#' @title Determines Bias of Quantile Forecasts
#'
#' @description
#' Determines bias from quantile forecasts. For an increasing number of
#' quantiles this measure converges against the sample based bias version
#' for integer and continuous forecasts.
#'
#' @details
#' For quantile forecasts, bias is measured as
#'
#' \deqn{
#' B_t = (1 - 2 \cdot \max \{i | q_{t,i} \in Q_t \land q_{t,i} \leq x_t\})
#'  \mathbf{1}( x_t \leq q_{t, 0.5}) \\
#' + (1 - 2 \cdot \min \{i | q_{t,i} \in Q_t \land q_{t,i} \geq x_t\})
#'  1( x_t \geq q_{t, 0.5}),}
#'
#' where \eqn{Q_t} is the set of quantiles that form the predictive
#' distribution at time \eqn{t}. They represent our
#' belief about what the observed value $x_t$ will be. For consistency, we define
#' \eqn{Q_t} such that it always includes the element
#' \eqn{q_{t, 0} = - \infty} and \eqn{q_{t,1} = \infty}.
#' \eqn{1()} is the indicator function that is \eqn{1} if the
#' condition is satisfied and $0$ otherwise. In clearer terms, \eqn{B_t} is
#' defined as the maximum percentile rank for which the corresponding quantile
#' is still below the observed value, if the observed value is smaller than the
#' median of the predictive distribution. If the observed value is above the
#' median of the predictive distribution, then $B_t$ is the minimum percentile
#' rank for which the corresponding quantile is still larger than the true
#' value. If the observed value is exactly the median, both terms cancel out and
#' \eqn{B_t} is zero. For a large enough number of quantiles, the
#' percentile rank will equal the proportion of predictive samples below the
#' observed value, and this metric coincides with the one for
#' continuous forecasts.
#'
#' Bias can assume values between
#' -1 and 1 and is 0 ideally (i.e. unbiased).
#' @param predicted vector of length corresponding to the number of quantiles
#' that holds predictions
#' @param quantile vector of corresponding size with the quantile levels for
#' which predictions were made. If this does not contain the median (0.5) then
#' the median is imputed as being the mean of the two innermost quantiles.
#' @inheritParams bias_range
#' @return scalar with the quantile bias for a single quantile prediction
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @examples
#'
#' predicted <- c(
#'   705.500, 1127.000, 4006.250, 4341.500, 4709.000, 4821.996,
#'   5340.500, 5451.000, 5703.500, 6087.014, 6329.500, 6341.000,
#'   6352.500, 6594.986, 6978.500, 7231.000, 7341.500, 7860.004,
#'   7973.000, 8340.500, 8675.750, 11555.000, 11976.500
#' )
#'
#' quantile <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
#'
#' observed <- 8062
#'
#' bias_quantile(observed, predicted, quantile)
#' @export
#' @keywords metric

bias_quantile <- function(observed, predicted, quantile) {

  assert_input_quantile(observed, predicted, quantile)

  n <- length(observed)
  N <- length(quantile)

  dt <- data.table(
    forecast_id = rep(1:n, each = N),
    observed = rep(observed, each = N),
    predicted = as.vector(t(predicted)),
    quantile = quantile
  )[order(forecast_id, quantile)]

  dt <- dt[, .(bias = bias_quantile_single(.SD)),   by = forecast_id]

  return(dt$bias)
}


bias_quantile_single <- function(dt) {

  dt <- dt[!is.na(quantile) & !is.na(predicted)]

  observed <- unique(dt$observed)

  if (nrow(dt) == 0) {
    return(NA_real_)
  }

  if (!all(diff(dt$predicted) >= 0)) {
    stop("Predictions must not be decreasing with increasing quantile level")
  }

  if (0.5 %in% dt$quantile) {
    median_prediction <- dt[quantile == 0.5]$predicted
  } else {
    # if median is not available, compute as mean of two innermost quantiles
    message(
      "Median not available, computing as mean of two innermost quantiles",
      " in order to compute bias."
    )
    median_prediction <-
      0.5 * dt[quantile == max(quantile[quantile < 0.5])]$predicted +
      0.5 * dt[quantile == min(quantile[quantile > 0.5])]$predicted
  }

  if (observed == median_prediction) {
    bias <- 0
    return(bias)
  } else if (observed < median_prediction) {
    if (observed < min(dt$predicted)) {
      bias <- 1
    } else {
      q <- max(dt[predicted <= observed]$quantile)
      bias <- 1 - 2 * q
    }
  } else if (observed > median_prediction) {
    if (observed > max(dt$predicted)) {
      bias <- -1
    } else {
      q <- min(dt[predicted >= observed]$quantile)
      bias <- 1 - 2 * q
    }
  }
  return(bias)
}


################################################################################
# Metrics with a one-to-one relationship between input and score
################################################################################


#' @title Quantile Score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions. Smaller values are better.
#' The quantile score is
#' closely related to the Interval score (see [interval_score()]) and is
#' the quantile equivalent that works with single quantiles instead of
#' central prediction intervals.
#'
#' @param quantile vector of size n with the quantile levels of the
#' corresponding predictions.
#' @param weigh if TRUE, weigh the score by alpha / 2, so it can be averaged
#' into an interval score that, in the limit, corresponds to CRPS. Alpha is the
#' value that corresponds to the (alpha/2) or (1 - alpha/2) quantiles provided
#' and will be computed from the quantile. Alpha is the decimal value that
#' represents how much is outside a central prediction interval (E.g. for a
#' 90 percent central prediction interval, alpha is 0.1). Default: `TRUE`.
#' @return vector with the scoring values
#' @inheritParams interval_score
#' @inheritParams ae_median_sample
#' @examples
#' observed <- rnorm(10, mean = 1:10)
#' alpha <- 0.5
#'
#' lower <- qnorm(alpha / 2, rnorm(10, mean = 1:10))
#' upper <- qnorm((1 - alpha / 2), rnorm(10, mean = 1:10))
#'
#' qs_lower <- quantile_score(observed,
#'   predicted = lower,
#'   quantile = alpha / 2
#' )
#' qs_upper <- quantile_score(observed,
#'   predicted = upper,
#'   quantile = 1 - alpha / 2
#' )
#' interval_score <- (qs_lower + qs_upper) / 2
#' @export
#' @keywords metric
#' @references Strictly Proper Scoring Rules, Prediction,and Estimation,
#' Tilmann Gneiting and Adrian E. Raftery, 2007, Journal of the American
#' Statistical Association, Volume 102, 2007 - Issue 477
#'
#' Evaluating epidemic forecasts in an interval format,
#' Johannes Bracher, Evan L. Ray, Tilmann Gneiting and Nicholas G. Reich,
#' <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618>
quantile_score <- function(observed,
                           predicted,
                           quantile,
                           weigh = TRUE) {

  # compute score - this is the version explained in the SI of Bracher et. al.
  error <- abs(predicted - observed)
  score <- 2 * ifelse(
    observed <= predicted, 1 - quantile, quantile
  ) * error

  # adapt score such that mean of unweighted quantile scores corresponds to
  # unweighted interval score of the corresponding prediction interval
  # --> needs central prediction interval which corresponds to given quantiles
  central_interval <- abs(0.5 - quantile) * 2
  alpha <- 1 - central_interval
  score <- 2 * score / alpha

  # if weigh, then reverse last operation
  if (weigh) {
    score <- score * alpha / 2
  }

  return(score)
}


# Weighted Interval Score, But With One-to-One Relationship
wis_one_to_one <- function(observed,
                           predicted,
                           quantile,
                           separate_results = FALSE,
                           output = c("matrix", "data.frame", "vector"),
                           weigh = TRUE) {

  # input checks
  assert_input_quantile(observed, predicted, quantile)

  # store original data
  n <- length(observed)
  N <- length(quantile)
  original_data <- data.table(
    forecast_id = rep(1:n, each = N),
    observed = rep(observed, each = N),
    predicted = as.vector(t(predicted)),
    quantile = quantile
  )

  # define output columns
  if (separate_results) {
    cols <- c("wis", "dispersion", "underprediction", "overprediction")
  } else {
    cols <- "wis"
  }

  # reformat input to interval format and calculate interval score
  reformatted <- quantile_to_interval(observed, predicted, quantile)
  reformatted[, eval(cols) := do.call(
    interval_score,
    list(observed = observed,
         lower = lower,
         upper = upper,
         interval_range = range,
         weigh = weigh,
         separate_results = separate_results
    )
  )]

  # melt data to long format, calclate quantiles, and merge back to original
  long <- melt(reformatted,
               measure.vars = c("lower", "upper"),
               variable.name = "boundary",
               value.name = "predicted",
               id.vars = c("forecast_id", "observed", "range", cols))
  # calculate quantiles
  long[, quantile := (100 - range) / 200] # lower quantiles
  long[boundary == "upper", quantile :=  1 - quantile] # upper quantiles
  # remove boundary, range, take unique value to get rid of duplicated median
  long[, c("boundary", "range") := NULL]
  long <- unique(long) # should maybe check for count_median_twice?
  out <- merge(
    original_data, long, all.x = TRUE,
    by = c("forecast_id", "observed", "predicted", "quantile")
  )[, forecast_id := NULL]

  # handle returns depending on the output format
  if (output == "data.frame") {
    return(out)
  }

  wis <- out$wis
  if (separate_results) {
    components <- list(
      underprediction = out$underprediction,
      overprediction = out$overprediction,
      dispersion = out$dispersion
    )
  }

  if (output == "vector" && separate_results) {
    return(c(wis = wis, components))
  } else if (output == "vector") {
    return(wis)
  }

  if (output == "matrix") {
    wis <- matrix(wis, nrow = n, ncol = N)
    if (separate_results) {
      components <- lapply(components, function(x) matrix(x, nrow = n, ncol = N))
      return(c(wis, components))
    } else {
      return(wis)
    }
  }
}


#' @title Absolute Error of the Median (Quantile-based Version)
#'
#' @description
#' Absolute error of the median calculated as
#'
#' \deqn{
#'   \textrm{abs}(\textrm{observed} - \textrm{prediction})
#' }{
#'   abs(observed - median_prediction)
#' }
#'
#' The function was created for internal use within [score()], but can also
#' used as a standalone function.
#'
#' @param predicted numeric vector with predictions, corresponding to the
#' quantiles in a second vector, `quantiles`.
#' @param quantiles numeric vector that denotes the quantile for the values
#' in `predicted`. Only those predictions where `quantiles == 0.5` will
#' be kept. If `quantiles` is `NULL`, then all `predicted` and
#' `observed` will be used (this is then the same as [abs_error()])
#' @return vector with the scoring values
#' @seealso [ae_median_sample()], [abs_error()]
#' @importFrom stats median
#' @inheritParams ae_median_sample
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- rnorm(30, mean = 1:30)
#' ae_median_quantile(observed, predicted_values, quantiles = 0.5)
#' @export
#' @keywords metric

ae_median_quantile <- function(observed, predicted, quantiles = NULL) {
  if (!is.null(quantiles)) {
    if (!any(quantiles == 0.5) && !anyNA(quantiles)) {
      return(NA_real_)
      warning(
        "in order to compute the absolute error of the median, `0.5` must be ",
        "among the quantiles given. Maybe you want to use `abs_error()`?"
      )
    }
    observed <- observed[quantiles == 0.5]
    predicted <- predicted[quantiles == 0.5]
  }
  abs_error_median <- abs(observed - predicted)
  return(abs_error_median)
}

################################################################################
# Metrics with a many-to-one relationship between input and score
################################################################################

#' Weighted Interval Score (WIS)
#' @description
#' The WIS is a proper scoring rule used to evaluate forecasts in an interval- /
#' quantile-based format. See Bracher et al. (2021). Smaller values are better.
#'
#' As the name suggest the score assumes that a forecast comes in the form of
#' one or multiple central prediction intervals. A prediction interval is
#' characterised by a lower and an upper bound formed by a pair of predictive
#' quantiles. For example, a 50% central prediction interval is formed by the
#' 0.25 and 0.75 quantiles of the predictive distribution.
#'
#' **Interval score**
#'
#' The interval score (IS) is the sum of three components:
#' overprediction, underprediction and dispersion. For a single prediction
#' interval only one of the components is non-zero. If for a single prediction
#' interval the observed value is below the lower bound, then the interval
#' score is equal to the absolute difference between the lower bound and the
#' observed value ("underprediction"). "Overprediction" is defined analogously.
#' If the observed value falls within the bounds of the prediction interval,
#' then the interval score is equal to the width of the prediction interval,
#' i.e. the difference between the upper and lower bound. For a single interval,
#' we therefore have:
#'
#' \deqn{
#' \textrm{IS} = (\textrm{upper} - \textrm{lower}) + \frac{2}{\alpha}(\textrm{lower}
#'  - \textrm{observed}) *
#' \mathbf{1}(\textrm{observed} < \textrm{lower}) +
#' \frac{2}{\alpha}(\textrm{observed} - \textrm{upper}) *
#' \mathbf{1}(\textrm{observed} > \textrm{upper})
#' }{
#' score = (upper - lower) + 2/alpha * (lower - observed) *
#' 1(observed < lower) + 2/alpha * (observed - upper) *
#' 1(observed > upper)
#' }
#' where \eqn{\mathbf{1}()}{1()} is the indicator function and
#' indicates how much is outside the prediction interval.
#' \eqn{\alpha}{alpha} is the decimal value that indicates how much is outside
#' the prediction interval. For a 90% prediction interval, for example,
#' \eqn{\alpha}{alpha} is equal to 0.1. No specific distribution is assumed,
#' but the interval formed by the quantiles has to be symmetric around the
#' median (i.e you can't use the 0.1 quantile as the lower bound and the 0.7
#' quantile as the upper bound).
#' Non-symmetric quantiles can be scored using the function [quantile_score()].
#'
#' Usually the interval score is weighted by a factor that makes sure that the
#' average score across an increasing number of equally spaced
#' quantiles, converges to the continuous ranked probability score (CRPS). This
#' weighted score is called the weihted interval score (WIS).
#' The weight commonly used is \eqn{\alpha / 2}{alpha / 2}.
#'
#' **Quantile score**
#'
#' In addition to the interval score, there also exists a quantile score (QS)
#' (see [quantile_score()]), which is equal to the so-called pinball loss.
#' The quantile score can be computed for a single quantile (whereas the
#' interval score requires two quantiles that form an interval). However,
#' the intuitive decomposition into overprediction, underprediction and
#' dispersion does not exist for the quantile score.
#'
#' **Two versions of the weighted interval score**
#'
#' There are two ways to conceptualise the weighted interval score across
#' several quantiles / prediction intervals and the median.
#'
#' In one view, you would treat the WIS as the average of quantile scores (and
#' the median as 0.5-quantile) (this is the default for `wis()`). In another
#' view, you would treat the WIS as the average of several interval scores +
#' the difference between observed value and median forecast. The effect of
#' that is that in contrast to the first view, the median has twice as much
#' weight (because it is weighted like a prediction interval, rather than like
#' a single quantile). Both are valid ways to conceptualise the WIS and you
#' can control the behvaviour with the `count_median_twice`-argument.
#'
#' **WIS components**:
#' WIS components can be computed individually using the functions
#' `overprediction`, `underprediction`, and `dispersion.`
#'
#' @inheritParams interval_score
#' @param observed numeric vector of size n with the observed values
#' @param predicted numeric nxN matrix of predictive
#' quantiles, n (number of rows) being the number of forecasts (corresponding
#' to the number of observed values) and N
#' (number of columns) the number of quantiles per forecast.
#' If `observed` is just a single number, then predicted can just be a
#' vector of size N.
#' @param quantile_level vector with quantile levels of size N
#' @param count_median_twice if TRUE, count the median twice in the score
#' @param na.rm if TRUE, ignore NA values when computing the score
#' @importFrom stats weighted.mean
#' @importFrom checkmate assert_logical
#' @return
#' `wis()`: a numeric vector with WIS values of size n (one per observation),
#' or a list with separate entries if `separate_results` is `TRUE`.
#' @export
#' @keywords metric
#' @examples
#' observed <- c(1, -15, 22)
#' predicted <- rbind(
#'   c(-1, 0, 1, 2, 3),
#'   c(-2, 1, 2, 2, 4),
#'   c(-2, 0, 3, 3, 4)
#' )
#' quantile_level <- c(0.1, 0.25, 0.5, 0.75, 0.9)
#' wis(observed, predicted, quantile_level)
wis <- function(observed,
                predicted,
                quantile_level,
                separate_results = FALSE,
                weigh = TRUE,
                count_median_twice = FALSE,
                na.rm = TRUE) {
  assert_input_quantile(observed, predicted, quantile_level)
  reformatted <- quantile_to_interval(observed, predicted, quantile_level)

  assert_logical(separate_results, len = 1)
  assert_logical(weigh, len = 1)
  assert_logical(count_median_twice, len = 1)
  assert_logical(na.rm, len = 1)

  if (separate_results) {
    cols <- c("wis", "dispersion", "underprediction", "overprediction")
  } else {
    cols <- "wis"
  }

  reformatted[, eval(cols) := do.call(
    interval_score,
    list(
      observed = observed,
      lower = lower,
      upper = upper,
      interval_range = interval_range,
      weigh = weigh,
      separate_results = separate_results
    )
  )]

  if (count_median_twice) {
    reformatted[, weight := 1]
  } else {
    reformatted[, weight := ifelse(interval_range == 0, 0.5, 1)]
  }

  # summarise results by forecast_id
  reformatted <- reformatted[
    , lapply(.SD, weighted.mean, na.rm = na.rm, w = weight),
    by = "forecast_id",
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


#' @return
#' `dispersion()`: a numeric vector with dispersion values (one per observation)
#' @param ... Additional arguments passed on to `wis()` from functions
#' `overprediction()`, `underprediction()` and `dispersion()`
#' @export
#' @rdname wis
#' @keywords metric
dispersion <- function(observed, predicted, quantile_level, ...) {
  args <- list(...)
  args$separate_results <- TRUE
  assert_input_quantile(observed, predicted, quantile_level)
  out <- do.call(
    wis, c(list(observed), list(predicted), list(quantile_level), args)
  )
  return(out$dispersion)
}


#' @return
#' `overprediction()`: a numeric vector with overprediction values (one per
#' observation)
#' @export
#' @rdname wis
#' @keywords metric
overprediction <- function(observed, predicted, quantile_level, ...) {
  args <- list(...)
  args$separate_results <- TRUE
  assert_input_quantile(observed, predicted, quantile_level)
  out <- do.call(
    wis, c(list(observed), list(predicted), list(quantile_level), args)
  )
  return(out$overprediction)
}


#' @return
#' `underprediction()`: a numeric vector with underprediction values (one per
#' observation)
#' @export
#' @rdname wis
#' @keywords metric
underprediction <- function(observed, predicted, quantile_level, ...) {
  args <- list(...)
  args$separate_results <- TRUE
  assert_input_quantile(observed, predicted, quantile_level)
  out <- do.call(
    wis, c(list(observed), list(predicted), list(quantile_level), args)
  )
  return(out$underprediction)
}


#' @title Interval Coverage (For Quantile-Based Forecasts)
#' @description Check whether the observed value is within a given central
#' prediction interval. The prediction interval is defined by a lower and an
#' upper bound formed by a pair of predictive quantiles. For example, a 50%
#' prediction interval is formed by the 0.25 and 0.75 quantiles of the
#' predictive distribution.
#' @inheritParams wis
#' @param interval_range A single number with the range of the prediction
#' interval in percent (e.g. 50 for a 50% prediction interval) for which you
#' want to compute interval coverage.
#' @importFrom checkmate assert_number
#' @importFrom cli cli_warn
#' @importFrom rlang caller_env
#' @return A vector of length n with elements either TRUE,
#' if the observed value is within the corresponding prediction interval, and
#' FALSE otherwise.
#' @name interval_coverage
#' @export
#' @keywords metric
#' @examples
#' observed <- c(1, -15, 22)
#' predicted <- rbind(
#'   c(-1, 0, 1, 2, 3),
#'   c(-2, 1, 2, 2, 4),
#'    c(-2, 0, 3, 3, 4)
#' )
#' quantile_level <- c(0.1, 0.25, 0.5, 0.75, 0.9)
#' interval_coverage(observed, predicted, quantile_level)
interval_coverage <- function(observed, predicted,
                              quantile_level, interval_range = 50) {
  assert_input_quantile(observed, predicted, quantile_level)
  assert_number(interval_range)
  necessary_quantiles <- c(
    (100 - interval_range) / 2,
    100 - (100 - interval_range) / 2
  ) / 100
  if (!all(necessary_quantiles %in% quantile_level)) {
    #nolint start: keyword_quote_linter object_usage_linter
    cli_warn(
      c(
        "!" = "To compute the interval coverage for an interval range of
        {interval_range}, the {.val {necessary_quantiles}} quantiles
        are required.",
        "i" = "Returning {.val {NA}}."
      )
    )
    #nolint end
    return(NA)
  }
  r <- interval_range
  reformatted <- quantile_to_interval(observed, predicted, quantile_level)
  reformatted <- reformatted[interval_range %in% r]
  reformatted[, interval_coverage := (observed >= lower) & (observed <= upper)]
  return(reformatted$interval_coverage)
}


#' @title Interval Coverage Deviation (For Quantile-Based Forecasts)
#' @description Check the agreement between desired and actual interval coverage
#' of a forecast.
#'
#' The function is similar to [interval_coverage()],
#' but takes all provided prediction intervals into account and
#' compares nominal interval coverage (i.e. the desired interval coverage) with
#' the actual observed interval coverage.
#'
#' A central symmetric prediction interval is defined by a lower and an
#' upper bound formed by a pair of predictive quantiles. For example, a 50%
#' prediction interval is formed by the 0.25 and 0.75 quantiles of the
#' predictive distribution. Ideally, a forecaster should aim to cover about
#' 50% of all observed values with their 50% prediction intervals, 90% of all
#' observed values with their 90% prediction intervals, and so on.
#'
#' For every prediction interval, the deviation is computed as the difference
#' between the observed interval coverage and the nominal interval coverage
#' For a single observed value and a single prediction interval, coverage is
#' always either 0 or 1 (`FALSE` or `TRUE`). This is not the case for a single
#' observed value and multiple prediction intervals,
#' but it still doesn't make that much
#' sense to compare nominal (desired) coverage and actual coverage for a single
#' observation. In that sense coverage deviation only really starts to make
#' sense as a metric when averaged across multiple observations).
#'
#' Positive values of interval coverage deviation are an indication for
#' underconfidence, i.e. the forecaster could likely have issued a narrower
#' forecast. Negative values are an indication for overconfidence, i.e. the
#' forecasts were too narrow.
#'
#' \deqn{
#' \textrm{interval coverage deviation} =
#' \mathbf{1}(\textrm{observed value falls within interval}) -
#' \textrm{nominal interval coverage}
#' }{
#' interval coverage deviation =
#' 1(observed value falls within interval) - nominal interval coverage
#' }
#' The interval coverage deviation is then averaged across all prediction
#' intervals. The median is ignored when computing coverage deviation.
#' @inheritParams wis
#' @importFrom cli cli_warn
#' @return A numeric vector of length n with the interval coverage deviation
#' for each forecast (comprising one or multiple prediction intervals).
#' @export
#' @keywords metric
#' @examples
#' observed <- c(1, -15, 22)
#' predicted <- rbind(
#'   c(-1, 0, 1, 2, 3),
#'   c(-2, 1, 2, 2, 4),
#'   c(-2, 0, 3, 3, 4)
#' )
#' quantile_level <- c(0.1, 0.25, 0.5, 0.75, 0.9)
#' interval_coverage_deviation(observed, predicted, quantile_level)
interval_coverage_deviation <- function(observed, predicted, quantile_level) {
  assert_input_quantile(observed, predicted, quantile_level)

  # transform available quantile_levels into central interval ranges
  available_ranges <- unique(get_range_from_quantile(quantile_level))

  # check if all necessary quantile_levels are available
  necessary_quantiles <- unique(
    c((100 - available_ranges) / 2, 100 - (100 - available_ranges) / 2) / 100
  )
  if (!all(necessary_quantiles %in% quantile_level)) {
    #nolint start: keyword_quote_linter object_usage_linter
    missing <- necessary_quantiles[!necessary_quantiles %in% quantile_level]
    cli_warn(
      c(
        "x" = "To compute interval coverage deviation, all quantiles must form
        central symmetric prediction intervals.",
        "i" = "Missing quantiles: {.val {missing}}. Returning {.val {NA}}."
      )
    )
    #nolint end
    return(NA)
  }

  reformatted <- quantile_to_interval(
    observed, predicted, quantile_level
  )[interval_range != 0]
  reformatted[, interval_coverage := (observed >= lower) & (observed <= upper)]
  reformatted[, interval_coverage_deviation :=
                interval_coverage - interval_range / 100]
  out <- reformatted[, .(
    interval_coverage_deviation = mean(interval_coverage_deviation)
  ), by = "forecast_id"]
  return(out$interval_coverage_deviation)
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
#' @param observed a single number representing the observed value
#' @param predicted vector of length corresponding to the number of quantiles
#' that holds predictions
#' @param quantile_level vector of corresponding size with the quantile levels for
#' which predictions were made. If this does not contain the median (0.5) then
#' the median is imputed as being the mean of the two innermost quantiles.
#' @param na.rm logical. Should missing values be removed?
#' @importFrom cli cli_inform
#' @return scalar with the quantile bias for a single quantile prediction
#' @export
#' @keywords metric
#' @examples
#' predicted <- c(
#'   705.500, 1127.000, 4006.250, 4341.500, 4709.000, 4821.996,
#'   5340.500, 5451.000, 5703.500, 6087.014, 6329.500, 6341.000,
#'   6352.500, 6594.986, 6978.500, 7231.000, 7341.500, 7860.004,
#'   7973.000, 8340.500, 8675.750, 11555.000, 11976.500
#' )
#'
#' quantile_level <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
#'
#' observed <- 8062
#'
#' bias_quantile(observed, predicted, quantile_level)
bias_quantile <- function(observed, predicted, quantile_level, na.rm = TRUE) {
  assert_input_quantile(observed, predicted, quantile_level)
  n <- length(observed)
  N <- length(quantile_level)
  if (is.null(dim(predicted))) {
    dim(predicted) <- c(n, N)
  }
  if (!(0.5 %in% quantile_level)) {
    #nolint start: keyword_quote_linter
    cli_inform(
      c(
        "i" = "Median not available, computing bias as mean of the two
        innermost quantiles in order to compute bias."
      )
    )
    #nolint end
  }
  bias <- sapply(1:n, function(i) {
    bias_quantile_single_vector(
      observed[i], predicted[i, ], quantile_level, na.rm
    )
  })
  return(bias)
}


#' Compute Bias for a Single Vector of Quantile Predictions
#' @description Internal function to compute bias for a single observed value,
#' a vector of predicted values and a vector of quantiles.
#' @param observed scalar with the observed value
#' @param predicted vector of length N corresponding to the number of quantiles
#' that holds predictions
#' @param quantile_level vector of corresponding size N with the quantile levels
#' for which predictions were made. If this does not contain the median (0.5)
#' then the median is imputed as being the mean of the two innermost quantiles.
#' @inheritParams bias_quantile
#' @importFrom cli cli_abort
#' @return scalar with the quantile bias for a single quantile prediction
#' @keywords internal
bias_quantile_single_vector <- function(observed, predicted,
                                        quantile_level, na.rm) {

  assert_number(observed)
  # other checks should have happend before

  predicted_has_NAs <- anyNA(predicted)
  quantile_has_NAs <- anyNA(quantile_level)

  if (any(predicted_has_NAs, quantile_has_NAs)) {
    if (na.rm) {
      quantile_level <- quantile_level[!is.na(predicted)]
      predicted <- predicted[!is.na(predicted)]
      predicted <- predicted[!is.na(quantile_level)]
      quantile_level <- quantile_level[!is.na(quantile_level)]
    } else {
      return(NA_real_)
    }
  }

  order <- order(quantile_level)
  predicted <- predicted[order]
  if (!all(diff(predicted) >= 0)) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Predictions must not be decreasing with increasing
        quantile level."
      )
    )
    #nolint end
  }

  if (0.5 %in% quantile_level) {
    median_prediction <- predicted[quantile_level == 0.5]
  } else {
    # if median is not available, compute as mean of two innermost quantile
    upper_level <- max(quantile_level[quantile_level < 0.5])
    lower_level <- min(quantile_level[quantile_level > 0.5])
    median_prediction <-
      0.5 * predicted[quantile_level == upper_level] +
      0.5 * predicted[quantile_level == lower_level]
  }

  if (observed == median_prediction) {
    bias <- 0
    return(bias)
  } else if (observed < median_prediction) {
    if (observed < min(predicted)) {
      bias <- 1
    } else {
      q <- max(quantile_level[predicted <= observed])
      bias <- 1 - 2 * q
    }
  } else if (observed > median_prediction) {
    if (observed > max(predicted)) {
      bias <- -1
    } else {
      q <- min(quantile_level[predicted >= observed])
      bias <- 1 - 2 * q
    }
  }
  return(bias)
}


#' @title Absolute Error of the Median (Quantile-based Version)
#' @description
#' Compute the absolute error of the median calculated as
#' \deqn{
#'   \textrm{abs}(\textrm{observed} - \textrm{median prediction})
#' }{
#'   abs(observed - median_prediction)
#' }
#' The median prediction is the predicted value for which quantile_level == 0.5,
#' the function therefore requires 0.5 to be among the quantile levels in
#' `quantile_level`.
#' @inheritParams wis
#' @return numeric vector of length N with the absolute error of the median
#' @seealso [ae_median_sample()]
#' @importFrom stats median
#' @importFrom cli cli_warn
#' @importFrom rlang caller_env
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- matrix(rnorm(30, mean = 1:30))
#' ae_median_quantile(observed, predicted_values, quantile_level = 0.5)
#' @export
#' @keywords metric
ae_median_quantile <- function(observed, predicted, quantile_level) {
  assert_input_quantile(observed, predicted, quantile_level)
  if (!any(quantile_level == 0.5)) {
    #nolint start: keyword_quote_linter
    cli_warn(
      c(
        "x" = "In order to compute the absolute error of the median,
        {.var 0.5} must be among the quantiles given.",
        "i" = "Returning `NA`."
      )
    )
    #nolint end
    return(NA_real_)
  }
  if (is.null(dim(predicted))) {
    predicted <- matrix(predicted, nrow = 1)
  }
  predicted <- predicted[, quantile_level == 0.5]
  abs_error_median <- abs(observed - predicted)
  return(abs_error_median)
}


################################################################################
# Metrics with a one-to-one relationship between input and score
################################################################################


#' @title Quantile Score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions. Smaller values are better.
#' The quantile score is
#' closely related to the Interval score (see [wis()]) and is
#' the quantile equivalent that works with single quantiles instead of
#' central prediction intervals.
#'
#' @param quantile_level vector of size n with the quantile levels of the
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
#'   quantile_level = alpha / 2
#' )
#' qs_upper <- quantile_score(observed,
#'   predicted = upper,
#'   quantile_level = 1 - alpha / 2
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
                           quantile_level,
                           weigh = TRUE) {

  # compute score - this is the version explained in the SI of Bracher et. al.
  error <- abs(predicted - observed)
  score <- 2 * ifelse(
    observed <= predicted, 1 - quantile_level, quantile_level
  ) * error

  # adapt score such that mean of unweighted quantile scores corresponds to
  # unweighted interval score of the corresponding prediction interval
  # --> needs central prediction interval which corresponds to given quantiles
  central_interval <- abs(0.5 - quantile_level) * 2
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
                           quantile_level,
                           separate_results = FALSE,
                           output = c("matrix", "data.frame", "vector"),
                           weigh = TRUE) {

  # input checks
  assert_input_quantile(observed, predicted, quantile_level)

  # store original data
  n <- length(observed)
  N <- length(quantile_level)
  original_data <- data.table(
    forecast_id = rep(1:n, each = N),
    observed = rep(observed, each = N),
    predicted = as.vector(t(predicted)),
    quantile_level = quantile_level
  )

  # define output columns
  if (separate_results) {
    cols <- c("wis", "dispersion", "underprediction", "overprediction")
  } else {
    cols <- "wis"
  }

  # reformat input to interval format and calculate interval score
  reformatted <- quantile_to_interval(observed, predicted, quantile_level)
  reformatted[, eval(cols) := do.call(
    interval_score,
    list(
      observed = observed,
      lower = lower,
      upper = upper,
      interval_range = interval_range,
      weigh = weigh,
      separate_results = separate_results
    )
  )]

  # melt data to long format, calculate quantile_levels, merge back to original
  long <- melt(reformatted,
               measure.vars = c("lower", "upper"),
               variable.name = "boundary",
               value.name = "predicted",
               id.vars = c("forecast_id", "observed", "interval_range", cols))
  # calculate quantile levels
  long[, quantile_level := (100 - interval_range) / 200] # lower quantile_levels
  long[boundary == "upper", quantile_level :=  1 - quantile_level] # upper quantile_levels
  # remove boundary, interval_range, take unique value to get rid of duplicated median
  long[, c("boundary", "interval_range") := NULL]
  long <- unique(long) # should maybe check for count_median_twice?
  out <- merge(
    original_data, long, all.x = TRUE,
    by = c("forecast_id", "observed", "predicted", "quantile_level")
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
      components <- lapply(components, matrix, nrow = n, ncol = N)
      return(c(wis, components))
    } else {
      return(wis)
    }
  }
}

#' @title Assert that inputs are correct for quantile-based forecast
#' @description
#' Function assesses whether the inputs correspond to the
#' requirements for scoring quantile-based forecasts.
#' @param predicted Input to be checked. Should be nxN matrix of predictive
#'   quantiles, n (number of rows) being the number of data points and N
#'   (number of columns) the number of quantiles per forecast.
#'   If `observed` is just a single number, then predicted can just be a
#'   vector of size N.
#' @param quantile_level Input to be checked. Should be a vector of size N that
#'   denotes the quantile levels corresponding to the columns of the prediction
#'   matrix.
#' @param unique_quantile_levels Whether the quantile levels are required to be
#'   unique (`TRUE`, the default) or not (`FALSE`).
#' @importFrom checkmate assert assert_numeric check_matrix check_vector
#' @inherit document_assert_functions params return
#' @keywords internal_input_check
assert_input_quantile <- function(observed, predicted, quantile_level,
                                  unique_quantile_levels = TRUE) {
  assert_numeric(observed, min.len = 1)
  n_obs <- length(observed)

  assert_numeric(
    quantile_level, min.len = 1, lower = 0, upper = 1,
    unique = unique_quantile_levels
  )
  n_quantiles <- length(quantile_level)
  if (n_obs == 1) {
    assert(
      # allow one of two options
      check_numeric_vector(predicted, min.len = n_quantiles),
      check_matrix(predicted, mode = "numeric",
                   nrows = n_obs, ncols = n_quantiles)
    )
    assert(check_vector(quantile_level, len = length(predicted)))
  } else {
    assert(
      check_matrix(predicted, mode = "numeric",
                   nrows = n_obs, ncols = n_quantiles)
    )
  }
  return(invisible(NULL))
}

#' @title Check that inputs are correct for quantile-based forecast
#' @inherit assert_input_quantile params description
#' @inherit check_input_sample return description
#' @keywords internal_input_check
check_input_quantile <- function(observed, predicted, quantile_level) {
  result <- check_try(
    assert_input_quantile(observed, predicted, quantile_level)
  )
  return(result)
}


#' Weighted interval score (WIS)
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
#' For a set of \eqn{k = 1, \dots, K} prediction intervals and the median
#' \eqn{m}, we can compute a weighted interval score (WIS) as the sum of the
#' interval scores for individual intervals:
#' \deqn{
#' \text{WIS}_{\alpha_{\{0:K\}}}(F, y) = \frac{1}{K + 1/2}
#' \times \left(w_0 \times |y - m| + \sum_{k=1}^{K}
#' \left\{ w_k \times \text{IS}_{\alpha_k}(F, y) \right\}\right)
#' }{
#' }
#'
#' The individual scores are usually weighted with
#' \eqn{w_k = \frac{\alpha_k}{2}}{alpha_k / 2}. This weight ensures that
#' for an increasing number of equally spaced quantiles, the WIS
#' converges to the continuous ranked probability score (CRPS).
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
#' the difference between the observed value and median forecast. The effect of
#' that is that in contrast to the first view, the median has twice as much
#' weight (because it is weighted like a prediction interval, rather than like
#' a single quantile). Both are valid ways to conceptualise the WIS and you
#' can control the behaviour with the `count_median_twice`-argument.
#'
#' **WIS components**:
#' WIS components can be computed individually using the functions
#' `overprediction`, `underprediction`, and `dispersion.`
#'
#' @inheritParams interval_score
#' @param observed Numeric vector of size n with the observed values.
#' @param predicted Numeric nxN matrix of predictive
#'   quantiles, n (number of rows) being the number of forecasts (corresponding
#'   to the number of observed values) and N
#'   (number of columns) the number of quantiles per forecast.
#'   If `observed` is just a single number, then predicted can just be a
#'   vector of size N.
#' @param quantile_level Vector of of size N with the quantile levels
#'   for which predictions were made.
#' @param count_median_twice If TRUE, count the median twice in the score.
#' @param na.rm If TRUE, ignore NA values when computing the score.
#' @importFrom stats weighted.mean
#' @importFrom checkmate assert_logical
#' @inheritSection illustration-input-metric-quantile Input format
#' @return
#' `wis()`: a numeric vector with WIS values of size n (one per observation),
#' or a list with separate entries if `separate_results` is `TRUE`.
#' @export
#' @references
#' Evaluating epidemic forecasts in an interval format,
#' Johannes Bracher, Evan L. Ray, Tilmann Gneiting and Nicholas G. Reich, 2021,
#' <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618>
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
                na.rm = FALSE) {
  assert_input_quantile(observed, predicted, quantile_level)
  reformatted <- quantile_to_interval(observed, predicted, quantile_level)

  # check that all quantile levels form valid prediction intervals
  interval_ranges <- get_range_from_quantile(
    quantile_level[quantile_level != 0.5]
  )
  complete_intervals <-
    duplicated(interval_ranges) | duplicated(interval_ranges, fromLast = TRUE)
  if (!all(complete_intervals) && !isTRUE(na.rm)) {
    #nolint start: keyword_quote_linter object_usage_linter
    incomplete <- quantile_level[quantile_level != 0.5][!complete_intervals]
    cli_abort(
      c(
        "!" = "Not all quantile levels specified form symmetric prediction
        intervals.
        The following quantile levels miss a corresponding lower/upper bound:
        {.val {incomplete}}.
        You can drop incomplete prediction intervals using `na.rm = TRUE`."
      )
    )
    #nolint end
  }

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
#' `dispersion_quantile()`: a numeric vector with dispersion values (one per
#' observation).
#' @param ... Additional arguments passed on to `wis()` from functions
#'   `overprediction_quantile()`, `underprediction_quantile()` and
#'   `dispersion_quantile()`.
#' @export
#' @rdname wis
#' @keywords metric
dispersion_quantile <- function(observed, predicted, quantile_level, ...) {
  args <- list(...)
  args$separate_results <- TRUE
  assert_input_quantile(observed, predicted, quantile_level)
  out <- do.call(
    wis, c(list(observed), list(predicted), list(quantile_level), args)
  )
  return(out$dispersion)
}


#' @return
#' `overprediction_quantile()`: a numeric vector with overprediction values
#' (one per observation).
#' @export
#' @rdname wis
#' @keywords metric
overprediction_quantile <- function(observed, predicted, quantile_level, ...) {
  args <- list(...)
  args$separate_results <- TRUE
  assert_input_quantile(observed, predicted, quantile_level)
  out <- do.call(
    wis, c(list(observed), list(predicted), list(quantile_level), args)
  )
  return(out$overprediction)
}


#' @return
#' `underprediction_quantile()`: a numeric vector with underprediction values
#' (one per observation)
#' @export
#' @rdname wis
#' @keywords metric
underprediction_quantile <- function(observed, predicted, quantile_level, ...) {
  args <- list(...)
  args$separate_results <- TRUE
  assert_input_quantile(observed, predicted, quantile_level)
  out <- do.call(
    wis, c(list(observed), list(predicted), list(quantile_level), args)
  )
  return(out$underprediction)
}


#' @title Interval coverage (for quantile-based forecasts)
#' @description
#' Check whether the observed value is within a given central
#' prediction interval. The prediction interval is defined by a lower and an
#' upper bound formed by a pair of predictive quantiles. For example, a 50%
#' prediction interval is formed by the 0.25 and 0.75 quantiles of the
#' predictive distribution.
#' @inheritParams wis
#' @inheritSection illustration-input-metric-quantile Input format
#' @param interval_range A single number with the range of the prediction
#'   interval in percent (e.g. 50 for a 50% prediction interval) for which you
#'   want to compute interval coverage.
#' @importFrom checkmate assert_number
#' @importFrom cli cli_warn
#' @return
#' A vector of length n with elements either TRUE,
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
    cli_abort(
      c(
        "!" = "To compute the interval coverage for an interval range of
        {.val {interval_range}%}, the {.val {necessary_quantiles}} quantiles
        are required"
      )
    )
    #nolint end
  }
  r <- interval_range
  reformatted <- quantile_to_interval(observed, predicted, quantile_level)
  reformatted <- reformatted[interval_range %in% r]
  reformatted[, interval_coverage := (observed >= lower) & (observed <= upper)]
  return(reformatted$interval_coverage)
}


#' @title Determines bias of quantile forecasts
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
#' distribution at time \eqn{t} and \eqn{x_t} is the observed value. For
#' consistency, we define \eqn{Q_t} such that it always includes the element
#' \eqn{q_{t, 0} = - \infty} and \eqn{q_{t,1} = \infty}.
#' \eqn{1()} is the indicator function that is \eqn{1} if the
#' condition is satisfied and \eqn{0} otherwise.
#'
#' In clearer terms, bias \eqn{B_t} is:
#' - \eqn{1 - 2 \cdot} the maximum percentile rank for which the corresponding
#' quantile is still smaller than or equal to the observed value,
#' *if the observed value is smaller than the median of the predictive
#' distribution.*
#' - \eqn{1 - 2 \cdot} the minimum percentile rank for which the corresponding
#' quantile is still larger than or equal to the observed value *if the observed
#' value is larger
#' than the median of the predictive distribution.*.
#' - \eqn{0} *if the observed value is exactly the median* (both terms cancel
#' out)
#'
#' Bias can assume values between -1 and 1 and is 0 ideally (i.e. unbiased).
#'
#' Note that if the given quantiles do not contain the median, the median is
#' imputed as a linear interpolation of the two innermost quantiles. If the
#' median is not available and cannot be imputed, an error will be thrown.
#' Note that in order to compute bias, quantiles must be non-decreasing with
#' increasing quantile levels.
#'
#' For a large enough number of quantiles, the
#' percentile rank will equal the proportion of predictive samples below the
#' observed value, and the bias metric coincides with the one for
#' continuous forecasts (see [bias_sample()]).
#'
#' @param quantile_level Vector of of size N with the quantile levels
#'   for which predictions were made. Note that if this does not contain the
#'   median (0.5) then the median is imputed as being the mean of the two
#'   innermost quantiles.
#' @param na.rm Logical. Should missing values be removed?
#' @importFrom cli cli_inform
#' @inheritParams wis
#' @inheritSection illustration-input-metric-quantile Input format
#' @returns scalar with the quantile bias for a single quantile prediction
#' @export
#' @keywords metric
#' @examples
#' predicted <- matrix(c(1.5:23.5, 3.3:25.3), nrow = 2, byrow = TRUE)
#' quantile_level <- c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)
#' observed <- c(15, 12.4)
#' bias_quantile(observed, predicted, quantile_level)
bias_quantile <- function(observed, predicted, quantile_level, na.rm = TRUE) {
  assert_input_quantile(observed, predicted, quantile_level)
  # for bias quantile to work, at least one quantile level has to be <= 0.5
  # and at least one >= 0.5
  assert_vector(quantile_level[quantile_level <= 0.5], min.len = 1)
  assert_vector(quantile_level[quantile_level >= 0.5], min.len = 1)

  n <- length(observed)
  N <- length(quantile_level)
  if (is.null(dim(predicted))) {
    dim(predicted) <- c(n, N)
  }
  if (!(0.5 %in% quantile_level)) {
    #nolint start: keyword_quote_linter
    cli_inform(
      c(
        "i" = "Median not available, interpolating median from the two
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


#' Compute bias for a single vector of quantile predictions
#' @description
#' Internal function to compute bias for a single observed value,
#' a vector of predicted values and a vector of quantiles.
#' @param observed Scalar with the observed value.
#' @param predicted Vector of length N (corresponding to the number of
#'   quantiles) that holds predictions.
#' @inheritParams bias_quantile
#' @importFrom cli cli_abort
#' @returns scalar with the quantile bias for a single quantile prediction
#' @keywords internal
bias_quantile_single_vector <- function(observed, predicted,
                                        quantile_level, na.rm) {

  assert_number(observed)
  # other checks should have happend before

  predicted_has_NAs <- anyNA(predicted)
  quantile_has_NAs <- anyNA(quantile_level)

  if (any(predicted_has_NAs, quantile_has_NAs)) {
    if (!na.rm) {
      return(NA_real_)
    }
    quantile_level <- quantile_level[!is.na(predicted)]
    predicted <- predicted[!is.na(predicted)]
    predicted <- predicted[!is.na(quantile_level)]
    quantile_level <- quantile_level[!is.na(quantile_level)]
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

  median_prediction <- interpolate_median(predicted, quantile_level)

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

#' Helper function to interpolate the median prediction if it is not available
#' @description
#' Internal function to interpolate the median prediction if it is not
#' available in the given quantile levels.
#' This is done using linear interpolation between the two innermost quantiles.
#' @inheritParams bias_quantile_single_vector
#' @inheritSection illustration-input-metric-quantile Input format
#' @returns scalar with the imputed median prediction
#' @keywords internal
interpolate_median <- function(predicted, quantile_level) {
  if (0.5 %in% quantile_level) {
    median_prediction <- predicted[quantile_level == 0.5]
  } else {
    # determine the two innermost quantiles
    upper_q_level <- max(quantile_level[quantile_level < 0.5])
    lower_q_level <- min(quantile_level[quantile_level > 0.5])
    upper_value <- predicted[quantile_level == upper_q_level]
    lower_value <- predicted[quantile_level == lower_q_level]

    # do a linear interpolation
    # weight is the proportion of the distance between the lower quantile and
    # the median relative to the distance between the upper and lower quantile
    w <- (0.5 - lower_q_level) / (upper_q_level - lower_q_level)
    median_prediction <- lower_value + w * (upper_value - lower_value)
  }
  return(median_prediction)
}


#' Absolute error of the median (quantile-based version)
#'
#' Compute the absolute error of the median calculated as
#' \deqn{
#'   |\text{observed} - \text{median prediction}|
#' }
#' The median prediction is the predicted value for which quantile_level == 0.5.
#' The function requires 0.5 to be among the quantile levels in `quantile_level`.
#'
#' @inheritParams wis
#' @inheritSection illustration-input-metric-quantile Input format
#' @returns Numeric vector of length N with the absolute error of the median.
#' @seealso [ae_median_sample()]
#' @importFrom stats median
#' @importFrom cli cli_warn
#' @keywords metric
#' @examples
#' observed <- rnorm(30, mean = 1:30)
#' predicted_values <- replicate(3, rnorm(30, mean = 1:30))
#' ae_median_quantile(
#'   observed, predicted_values, quantile_level = c(0.2, 0.5, 0.8)
#' )
#' @export
ae_median_quantile <- function(observed, predicted, quantile_level) {
  assert_input_quantile(observed, predicted, quantile_level)
  if (!any(quantile_level == 0.5)) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "In order to compute the absolute error of the median,
        {.val 0.5} must be among the quantiles given"
      )
    )
    #nolint end
  }
  if (is.null(dim(predicted))) {
    predicted <- matrix(predicted, nrow = 1)
  }
  predicted <- predicted[, quantile_level == 0.5]
  abs_error_median <- abs(observed - predicted)
  return(abs_error_median)
}


#' @title Quantile score
#'
#' @description
#' Proper Scoring Rule to score quantile predictions. Smaller values are better.
#' The quantile score is closely related to the interval score (see [wis()]) and
#' is the quantile equivalent that works with single quantiles instead of
#' central prediction intervals.
#'
#' The quantile score, also called pinball loss, for a single quantile
#' level \eqn{\tau} is defined as
#' \deqn{
#'   \text{QS}_\tau(F, y) = 2 \cdot \{ \mathbf{1}(y \leq q_\tau) - \tau\} \cdot (q_\tau - y) =
#'   \begin{cases}
#' 2 \cdot (1 - \tau) * q_\tau - y,       & \text{if } y \leq q_\tau\\
#' 2 \cdot \tau * |q_\tau - y|,           & \text{if } y > q_\tau,
#' \end{cases}
#' }
#' with \eqn{q_\tau} being the \eqn{\tau}-quantile of the predictive
#' distribution \eqn{F}, and \eqn{\mathbf{1}(\cdot)} the indicator function.
#'
#' The weighted interval score for a single prediction interval can be obtained
#' as the average of the quantile scores for the lower and upper quantile of
#' that prediction interval:
#' \deqn{
#'   \text{WIS}_\alpha(F, y) = \frac{\text{QS}_{\alpha/2}(F, y)
#'   + \text{QS}_{1 - \alpha/2}(F, y)}{2}.
#' }
#' See the SI of Bracher et al. (2021) for more details.
#'
#' `quantile_score()` returns the average quantile score across the quantile
#' levels provided. For a set of quantile levels that form pairwise central
#' prediction intervals, the quantile score is equivalent to the interval score.
#' @returns Numeric vector of length n with the quantile score. The scores are
#' averaged across quantile levels if multiple quantile levels are provided
#' (the result of calling `rowMeans()` on the matrix of quantile scores that
#' is computed based on the observed and predicted values).
#' @inheritParams wis
#' @inheritSection illustration-input-metric-quantile Input format
#' @examples
#' observed <- rnorm(10, mean = 1:10)
#' alpha <- 0.5
#'
#' lower <- qnorm(alpha / 2, observed)
#' upper <- qnorm((1 - alpha / 2), observed)
#'
#' qs_lower <- quantile_score(observed,
#'   predicted = matrix(lower),
#'   quantile_level = alpha / 2
#' )
#' qs_upper <- quantile_score(observed,
#'   predicted = matrix(upper),
#'   quantile_level = 1 - alpha / 2
#' )
#' interval_score <- (qs_lower + qs_upper) / 2
#' interval_score2 <- quantile_score(
#'   observed,
#'   predicted = cbind(lower, upper),
#'   quantile_level = c(alpha / 2, 1 - alpha / 2)
#' )
#'
#' # this is the same as the following
#' wis(
#'   observed,
#'   predicted = cbind(lower, upper),
#'   quantile_level = c(alpha / 2, 1 - alpha / 2)
#' )
#' @importFrom checkmate test_atomic_vector
#' @export
#' @keywords metric
#' @references Strictly Proper Scoring Rules, Prediction,and Estimation,
#' Tilmann Gneiting and Adrian E. Raftery, 2007, Journal of the American
#' Statistical Association, Volume 102, 2007 - Issue 477
#'
#' Evaluating epidemic forecasts in an interval format,
#' Johannes Bracher, Evan L. Ray, Tilmann Gneiting and Nicholas G. Reich, 2021,
#' <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618>
quantile_score <- function(observed,
                           predicted,
                           quantile_level,
                           weigh = TRUE) {
  assert_input_quantile(observed, predicted, quantile_level)

  # compute score - this is the version explained in the SI of Bracher et. al.
  obs_smaller_pred <- observed <= predicted

  if (test_atomic_vector(predicted)) {
    obs_smaller_pred <- matrix(obs_smaller_pred, nrow = 1)
  }
  # subtract quantile level tau from result of indicator function (1(y <= q_tau))
  # (subtraction is element-wise and by row)
  tau_diff <- sweep(obs_smaller_pred, 2, quantile_level)

  error <- predicted - observed

  score <- 2 * tau_diff * error

  # By default, the quantile score already corresponds to the WIS such that
  # simply averaging the quantile scores gives the WIS
  if (weigh) {
    return(rowMeans(score))
  } else {
    # adapt score such that mean of adapted quantile scores corresponds to
    # unweighted interval score of the corresponding prediction interval
    central_interval <- abs(0.5 - quantile_level) * 2
    alpha <- 1 - central_interval

    # unweighted score' = 2 * score / alpha
    score <- 2 * sweep(score, 2, alpha, FUN = "/")
    return(rowMeans(score))
  }
}

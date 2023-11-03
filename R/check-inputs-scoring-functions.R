#' @title Assert that inputs are correct for sample-based forecast
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring sample-based forecasts.
#' @param observed Input to be checked. Should be a numeric vector with the
#' observed values of size n
#' @param predicted Input to be checked. Should be a numeric nxN matrix of
#' predictive samples, n (number of rows) being the number of data points and N
#' (number of columns) the number of samples per forecast.
#' If `observed` is just a single number, then predicted values can just be a
#' vector of size N.
#' @importFrom checkmate assert assert_numeric check_matrix
#' @inherit document_assert_functions return
#' @keywords check-inputs
assert_input_sample <- function(observed, predicted) {
  assert_numeric(observed, min.len = 1)
  n_obs <- length(observed)

  if (n_obs == 1) {
    assert(
      # allow one of two options
      check_numeric_vector(predicted, min.len = 1),
      check_matrix(predicted, mode = "numeric", nrows = n_obs)
    )
  } else {
    assert(check_matrix(predicted, mode = "numeric", nrows = n_obs))
  }
  return(invisible(NULL))
}

#' @title Check that inputs are correct for sample-based forecast
#' @inherit assert_input_sample params description
#' @inherit document_check_functions return
#' @keywords check-inputs
check_input_sample <- function(observed, predicted) {
  result <- check_try(assert_input_sample(observed, predicted))
  return(result)
}


#' @title Assert that inputs are correct for quantile-based forecast
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring quantile-based forecasts.
#' @param observed Input to be checked. Should be a numeric vector with the
#' observed values of size n
#' @param predicted Input to be checked. Should be nxN matrix of predictive
#' quantiles, n (number of rows) being the number of data points and N
#' (number of columns) the number of quantiles per forecast.
#' If `observed` is just a single number, then predicted can just be a
#' vector of size N.
#' @param quantile Input to be checked. Should be a vector of size N that
#' denotes the quantile levels corresponding to the columns of the prediction
#' matrix.
#' @importFrom checkmate assert assert_numeric check_matrix
#' @inherit document_assert_functions return
#' @keywords internal
assert_input_quantile <- function(observed, predicted, quantile) {
  assert_numeric(observed, min.len = 1)
  n_obs <- length(observed)

  assert_numeric(quantile, min.len = 1, lower = 0, upper = 1)
  n_quantiles <- length(quantile)
  if (n_obs == 1) {
    assert(
      # allow one of two options
      check_numeric_vector(predicted, min.len = 1),
      check_matrix(predicted, mode = "numeric",
                   nrows = n_obs, ncols = n_quantiles)
    )
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
#' @keywords check-inputs
check_input_quantile <- function(observed, predicted, quantile) {
  result <- check_try(assert_input_quantile(observed, predicted, quantile))
  return(result)
}


#' @title Assert that inputs are correct for interval-based forecast
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring interval-based forecasts.
#' @param observed Input to be checked. Should be a numeric vector with the
#' observed values of size n
#' @param lower Input to be checked. Should be a numeric vector of size n that
#' holds the predicted value for the lower bounds of the prediction intervals.
#' @param upper Input to be checked. Should be a numeric vector of size n that
#' holds the predicted value for the upper bounds of the prediction intervals.
#' @param range Input to be checked. Should be a vector of size n that
#' denotes the interval range in percent. E.g. a value of 50 denotes a
#' (25%, 75%) prediction interval.
#' @importFrom rlang warn
#' @inherit document_assert_functions return
#' @keywords internal
assert_input_interval <- function(observed, lower, upper, range) {

  assert_numeric_vector(observed, min.len = 1)
  n <- length(observed)
  assert_numeric_vector(lower, len = n)
  assert_numeric_vector(upper, len = n)
  assert_numeric_vector(range, len = n, lower = 0, upper = 100)

  diff <- upper - lower
  if (any(diff < 0)) {
    stop(
      "All values in `upper` need to be greater than or equal to ",
      "the corresponding values in `lower`"
    )
  }
  if (any(range > 0 & range < 1, na.rm = TRUE)) {
    msg <- paste(
      "Found interval ranges between 0 and 1. Are you sure that's right? An",
      "interval range of 0.5 e.g. implies a (49.75%, 50.25%) prediction",
      "interval. If you want to score a (25%, 75%) prediction interval, set",
      "`interval_range = 50`."
    )
    rlang::warn(
      message = msg, .frequency = "once",
      .frequency_id = "small_interval_range"
    )
  }
  return(invisible(NULL))
}


#' @title Check that inputs are correct for interval-based forecast
#' @inherit assert_input_interval params description
#' @inherit check_input_sample return description
#' @keywords check-inputs
check_input_interval <- function(observed, lower, upper, range) {
  result <- check_try(assert_input_quantile(observed, lower, upper, range))
  return(result)
}


#' @title Assert that inputs are correct for binary forecast
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring binary forecasts.
#' @param observed Input to be checked. Should be a factor of length n with
#' exactly two levels, holding the observed values.
#' The highest factor level is assumed to be the reference level. This means
#' that `predicted` represents the probability that the observed value is equal
#' to the highest factor level.
#' @param predicted Input to be checked. `predicted` should be a vector of
#' length n, holding probabilities. Values represent the probability that
#' the corresponding value in `observed` will be equal to the highest
#' available factor level.
#' @importFrom checkmate assert assert_factor
#' @inherit document_assert_functions return
#' @keywords check-inputs
assert_input_binary <- function(observed, predicted) {
  if (length(observed) != length(predicted)) {
    stop("`observed` and `predicted` need to be ",
         "of same length when scoring binary forecasts")
  }
  assert_factor(observed, n.levels = 2)
  levels <- levels(observed)
  assert(
    check_numeric_vector(predicted, min.len = 1, lower = 0, upper = 1)
  )
  return(invisible(NULL))
}

#' @title Check that inputs are correct for binary forecast
#' @inherit assert_input_binary params description
#' @inherit document_check_functions return
#' @keywords check-inputs
check_input_binary <- function(observed, predicted) {
  result <- check_try(assert_input_binary(observed, predicted))
  return(result)
}


#' @title Assert that inputs are correct for point forecast
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring point forecasts.
#' @param observed Input to be checked. Should be a numeric vector with the
#' observed values of size n
#' @param predicted Input to be checked. Should be a numeric vector with the
#' predicted values of size n
#' @inherit document_assert_functions return
#' @keywords check-inputs
assert_input_point <- function(observed, predicted) {
  assert(check_numeric_vector(observed, min.len = 1))
  assert(check_numeric_vector(predicted, min.len = 1))
  if (length(observed) != length(predicted)) {
    stop("`observed` and `predicted` need to be ",
         "of same length when scoring point forecasts")
  }
  return(invisible(NULL))
}

#' @title Check that inputs are correct for point forecast
#' @inherit assert_input_point params description
#' @inherit document_check_functions return
#' @keywords check-inputs
check_input_point <- function(observed, predicted) {
  result <- check_try(assert_input_point(observed, predicted))
  return(result)
}

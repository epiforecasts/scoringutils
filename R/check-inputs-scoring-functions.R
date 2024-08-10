#' @title Assert that inputs are correct for sample-based forecast
#' @description
#' Function assesses whether the inputs correspond to the requirements for
#' scoring sample-based forecasts.
#' @param predicted Input to be checked. Should be a numeric nxN matrix of
#'   predictive samples, n (number of rows) being the number of data points and
#'   N (number of columns) the number of samples per forecast.
#'   If `observed` is just a single number, then predicted values can just be a
#'   vector of size N.
#' @importFrom checkmate assert assert_numeric check_matrix assert_matrix
#' @inherit document_assert_functions params return
#' @keywords internal_input_check
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
    assert_matrix(predicted, mode = "numeric", nrows = n_obs)
  }
  return(invisible(NULL))
}

#' @title Check that inputs are correct for sample-based forecast
#' @inherit assert_input_sample params description
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_input_sample <- function(observed, predicted) {
  result <- check_try(assert_input_sample(observed, predicted))
  return(result)
}


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


#' @title Assert that inputs are correct for interval-based forecast
#' @description
#' Function assesses whether the inputs correspond to the
#' requirements for scoring interval-based forecasts.
#' @param lower Input to be checked. Should be a numeric vector of size n that
#'   holds the predicted value for the lower bounds of the prediction intervals.
#' @param upper Input to be checked. Should be a numeric vector of size n that
#'   holds the predicted value for the upper bounds of the prediction intervals.
#' @param interval_range Input to be checked. Should be a vector of size n that
#'   denotes the interval range in percent. E.g. a value of 50 denotes a
#'   (25%, 75%) prediction interval.
#' @importFrom cli cli_warn cli_abort
#' @inherit document_assert_functions params return
#' @keywords internal_input_check
assert_input_interval <- function(observed, lower, upper, interval_range) {

  assert(check_numeric_vector(observed, min.len = 1))
  n <- length(observed)
  assert(check_numeric_vector(lower, len = n))
  assert(check_numeric_vector(upper, len = n))
  assert(
    check_numeric_vector(interval_range, len = 1, lower = 0, upper = 100),
    check_numeric_vector(interval_range, len = n, lower = 0, upper = 100)
  )

  diff <- upper - lower
  diff <- diff[!is.na(diff)]
  if (any(diff < 0)) {
    cli_abort(
      c(
        "!" = "All values in `upper` need to be greater than or equal to
        the corresponding values in `lower`"
      )
    )
  }
  if (any(interval_range > 0 & interval_range < 1, na.rm = TRUE)) {
    #nolint start: keyword_quote_linter
    cli_warn(
      c(
        "!" = "Found interval ranges between 0 and 1. Are you sure that's
        right? An interval range of 0.5 e.g. implies a (49.75%, 50.25%)
        prediction interval.",
        "i" = "If you want to score a (25%, 75%) prediction interval, set
        `interval_range = 50`."
      ),
      .frequency = "once",
      .frequency_id = "small_interval_range"
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @title Check that inputs are correct for interval-based forecast
#' @inherit assert_input_interval params description
#' @inherit check_input_sample return description
#' @keywords internal_input_check
check_input_interval <- function(observed, lower, upper, interval_range) {
  result <- check_try(
    assert_input_interval(observed, lower, upper, interval_range)
  )
  return(result)
}


#' @title Assert that inputs are correct for binary forecast
#' @description
#' Function assesses whether the inputs correspond to the
#' requirements for scoring binary forecasts.
#' @param observed Input to be checked. Should be a factor of length n with
#'   exactly two levels, holding the observed values.
#'   The highest factor level is assumed to be the reference level. This means
#'   that `predicted` represents the probability that the observed value is
#'   equal to the highest factor level.
#' @param predicted Input to be checked. `predicted` should be a vector of
#'   length n, holding probabilities. Alternatively, `predicted` can be a matrix
#'   of size n x 1. Values represent the probability that
#'   the corresponding value in `observed` will be equal to the highest
#'   available factor level.
#' @importFrom checkmate assert assert_factor
#' @inherit document_assert_functions return
#' @keywords internal_input_check
assert_input_binary <- function(observed, predicted) {
  assert_factor(observed, n.levels = 2, min.len = 1)
  assert_numeric(predicted, lower = 0, upper = 1)
  assert_dims_ok_point(observed, predicted)
  return(invisible(NULL))
}


#' @title Check that inputs are correct for binary forecast
#' @inherit assert_input_binary params description
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_input_binary <- function(observed, predicted) {
  result <- check_try(assert_input_binary(observed, predicted))
  return(result)
}


#' @title Assert that inputs are correct for nominal forecasts
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring nominal forecasts.
#' @param observed Input to be checked. Should be a factor of length n with
#'   N levels holding the observed values. n is the number of observations and
#'   N is the number of possible outcomes the observed values can assume.
#'   output)
#' @param predicted Input to be checked. Should be nxN matrix of predictive
#'   quantiles, n (number of rows) being the number of data points and N
#'   (number of columns) the number of possible outcomes the observed values
#'   can assume.
#'   If `observed` is just a single number, then predicted can just be a
#'   vector of size N.
#' @param predicted Input to be checked. `predicted` should be a vector of
#'   length n, holding probabilities. Alternatively, `predicted` can be a matrix
#'   of size n x 1. Values represent the probability that
#'   the corresponding value in `observed` will be equal to the highest
#'   available factor level.
#' @param predicted_label Factor of length N with N levels, where N is the
#'   number of possible outcomes the observed values can assume.
#' @importFrom checkmate assert_factor assert_numeric assert_set_equal
#' @inherit document_assert_functions return
#' @keywords internal_input_check
assert_input_nominal <- function(observed, predicted, predicted_label) {
  # observed
  assert_factor(observed, min.len = 1, min.levels = 2)
  levels <- levels(observed)
  n <- length(observed)
  N <- length(levels)

  # predicted label
  assert_factor(
    predicted_label, len = N,
    any.missing = FALSE, empty.levels.ok = FALSE
  )
  assert_set_equal(levels(observed), levels(predicted_label))

  # predicted
  assert_numeric(predicted, min.len = 1, lower = 0, upper = 1)
  if (n == 1) {
    assert(
      # allow one of two options
      check_vector(predicted, len = N),
      check_matrix(predicted, nrows = n, ncols = N)
    )
    summed_predictions <- .rowSums(predicted, m = 1, n = N, na.rm = TRUE)
  } else {
    assert_matrix(predicted, nrows = n)
    summed_predictions <- round(rowSums(predicted, na.rm = TRUE), 10) # avoid numeric errors
  }
  if (!all(summed_predictions == 1)) {
    #nolint start: keyword_quote_linter object_usage_linter
    row_indices <- as.character(which(summed_predictions != 1))
    cli_abort(
      c(
        `!` = "Probabilities belonging to a single forecast must sum to one",
        `i` = "Found issues in row{?s} {row_indices} of {.var predicted}"
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @title Assert that inputs are correct for point forecast
#' @description
#' Function assesses whether the inputs correspond to the
#' requirements for scoring point forecasts.
#' @param predicted Input to be checked. Should be a numeric vector with the
#'   predicted values of size n.
#' @inherit document_assert_functions params return
#' @keywords internal_input_check
assert_input_point <- function(observed, predicted) {
  assert(check_numeric(observed))
  assert(check_numeric(predicted))
  assert(check_dims_ok_point(observed, predicted))
  return(invisible(NULL))
}

#' @title Check that inputs are correct for point forecast
#' @inherit assert_input_point params description
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_input_point <- function(observed, predicted) {
  result <- check_try(assert_input_point(observed, predicted))
  return(result)
}


#' @title Assert Inputs Have Matching Dimensions
#' @description
#' Function assesses whether input dimensions match. In the
#' following, n is the number of observations / forecasts. Scalar values may
#' be repeated to match the length of the other input.
#' Allowed options are therefore:
#' - `observed` is vector of length 1 or length n
#' - `predicted` is:
#'     - a vector of of length 1 or length n
#'     - a matrix with n rows and 1 column
#' @inherit assert_input_binary
#' @inherit document_assert_functions return
#' @importFrom checkmate assert_vector check_matrix check_vector assert
#' @importFrom cli cli_abort
#' @keywords internal_input_check
assert_dims_ok_point <- function(observed, predicted) {
  assert_vector(observed, min.len = 1)
  n_obs <- length(observed)
  assert(
    check_vector(predicted, min.len = 1, strict = TRUE),
    check_matrix(predicted, ncols = 1, nrows = n_obs)
  )
  n_pred <- length(as.vector(predicted))
  # check that both are either of length 1 or of equal length
  if ((n_obs != 1) && (n_pred != 1) && (n_obs != n_pred)) {
    #nolint start: keyword_quote_linter object_usage_linter
    cli_abort(
      c(
        "!" = "`observed` and `predicted` must either be of length 1 or
         of equal length.",
        "i" = "Found {n_obs} and {n_pred}."
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @title Check Inputs Have Matching Dimensions
#' @inherit assert_dims_ok_point params description
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_dims_ok_point <- function(observed, predicted) {
  result <- check_try(assert_dims_ok_point(observed, predicted))
  return(result)
}

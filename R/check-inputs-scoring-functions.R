#' @title Assert that inputs are correct for sample-based forecast
#'
#' @description Helper function to assert whether the input is suitable for
#' scoring.
#' @param observed Input to be checked. Should be a numeric vector with the
#' observed values of size n
#' @param predicted Input to be checked. Should be a numeric nxN matrix of
#' predictive samples, n (number of rows) being the number of data points and N
#' (number of columns) the number of samples per forecast.
#' If `observed` is just a single number, then predicted values can just be a
#' vector of size N.
#' @importFrom checkmate assert assert_numeric check_matrix
#' @return Returns NULL invisibly if the check was successful and throws an
#' error otherwise.
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
#'
#' @description Helper function to check whether the input is suitable for
#' scoring.
#' @inheritParams assert_input_sample
#' @return Returns TRUE if the check was successful and a string with the
#' error message otherwise
#' @keywords check-inputs
check_input_sample <- function(observed, predicted) {
  result <- check_try(assert_input_sample(observed, predicted))
  return(result)
}


#' @title Assert that inputs are correct for sample-based forecast
#'
#' @description Helper function to assert whether the input is suitable for
#' scoring.
#' @param observed Input to be checked. Should be a vector with the
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
#' @return Returns NULL invisibly if the check was successful and throws an
#' error otherwise.
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
#'
#' @description Helper function to check whether the input is suitable for
#' scoring.
#' @inheritParams assert_input_quantile
#' @return Returns TRUE if the check was successful and a string with the
#' error message otherwise
#' @keywords check-inputs
check_input_quantile <- function(observed, predicted, quantile) {
  result <- check_try(assert_input_quantile(observed, predicted, quantile))
  return(result)
}


#' @title Assert that inputs are correct for binary forecast
#'
#' @description Helper function to assert whether the input is suitable for
#' scoring.
#' @param observed Input to be checked. Should be a factor of length n with
#' exactly two levels, holding the observed values.
#' The highest factor level is assumed to be the reference level. This means
#' that `predicted` represents the probability that the observed value is equal
#' to the highest factor level.
#' @param predicted Input to be checked. `predicted` should be a vector of
#' length n, holding probabilities. Values represent the probability that
#' the corresponding value in `observed` will be equal to the highest
#' available factor level.
#' @param ... additional arguments passed to other functions
#' @importFrom checkmate assert assert_factor
#' @return Returns NULL invisibly if the check was successful and throws an
#' error otherwise.
#' @keywords check-inputs
assert_input_binary <- function(observed, predicted, ...) {
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
#'
#' @description Helper function to check whether the input is suitable for
#' scoring.
#' @inheritParams assert_input_binary
#' @return Returns TRUE if the check was successful and a string with the
#' error message otherwise
#' @keywords check-inputs
check_input_binary <- function(observed, predicted) {
  result <- check_try(assert_input_binary(observed, predicted, call_levels_up = 8))
  return(result)
}


#' @title Assert that inputs are correct for point forecast
#'
#' @description Helper function to assert whether the input is suitable for
#' scoring.
#' @param observed Input to be checked. Should be a numeric vector with the
#' observed values of size n
#' @param predicted Input to be checked. Should be a numeric vector with the
#' predicted values of size n
#' @return Returns NULL invisibly if the check was successful and throws an
#' error otherwise.
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
#'
#' @description Helper function to check whether the input is suitable for
#' scoring.
#' @inheritParams assert_input_point
#' @return Returns TRUE if the check was successful and a string with the
#' error message otherwise
#' @keywords check-inputs
check_input_point <- function(observed, predicted) {
  result <- check_try(assert_input_point(observed, predicted))
  return(result)
}

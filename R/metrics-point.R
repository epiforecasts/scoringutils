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

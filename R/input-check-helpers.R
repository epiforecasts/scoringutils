#' @title Check input for sample-based forecast
#'
#' @description Helper function to check whether the input is suitable for
#' scoring.
#' @param observed Input to be checked. Should be a vector with the true
#' observed values of size n
#' @param predicted Input to be checked. Should be nxN matrix of predictive
#' samples, n (number of rows) being the number of data points and N (number of
#' columns) the number of samples per forecast.
#' If `observed` is just a single number, then predicted values can just be a
#' vector of size N.
#' @importFrom checkmate assert assert_numeric check_matrix
#' @return Returns NULL invisibly if the check was successful and throws an
#' error otherwise.
#' @keywords internal

check_input_sample <- function(observed, predicted) {
  # things that need to be checked
  # - observed and predicted need to be numeric
  # - observed needs to be a scalar or a vector of size n x 1
  # - predicted needs to be a vector or a matrix of size n x N

  assert_numeric(observed, min.len = 1)
  n_obs <- length(observed)

  if (n_obs == 1) {
    assert(
      # allow one of two options
      check_numeric_vector(predicted, min.len = 1),
      check_matrix(predicted, mode = "numeric")
      )
  } else {
    assert(check_matrix(predicted, mode = "numeric"))
  }

  if (n_obs != nrow(predicted)) {
    # create custom, more informative error message
    msg <- sprintf(
      "Mismatch: 'observed' has length `%s`, but 'predicted' has `%s` rows.",
      n_obs, nrow(predicted)
    )
    stop(msg)
  }
  return(invisible(NULL))
}

#' @title Check input for sample-based forecast
#'
#' @description Helper function to check whether the input is suitable for
#' scoring.
#' @param observed Input to be checked. Should be a vector with the true
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

check_input_quantile <- function(observed, predicted, quantile) {
  # things that need to be checked
  # - all inputs need to be numeric
  # - quantile needs to be a numeric vector of size N between 0 and 1
  # - observed needs to be a scalar or a vector of size n x 1
  # - predicted needs to be a vector or a matrix of size n x N

  assert_numeric(observed, min.len = 1)
  n_obs <- length(observed)

  assert_numeric(quantile, min.len = 1, lower = 0, upper = 1)
  n_quantiles <- length(quantile)
  assert(
    check_numeric_vector(predicted, min.len = 1),
    check_matrix(predicted, mode = "numeric",
                 nrows = n_obs, ncols = n_quantiles)
  )
  return(invisible(NULL))
}

#' @title Check input for binary forecast
#'
#' @description Helper function to check whether the input is suitable for
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
#' @importFrom checkmate assert assert_factor
#' @return Returns NULL invisibly if the check was successful and throws an
#' error otherwise.
#' @keywords internal
check_input_binary <- function(observed, predicted) {
  # things that need to be checked
  # - all inputs need to be vectors of the same length
  # - observed needs to be a factor
  # - they need to have the same levels
  # - predicted needs to be numeric between 0 and 1

  check_equal_length(observed, predicted)
  assert_factor(observed, n.levels = 2)
  levels <- levels(observed)
  assert(
    check_numeric_vector(predicted, min.len = 1, lower = 0, upper = 1)
  )
  return(invisible(NULL))
}

check_input_point <- function(observed, predicted) {
  # things that need to be checked
  # - all inputs need to be numeric vectors
  # - they need to have the same length

  assert(check_numeric_vector(observed, min.len = 1))
  assert(check_numeric_vector(predicted, min.len = 1))
  if (length(observed) != length(predicted)) {
    stop("observed and predicted need to be",
         "of same length when scoring point forecasts")
  }
  return(invisible(NULL))
}


#' @title Check whether an input is an atomic vector of mode 'numeric'
#'
#' @description Helper function
#' @param x input to check
#' @param x additional arguments to pass to `check_numeric()`
#' @importFrom checkmate check_atomic_vector check_numeric
#' @return Either TRUE if the test is successful or a string with an error
#' message
#' @keywords internal
check_numeric_vector = function(x, ...) {
  # check functions must return TRUE on success
  # and a custom error message otherwise
  numeric <- check_numeric(x, ...)
  vector <- check_atomic_vector(x)
  if (!isTRUE(numeric)) {
    return(numeric)
  } else if (!isTRUE(vector)) {
    return(vector)
  }
  return(TRUE)
}




#' @title Check Prediction Input For Lower-level Scoring Functions
#'
#' @description
#' Helper function to check inputs for lower-level score functions.
#' @param predicted an object with predictions. Depending on whether
#' `class = vector` or `class = "matrix"` this can be either a vector of length
#' n (corresponding to the length of the observed values) or a nxN matrix of
#' predictive samples, n (number of rows) being the number of data points and
#' N (number of columns) the number of Monte Carlo samples
#' @param type character, one of "continuous" (default), "integer" or "binary" that
#' defines the type of the forecast
#' @param class character, either "vector" (default) or "matrix" that determines the
#' class the input has to correspond to
#' @inheritParams ae_median_sample
#' @return NULL
#' @keywords internal

check_predicted <- function(predicted,
                              observed = NULL,
                              type = c("continuous", "integer", "binary"),
                              class = c("vector", "matrix")) {
  type <- match.arg(type)
  class <- match.arg(class)

  if (missing(predicted)) {
    stop("argument 'predicted' missing")
  }

  if (class == "vector") {
    if (!is.vector(predicted)) {
      msg <- sprintf(
        "'predicted' should be a vector. Instead `%s` was found",
        class(predicted)[1]
      )
      stop(msg)
    }
    if (!is.null(observed) && length(predicted) != length(observed)) {
      msg <- sprintf(
        "Mismatch: 'observed' has length `%s`, but 'predicted' has length `%s`.", # nolint
        length(observed), length(predicted)
      )
      stop(msg)
    }
  }

  if (class == "matrix") {
    if (!is.matrix(predicted)) {
      msg <- sprintf(
        "'predicted' should be a matrix. Instead `%s` was found",
        class(predicted[1])
      )
      stop(msg)
    }
    if (!is.null(observed) && nrow(predicted) != length(observed)) {
      msg <- sprintf(
        "Mismatch: 'observed' has length `%s`, but 'predicted' has `%s` rows.",
        length(observed), nrow(predicted)
      )
      stop(msg)
    }
  }

  if (type == "integer" &&
      isFALSE(all.equal(as.vector(predicted), as.integer(predicted)))
  ) {
    warning(
      "Prediction type should be 'integer', but some of the predicted values are",  " not integers"
    )
  }

  if (type == "binary" &&
      isFALSE(all(predicted >= 0) && all(predicted <= 1))
  ) {
    stop(
      "For a binary forecast, all predicted values should be probabilities between",
      " 0 or 1."
    )
  }

  return(NULL)
}


#' @title Check Observed Value Input For Lower-level Scoring Functions
#'
#' @description
#' Helper function to check inputs for lower-level score functions.
#' @inheritParams check_predicted
#' @return NULL
#' @keywords internal

check_observed <- function(observed,
                              type = c("continuous", "integer", "binary")) {
  type <- match.arg(type)
  if (missing(observed)) {
    stop("observed argument is missing")
  }

  if (type == "integer" &&
      isFALSE(all.equal(observed, as.integer(observed)))
  ) {
    stop("Some of the observed values are not integers")
  }

  if (type == "binary" &&
      isFALSE(all(observed %in% c(0, 1)))
  ) {
    stop("For a binary forecast, all observed values should be either 0 or 1.")
  }
}

#' @title Check Variable is not NULL
#'
#' @description
#' Check whether a certain variable is not `NULL` and return the name of that
#' variable and the function call where the variable is missing. This function
#' is a helper function that should only be called within other functions
#' @param ... The variables to check
#' @return The function returns `NULL`, but throws an error if the variable is
#' missing.
#'
#' @keywords internal
check_not_null <- function(...) {
  vars <- list(...)
  varnames <- names(vars)

  calling_function <- deparse(sys.calls()[[sys.nframe() - 1]])

  for (i in seq_along(vars)) {
    varname <- varnames[i]
    if (is.null(vars[[i]])) {
      stop(
        "variable '", varname,
        "' is `NULL` in the following function call: '",
        calling_function, "'"
      )
    }
  }
  return(invisible(NULL))
}


#' @title Check Length
#'
#' @description
#' Check whether variables all have the same length
#' @param ... The variables to check
#' @param one_allowed logical, allow arguments of length one that can be
#' recycled
#' @param call_levels_up How many levels to go up when including the function
#' call in the error message. This is useful when calling `check_equal_length()`
#' within another checking function.
#'
#' @return The function returns `NULL`, but throws an error if variable lengths
#' differ
#'
#' @keywords internal
check_equal_length <- function(...,
                               one_allowed = TRUE,
                               call_levels_up = 2) {
  vars <- list(...)
  lengths <- lengths(vars)

  lengths <- unique(lengths)

  if (one_allowed) {
    # check passes if all have length 1
    if (all(lengths == 1)) {
      return(invisible(NULL))
    }
    # ignore those where length is one for later checks, as we allow length 1
    lengths <- lengths[lengths != 1]
  }

  if (length(unique(lengths)) != 1) {
    calling_function <- deparse(sys.calls()[[sys.nframe() - call_levels_up]])

    lengths_message <- ifelse(
      one_allowed,
      "' should have the same length (or length one). Actual lengths: ",
      "' should have the same length. Actual lengths: "
      )

    stop(
      "Arguments to the following function call: '",
      calling_function,
      lengths_message,
      toString(lengths)
    )
  }
  return(invisible(NULL))
}



#' @title Check whether the desired metrics are available in scoringutils
#'
#' @description Helper function to check whether desired metrics are
#' available. If the input is `NULL`, all metrics will be returned.
#'
#' @param metrics character vector with desired metrics
#'
#' @return A character vector with metrics that can be used for downstream
#' computation
#'
#' @keywords internal

check_metrics <- function(metrics) {
  # use all available metrics if none are given
  if (is.null(metrics)) {
    metrics <- available_metrics()
  }

  # check desired metrics are actually available in scoringutils
  available_metrics <- available_metrics()
  if (!all(metrics %in% available_metrics)) {
    msg <- paste(
      "The following metrics are not available:",
      toString(setdiff(metrics, available_metrics))
    )
    warning(msg)
  }
  return(metrics)
}

#' Check that quantiles are valid
#'
#' @description
#' Helper function to check that input quantiles are valid.
#' Quantiles must be in the range specified, increase monotonically,
#' and contain no duplicates.
#'
#' This is used in [bias_range()]() and [bias_quantile()]() to
#' provide informative errors to users.
#'
#' @param quantiles Numeric vector of quantiles to check
#' @param name Character name to use in error messages
#' @param range Numeric vector giving allowed range
#'
#' @return None. Function errors if quantiles are invalid.
#'
#' @keywords internal

check_quantiles <- function(quantiles, name = "quantiles", range = c(0, 1)) {
  if (any(quantiles < range[1]) || any(quantiles > range[2])) {
    stop(name, " must be between ", range[1], " and ", range[2])
  }

  if (!all(diff(quantiles) > 0)) {
    stop(name, " must be increasing")
  }
}

#' @title Check Prediction Input For Lower-level Scoring Functions
#'
#' @description
#' Helper function to check inputs for lower-level score functions.
#' @param predictions an object with predictions. Depending on whether
#' `class = vector` or `class = "matrix"` this can be either a vector of length
#' n (corresponding to the length of the true_values) or a nxN matrix of
#' predictive samples, n (number of rows) being the number of data points and
#' N (number of columns) the number of Monte Carlo samples
#' @param type character, one of "continuous" (default), "integer" or "binary" that
#' defines the type of the forecast
#' @param class character, either "vector" (default) or "matrix" that determines the
#' class the input has to correspond to
#' @inheritParams ae_median_sample
#' @return NULL
#' @keywords internal

check_predictions <- function(predictions,
                              true_values = NULL,
                              type = c("continuous", "integer", "binary"),
                              class = c("vector", "matrix")) {
  type <- match.arg(type)
  class <- match.arg(class)

  if (missing(predictions)) {
    stop("argument 'predictions' missing")
  }

  if (class == "vector") {
    if (!is.vector(predictions)) {
      msg <- sprintf(
        "'predictions' should be a vector. Instead `%s` was found",
        class(predictions)[1]
      )
      stop(msg)
    }
    if (!is.null(true_values) && length(predictions) != length(true_values)) {
      msg <- sprintf(
        "Mismatch: 'true_values' has length `%s`, but 'predictions' has length `%s`.", # nolint
        length(true_values), length(predictions)
      )
      stop(msg)
    }
  }

  if (class == "matrix") {
    if (!is.matrix(predictions)) {
      msg <- sprintf(
        "'predictions' should be a matrix. Instead `%s` was found",
        class(predictions[1])
      )
      stop(msg)
    }
    if (!is.null(true_values) && nrow(predictions) != length(true_values)) {
      msg <- sprintf(
        "Mismatch: 'true_values' has length `%s`, but 'predictions' has `%s` rows.",
        length(true_values), nrow(predictions)
      )
      stop(msg)
    }
  }

  if (type == "integer") {
    if (isFALSE(all.equal(predictions, as.integer(predictions)))) {
      warning("Prediction type should be 'integer', but some of the predictions are not integers")
    }
  }

  if (type == "binary") {
    if (isFALSE(all(predictions >= 0) & all(predictions <= 1))) {
      stop("For a binary forecast, all predictions should be probabilities between 0 or 1.")
    }
  }

  return(NULL)
}


#' @title Check Observed Value Input For Lower-level Scoring Functions
#'
#' @description
#' Helper function to check inputs for lower-level score functions.
#' @inheritParams check_predictions
#' @return NULL
#' @keywords internal

check_true_values <- function(true_values,
                              type = c("continuous", "integer", "binary")) {
  type <- match.arg(type)
  if (missing(true_values)) {
    stop("true_values argument is missing")
  }

  if (type == "integer") {
    if (isFALSE(all.equal(true_values, as.integer(true_values)))) {
      stop("Some of the true_values are not integers")
    }
  }

  if (type == "binary") {
    if (isFALSE(all(true_values %in% c(0, 1)))) {
      stop("For a binary forecast, all true_values should be either 0 or 1.")
    }
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

  calling_function <- deparse1(sys.calls()[[sys.nframe() - 1]])

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
#'
#' @return The function returns `NULL`, but throws an error if variable lengths
#' differ
#'
#' @keywords internal
check_equal_length <- function(...,
                               one_allowed = TRUE) {
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
    calling_function <- deparse1(sys.calls()[[sys.nframe() - 1]])
    stop(paste0(
      "Arguments passed to the following function call: '",
      calling_function,
      "' should have the same length (or length one). Arguments have the following lengths: ",
      paste0(lengths, collapse = ", ")
    ))
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
      paste(setdiff(metrics, available_metrics), collapse = ", ")
    )
    warning(msg)
  }
  return(metrics)
}

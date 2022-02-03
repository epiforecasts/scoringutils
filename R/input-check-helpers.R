#' @title Check Prediction Input For Lower-level Scoring Functions
#'
#' @description
#' Helper function to check inputs for lower-level score functions.
#' @param true_values A vector with the true observed values of size n
#' @param predictions an object with predictions. Depending on whether
#' `class = vector` or `class = "matrix"` this can be either a vector of length
#' n (corresponding to the length of the true_values) or a nxN matrix of
#' predictive samples, n (number of rows) being the number of data points and
#' N (number of columns) the number of Monte Carlo samples
#' @param type character, one of "continuous", "integer" or "binary" that
#' defines the type of the forecast
#' @param class character, either "vector" or "matrix" that determines the
#' class the input has to correspond to
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
        class(predictions[1])
      )
      stop(msg)
    }
    if (!is.null(true_values) && length(predictions) != length(true_values)) {
      msg <- sprintf(
        "Mismatch: 'true_values' has length `%s`, but 'predictions' has length `%s`.",
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

  for (i in seq_len(vars)) {
    varname <- varnames[i]
    if (is.null(vars[[i]])) {
      calling_function <- deparse1(sys.calls()[[sys.nframe() - 1]])
      stop(paste0(
        "variable '", varname,
        "' is `NULL` in the following function call: '",
        calling_function, "'"
      ))
    }
  }
  return(invisible(NULL))
}


#' @title Check Length
#'
#' @description
#' Check whether variables all have the same length
#' @param ... The variables to check
#' @param one_allowed logical, allow arguments of length one that can be recycled
#'
#' @return The function returns `NULL`, but throws an error if variable lengths
#' differ
#'
#' @keywords internal
check_equal_length <- function(...,
                               one_allowed = TRUE) {
  vars <- list(...)
  lengths <- sapply(vars,
    FUN = function(x) {
      length(x)
    }
  )

  lengths <- unique(lengths)

  if (one_allowed) {
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


#' @title Clean forecast data
#'
#' @description Helper function to check that the input is in fact a data.frame
#' or similar and remove rows with no value for `prediction` or `true_value`
#'
#' @param data A data.frame or similar as it gets passed to [score()].
#' @param verbose Boolean (default is `TRUE`), whether or not to print warnings
#'
#' @return A data.table with NA values in `true_value` or `prediction` removed.
#'
#' @importFrom data.table as.data.table
#'
#' @keywords internal

check_clean_data <- function(data, verbose = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input should be a data.frame or similar")
  }
  data <- data.table::as.data.table(data)

  # make sure necessary columns are present
  if (!all(c("true_value", "prediction") %in% colnames(data))) {
    stop("Data needs to have columns called `true_value` and `prediction`")
  }

  if (any(colnames(data) %in% available_metrics())) {
    warning(
      "At least one column in the data corresponds to the name of a ",
      "metric that will be computed by scoringutils. This may be a ",
      "problem. Please check `available_metrics()`"
    )
  }

  # remove rows where prediction or true value are NA
  if (anyNA(data$true_value)) {
    if (verbose) {
      warning("Some values for `true_value` are NA in the data provided")
    }
  }
  data <- data[!is.na(true_value)]

  if (anyNA(data$prediction)) {
    if (verbose) {
      warning("Some values for `prediction` are NA in the data provided")
    }
  }
  data <- data[!is.na(prediction)]
  if (nrow(data) == 0) {
    stop("After removing all NA true values and predictions, there were no observations left")
  }
  return(data)
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

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
                              true_values,
                              type = c("continuous", "integer", "binary"),
                              class = c("vector", "matrix")) {

  type <- match.arg(type)
  class <- match.arg(class)

  if (missing(true_values) | missing(predictions)) {
    stop("true_values or predictions argument missing")
  }

  n <- length(true_values)

  if (class == "vector") {
    if (!is.vector(predictions)) {
      msg <- sprintf(
        "'predictions' should be a vector. Instead `%s` was found",
        class(predictions[1])
      )
      stop(msg)
    }
    if (length(predictions) != n) {
      msg <- sprintf(
        "Mismatch: 'true_values' has length `%s`, but 'predictions' has length `%s`.",
        n, length(predictions)
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
    if (nrow(predictions) != n) {
      msg <- sprintf(
        "Mismatch: 'true_values' has length `%s`, but 'predictions' has `%s` rows.",
        n, nrow(predictions)
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

#' @title Assert that inputs are correct for nominal forecasts
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring nominal forecasts.
#' @param observed Input to be checked. Should be an unordered factor of length
#'   n with N levels holding the observed values. n is the number of
#'   observations and N is the number of possible outcomes the observed values
#'   can assume.
#' @param predicted_label Unordered factor of length N with N levels, where N
#'   is the number of possible outcomes the observed values can assume.
#' @inheritParams assert_input_categorical
#' @importFrom checkmate assert_factor assert_numeric assert_set_equal
#' @inherit document_assert_functions return
#' @keywords internal_input_check
assert_input_nominal <- function(observed, predicted, predicted_label) {
  assert_input_categorical(
    observed, predicted, predicted_label, ordered = FALSE
  )
  return(invisible(NULL))
}


#' @title Assert that inputs are correct for categorical forecasts
#' @description Function assesses whether the inputs correspond to the
#' requirements for scoring categorical, i.e. either nominal or ordinal
#' forecasts.
#' @param observed Input to be checked. Should be a factor of length n with
#'   N levels holding the observed values. n is the number of observations and
#'   N is the number of possible outcomes the observed values can assume.
#' @param predicted Input to be checked. Should be nxN matrix of predicted
#'   probabilities, n (number of rows) being the number of data points and N
#'   (number of columns) the number of possible outcomes the observed values
#'   can assume.
#'   If `observed` is just a single number, then predicted can just be a
#'   vector of size N.
#'   Values represent the probability that the corresponding value
#'   in `observed` will be equal to the factor level referenced in
#'   `predicted_label`.
#' @param predicted_label Factor of length N with N levels, where N is the
#'   number of possible outcomes the observed values can assume.
#' @param ordered Value indicating whether factors have to be ordered or not.
#'   Defaults to `NA`, which means that the check is not performed.
#' @importFrom checkmate assert_factor assert_numeric assert_set_equal
#' @inherit document_assert_functions return
#' @keywords internal_input_check
assert_input_categorical <- function(
  observed, predicted, predicted_label, ordered = NA
) {
  # observed
  assert_factor(observed, min.len = 1, min.levels = 2, ordered = ordered)
  levels <- levels(observed)
  n <- length(observed)
  N <- length(levels)

  # predicted label
  assert_factor(
    predicted_label, len = N,
    any.missing = FALSE, empty.levels.ok = FALSE,
    ordered = ordered
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
  # Allow for numeric errors
  invalid_rows <- abs(summed_predictions - 1) > 1e-4
  if (any(invalid_rows)) {
    #nolint start: keyword_quote_linter object_usage_linter
    row_indices <- which(invalid_rows)
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


#' Log score for categorical outcomes
#'
#' @description
#' **Log score for categorical (nominal or ordinal) outcomes**
#'
#' The Log Score is the negative logarithm of the probability
#' assigned to the observed value. It is a proper scoring rule. Small values
#' are better (best is zero, worst is infinity).
#'
#' @param observed Factor of length n with N levels holding the
#'   observed values.
#' @param predicted nxN matrix of predictive probabilities, n (number of rows)
#'   being the number of observations and N (number of columns) the number of
#'   possible outcomes.
#' @param predicted_label Factor of length N, denoting the outcome
#'   that the probabilities in `predicted` correspond to.
#' @returns A numeric vector of size n with log scores
#' @inheritSection illustration-input-metric-nominal Input format
#' @importFrom methods hasArg
#' @export
#' @keywords metric
#' @rdname scoring-functions-nominal
#' @family log score functions
#' @examples
#' factor_levels <- c("one", "two", "three")
#' predicted_label <- factor(c("one", "two", "three"), levels = factor_levels)
#' observed <- factor(c("one", "three", "two"), levels = factor_levels)
#' predicted <- matrix(
#'   c(0.8, 0.1, 0.1,
#'     0.1, 0.2, 0.7,
#'     0.4, 0.4, 0.2),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#' logs_categorical(observed, predicted, predicted_label)
logs_categorical <- function(observed, predicted, predicted_label) {
  assert_input_categorical(observed, predicted, predicted_label)
  n <- length(observed)
  if (n == 1) {
    predicted <- matrix(predicted, nrow = 1)
  }
  observed_indices <- as.numeric(observed)
  pred_for_observed <- predicted[cbind(1:n, observed_indices)]
  logs <- -log(pred_for_observed)
  return(logs)
}

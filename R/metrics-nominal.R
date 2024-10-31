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


#' Log score for nominal outcomes
#'
#' @description
#' **Log score for nominal outcomes**
#'
#' The Log Score is the negative logarithm of the probability
#' assigned to the observed value. It is a proper scoring rule. Small values
#' are better (best is zero, worst is infinity).
#' @param observed A factor of length n with N levels holding the observed
#'   values.
#' @param predicted nxN matrix of predictive probabilities, n (number of rows)
#'   being the number of observations and N (number of columns) the number of
#'   possible outcomes.
#' @param predicted_label A factor of length N, denoting the outcome that the
#'   probabilities in `predicted` correspond to.
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
#' predicted <- matrix(c(0.8, 0.1, 0.4,
#'                       0.1, 0.2, 0.4,
#'                       0.1, 0.7, 0.2),
#'                     nrow = 3)
#' logs_nominal(observed, predicted, predicted_label)
logs_nominal <- function(observed, predicted, predicted_label) {
  assert_input_nominal(observed, predicted, predicted_label)
  n <- length(observed)
  if (n == 1) {
    predicted <- matrix(predicted, nrow = 1)
  }
  observed_indices <- as.numeric(observed)
  pred_for_observed <- predicted[cbind(1:n, observed_indices)]
  logs <- -log(pred_for_observed)
  return(logs)
}

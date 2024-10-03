#' @title Run a function safely
#' @description
#' This is a wrapper function designed to run a function safely
#' when it is not completely clear what arguments could be passed to the
#' function.
#'
#' All named arguments in `...` that are not accepted by `fun` are removed.
#' All unnamed arguments are passed on to the function. In case `fun` errors,
#' the error will be converted to a warning and `run_safely` returns `NULL`.
#'
#' `run_safely` can be useful when constructing functions to be used as
#' metrics in [score()].
#'
#' @param ... Arguments to pass to `fun`.
#' @param fun A function to execute.
#' @param metric_name A character string with the name of the metric. Used to
#'   provide a more informative warning message in case `fun` errors.
#' @importFrom cli cli_warn
#' @importFrom checkmate assert_function
#' @return The result of `fun` or `NULL` if `fun` errors
#' @keywords internal
#' @examples
#' f <- function(x) {x}
#' scoringutils:::run_safely(2, fun = f, metric_name = "f")
#' scoringutils:::run_safely(2, y = 3, fun = f, metric_name = "f")
#' scoringutils:::run_safely(fun = f, metric_name = "f")
#' scoringutils:::run_safely(y = 3, fun = f, metric_name = "f")
run_safely <- function(..., fun, metric_name) {
  assert_function(fun)
  args <- list(...)
  # Check if the function accepts ... as an argument
  if ("..." %in% names(formals(fun))) {
    valid_args <- args
  } else if (is.null(names(args))) {
    # if no arguments are named, just pass all arguments on
    valid_args <- args
  } else {
    # Identify the arguments that fun() accepts
    possible_args <- names(formals(fun))
    # keep valid arguments as well as unnamed arguments
    valid_args <- args[names(args) == "" | names(args) %in% possible_args]
  }

  result <- try(do.call(fun, valid_args), silent = TRUE)

  if (inherits(result, "try-error")) {
    #nolint start: object_usage_linter
    msg <- conditionMessage(attr(result, "condition"))
    cli_warn(
      c(
        "!" = "Computation for {.var {metric_name}} failed.
        Error: {msg}."
      )
    )
    #nolint end
    return(NULL)
  }
  return(result)
}


#' Ensure that an object is a `data.table`
#' @description
#' This function ensures that an object is a `data table`.
#' If the object is not a data table, it is converted to one. If the object
#' is a data table, a copy of the object is returned.
#' @param data An object to ensure is a data table.
#' @return A data.table/a copy of an existing data.table.
#' @keywords internal
#' @importFrom data.table copy is.data.table as.data.table
ensure_data.table <- function(data) {
  if (is.data.table(data)) {
    data <- copy(data)
  } else {
    data <- as.data.table(data)
  }
  return(data)
}


#' @title Check whether an input is an atomic vector of mode 'numeric'
#'
#' @description Helper function to check whether an input is a numeric vector.
#' @param x input to check
#' @inheritDotParams checkmate::check_numeric
#' @importFrom checkmate check_atomic_vector check_numeric
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_numeric_vector <- function(x, ...) {
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


#' Get forecast type from forecast object
#' @inheritParams score
#' @return
#' Character vector of length one with the forecast type.
#' @keywords internal_input_check
get_forecast_type <- function(forecast) {
  classname <- class(forecast)[1]
  if (grepl("forecast_", classname, fixed = TRUE)) {
    type <- gsub("forecast_", "", classname, fixed = TRUE)
    return(type)
  } else {
    cli_abort(
      "Input is not a valid forecast object
      (it's first class should begin with `forecast_`)."
    )
  }
}


#' Assert that forecast type is as expected
#' @param data A forecast object (see [as_forecast()]).
#' @param actual The actual forecast type of the data
#' @param desired The desired forecast type of the data
#' @inherit document_assert_functions return
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_character
#' @keywords internal_input_check
assert_forecast_type <- function(data,
                                 actual = get_forecast_type(data),
                                 desired = NULL) {
  assert_character(desired, null.ok = TRUE)
  if (!is.null(desired) && desired != actual) {
    #nolint start: object_usage_linter keyword_quote_linter
    cli_abort(
      c(
        "!" = "Forecast type determined by scoringutils based on input:
        {.val {actual}}.",
        "i" = "Desired forecast type: {.val {desired}}."
      )
    )
    #nolint end
  }
  return(invisible(NULL))
}


#' @title Get type of a vector or matrix of observed values or predictions
#'
#' @description
#' Internal helper function to get the type of a vector (usually
#' of observed or predicted values). The function checks whether the input is
#' a factor, or else whether it is integer (or can be coerced to integer) or
#' whether it's continuous.
#' @param x Input the type should be determined for.
#' @importFrom cli cli_abort
#' @return
#' Character vector of length one with either "classification",
#' "integer", or "continuous".
#' @keywords internal_input_check
get_type <- function(x) {
  if (is.factor(x)) {
    return("classification")
  }
  assert_numeric(as.vector(x))
  if (all(is.na(as.vector(x)))) {
    cli_abort("Can't get type: all values of are {.val NA}.")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (
    isTRUE(all.equal(as.vector(x), as.integer(x))) && !all(is.na(as.integer(x)))
  ) {
    return("integer")
  } else {
    return("continuous")
  }
}


#' @title Get protected columns from data
#'
#' @description Helper function to get the names of all columns in a data frame
#' that are protected columns.
#'
#' @inheritParams as_forecast
#'
#' @return
#' A character vector with the names of protected columns in the data.
#' If data is `NULL` (default) then it returns a list of all columns that are
#' protected in scoringutils.
#'
#' @keywords internal
get_protected_columns <- function(data = NULL) {

  protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value", "interval_range", "boundary", "predicted_label",
    "interval_coverage", "interval_coverage_deviation",
    "quantile_coverage", "quantile_coverage_deviation",
    grep("_relative_skill$", names(data), value = TRUE),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )

  if (is.null(data)) {
    return(protected_columns)
  }

  # only return protected columns that are present
  datacols <- colnames(data)
  protected_columns <- intersect(
    datacols,
    protected_columns
  )

  return(protected_columns)
}

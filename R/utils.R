#' @title Available metrics in scoringutils
#'
#' @return A vector with the name of all available metrics
#' @export
#' @keywords info
available_metrics <- function() {
  return(unique(c(scoringutils::metrics$Name,
                  "wis", "interval_coverage_50", "interval_coverage_90",
                  "interval_coverage_deviation")))
}


#' @title Collapse several messages to one
#'
#' @description Internal helper function to facilitate generating messages
#' and warnings.
#'
#' @param type character, should be either "messages", "warnings" or "errors"
#' @param messages the messages or warnings to collapse
#'
#' @return string with the message or warning
#' @keywords internal
collapse_messages <- function(type = "messages", messages) {
  paste0(
    "The following ",  type, " were produced when checking inputs:\n",
    paste(paste0(seq_along(messages), ". "), messages, collapse = "\n")
  )
}


#' @title Filter function arguments
#'
#' @description This function compares a list of arguments with the arguments
#' that a function can accept. It only returns those arguments that can be
#' passed to the function.
#'
#' The function is used in [score()] to handle additional arguments passed to
#' [score()] that get then passed along to the different scoring functions.
#'
#' @param fun A function to which arguments shall be passed
#' @param args A list of arguments that shall be passed to fun
#'
#' @return A list of function arguments (a subset of `args`) that `fun` can
#' accept.
#' @keywords internal
filter_function_args <- function(fun, args) {
  # Check if the function accepts ... as an argument
  if ("..." %in% names(formals(fun))) {
    # If it does, return all arguments
    return(args)
  } else {
    # Identify the arguments that fun() accepts and only keep valid ones
    valid_args <- names(formals(fun))
    return(args[names(args) %in% valid_args])
  }
}


#' @title Run a function safely
#' @description This is a wrapper function designed to run a function safely
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
#' @param ... Arguments to pass to `fun`
#' @param fun A function to execute
#' @importFrom cli cli_warn
#' @return The result of `fun` or `NULL` if `fun` errors
#' @export
#' @keywords scoring
#' @examples
#' f <- function(x) {x}
#' run_safely(2, fun = f)
#' run_safely(2, y = 3, fun = f)
#' run_safely(fun = f)
#' run_safely(y = 3, fun = f)
run_safely <- function(..., fun) {
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
      "Function execution failed, returning NULL.
      Error: {msg}."
    )
    #nolint end
    return(NULL)
  }
  return(result)
}


#' Ensure That an Object is a Data Table
#' @description This function ensures that an object is a data table.
#' If the object is not a data table, it is converted to one. If the object
#' is a data table, a copy of the object is returned.
#' @param data An object to ensure is a data table
#' @return A data table
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

#' @title Print Information About A Forecast Object
#' @description This function prints information about a forecast object,
#' including "Forecast type", "Score columns",
#' "Forecast unit".
#'
#' @param x An object of class 'forecast_*' object as produced by
#' `as_forecast()`
#' @param ... additional arguments for [print()]
#' @importFrom cli cli_inform cli_warn col_blue cli_text
#' @return NULL
#' @export
#' @keywords check-forecasts
#' @examples
#' dat <- as_forecast(example_quantile)
#' print(dat)
print.forecast_binary <- function(x, ...) {

  # check whether object passes validation
  validation <- try(do.call(validate_forecast, list(data = x)), silent = TRUE)
  if (inherits(validation, "try-error")) {
    cli_warn(
      "Error in validating forecast object: {validation}."
    )
  }

  # get forecast type, forecast unit and score columns
  forecast_type <- try(
    do.call(get_forecast_type, list(data = x)),
    silent = TRUE
  )
  forecast_unit <- get_forecast_unit(x)
  score_cols <- get_score_names(x)

  # Print forecast object information
  if (inherits(forecast_type, "try-error")) {
    cli_inform(
      "Could not determine forecast type due to error in validation."
    )
  } else {
    cli_text(
      col_blue(
        "Forecast type:"
      )
    )
    cli_text(
      "{forecast_type}"
    )
  }

  if (length(forecast_unit) == 0) {
    cli_inform(
      "Could not determine forecast unit."
    )
  } else {
    cli_text(
      col_blue(
        "Forecast unit:"
      )
    )
    cli_text(
      "{forecast_unit}"
    )
  }

  if (!is.null(score_cols)) {
    cli_text(
      col_blue(
        "Score columns:"
      )
    )
    cli_text(
      "{score_cols}"
    )
  }

  cat("\n")
  NextMethod(x, ...)

  return(invisible(x))
}

#' @rdname print.forecast_binary
#' @export
print.forecast_quantile <- print.forecast_binary

#' @rdname print.forecast_binary
#' @export
print.forecast_point <- print.forecast_binary

#' @rdname print.forecast_binary
#' @export
print.forecast_sample <- print.forecast_binary

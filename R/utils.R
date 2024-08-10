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

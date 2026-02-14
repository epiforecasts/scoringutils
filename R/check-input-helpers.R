#' Ensure that an object is a `data.table`
#' @description
#' This function ensures that an object is a `data table`.
#' If the object is not a data table, it is converted to one. If the object
#' is a data table, a copy of the object is returned.
#' @param data An object to ensure is a data table.
#' @returns A data.table/a copy of an existing data.table.
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


# ==============================================================================
# functinos below will be deleted in the future

#' @title Helper function to convert assert statements into checks
#'
#' @description
#' Tries to execute an expression. Internally, this is used to
#' see whether assertions fail when checking inputs (i.e. to convert an
#' `assert_*()` statement into a check). If the expression fails, the error
#' message is returned. If the expression succeeds, `TRUE` is returned.
#' @param expr an expression to be evaluated
#' @importFrom checkmate assert assert_numeric check_matrix
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_try <- function(expr) {
  result <- try(expr, silent = TRUE)
  if (is.null(result)) {
    return(TRUE)
  }
  msg <- conditionMessage(attr(result, "condition"))
  return(msg)
}


#' Test whether column names are NOT present in a data.frame
#' @description The function checks whether all column names are NOT present.
#' If none of the columns are present, the function returns TRUE. If one or
#' more columns are present, the function returns FALSE.
#' @inheritParams document_check_functions
#' @returns Returns TRUE if none of the columns are present and FALSE otherwise
#' @importFrom checkmate test_names
#' @keywords internal_input_check
test_columns_not_present <- function(data, columns) {
  test_names(colnames(data), disjunct.from = columns)
}

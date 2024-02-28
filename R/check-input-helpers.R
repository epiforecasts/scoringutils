#' @title Check whether an input is an atomic vector of mode 'numeric'
#'
#' @description Helper function
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


#' Check that quantiles are valid
#'
#' @description
#' Helper function to check that input quantiles are valid.
#' Quantiles must be in the range specified, increase monotonically,
#' and contain no duplicates.
#'
#' This is used in [bias_interval()]() and [bias_quantile()]() to
#' provide informative errors to users.
#'
#' @param quantiles Numeric vector of quantiles to check
#' @param name Character name to use in error messages
#' @param range Numeric vector giving allowed range
#'
#' @return None. Function errors if quantiles are invalid.
#'
#' @keywords internal_input_check
check_quantiles <- function(quantiles, name = "quantiles", range = c(0, 1)) {
  if (any(quantiles < range[1]) || any(quantiles > range[2])) {
    stop(name, " must be between ", range[1], " and ", range[2])
  }

  if (!all(diff(quantiles) > 0)) {
    stop(name, " must be increasing")
  }
}


#' @title Helper function to convert assert statements into checks
#'
#' @description Tries to execute an expression. Internally, this is used to
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


#' @title Check Variable is not NULL
#'
#' @description
#' Check whether a certain variable is not `NULL` and return the name of that
#' variable and the function call where the variable is missing. This function
#' is a helper function that should only be called within other functions
#' @param ... The variables to check
#' @inherit document_assert_functions return
#' @return The function returns `NULL`, but throws an error if the variable is
#' missing.
#'
#' @keywords internal_input_check
assert_not_null <- function(...) {
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


#' @title Check Whether There Is a Conflict Between Data and Attributes
#' @description
#' Check whether there is a conflict between a stored attribute and the
#' same value as inferred from the data. For example, this could be if
#' an attribute `forecast_unit` is stored, but is different from the
#' `forecast_unit` inferred from the data. The check is successful if
#' the stored and the inferred value are the same.
#' @param object The object to check
#' @param attribute The name of the attribute to check
#' @param expected The expected value of the attribute
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_attribute_conflict <- function(object, attribute, expected) {
  existing <- attr(object, attribute)
  if (is.vector(existing) && is.vector(expected)) {
    existing <- sort(existing)
    expected <- sort(expected)
  }

  if (!is.null(existing) && !identical(existing, expected)) {
    msg <- paste0(
      "Object has an attribute `", attribute, "`, but it looks different ",
      "from what's expected based on the data.\n",
      "Existing: ", toString(existing), "\n",
      "Expected: ", toString(expected), "\n",
      "Running `as_forecast()` again might solve the problem"
    )
    return(msg)
  }
  return(TRUE)
}


#' @title Assure that Data Has a `model` Column
#'
#' @description
#' Check whether the data.table has a column called `model`.
#' If not, a column called `model` is added with the value `Unspecified model`.
#' @inheritParams score
#' @return The data.table with a column called `model`
#' @keywords internal_input_check
assure_model_column <- function(data) {
  if (!("model" %in% colnames(data))) {
    message(
      "There is no column called `model` in the data.",
      "scoringutils assumes that all forecasts come from the same model" # nolint
    )
    data[, model := "Unspecified model"]
  }
  return(data[])
}


#' Check that all forecasts have the same number of quantiles or samples
#' @description Function checks the number of quantiles or samples per forecast.
#' If the number of quantiles or samples is the same for all forecasts, it
#' returns TRUE and a string with an error message otherwise.
#' @param forecast_unit Character vector denoting the unit of a single forecast.
#' @inherit document_check_functions params return
#' @keywords internal_input_check
check_number_per_forecast <- function(data, forecast_unit) {
  data <- na.omit(data)
  # check whether there are the same number of quantiles, samples --------------
  data[, scoringutils_InternalNumCheck := length(predicted), by = forecast_unit]
  n <- unique(data$scoringutils_InternalNumCheck)
  data[, scoringutils_InternalNumCheck := NULL]
  if (length(n) > 1) {
    msg <- paste0(
      "Some forecasts have different numbers of rows ",
      "(e.g. quantiles or samples). ",
      "scoringutils found: ", toString(n),
      ". This may be a problem (it can potentially distort scores, ",
      "making it more difficult to compare them), ",
      "so make sure this is intended."
    )
    return(msg)
  }
  return(TRUE)
}


#' Check columns in data.frame don't have NA values
#' @description Function checks whether any of the columns in a data.frame,
#' as specified in `columns`, have NA values. If so, it returns a string with
#' an error message, otherwise it returns TRUE.
#' @inherit document_check_functions params return
#'
#' @keywords internal_input_check
check_no_NA_present <- function(data, columns) {
  for (x in columns){
    if (anyNA(data[[x]])) {
      msg <- paste0(
        "Checking `data`: ",
        sum(is.na(data[[x]])),
        " values in column `",
        x,
        "`` are NA and corresponding rows will be removed. This is fine if not unexpected." # nolint
      )
      return(msg)
    }
  }
  return(TRUE)
}


#' Check that there are no duplicate forecasts
#'
#' @description
#' Runs [get_duplicate_forecasts()] and returns a message if an issue is encountered
#' @inheritParams get_duplicate_forecasts
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_duplicates <- function(data, forecast_unit = NULL) {
  check_duplicates <- get_duplicate_forecasts(data, forecast_unit = forecast_unit)

  if (nrow(check_duplicates) > 0) {
    msg <- paste0(
      "There are instances with more than one forecast for the same target. ",
      "This can't be right and needs to be resolved. Maybe you need to ",
      "check the unit of a single forecast and add missing columns? Use ",
      "the function get_duplicate_forecasts() to identify duplicate rows"
    )
    return(msg)
  }
  return(TRUE)
}


#' Check column names are present in a data.frame
#' @description
#' The functions loops over the column names and checks whether they are
#' present. If an issue is encountered, the function immediately stops
#' and returns a message with the first issue encountered.
#' @inherit document_check_functions params return
#' @importFrom checkmate assert_character
#' @keywords internal_input_check
check_columns_present <- function(data, columns) {
  if (is.null(columns)) {
    return(TRUE)
  }
  assert_character(columns, min.len = 1)
  colnames <- colnames(data)
  missing <- list()
  for (x in columns){
    if (!(x %in% colnames)) {
      missing[[x]] <- x
    }
  }
  missing <- unlist(missing)
  if (length(missing) > 1) {
    msg <- paste0(
      "Columns '", paste(missing, collapse = "', '"), "' not found in data"
    )
    return(msg)
  } else if (length(missing) == 1) {
    msg <- paste0("Column '", missing, "' not found in data")
    return(msg)
  }
  return(TRUE)
}

#' Test whether all column names are present in a data.frame
#' @description The function checks whether all column names are present. If
#' one or more columns are missing, the function returns FALSE. If all columns
#' are present, the function returns TRUE.
#' @inheritParams document_check_functions
#' @return Returns TRUE if all columns are present and FALSE otherwise
#' @keywords internal_input_check
test_columns_present <- function(data, columns) {
  check <- check_columns_present(data, columns)
  return(is.logical(check))
}

#' Test whether column names are NOT present in a data.frame
#' @description The function checks whether all column names are NOT present.
#' If none of the columns are present, the function returns TRUE. If one or
#' more columns are present, the function returns FALSE.
#' @inheritParams document_check_functions
#' @return Returns TRUE if none of the columns are present and FALSE otherwise
#' @keywords internal_input_check
test_columns_not_present <- function(data, columns) {
  if (any(columns %in% colnames(data))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Check whether data is data.frame with correct columns
#' @description Checks whether data is a data.frame, whether columns
#' "observed" and "predicted" are present, and checks that only one of
#' "quantile_level" and "sample_id" is present.
#' @inherit document_check_functions params return
#' @importFrom checkmate check_data_frame
#' @keywords internal_input_check
check_data_columns <- function(data) {
  is_data <- check_data_frame(data, min.rows = 1)
  if (!is.logical(is_data)) {
    return(is_data)
  }
  needed <- test_columns_present(data, c("observed", "predicted"))
  if (!needed) {
    return("Both columns `observed` and predicted` are needed")
  }
  problem <- test_columns_present(data, c("sample_id", "quantile_level"))
  if (problem) {
    return(
      "Found columns `quantile_level` and `sample_id`. Only one of these is allowed"
    )
  }
  return(TRUE)
}


#' Check whether an attribute is present
#' @description Checks whether an object has an attribute
#' @param object An object to be checked
#' @param attribute name of an attribute to be checked
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_has_attribute <- function(object, attribute) {
  if (is.null(attr(object, attribute))) {
    return(
      paste0("Found no attribute `", attribute, "`")
    )
  } else {
    return(TRUE)
  }
}

#' @title Check whether an input is an atomic vector of mode 'numeric'
#'
#' @description Helper function
#' @param x input to check
#' @param ... additional arguments to pass to `check_numeric()`
#' @importFrom checkmate check_atomic_vector check_numeric
#' @return Either TRUE if the test is successful or a string with an error
#' message
#' @keywords internal
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
#' This is used in [bias_range()]() and [bias_quantile()]() to
#' provide informative errors to users.
#'
#' @param quantiles Numeric vector of quantiles to check
#' @param name Character name to use in error messages
#' @param range Numeric vector giving allowed range
#'
#' @return None. Function errors if quantiles are invalid.
#'
#' @keywords internal

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
#' see whether assertions fail when checking inputs
#' @param expr an expression to be evaluated
#' @importFrom checkmate assert assert_numeric check_matrix
#' @return Returns TRUE if expression was executed successfully, otherwise
#' returns a string with the resulting error message
#' @keywords internal

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
#' @return The function returns `NULL`, but throws an error if the variable is
#' missing.
#'
#' @keywords internal
check_not_null <- function(...) {
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


#' @title Check Length
#'
#' @description
#' Check whether variables all have the same length
#' @param ... The variables to check
#' @param one_allowed logical, allow arguments of length one that can be
#' recycled
#' @param call_levels_up How many levels to go up when including the function
#' call in the error message. This is useful when calling `check_equal_length()`
#' within another checking function.
#'
#' @return The function returns `NULL`, but throws an error if variable lengths
#' differ
#'
#' @keywords internal
check_equal_length <- function(...,
                               one_allowed = TRUE,
                               call_levels_up = 2) {
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
    calling_function <- deparse(sys.calls()[[sys.nframe() - call_levels_up]])

    lengths_message <- ifelse(
      one_allowed,
      "' should have the same length (or length one). Actual lengths: ",
      "' should have the same length. Actual lengths: "
    )

    stop(
      "Arguments to the following function call: '",
      calling_function,
      lengths_message,
      toString(lengths)
    )
  }
  return(invisible(NULL))
}


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
      "Running `validate()` again might solve the problem"
    )
    return(msg)
  }
  return(TRUE)
}

toString

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
#' @inheritParams check_data_doc_template
#' @param forecast_unit Character vector denoting the unit of a single forecast.
#' @return Returns an string with a message if any forecasts have differing
#' numbers of samples or quantiles, otherwise returns TRUE
#'
#' @keywords internal
check_number_per_forecast <- function(data, forecast_unit) {
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
#' @inheritParams check_columns_present
#' @return Returns an string with a message if any of the column names
#' have NA values, otherwise returns TRUE
#'
#' @keywords internal
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




# print stuff
diagnose <- function(data) {

}

#' Check that there are no duplicate forecasts
#'
#' @description
#' Runs [get_duplicate_forecasts()] and returns a message if an issue is encountered
#' @inheritParams get_duplicate_forecasts
#' @return Returns an string with an error message if an issue is found,
#' otherwise returns TRUE
#'
#' @keywords internal
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


# Function to check input for methods
# there should not be a name clash between a metric and a column name
#    --> maybe this should be checked by the actual method that computes scores
# check whether any column name is a scoringutils metric
# clashing_colnames <- intersect(colnames(data), available_metrics())
# if (length(clashing_colnames) > 0) {
#   clashing_colnames <- paste0('"', clashing_colnames, '"')
#   warnings <- c(
#     warnings,
#     paste0(
#       "At least one column in the data ",
#       "(", toString(clashing_colnames), ") ",
#       "corresponds to the name of a metric that will be computed by ",
#       "scoringutils. Please check `available_metrics()`"
#     )
#   )
# }


#' Check column names are present in a data.frame
#' @inheritParams check_data_doc_template
#' @return Returns string with a message with the first issue encountered if
#'  any of the column names are not in data, otherwise returns TRUE
#' @importFrom checkmate assert_character
#' @keywords check-inputs
check_columns_present <- function(data, columns) {
  if (is.null(columns)) {
    return(TRUE)
  }
  assert_character(columns, min.len = 1)
  colnames <- colnames(data)
  for (x in columns){
    if (!(x %in% colnames)) {
      msg <- paste0("Column '", x, "' not found in data")
      return(msg)
    }
  }
  return(TRUE)
}

#' Test whether all column names are present in a data.frame
#' @inheritParams check_data_doc_template
#' @return Returns TRUE if all columns are present and FALSE otherwise
#' @keywords internal
test_columns_present <- function(data, columns) {
  check <- check_columns_present(data, columns)
  return(is.logical(check))
}

#' Test whether column names are NOT present in a data.frame
#' @inheritParams check_data_doc_template
#' @return Returns TRUE if none of the columns are present and FALSE otherwise
#' @keywords internal
test_columns_not_present <- function(data, columns) {
  if (any(columns %in% colnames(data))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Check whether data is data.frame with correct columns
#' @description Checks whether data is a data.frame, whether columns
#' "observed" and "predicted" are presents
#' and checks that only one of "quantile" and "sample_id" is present.
#' @inheritParams check_data_doc_template
#' @importFrom checkmate check_data_frame
#' @return Returns TRUE if basic requirements are satisfied and a string with
#' an error message otherwise
#' @keywords check-inputs
check_data_columns <- function(data) {
  is_data <- check_data_frame(data, min.rows = 1)
  if (!is.logical(is_data)) {
    return(is_data)
  }
  needed <- test_columns_present(data, c("observed", "predicted"))
  if (!needed) {
    return("Both columns `observed` and predicted` are needed")
  }
  problem <- test_columns_present(data, c("sample_id", "quantile"))
  if (problem) {
    return(
      "Found columns `quantile` and `sample_id`. Only one of these is allowed"
    )
  }
  return(TRUE)
}


#' Check whether an attribute is present
#' @description Checks whether an object has an attribute
#' @param object An object to be checked
#' @param attribute name of an attribute to be checked
#' @return Returns TRUE if attribute is there and an error message as
#' a string otherwise
#' @keywords check-inputs
check_has_attribute <- function(object, attribute) {
  if (is.null(attr(object, attribute))) {
    return(
      paste0("Found no attribute `", attribute, "`")
    )
  } else {
    return(TRUE)
  }
}

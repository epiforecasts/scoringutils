#' @title Check whether an input is an atomic vector of mode 'numeric'
#'
#' @description Helper function
#' @param x input to check
#' @param x additional arguments to pass to `check_numeric()`
#' @importFrom checkmate check_atomic_vector check_numeric
#' @return Either TRUE if the test is successful or a string with an error
#' message
#' @keywords internal
check_numeric_vector = function(x, ...) {
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
      toString(setdiff(metrics, available_metrics))
    )
    warning(msg)
  }
  return(metrics)
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


check_has_attribute <- function(object, attribute) {
  if (is.null(attr(object, attribute))) {
    return(
      paste0("Attribute `", attribute, "` is missing")
    )
  } else {
    return(TRUE)
  }
}

test_has_attribute <- function(object, attribute) {
  check <- check_has_attribute(object, attribute)
  if (is.logical(check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


check_attribute_conflict <- function(object, attribute, expected) {
  existing <- attr(object, attribute)
  if (!is.null(existing) && !identical(existing, expected)) {
    msg <- paste0(
      "Object has an attribute `", attribute, "`, but it looks different ",
      "from what's expected.\n",
      "Existing: ", paste(existing, collapse = ", "), "\n",
      "Expected: ", paste(expected, collapse = ", "), "\n",
      "Running `validate()` again might solve the problem"
    )
    return(msg)
  }
  return(TRUE)
}



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

remove_na_observed_predicted <- function(data) {
  # remove rows where predicted or observed value are NA -----------------------
  data <- data[!is.na(observed) & !is.na(predicted)]
  if (nrow(data) == 0) {
    stop("After removing NA values in `observed` and `predicted`, there were no observations left")
  }
  return(data[])
}



#' Check that all forecasts have the same number of quantiles or samples
#' @param data data.frame to check
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


#' @title Print output from `check_forecasts()`
#'
#' @description Helper function that prints the output generated by
#' `check_forecasts()`
#'
#' @param x An object of class 'scoringutils_check' as produced b y
#' `check_forecasts()`
#' @param ... additional arguments (not used here)
#'
#' @return NULL
#' @export
#' @keywords check-forecasts
#' @examples
#' check <- validate(example_quantile)
#' print(check)
print.scoringutils_check <- function(x, ...) {
  cat("Your forecasts seem to be for a target of the following type:\n")
  print(x["target_type"])
  cat("and in the following format:\n")
  print(x["prediction_type"])

  cat("The unit of a single forecast is defined by:\n")
  print(x["forecast_unit"])

  cat("Cleaned data, rows with NA values in predicted or observed removed:\n")
  print.default(x["cleaned_data"])

  cat("Number of unique values per column per model:\n")
  print.default(x["unique_values"])

  colnames <- names(x)[names(x) %in% c("messages", "warnings", "errors")]
  if (length(colnames) > 0) {
    print.default(x[colnames])
  }

  return(invisible(x))
}


# print stuff
diagnose <- function(data) {

}



#' @title Find duplicate forecasts
#'
#' @description Helper function to identify duplicate forecasts, i.e.
#' instances where there is more than one forecast for the same prediction
#' target.
#'
#' @param data A data.frame as used for [score()]
#'
#' @param forecast_unit A character vector with the column names that define
#' the unit of a single forecast. If `NULL` (the default) the function tries
#' to infer the unit of a single forecast.
#'
#' @return A data.frame with all rows for which a duplicate forecast was found
#' @export
#' @keywords check-forecasts
#' @examples
#' example <- rbind(example_quantile, example_quantile[1000:1010])
#' find_duplicates(example)

find_duplicates <- function(data, forecast_unit = NULL) {
  type <- c("sample_id", "quantile")[c("sample_id", "quantile") %in% colnames(data)]
  if (is.null(forecast_unit)) {
    forecast_unit <- get_forecast_unit(data)
  }
  data <- as.data.table(data)
  data[, scoringutils_InternalDuplicateCheck := .N, by = c(forecast_unit, type)]
  out <- data[scoringutils_InternalDuplicateCheck > 1]
  out[, scoringutils_InternalDuplicateCheck := NULL]
  return(out[])
}

#' Check that there are no duplicate forecasts
#'
#' @description
#' Runs [find_duplicates()] and returns a message if an issue is encountered
#' @inheritParams find_duplicates
#' @return Returns an string with an error message if an issue is found,
#' otherwise returns TRUE
#'
#' @keywords internal
check_duplicates <- function(data, forecast_unit = NULL) {
  check_duplicates <- find_duplicates(data, forecast_unit = forecast_unit)

  if (nrow(check_duplicates) > 0) {
    msg <- paste0(
      "There are instances with more than one forecast for the same target. ",
      "This can't be right and needs to be resolved. Maybe you need to ",
      "check the unit of a single forecast and add missing columns? Use ",
      "the function find_duplicates() to identify duplicate rows"
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


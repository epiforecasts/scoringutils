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


#' @title Assure that data has a `model` column
#'
#' @description
#' Check whether the data.table has a column called `model`.
#' If not, a column called `model` is added with the value `Unspecified model`.
#' @inheritParams as_forecast
#' @importFrom cli cli_inform
#' @importFrom checkmate assert_data_table
#' @return The data.table with a column called `model`
#' @keywords internal_input_check
ensure_model_column <- function(data) {
  assert_data_table(data)
  if (!("model" %in% colnames(data))) {
    #nolint start: keyword_quote_linter
    cli_warn(
      c(
        "!" = "There is no column called `model` in the data.",
        "i" = "scoringutils assumes that all forecasts come from the
        same model"
      )
    )
    #nolint end
    data[, model := "Unspecified model"]
  }
  return(data[])
}


#' Check that all forecasts have the same number of quantiles or samples
#' @description
#' Function checks the number of quantiles or samples per forecast.
#' If the number of quantiles or samples is the same for all forecasts, it
#' returns TRUE and a string with an error message otherwise.
#' @param forecast_unit Character vector denoting the unit of a single forecast.
#' @importFrom checkmate assert_subset
#' @inherit document_check_functions params return
#' @keywords internal_input_check
check_number_per_forecast <- function(data, forecast_unit) {
  data <- ensure_data.table(data)
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


#' Check that there are no duplicate forecasts
#'
#' @description
#' Runs [get_duplicate_forecasts()] and returns a message if an issue is
#' encountered
#' @inheritParams get_duplicate_forecasts
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_duplicates <- function(data) {
  check_duplicates <- get_duplicate_forecasts(data)

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
  for (x in columns) {
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
  return(isTRUE(check))
}

#' Test whether column names are NOT present in a data.frame
#' @description The function checks whether all column names are NOT present.
#' If none of the columns are present, the function returns TRUE. If one or
#' more columns are present, the function returns FALSE.
#' @inheritParams document_check_functions
#' @return Returns TRUE if none of the columns are present and FALSE otherwise
#' @importFrom checkmate test_names
#' @keywords internal_input_check
test_columns_not_present <- function(data, columns) {
  test_names(colnames(data), disjunct.from = columns)
}

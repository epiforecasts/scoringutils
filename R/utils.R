#' @title Available metrics in scoringutils
#'
#' @return A vector with the name of all available metrics
#' @export
#' @keywords info

available_metrics <- function() {
  return(unique(scoringutils::metrics$Name))
}


#' Safely delete Columns From a Data.table
#'
#' @description take a vector of column names and delete the columns if they
#' are present in the data.table
#' @param df A data.table or data.frame from which columns shall be deleted
#' @param cols_to_delete character vector with names of columns to be deleted
#' @param make_unique whether to make the data set unique after removing columns
#' @importFrom data.table as.data.table
#' @return A data.table
#'
#' @keywords internal
#'
delete_columns <- function(df, cols_to_delete, make_unique = FALSE) {
  df <- data.table::as.data.table(df)
  delete_columns <- names(df)[names(df) %in% cols_to_delete]
  if (length(delete_columns) > 0) {
    if (make_unique) {
      df <- unique(df[, eval(delete_columns) := NULL])
    } else {
      df <- df[, eval(delete_columns) := NULL]
    }
  }
  return(df)
}


get_prediction_type <- function(data) {
  if (is.data.frame(data)) {
    data <- data$predicted
  }
  if (isTRUE(all.equal(as.vector(data), as.integer(data))) &&
        !all(is.na(as.integer(data)))) {
    return("discrete")
  } else if (suppressWarnings(!all(is.na(as.numeric(data))))) {
    return("continuous")
  } else {
    stop("Input is not numeric and cannot be coerced to numeric")
  }
}

#' @title Get type of the target true values of a forecast
#'
#' @description Internal helper function to get the type of the target
#' true values of a forecast. That is inferred based on the type and the
#' content of the `observed` column.
#'
#' @inheritParams validate
#'
#' @return Character vector of length one with either "binary", "integer", or
#' "continuous"
#'
#' @keywords internal

get_target_type <- function(data) {
  if (is.factor(data$observed)) {
    return("classification")
  }
  if (isTRUE(all.equal(data$observed, as.integer(data$observed)))) {
    return("integer")
  } else {
    return("continuous")
  }
}



#' @title Get unit of a single forecast
#'
#' @description Helper function to get the unit of a single forecast, i.e.
#' the column names that define where a single forecast was made for.
#' This just takes all columns that are available in the data and subtracts
#' the columns that are protected, i.e. those returned by
#' [get_protected_columns()].
#'
#' @inheritParams validate
#'
#' @return A character vector with the column names that define the unit of
#' a single forecast
#'
#' @keywords internal

get_forecast_unit <- function(data) {
  protected_columns <- get_protected_columns(data)
  forecast_unit <- setdiff(colnames(data), protected_columns)
  return(forecast_unit)
}


#' @title Get protected columns from a data frame
#'
#' @description Helper function to get the names of all columns in a data frame
#' that are protected columns.
#'
#' @inheritParams validate
#'
#' @return A character vector with the names of protected columns in the data.
#' If data is `NULL` (default) then it returns a list of all columns that are
#' protected in scoringutils.
#'
#' @keywords internal

get_protected_columns <- function(data = NULL) {

  protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile", "upper", "lower",
    "pit_value", "range", "boundary", available_metrics(),
    grep("coverage_", names(data), fixed = TRUE, value = TRUE)
  )

  if(is.null(data)) {
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

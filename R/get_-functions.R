# Functions that help to obtain information about the data

#' @title Infer the type of a forecast based on a data.frame
#'
#' @description Internal helper function to get the type of the forecast.
#' Options are "sample-based", "quantile-based", "binary" or "point" forecast.
#' The function runs additional checks to make sure the data satisfies
#' requirements and throws an informative error if any issues are found.
#'
#' @inheritParams validate
#'
#' @return Character vector of length one with either "binary", "quantile",
#' "sample" or "point".
#'
#' @keywords internal

get_forecast_type <- function(data) {
  if (test_forecast_type_is_binary(data)) {
    return("binary")
  }
  if (test_forecast_type_is_quantile(data)) {
    return("quantile")
  }
  if (test_forecast_type_is_sample(data)) {
    return("sample")
  }
  if (test_forecast_type_is_point(data)) {
    return("point")
  }
  stop("Checking `data`: input doesn't satisfy the criteria for any forecast type.",
       "Are you missing a column `quantile` or `sample_id`?",
       "Please check the vignette for additional info.")
}


#' Test whether data could be a binary forecast.
#' @description Checks type of the necessary columns.
#' @param data A data.frame or similar to be checked
#' @importFrom checkmate test_factor test_numeric
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal
test_forecast_type_is_binary <- function(data) {
  observed_correct <- test_factor(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  return(observed_correct && predicted_correct)
}

#' Test whether data could be a sample-based forecast.
#' @description Checks type of the necessary columns.
#' @param data A data.frame or similar to be checked
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal
test_forecast_type_is_sample <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "sample_id")
  return(observed_correct && predicted_correct && columns_correct)
}

#' Test whether data could be a point forecast.
#' @description Checks type of the necessary columns.
#' @param data A data.frame or similar to be checked
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal
test_forecast_type_is_point <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_not_present(data, c("sample_id", "quantile"))
  return(observed_correct && predicted_correct && columns_correct)
}

#' Test whether data could be a quantile forecast.
#' @description Checks type of the necessary columns.
#' @param data A data.frame or similar to be checked
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal
test_forecast_type_is_quantile <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "quantile")
  return(observed_correct && predicted_correct && columns_correct)
}




# need to think about whether we want or keep this function
get_prediction_type <- function(data) {
  if (is.data.frame(data)) {
    data <- data$predicted
  }
  if (
    isTRUE(all.equal(as.vector(data), as.integer(data))) &&
    !all(is.na(as.integer(data)))
  ) {
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
#' get_duplicate_forecasts(example)

get_duplicate_forecasts <- function(data, forecast_unit = NULL) {
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


#' @title Get a list of all attributes of a scoringutils object
#'
#' @param object A object of class `scoringutils_`
#'
#' @return A named list with the attributes of that object.
#' @keywords internal
get_scoringutils_attributes <- function(object) {
  possible_attributes <- c(
    "by",
    "forecast_unit",
    "forecast_type",
    "metric_names",
    "messages",
    "warnings"
  )

  attr_list <- list()
  for (attr_name in possible_attributes) {
    attr_list[[attr_name]] <- attr(object, attr_name)
  }
  return(attr_list)
}

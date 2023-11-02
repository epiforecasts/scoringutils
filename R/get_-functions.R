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
    forecast_type <- "binary"
  } else if (test_forecast_type_is_quantile(data)) {
    forecast_type <- "quantile"
  } else if (test_forecast_type_is_sample(data)) {
    forecast_type <- "sample"
  } else if (test_forecast_type_is_point(data)) {
    forecast_type <- "point"
  } else {
    stop(
      "Checking `data`: input doesn't satisfy criteria for any forecast type.",
      "Are you missing a column `quantile` or `sample_id`?",
      "Please check the vignette for additional info."
    )
  }
  conflict <- check_attribute_conflict(data, "forecast_type", forecast_type)
  if (!is.logical(conflict)) {
    warning(conflict)
  }
  return(forecast_type)
}


#' Test whether data could be a binary forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
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
#' @inheritParams document_check_functions
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
#' @inheritParams document_check_functions
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
#' @inheritParams document_check_functions
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal
test_forecast_type_is_quantile <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "quantile")
  return(observed_correct && predicted_correct && columns_correct)
}


#' @title Get type of a vector or matrix of observed values or predictions
#'
#' @description Internal helper function to get the type of a vector (usually
#' of observed or predicted values). The function checks whether the input is
#' a factor, or else whether it is integer (or can be coerced to integer) or
#' whether it's continuous.
#'
#' @param x Input used to get the type.
#'
#' @return Character vector of length one with either "classification",
#' "integer", or "continuous"
#'
#' @keywords internal
get_type <- function(x) {
  if (is.factor(x)) {
    return("classification")
  }
  assert_numeric(as.vector(x))
  if (all(is.na(as.vector(x)))) {
    stop("Can't get type: all values of are NA")
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


#' @title Get metrics that were used for scoring
#'
#' @description Internal helper function to get the metrics that were used
#' to score forecasts.
#' @param score A data.table with an attribute `metric_names`
#'
#' @return Character vector with the metrics that were used for scoring.
#'
#' @keywords internal

get_metrics <- function(scores) {
  metric_names <- attr(scores, "metric_names")
  if (is.null(metric_names)) {
    stop("The data needs to have an attribute `metric_names` with the names ",
         " of the metrics that were used for scoring. This should be the case ",
         "if the data was produced using `score()`. Either run `score()` ",
         "again, or set the attribute manually using ",
         "`attr(data, 'metric_names') <- names_of_the_scoring_metrics")
  }
  return(metric_names)
}


#' @title Get unit of a single forecast
#'
#' @description Helper function to get the unit of a single forecast, i.e.
#' the column names that define where a single forecast was made for.
#' This just takes all columns that are available in the data and subtracts
#' the columns that are protected, i.e. those returned by
#' [get_protected_columns()] as well as the names of the metrics that were
#' specified during scoring, if any.
#'
#' @inheritParams validate
#' @param check_conflict Whether or not to check whether there is a conflict
#' between a stored attribute and the inferred forecast unit. Defaults to FALSE.
#'
#' @return A character vector with the column names that define the unit of
#' a single forecast
#'
#' @keywords internal
get_forecast_unit <- function(data, check_conflict = FALSE) {
  # check whether there is a conflict in the forecast_unit and if so warn
  protected_columns <- get_protected_columns(data)
  protected_columns <- c(protected_columns, attr(data, "metric_names"))

  forecast_unit <- setdiff(colnames(data), unique(protected_columns))

  conflict <- check_attribute_conflict(data,  "forecast_unit", forecast_unit)
  if (check_conflict && !is.logical(conflict)) {
    warning(conflict)
  }

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
    "pit_value", "range", "boundary", "relative_skill", "scaled_rel_skill",
    available_metrics(),
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
    "scoringutils_by",
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

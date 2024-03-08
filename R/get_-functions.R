# Functions that help to obtain information about the data

#' @title Infer Forecast Type
#' @description Helper function to infer the forecast type based on a
#' data.frame or similar with predictions. Please check the vignettes to
#' learn more about forecast types.
#'
#' Possible forecast types are
#' - "sample-based"
#' - "quantile-based"
#' - "binary"
#' - "point" forecast.
#'
#' The function runs additional checks to make sure the data satisfies the
#' requirements of the respective forecast type and throws an
#' informative error if any issues are found.
#' @inheritParams as_forecast
#' @importFrom cli cli_abort
#' @return Character vector of length one with either "binary", "quantile",
#' "sample" or "point".
#' @export
#' @keywords check-forecasts
get_forecast_type <- function(data) {
  assert_data_frame(data)
  assert(check_columns_present(data, c("observed", "predicted")))
  if (test_forecast_type_is_binary(data)) {
    forecast_type <- "binary"
  } else if (test_forecast_type_is_quantile(data)) {
    forecast_type <- "quantile"
  } else if (test_forecast_type_is_sample(data)) {
    forecast_type <- "sample"
  } else if (test_forecast_type_is_point(data)) {
    forecast_type <- "point"
  } else {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Checking `data`: input doesn't satisfy criteria for any
        forecast type. ",
        "i" = "Are you missing a column `quantile_level` or `sample_id`?
        Please check the vignette for additional info."
      )
    )
    #nolint end
  }
  return(forecast_type)
}


#' Test whether data could be a binary forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
#' @importFrom checkmate test_factor test_numeric
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal_input_check
test_forecast_type_is_binary <- function(data) {
  observed_correct <- test_factor(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  return(observed_correct && predicted_correct)
}

#' Test whether data could be a sample-based forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal_input_check
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
#' @keywords internal_input_check
test_forecast_type_is_point <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_not_present(
    data, c("sample_id", "quantile_level")
  )
  return(observed_correct && predicted_correct && columns_correct)
}

#' Test whether data could be a quantile forecast.
#' @description Checks type of the necessary columns.
#' @inheritParams document_check_functions
#' @return Returns TRUE if basic requirements are satisfied and FALSE otherwise
#' @keywords internal_input_check
test_forecast_type_is_quantile <- function(data) {
  observed_correct <- test_numeric(x = data$observed)
  predicted_correct <- test_numeric(x = data$predicted)
  columns_correct <- test_columns_present(data, "quantile_level")
  return(observed_correct && predicted_correct && columns_correct)
}


#' @title Get type of a vector or matrix of observed values or predictions
#'
#' @description Internal helper function to get the type of a vector (usually
#' of observed or predicted values). The function checks whether the input is
#' a factor, or else whether it is integer (or can be coerced to integer) or
#' whether it's continuous.
#' @param x Input used to get the type.
#' @importFrom cli cli_abort
#' @return Character vector of length one with either "classification",
#' "integer", or "continuous"
#' @keywords internal_input_check
get_type <- function(x) {
  if (is.factor(x)) {
    return("classification")
  }
  assert_numeric(as.vector(x))
  if (all(is.na(as.vector(x)))) {
    cli_abort("Can't get type: all values of are {.val NA}.")
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


#' @title Get Names Of The Metrics That Were Used For Scoring
#' @description
#' When applying a scoring rule via [score()], the names of the scoring rules
#' become column names of the
#' resulting data.table. In addition, an attribute `score_names` will be
#' added to the output, holding the names of the scores as a vector.
#' This is done so that a function like [get_forecast_unit()] can still
#' identify which columns are part of the forecast unit and which hold a score.
#'
#' `get_score_names()` access and returns this attribute. If there is no
#' attribute, the function will return NULL. Users can control whether the
#' function should error instead via the `error` argument.
#'
#' `get_score_names()` also checks whether the names of the scores stored in
#' the attribute are column names of the data and will throw a warning if not.
#' This can happen if you rename columns after scoring. You can either run
#' [score()] again, specifying names for the scoring rules manually, or you
#' can update the attribute manually using
#' `attr(scores, "score_names") <- c("names", "of", "your", "scores")` (the
#' order does not matter).
#'
#' @param scores A data.table with an attribute `score_names`
#' @param error Throw an error if there is no attribute called `score_names`?
#' Default is FALSE.
#' @importFrom cli cli_abort cli_warn
#' @return Character vector with the names of the scoring rules that were used
#' for scoring or `NULL` if no scores were computed previously.
#' @keywords check-forecasts
#' @export
get_score_names <- function(scores, error = FALSE) {
  score_names <- attr(scores, "score_names")
  if (error && is.null(score_names)) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "!" = "Object needs an attribute `score_names` with the names of the
         scoring rules that were used for scoring.",
        "i" = "See `?get_score_names` for further information."
      )
    )
    #nolint end
  }

  if (!all(score_names %in% names(scores))) {
    #nolint start: keyword_quote_linter object_usage_linter
    missing <- setdiff(score_names, names(scores))
    cli_warn(
      c(
        "!" = "The following scores have been previously computed, but are no
            longer column names of the data: {.val {missing}}",
        "i" = "See {.code ?get_score_names} for further information."
      )
    )
    #nolint end
  }

  return(score_names)
}


#' @title Get Unit Of A Single Forecast
#' @description Helper function to get the unit of a single forecast, i.e.
#' the column names that define where a single forecast was made for.
#' This just takes all columns that are available in the data and subtracts
#' the columns that are protected, i.e. those returned by
#' [get_protected_columns()] as well as the names of the metrics that were
#' specified during scoring, if any.
#' @inheritParams as_forecast
#' @return A character vector with the column names that define the unit of
#' a single forecast
#' @export
#' @keywords check-forecasts
get_forecast_unit <- function(data) {
  protected_columns <- get_protected_columns(data)
  protected_columns <- c(protected_columns, attr(data, "score_names"))
  forecast_unit <- setdiff(colnames(data), unique(protected_columns))
  return(forecast_unit)
}


#' @title Get Protected Columns From Data
#'
#' @description Helper function to get the names of all columns in a data frame
#' that are protected columns.
#'
#' @inheritParams as_forecast
#'
#' @return A character vector with the names of protected columns in the data.
#' If data is `NULL` (default) then it returns a list of all columns that are
#' protected in scoringutils.
#'
#' @keywords internal
get_protected_columns <- function(data = NULL) {

  protected_columns <- c(
    "predicted", "observed", "sample_id", "quantile_level", "upper", "lower",
    "pit_value", "interval_range", "boundary",
    "interval_coverage", "interval_coverage_deviation",
    "quantile_coverage", "quantile_coverage_deviation",
    grep("_relative_skill$", names(data), value = TRUE),
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


#' @title Find Duplicate Forecasts
#'
#' @description Helper function to identify duplicate forecasts, i.e.
#' instances where there is more than one forecast for the same prediction
#' target.
#'
#' @param data A data.frame as used for [score()]
#'
#' @param forecast_unit A character vector with the column names that define
#' the unit of a single forecast. By default the forecast unit will be
#' automatically inferred from the data (see [get_forecast_unit()])
#'
#' @return A data.frame with all rows for which a duplicate forecast was found
#' @export
#' @importFrom checkmate assert_data_frame assert_subset
#' @keywords check-forecasts
#' @examples
#' example <- rbind(example_quantile, example_quantile[1000:1010])
#' get_duplicate_forecasts(example)

get_duplicate_forecasts <- function(
  data,
  forecast_unit = get_forecast_unit(data)
) {
  assert_data_frame(data)
  assert_subset(forecast_unit, colnames(data))
  available_type <- c("sample_id", "quantile_level") %in% colnames(data)
  type <- c("sample_id", "quantile_level")[available_type]
  data <- as.data.table(data)
  data[, scoringutils_InternalDuplicateCheck := .N, by = c(forecast_unit, type)]
  out <- data[scoringutils_InternalDuplicateCheck > 1]
  out[, scoringutils_InternalDuplicateCheck := NULL]
  return(out[])
}

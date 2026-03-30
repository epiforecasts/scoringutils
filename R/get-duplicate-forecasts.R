#' @title Find duplicate forecasts
#'
#' @description
#' Identify duplicate forecasts, i.e. instances where there is more than
#' one forecast for the same prediction target.
#'
#' `get_duplicate_forecasts()` is an S3 generic. When called on a validated
#' `forecast` object it uses the type-specific columns returned by
#' [get_duplicate_columns()]. When called on a plain `data.frame` the
#' default method falls back to detecting columns by name.
#'
#' @inheritParams as_forecast_doc_template
#' @param counts Should the output show the number of duplicates per
#'   forecast unit instead of the individual duplicated rows?
#'   Default is `FALSE`.
#' @param ... Additional arguments passed to methods.
#' @returns A data.frame with all rows for which a duplicate forecast
#'   was found
#' @export
#' @importFrom checkmate assert_data_frame assert_subset
#' @importFrom data.table setorderv
#' @keywords diagnose-inputs
#' @examples
#' example <- rbind(example_quantile, example_quantile[1000:1010])
#' get_duplicate_forecasts(example)
get_duplicate_forecasts <- function(data, ...) {
  UseMethod("get_duplicate_forecasts")
}


#' @rdname get_duplicate_forecasts
#' @export
get_duplicate_forecasts.default <- function(
  data,
  forecast_unit = NULL,
  counts = FALSE,
  ...
) {
  assert_data_frame(data)
  find_duplicates(
    ensure_data.table(data),
    forecast_unit = forecast_unit,
    counts = counts
  )
}


#' @rdname get_duplicate_forecasts
#' @export
get_duplicate_forecasts.forecast <- function(
  data,
  forecast_unit = NULL,
  counts = FALSE,
  ...
) {
  find_duplicates(
    ensure_data.table(data),
    forecast_unit = forecast_unit,
    counts = counts
  )
}


#' Find duplicate rows in forecast data
#'
#' @inheritParams get_duplicate_forecasts
#' @returns A `data.table` of duplicate rows (or counts).
#' @keywords internal
find_duplicates <- function(data, forecast_unit = NULL, counts = FALSE) {
  if (!is.null(forecast_unit)) {
    data <- set_forecast_unit(data, forecast_unit)
  }
  forecast_unit <- get_forecast_unit(data)
  type <- get_duplicate_columns(data)
  data <- as.data.table(data)
  data[,
    scoringutils_InternalDuplicateCheck := .N,
    by = c(forecast_unit, type)
  ]
  out <- data[scoringutils_InternalDuplicateCheck > 1]

  setorderv(out, cols = c(forecast_unit, type, "predicted"))
  out[, scoringutils_InternalDuplicateCheck := NULL]

  if (counts) {
    out <- out[,
      .(n_duplicates = .N),
      by = c(get_forecast_unit(out))
    ]
  }

  return(out[])
}


#' @title Get type-specific columns for duplicate detection
#'
#' @description
#' Internal S3 generic that returns the column names (beyond the forecast
#' unit) needed to identify a unique row for duplicate detection. Each
#' forecast type method returns the columns specific to that type.
#'
#' @inheritParams as_forecast_doc_template
#' @returns A character vector of column names.
#' @keywords internal
get_duplicate_columns <- function(data) {
  UseMethod("get_duplicate_columns")
}


#' @export
get_duplicate_columns.default <- function(data) {
  available <- c(
    "sample_id", "quantile_level", "predicted_label"
  ) %in% colnames(data)
  c("sample_id", "quantile_level", "predicted_label")[available]
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

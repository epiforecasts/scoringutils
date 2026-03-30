#' @title Find duplicate forecasts
#'
#' @description
#' Identify duplicate forecasts, i.e. instances where there is more than
#' one forecast for the same prediction target.
#'
#' Uses [get_forecast_type_ids()] to determine the type-specific columns
#' (beyond the forecast unit) that identify a unique row. For validated
#' `forecast` objects this dispatches to the appropriate method; for
#' plain `data.frame`s the default method detects columns by name.
#'
#' @inheritParams as_forecast_doc_template
#' @param counts Should the output show the number of duplicates per
#'   forecast unit instead of the individual duplicated rows?
#'   Default is `FALSE`.
#' @returns A data.frame with all rows for which a duplicate forecast
#'   was found
#' @export
#' @importFrom checkmate assert_data_frame assert_subset
#' @importFrom data.table setorderv
#' @keywords diagnose-inputs
#' @examples
#' example <- rbind(example_quantile, example_quantile[1000:1010])
#' get_duplicate_forecasts(example)
get_duplicate_forecasts <- function(
  data,
  forecast_unit = NULL,
  counts = FALSE
) {
  assert_data_frame(data)
  data <- ensure_data.table(data)

  if (!is.null(forecast_unit)) {
    data <- set_forecast_unit(data, forecast_unit)
  }
  forecast_unit <- get_forecast_unit(data)
  type <- get_forecast_type_ids(data)
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


#' @title Get type-specific ID columns for a forecast
#'
#' @description
#' Internal S3 generic that returns the column names (beyond the forecast
#' unit) that identify a unique row for a given forecast type. Each
#' forecast type method returns the columns specific to that type.
#' The default method falls back to detecting columns by name.
#'
#' @inheritParams as_forecast_doc_template
#' @returns A character vector of column names.
#' @keywords internal
get_forecast_type_ids <- function(data) {
  UseMethod("get_forecast_type_ids")
}


#' @export
get_forecast_type_ids.default <- function(data) {
  character(0)
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

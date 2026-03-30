#' @title Find duplicate forecasts
#'
#' @description
#' Identify duplicate forecasts, i.e. instances where there is more than
#' one forecast for the same prediction target.
#'
#' Uses [get_forecast_type_ids()] to determine the type-specific columns
#' (beyond the forecast unit) that identify a unique row. For `forecast`
#' objects the type is detected automatically. For plain `data.frame`s
#' you should pass `type` (e.g. `"quantile"`, `"sample"`) so that the
#' correct columns are used. Calling on a plain `data.frame` without
#' `type` is deprecated; it falls back to column-name detection but
#' this behaviour will be removed in a future version.
#'
#' @inheritParams as_forecast_doc_template
#' @param type Character string naming the forecast type, corresponding
#'   to the class suffix after `forecast_` (e.g. `"quantile"` for
#'   class `forecast_quantile`, `"sample"` for `forecast_sample`).
#'   Used to determine type-specific ID columns when `data` is not
#'   already a `forecast` object. Ignored when `data` already
#'   inherits from `forecast`.
#' @param counts Should the output show the number of duplicates per
#'   forecast unit instead of the individual duplicated rows?
#'   Default is `FALSE`.
#' @returns A data.frame with all rows for which a duplicate forecast
#'   was found
#' @export
#' @importFrom checkmate assert_data_frame assert_subset
#' @importFrom data.table setorderv
#' @importFrom lifecycle deprecated deprecate_warn
#' @keywords diagnose-inputs
#' @examples
#' example <- rbind(example_quantile, example_quantile[1000:1010])
#' get_duplicate_forecasts(example, type = "quantile")
get_duplicate_forecasts <- function(
  data,
  forecast_unit = NULL,
  type = NULL,
  counts = FALSE
) {
  assert_data_frame(data)
  checkmate::assert_string(type, null.ok = TRUE)
  data <- ensure_data.table(data)

  if (!is.null(forecast_unit)) {
    data <- set_forecast_unit(data, forecast_unit)
  }
  forecast_unit <- get_forecast_unit(data)

  if (inherits(data, "forecast")) {
    type_cols <- get_forecast_type_ids(data)
  } else if (!is.null(type)) {
    tmp <- new_forecast(data, paste0("forecast_", type))
    type_cols <- get_forecast_type_ids(tmp)
  } else {
    lifecycle::deprecate_warn(
      "2.2.0",
      "get_duplicate_forecasts(type = )",
      details = paste(
        "Pass `type` (e.g. \"quantile\", \"sample\") to detect",
        "type-specific duplicates on plain data.frames."
      )
    )
    # deprecated fallback: detect type columns by name
    known <- c("sample_id", "quantile_level", "predicted_label")
    type_cols <- intersect(known, colnames(data))
  }
  data <- as.data.table(data)
  data[,
    scoringutils_InternalDuplicateCheck := .N,
    by = c(forecast_unit, type_cols)
  ]
  out <- data[scoringutils_InternalDuplicateCheck > 1]

  setorderv(out, cols = c(forecast_unit, type_cols, "predicted"))
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
#' S3 generic that returns the column names (beyond the forecast unit)
#' that identify a unique row for a given forecast type. Each forecast
#' type method returns the columns specific to that type. The default
#' returns `character(0)` (no type-specific columns).
#'
#' Custom forecast types should define a method returning the relevant
#' column names.
#'
#' @inheritParams as_forecast_doc_template
#' @returns A character vector of column names.
#' @export
#' @keywords as_forecast
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
  duplicates <- get_duplicate_forecasts(data)

  if (nrow(duplicates) > 0) {
    type_hint <- ""
    if (inherits(data, "forecast")) {
      forecast_type <- get_forecast_type(data)
      type_hint <- paste0(', type = "', forecast_type, '"')
    }
    msg <- paste0(
      "There are instances with more than one forecast for the ",
      "same target. This can't be right and needs to be resolved. ",
      "Maybe you need to check the unit of a single forecast and ",
      "add missing columns? Use ",
      "`get_duplicate_forecasts(data", type_hint, ")` ",
      "to identify duplicate rows"
    )
    return(msg)
  }
  return(TRUE)
}

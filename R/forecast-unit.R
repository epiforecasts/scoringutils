#' @title Set unit of a single forecast manually
#'
#' @description
#' Helper function to set the unit of a single forecast (i.e. the
#' combination of columns that uniquely define a single forecast) manually.
#' This simple function keeps the columns specified in `forecast_unit` (plus
#' additional protected columns, e.g. for observed values, predictions or
#' quantile levels) and removes duplicate rows. `set_forecast_unit()` will
#' mainly be called when constructing a `forecast` object
#' via the `forecast_unit` argument in `as_forecast_<type>`.
#'
#' If not done explicitly, `scoringutils` attempts to determine the unit
#' of a single forecast automatically by simply assuming that all column names
#' are relevant to determine the forecast unit. This may lead to unexpected
#' behaviour, so setting the forecast unit explicitly can help make the code
#' easier to debug and easier to read.
#'
#' @inheritParams as_forecast_doc_template
#' @param forecast_unit Character vector with the names of the columns that
#'   uniquely identify a single forecast.
#' @importFrom cli cli_warn
#' @returns A data.table with only those columns kept that are relevant to
#'   scoring or denote the unit of a single forecast as specified by the user.
#' @importFrom data.table ':=' is.data.table copy
#' @importFrom checkmate assert_character assert_subset
#' @keywords as_forecast
#' @examples
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   scoringutils:::set_forecast_unit(
#'     c("location", "target_end_date", "target_type", "horizon", "model")
#'   )
set_forecast_unit <- function(data, forecast_unit) {
  data <- ensure_data.table(data)
  assert_subset(forecast_unit, names(data), empty.ok = FALSE)
  keep_cols <- c(get_protected_columns(data), forecast_unit)
  out <- unique(data[, .SD, .SDcols = keep_cols])
  return(out)
}


#' @title Get unit of a single forecast
#' @description
#' Helper function to get the unit of a single forecast, i.e.
#' the column names that define where a single forecast was made for.
#' This just takes all columns that are available in the data and subtracts
#' the columns that are protected, i.e. those returned by
#' [get_protected_columns()] as well as the names of the metrics that were
#' specified during scoring, if any.
#' @inheritParams as_forecast_doc_template
#' @inheritSection forecast_types Forecast unit
#' @return
#' A character vector with the column names that define the unit of
#' a single forecast
#' @importFrom checkmate assert_data_frame
#' @export
#' @keywords diagnose-inputs
get_forecast_unit <- function(data) {
  assert_data_frame(data)
  protected_columns <- get_protected_columns(data)
  protected_columns <- c(protected_columns, attr(data, "metrics"))
  forecast_unit <- setdiff(colnames(data), unique(protected_columns))
  return(forecast_unit)
}

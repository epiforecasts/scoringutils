#' @title Find non-monotonic quantile predictions
#'
#' @description
#' Identifies forecast units where predicted values are not monotonically
#' non-decreasing with increasing quantile levels. This is a diagnostic helper
#' function modeled on [get_duplicate_forecasts()].
#'
#' Quantile forecasts should have non-decreasing predictions as quantile levels
#' increase (e.g., the predicted value at the 0.75 quantile should be at least
#' as large as the predicted value at the 0.5 quantile). Violations indicate
#' a problem with the forecasting method.
#'
#' @inheritParams as_forecast_doc_template
#' @param counts Should the output show the number of quantile rows per
#'   affected forecast unit instead of the individual rows? Default is `FALSE`.
#' @returns A data.table with all rows for which a non-monotonic prediction
#'   was found
#' @export
#' @importFrom checkmate assert_data_frame
#' @importFrom data.table setorderv
#' @keywords diagnose-inputs
#' @examples
#' # well-formed data returns 0 rows
#' get_non_monotonic_forecasts(example_quantile)
#'
#' # non-monotonic data
#' bad_data <- data.frame(
#'   model = "m1", date = as.Date("2020-01-01"), observed = 5,
#'   quantile_level = c(0.25, 0.5, 0.75), predicted = c(3, 7, 4)
#' )
#' get_non_monotonic_forecasts(bad_data)
get_non_monotonic_forecasts <- function(
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

  data <- as.data.table(data)
  setorderv(data, cols = c(forecast_unit, "quantile_level"))

  # For each forecast unit, check if predictions are non-decreasing
  data[, scoringutils_InternalMonoCheck := {
    ordered_pred <- predicted[order(quantile_level)]
    any(diff(ordered_pred) < 0)
  }, by = forecast_unit]

  out <- data[scoringutils_InternalMonoCheck == TRUE]
  out[, scoringutils_InternalMonoCheck := NULL]
  data[, scoringutils_InternalMonoCheck := NULL]

  if (counts) {
    out <- out[, .(n_non_monotonic = .N), by = c(get_forecast_unit(out))]
  }

  return(out[])
}


#' Check that predictions are monotonically non-decreasing with quantile level
#'
#' @description
#' Runs [get_non_monotonic_forecasts()] and returns a message if an issue is
#' encountered
#' @inheritParams get_non_monotonic_forecasts
#' @inherit document_check_functions return
#' @keywords internal_input_check
check_monotonicity <- function(data) {
  non_monotonic <- get_non_monotonic_forecasts(data)

  if (nrow(non_monotonic) > 0) {
    msg <- paste0(
      "Some forecasts have predictions that are not monotonically ",
      "non-decreasing with increasing quantile level. ",
      "This may cause issues with some scoring metrics (e.g. bias). ",
      "Use the function get_non_monotonic_forecasts() to identify ",
      "affected forecast units."
    )
    return(msg)
  }
  return(TRUE)
}

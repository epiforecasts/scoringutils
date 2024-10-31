#' @title Find duplicate forecasts
#'
#' @description
#' Internal helper function to identify duplicate forecasts, i.e.
#' instances where there is more than one forecast for the same prediction
#' target.
#'
#' @inheritParams as_forecast_doc_template
#' @param counts Should the output show the number of duplicates per forecast
#'   unit instead of the individual duplicated rows? Default is `FALSE`.
#' @returns A data.frame with all rows for which a duplicate forecast was found
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
  available_type <- c("sample_id", "quantile_level", "predicted_label") %in% colnames(data)
  type <- c("sample_id", "quantile_level", "predicted_label")[available_type]
  data <- as.data.table(data)
  data[, scoringutils_InternalDuplicateCheck := .N, by = c(forecast_unit, type)]
  out <- data[scoringutils_InternalDuplicateCheck > 1]

  col <- colnames(data)[
    colnames(data) %in% c("sample_id", "quantile_level", "predicted_label")
  ]
  setorderv(out, cols = c(forecast_unit, col, "predicted"))
  out[, scoringutils_InternalDuplicateCheck := NULL]

  if (counts) {
    out <- out[, .(n_duplicates = .N), by = c(get_forecast_unit(out))]
  }

  return(out[])
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

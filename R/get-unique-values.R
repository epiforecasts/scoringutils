#' @title Get unique value counts per forecast unit column
#'
#' @description
#' Given a forecast object, this function counts the number of unique values
#' in each column that defines the forecast unit (as determined by
#' [get_forecast_unit()]). This is useful for getting a quick overview of the
#' structure and scope of a forecast dataset.
#'
#' @param by character vector (default: `NULL`). If specified, results are
#'   grouped by this column, showing unique value counts for each forecast
#'   unit column per group. The grouping column itself is excluded from the
#'   column-level counts.
#'
#' @returns A data.table with columns `"column"` (the name of the forecast unit
#' column) and `"N_unique"` (the number of unique values in that column).
#' If `by` is specified, there is an additional column for the grouping
#' variable.
#'
#' @inheritParams score
#' @importFrom checkmate assert_subset
#' @export
#' @keywords gain-insights
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' example_quantile |>
#'   as_forecast_quantile() |>
#'   get_unique_values()
#'
#' example_quantile |>
#'   as_forecast_quantile() |>
#'   get_unique_values(by = "model")
get_unique_values <- function(forecast, by = NULL) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast_unit <- get_forecast_unit(forecast)

  if (!is.null(by)) {
    assert_subset(by, forecast_unit, empty.ok = FALSE)
  }

  forecast <- as.data.table(forecast)

  cols <- if (!is.null(by)) setdiff(forecast_unit, by) else forecast_unit

  if (is.null(by)) {
    out <- data.table(
      column = cols,
      N_unique = vapply(
        cols,
        function(col) length(unique(forecast[[col]])),
        integer(1)
      )
    )
  } else {
    out_list <- forecast[,
      {
        vals <- lapply(cols, function(col) length(unique(.SD[[col]])))
        list(column = cols, N_unique = unlist(vals))
      },
      by = by,
      .SDcols = cols
    ]
    out <- out_list
  }

  return(out[])
}

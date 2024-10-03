#' @title Count number of available forecasts
#'
#' @description
#' Given a data set with forecasts, this function counts the number of
#' available forecasts.
#' The level of grouping can be specified using the `by` argument (e.g. to
#' count the number of forecasts per model, or the number of forecasts per
#' model and location).
#' This is useful to determine whether there are any missing forecasts.
#'
#' @param by character vector or `NULL` (the default) that denotes the
#'   categories over which the number of forecasts should be counted.
#'   By default this will be the unit of a single forecast (i.e.
#'   all available columns (apart from a few "protected" columns such as
#'   'predicted' and 'observed') plus "quantile_level" or "sample_id" where
#'   present).
#'
#' @param collapse character vector (default: `c("quantile_level", "sample_id"`)
#'   with names of categories for which the number of rows should be collapsed
#'   to one when counting. For example, a single forecast is usually represented
#'   by a set of several quantiles or samples and collapsing these to one makes
#'   sure that a single forecast only gets counted once. Setting
#'   `collapse = c()` would mean that all quantiles / samples would be counted
#'   as individual forecasts.
#'
#' @return A data.table with columns as specified in `by` and an additional
#' column "count" with the number of forecasts.
#'
#' @inheritParams score
#' @importFrom data.table .I .N nafill
#' @export
#' @keywords gain-insights
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#'
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   as_forecast_quantile() %>%
#'   get_forecast_counts(by = c("model", "target_type"))
get_forecast_counts <- function(forecast,
                                by = get_forecast_unit(forecast),
                                collapse = c("quantile_level", "sample_id")) {
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  forecast_unit <- get_forecast_unit(forecast)
  assert_subset(by, names(forecast), empty.ok = FALSE)
  forecast <- as.data.table(forecast)

  # collapse several rows to 1, e.g. treat a set of 10 quantiles as one,
  # because they all belong to one single forecast that should be counted once
  collapse_by <- setdiff(
    c(forecast_unit, "quantile_level", "sample_id"),
    collapse
  )
  # filter "quantile_level", "sample" if in `collapse_by`, but not the forecast
  collapse_by <- intersect(collapse_by, names(forecast))

  forecast <- forecast[forecast[, .I[1], by = collapse_by]$V1]

  # count number of rows = number of forecasts
  out <- forecast[, .(count = .N), by = by]

  # make sure that all combinations in "by" are included in the output (with
  # count = 0). To achieve that, take unique values in `forecast` and expand grid
  col_vecs <- unclass(out)
  col_vecs$count <- NULL
  col_vecs <- lapply(col_vecs, unique)
  out_empty <- expand.grid(col_vecs, stringsAsFactors = FALSE)

  out <- merge(out, out_empty, by = by, all.y = TRUE)
  out[, count := nafill(count, fill = 0)]

  return(out[])
}

#' @title Count Number of Available Forecasts
#'
#' @description
#'
#' Given a data set with forecasts, count the number of available forecasts
#' for arbitrary grouping (e.g. the number of forecasts per model, or the
#' number of forecasts per model and location).
#' This is useful to determine whether there are any missing forecasts.
#'
#' @param by character vector or `NULL` (the default) that denotes the
#' categories over which the number of forecasts should be counted.
#' By default (`by = NULL`) this will be the unit of a single forecast (i.e.
#' all available columns (apart from a few "protected" columns such as
#' 'predicted' and 'observed') plus "quantile" or "sample_id" where present).
#'
#' @param collapse character vector (default is `c("quantile", "sample_id"`)
#' with names of categories for which the number of rows should be collapsed to
#' one when counting. For example, a single forecast is usually represented by a
#' set of several quantiles or samples and collapsing these to one makes sure
#' that a single forecast only gets counted once. Setting `collapse = c()`
#' would mean that all quantiles / samples would be counted as individual
#' forecasts.
#'
#' @return A data.table with columns as specified in `by` and an additional
#' column "count" with the number of forecasts.
#'
#' @inheritParams score
#' @importFrom data.table .I .N nafill
#' @export
#' @keywords check-forecasts
#' @examples
#' data.table::setDTthreads(1) # only needed to avoid issues on CRAN
#'
#' get_forecast_counts(example_quantile,
#'   by = c("model", "target_type")
#' )
get_forecast_counts <- function(data,
                                by = NULL,
                                collapse = c("quantile", "sample_id")) {

  data <- as_forecast(data)
  forecast_unit <- attr(data, "forecast_unit")
  data <- na.omit(data)

  if (is.null(by)) {
    by <- forecast_unit
  }

  # collapse several rows to 1, e.g. treat a set of 10 quantiles as one,
  # because they all belong to one single forecast that should be counted once
  collapse_by <- setdiff(
    c(forecast_unit, "quantile", "sample_id"),
    collapse
  )
  # filter out "quantile" or "sample" if present in collapse_by, but not data
  collapse_by <- intersect(collapse_by, names(data))

  data <- data[data[, .I[1], by = collapse_by]$V1]

  # count number of rows = number of forecasts
  out <- data[, .(count = .N), by = by]

  # make sure that all combinations in "by" are included in the output (with
  # count = 0). To achieve that, take the unique values in data and expand grid
  col_vecs <- unclass(out)
  col_vecs$count <- NULL
  col_vecs <- lapply(col_vecs, unique)
  out_empty <- expand.grid(col_vecs, stringsAsFactors = FALSE)

  out <- merge(out, out_empty, by = by, all.y = TRUE)
  out[, count := nafill(count, fill = 0)]

  return(out[])
}

#' @title Display Number of Forecasts Available
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
#' 'prediction' and 'true value') plus "quantile" or "sample" where present).
#'
#' @param collapse character vector (default is `c("quantile", "sample"`) with
#' names of categories for which the number of rows should be collapsed to one
#' when counting. For example, a single forecast is usually represented by a
#' set of several quantiles or samples and collapsing these to one makes sure
#' that a single forecast only gets counted once.
#'
#' @return A data.table with columns as specified in `by` and an additional
#' column with the number of forecasts.
#'
#' @inheritParams score
#' @importFrom data.table .I .N
#' @export
#' @keywords check-forecasts
#' @examples
#' data.table::setDTthreads(1) # only needed to avoid issues on CRAN
#'
#' avail_forecasts(example_quantile,
#'   collapse = c("quantile"),
#'   by = c("model", "target_type")
#' )
avail_forecasts <- function(data,
                            by = NULL,
                            collapse = c("quantile", "sample")) {

  check_data <- check_forecasts(data)


  data <- check_data$cleaned_data
  forecast_unit <- check_data$forecast_unit

  if (is.null(by)) {
    by <- forecast_unit
  }

  # collapse several rows to 1, e.g. treat a set of 10 quantiles as one,
  # because they all belong to one single forecast that should be counted once
  collapse_by <- setdiff(
    c(forecast_unit, "quantile", "sample"),
    collapse
  )
  # filter out "quantile" or "sample" if present in collapse_by, but not data
  collapse_by <- intersect(collapse_by, names(data))

  data <- data[data[, .I[1], by = collapse_by]$V1]

  # count number of rows = number of forecasts
  out <- data[, .(`Number forecasts` = .N), by = by]

  return(out[])
}

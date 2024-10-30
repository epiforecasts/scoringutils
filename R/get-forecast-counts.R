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
#' @returns A data.table with columns as specified in `by` and an additional
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


#' @title Visualise the number of available forecasts
#'
#' @description
#' Visualise Where Forecasts Are Available.
#' @param forecast_counts A data.table (or similar) with a column `count`
#'   holding forecast counts, as produced by [get_forecast_counts()].
#' @param x Character vector of length one that denotes the name of the column
#'   to appear on the x-axis of the plot.
#' @param y Character vector of length one that denotes the name of the column
#'   to appear on the y-axis of the plot. Default is "model".
#' @param x_as_factor Logical (default is `TRUE`). Whether or not to convert
#'   the variable on the x-axis to a factor. This has an effect e.g. if dates
#'   are shown on the x-axis.
#' @param show_counts Logical (default is `TRUE`) that indicates whether
#'   or not to show the actual count numbers on the plot.
#' @returns A ggplot object with a plot of forecast counts
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#'   geom_tile scale_fill_gradient .data
#' @importFrom data.table dcast .I .N
#' @importFrom checkmate assert_subset assert_logical
#' @export
#' @examples
#' library(ggplot2)
#' library(magrittr) # pipe operator
#' forecast_counts <- example_quantile %>%
#'   as_forecast_quantile %>%
#'   get_forecast_counts(by = c("model", "target_type", "target_end_date"))
#' plot_forecast_counts(
#'  forecast_counts, x = "target_end_date", show_counts = FALSE
#' ) +
#'  facet_wrap("target_type")

plot_forecast_counts <- function(forecast_counts,
                                 x,
                                 y = "model",
                                 x_as_factor = TRUE,
                                 show_counts = TRUE) {

  forecast_counts <- ensure_data.table(forecast_counts)
  assert_subset(y, colnames(forecast_counts))
  assert_subset(x, colnames(forecast_counts))
  assert_logical(x_as_factor, len = 1)
  assert_logical(show_counts, len = 1)

  if (x_as_factor) {
    forecast_counts[, eval(x) := as.factor(get(x))]
  }

  setnames(forecast_counts, old = "count", new = "Count")

  plot <- ggplot(
    forecast_counts,
    aes(y = .data[[y]], x = .data[[x]])
  ) +
    geom_tile(aes(fill = `Count`),
              width = 0.97, height = 0.97) +
    scale_fill_gradient(
      low = "grey95", high = "steelblue",
      na.value = "lightgrey"
    ) +
    theme_scoringutils() +
    theme(
      axis.text.x = element_text(
        angle = 90, vjust = 1,
        hjust = 1
      )
    ) +
    theme(panel.spacing = unit(2, "lines"))
  if (show_counts) {
    plot <- plot +
      geom_text(aes(label = `Count`))
  }
  return(plot)
}

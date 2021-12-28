#' @title Display Number of Forecasts Available
#'
#' @description
#'
#' Given a data set with forecasts, count the number of available forecasts
#' for arbitrary grouping (e.g. the number of forecasts per model, or the
#' number of forecasts per model and location).
#' This is useful to determine whether there are any missing forecasts.
#'
#' @param data data.frame with predictions in the same format required for
#' [score()]
#' @param by character vector or `NULL` (the default) that
#' denotes the categories over which the number of forecasts should be counted.
#' By default (`by = NULL`) this will be the unit of a single forecast (i.e.
#' all available columns (apart from a few "protected" columns such as
#' 'prediction' and 'true value') plus "quantile" or "sample" where present).
#' @param collapse character vector (default is `c("quantile", "sample"`) with
#' names of categories for which the number of rows should be collapsed to one
#' when counting. For example, a single forecast is usually represented by a
#' set of several quantiles or samples and collapsing these to one makes sure
#' that a single forecast only gets counted once.
#' @return A data.table with columns as specified in `by` and an additional
#' column with the number of forecasts.
#' @importFrom data.table .I .N
#' @export
#'
#' @examples
#' avail_forecasts(example_quantile,
#'   collapse = c("quantile"),
#'   by = c("model", "target_type")
#' )
avail_forecasts <- function(data,
                            by = NULL,
                            collapse = c("quantile", "sample")) {
  data <- check_clean_data(data, verbose = FALSE)

  forecast_unit <- get_unit_of_forecast(data)

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


#' @title Visualise Where Forecasts Are Available
#'
#' @description
#' Visualise Where Forecasts Are Available
#'
#' @param avail_forecasts data.frame with a column called `Number forecasts` as
#' produced by [avail_forecasts()]
#' @param y character vector of length one that denotes the name of the column
#' to appear on the y-axis of the plot. Default is "model".
#' @param x character vector of length one that denotes the name of the column
#' to appear on the x-axis of the plot. Default is "forecast_date".
#' @param make_x_factor logical (default is TRUE). Whether or not to convert
#' the variable on the x-axis to a factor. This has an effect e.g. if dates
#' are shown on the x-axis.
#' @param show_numbers logical (default is `TRUE`) that indicates whether
#' or not to show the actual count numbers on the plot
#' @return ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual
#' geom_tile scale_fill_gradient aes_string
#' @importFrom data.table dcast .I .N
#' @export
#'
#' @examples
#' library(scoringutils)
#' library(ggplot2)
#' avail_forecasts <- avail_forecasts(example_quantile,
#'   by = c(
#'     "model", "target_type",
#'     "target_end_date"
#'   )
#' )
#' plot_avail_forecasts(avail_forecasts,
#'   x = "target_end_date",
#'   show_numbers = FALSE
#' ) +
#'   facet_wrap("target_type")
plot_avail_forecasts <- function(avail_forecasts,
                                 y = "model",
                                 x = "forecast_date",
                                 make_x_factor = TRUE,
                                 show_numbers = TRUE) {
  avail_forecasts <- as.data.table(avail_forecasts)

  if (make_x_factor) {
    avail_forecasts[, eval(x) := as.factor(get(x))]
  }

  plot <- ggplot(
    avail_forecasts,
    aes_string(y = y, x = x)
  ) +
    geom_tile(aes(fill = `Number forecasts`),
      width = 0.97, height = 0.97
    ) +
    scale_fill_gradient(
      low = "grey95", high = "steelblue",
      na.value = "lightgrey"
    ) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(
        angle = 90, vjust = 1,
        hjust = 1
      )
    ) +
    theme(panel.spacing = unit(2, "lines"))

  if (show_numbers) {
    plot <- plot +
      geom_text(aes(label = `Number forecasts`))
  }

  return(plot)
}

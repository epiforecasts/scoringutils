#' @title Get quantile and interval coverage values for quantile-based forecasts
#'
#' @description
#' For a validated forecast object in a quantile-based format
#' (see [as_forecast_quantile()] for more information), this function computes:
#' - interval coverage of central prediction intervals
#' - quantile coverage for predictive quantiles
#' - the deviation between desired and actual coverage (both for interval and
#' quantile coverage)
#'
#' Coverage values are computed for a specific level of grouping, as specified
#' in the `by` argument. By default, coverage values are computed per model.
#'
#' **Interval coverage**
#'
#' Interval coverage for a given interval range is defined as the proportion of
#' observations that fall within the corresponding central prediction intervals.
#' Central prediction intervals are symmetric around the median and formed
#' by two quantiles that denote the lower and upper bound. For example, the 50%
#' central prediction interval is the interval between the 0.25 and 0.75
#' quantiles of the predictive distribution.
#'
#' **Quantile coverage**
#'
#' Quantile coverage for a given quantile level is defined as the proportion of
#' observed values that are smaller than the corresponding predictive quantile.
#' For example, the 0.5 quantile coverage is the proportion of observed values
#' that are smaller than the 0.5 quantile of the predictive distribution.
#' Just as above, for a single observation and the quantile of a single
#' predictive distribution, the value will either be `TRUE` or `FALSE`.
#'
#' **Coverage deviation**
#'
#' The coverage deviation is the difference between the desired coverage
#' (can be either interval or quantile coverage) and the
#' actual coverage. For example, if the desired coverage is 90% and the actual
#' coverage is 80%, the coverage deviation is -0.1.
#' @return
#' A data.table with columns as specified in `by` and additional
#' columns for the coverage values described above
#' @inheritParams score
#' @param by character vector that denotes the level of grouping for which the
#'   coverage values should be computed. By default (`"model"`), one coverage
#'   value per model will be returned.
#' @return
#' a data.table with columns "interval_coverage",
#' "interval_coverage_deviation", "quantile_coverage",
#' "quantile_coverage_deviation" and the columns specified in `by`.
#' @importFrom data.table setcolorder
#' @importFrom checkmate assert_subset
#' @examples
#' library(magrittr) # pipe operator
#' example_quantile %>%
#'   as_forecast_quantile() %>%
#'   get_coverage(by = "model")
#' @export
#' @keywords scoring
#' @export
get_coverage <- function(forecast, by = "model") {
  # input checks ---------------------------------------------------------------
  forecast <- clean_forecast(forecast, copy = TRUE, na.omit = TRUE)
  assert_subset(get_forecast_type(forecast), "quantile")

  # remove "quantile_level" and "interval_range" from `by` if present, as these
  # are included anyway
  by <- setdiff(by, c("quantile_level", "interval_range"))
  assert_subset(by, names(forecast))

  # convert to wide interval format and compute interval coverage --------------
  interval_forecast <- quantile_to_interval(forecast, format = "wide")
  interval_forecast[,
    interval_coverage := (observed <= upper) & (observed >= lower)
  ][, c("lower", "upper", "observed") := NULL]
  interval_forecast[, interval_coverage_deviation :=
                      interval_coverage - interval_range / 100]

  # merge interval range data with original data -------------------------------
  # preparations
  forecast[, interval_range := get_range_from_quantile(quantile_level)]
  forecast_cols <- colnames(forecast) # store so we can reset column order later
  forecast_unit <- get_forecast_unit(forecast)

  forecast <- merge(forecast, interval_forecast,
                    by = unique(c(forecast_unit, "interval_range")))

  # compute quantile coverage and deviation ------------------------------------
  forecast[, quantile_coverage := observed <= predicted]
  forecast[, quantile_coverage_deviation := quantile_coverage - quantile_level]

  # summarise coverage values according to `by` and cleanup --------------------
  # reset column order
  new_metrics <- c("interval_coverage", "interval_coverage_deviation",
                   "quantile_coverage", "quantile_coverage_deviation")
  setcolorder(forecast, unique(c(forecast_cols, "interval_range", new_metrics)))
  # remove forecast class and convert to regular data.table
  forecast <- as.data.table(forecast)
  by <- unique(c(by, "quantile_level", "interval_range"))
  # summarise
  forecast <- forecast[, lapply(.SD, mean), by = by, .SDcols = new_metrics]
  return(forecast[])
}


#' @title Plot interval coverage
#'
#' @description
#' Plot interval coverage values (see [get_coverage()] for more information).
#'
#' @param coverage A data frame of coverage values as produced by
#' [get_coverage()].
#' @param colour According to which variable shall the graphs be coloured?
#' Default is "model".
#' @returns ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual .data
#' facet_wrap facet_grid geom_polygon geom_line xlab ylab
#' @importFrom checkmate assert_subset
#' @importFrom data.table dcast
#' @export
#' @examples
#' \dontshow{
#'   data.table::setDTthreads(2) # restricts number of cores used on CRAN
#' }
#' example <- as_forecast_quantile(example_quantile)
#' coverage <- get_coverage(example, by = "model")
#' plot_interval_coverage(coverage)
plot_interval_coverage <- function(coverage,
                                   colour = "model") {
  coverage <- ensure_data.table(coverage)
  assert_subset(colour, names(coverage))

  # in case quantile columns are present, remove them and then take unique
  # values. This doesn't visually affect the plot, but prevents lines from being
  # drawn twice.
  del <- c("quantile_level", "quantile_coverage", "quantile_coverage_deviation")
  suppressWarnings(coverage[, eval(del) := NULL])
  coverage <- unique(coverage)

  ## overall model calibration - empirical interval coverage
  p1 <- ggplot(coverage, aes(
    x = interval_range,
    colour = .data[[colour]]
  )) +
    geom_polygon(
      data = data.frame(
        x = c(0, 0, 100),
        y = c(0, 100, 100),
        g = c("o", "o", "o"),
        stringsAsFactors = TRUE
      ),
      aes(
        x = x, y = y, group = g,
        fill = g
      ),
      alpha = 0.05,
      colour = "white",
      fill = "olivedrab3"
    ) +
    geom_line(
      aes(y = interval_range),
      colour = "grey",
      linetype = "dashed"
    ) +
    geom_line(aes(y = interval_coverage * 100)) +
    theme_scoringutils() +
    ylab("% Obs inside interval") +
    xlab("Nominal interval coverage") +
    coord_cartesian(expand = FALSE)

  return(p1)
}


#' @title Plot quantile coverage
#'
#' @description
#' Plot quantile coverage values (see [get_coverage()] for more information).
#'
#' @inheritParams plot_interval_coverage
#' @param colour String, according to which variable shall the graphs be
#' coloured? Default is "model".
#' @returns A ggplot object with a plot of interval coverage
#' @importFrom ggplot2 ggplot scale_colour_manual scale_fill_manual .data aes
#'   scale_y_continuous geom_line
#' @importFrom checkmate assert_subset assert_data_frame
#' @importFrom data.table dcast
#' @export
#' @examples
#' example <- as_forecast_quantile(example_quantile)
#' coverage <- get_coverage(example, by = "model")
#' plot_quantile_coverage(coverage)

plot_quantile_coverage <- function(coverage,
                                   colour = "model") {
  coverage <- assert_data_frame(coverage)
  assert_subset(colour, names(coverage))

  p2 <- ggplot(
    data = coverage,
    aes(x = quantile_level, colour = .data[[colour]])
  ) +
    geom_polygon(
      data = data.frame(
        x = c(
          0, 0.5, 0.5,
          0.5, 0.5, 1
        ),
        y = c(
          0, 0, 0.5,
          0.5, 1, 1
        ),
        g = c("o", "o", "o"),
        stringsAsFactors = TRUE
      ),
      aes(
        x = x, y = y, group = g,
        fill = g
      ),
      alpha = 0.05,
      colour = "white",
      fill = "olivedrab3"
    ) +
    geom_line(
      aes(y = quantile_level),
      colour = "grey",
      linetype = "dashed"
    ) +
    geom_line(aes(y = quantile_coverage)) +
    theme_scoringutils() +
    xlab("Quantile level") +
    ylab("% Obs below quantile level") +
    scale_y_continuous(
      labels = function(x) {
        paste(100 * x)
      }
    ) +
    coord_cartesian(expand = FALSE)

  return(p2)
}

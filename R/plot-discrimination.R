#' @title Plot discrimination for binary forecasts
#'
#' @description
#' Visualise the discrimination ability of binary forecasts by plotting the
#' distribution of predicted probabilities, stratified by the observed outcome.
#' A well-discriminating model will show clearly separated distributions for
#' the two observed levels.
#'
#' @param forecast A `forecast_binary` object (see [as_forecast_binary()]).
#' @param type Character, either `"histogram"` (default) or `"density"`.
#'   `"histogram"` shows a histogram with proportions on the y-axis;
#'   `"density"` shows kernel density curves.
#' @param ... Additional arguments passed to [ggplot2::geom_histogram()] or
#'   [ggplot2::geom_density()], depending on `type`. For example, `bins` or
#'   `binwidth` for histograms, or `bw` and `adjust` for density plots.
#' @returns A ggplot object showing the distribution of predicted
#'   probabilities, coloured by observed outcome level.
#' @importFrom ggplot2 ggplot aes geom_density geom_histogram
#'   after_stat labs .data
#' @export
#' @examples
#' library(ggplot2)
#' forecast <- as_forecast_binary(na.omit(example_binary))
#'
#' plot_discrimination(forecast)
#'
#' plot_discrimination(forecast, type = "density")
#'
#' plot_discrimination(forecast, bins = 10)
#'
#' plot_discrimination(forecast) +
#'   facet_wrap(~model)

plot_discrimination <- function(forecast, type = c("histogram", "density"),
                                ...) {
  assert_forecast(forecast, forecast_type = "binary", verbose = FALSE)
  type <- match.arg(type)

  plot <- ggplot(
    forecast,
    aes(x = .data[["predicted"]], fill = .data[["observed"]])
  )

  if (type == "density") {
    plot <- plot +
      geom_density(alpha = 0.5, ...) + # nolint: object_usage_linter.
      labs(y = "Density")
  } else {
    # `count` and `group` are columns created by `stat_bin()` at plot-build
    # time and referenced via `after_stat()`. `object_usage_linter` cannot see
    # these (or the conditionally used geoms) statically, so it is disabled for
    # this block.
    plot <- plot +
      geom_histogram( # nolint: object_usage_linter.
        aes(y = after_stat( # nolint: object_usage_linter.
          ave(count, group, FUN = function(x) x / sum(x)) # nolint: object_usage_linter.
        )),
        position = "identity", alpha = 0.5, ...
      ) +
      labs(y = "Proportion")
  }

  plot <- plot +
    labs(
      x = "Predicted probability",
      fill = "Observed"
    ) +
    theme_scoringutils()

  return(plot)
}

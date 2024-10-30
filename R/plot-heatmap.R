#' @title Create a heatmap of a scoring metric
#'
#' @description
#' This function can be used to create a heatmap of one metric across different
#' groups, e.g. the interval score obtained by several forecasting models in
#' different locations.
#'
#' @param scores A data.frame of scores based on quantile forecasts as
#' produced by [score()].
#' @param y The variable from the scores you want to show on the y-Axis. The
#'   default for this is "model"
#' @param x The variable from the scores you want to show on the x-Axis. This
#'   could be something like "horizon", or "location"
#' @param metric String, the metric that determines the value and colour shown
#'   in the tiles of the heatmap.
#' @returns A ggplot object showing a heatmap of the desired metric
#' @importFrom data.table setDT `:=`
#' @importFrom ggplot2 ggplot  aes geom_tile geom_text .data
#' scale_fill_gradient2 labs element_text coord_cartesian
#' @importFrom checkmate assert_subset
#' @export
#' @examples
#' library(magrittr) # pipe operator
#' scores <- example_quantile %>%
#'   as_forecast_quantile %>%
#'   score()
#' scores <- summarise_scores(scores, by = c("model", "target_type"))
#' scores <- summarise_scores(
#'   scores, by = c("model", "target_type"),
#'   fun = signif, digits = 2
#' )
#'
#' plot_heatmap(scores, x = "target_type", metric = "bias")

plot_heatmap <- function(scores,
                         y = "model",
                         x,
                         metric) {
  scores <- ensure_data.table(scores)
  assert_subset(y, names(scores))
  assert_subset(x, names(scores))
  assert_subset(metric, names(scores))

  plot <- ggplot(
    scores,
    aes(
      y = .data[[y]],
      x = .data[[x]],
      fill = .data[[metric]]
    )
  ) +
    geom_tile() +
    geom_text(aes(label = .data[[metric]])) +
    scale_fill_gradient2(low = "steelblue", high = "salmon") +
    theme_scoringutils() +
    theme(axis.text.x = element_text(
      angle = 90, vjust = 1,
      hjust = 1
    )) +
    coord_cartesian(expand = FALSE)

  return(plot)
}

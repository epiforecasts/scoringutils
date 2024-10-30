#' @title Plot contributions to the weighted interval score
#'
#' @description
#' Visualise the components of the weighted interval score: penalties for
#' over-prediction, under-prediction and for high dispersion (lack of
#' sharpness).
#'
#' @param scores A data.table of scores based on quantile forecasts as
#'   produced by [score()] and summarised using [summarise_scores()].
#' @param x The variable from the scores you want to show on the x-Axis.
#'   Usually this will be "model".
#' @param relative_contributions Logical. Show relative contributions instead
#'   of absolute contributions? Default is `FALSE` and this functionality is not
#'   available yet.
#' @param flip Boolean (default is `FALSE`), whether or not to flip the axes.
#' @returns A ggplot object showing a contributions from the three components of
#'   the weighted interval score.
#' @importFrom ggplot2 ggplot aes geom_linerange facet_wrap labs
#' scale_fill_discrete coord_flip geom_col
#' theme theme_light unit guides guide_legend .data
#' @importFrom data.table melt
#' @importFrom checkmate assert_subset assert_logical
#' @returns A ggplot object with a visualisation of the WIS decomposition
#' @export
#' @examples
#' library(ggplot2)
#' library(magrittr) # pipe operator
#' scores <- example_quantile %>%
#'   as_forecast_quantile %>%
#'   score()
#' scores <- summarise_scores(scores, by = c("model", "target_type"))
#'
#' plot_wis(scores,
#'   x = "model",
#'   relative_contributions = TRUE
#' ) +
#'   facet_wrap(~target_type)
#' plot_wis(scores,
#'   x = "model",
#'   relative_contributions = FALSE
#' ) +
#'   facet_wrap(~target_type, scales = "free_x")
#' @references
#' Bracher J, Ray E, Gneiting T, Reich, N (2020) Evaluating epidemic forecasts
#' in an interval format. <https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618>

plot_wis <- function(scores,
                     x = "model",
                     relative_contributions = FALSE,
                     flip = FALSE) {
  # input checks
  scores <- ensure_data.table(scores)
  wis_components <- c("overprediction", "underprediction", "dispersion")
  assert(check_columns_present(scores, wis_components))
  assert_subset(x, names(scores))
  assert_logical(relative_contributions, len = 1)
  assert_logical(flip, len = 1)

  scores <- melt(
    scores,
    measure.vars = wis_components,
    variable.name = "wis_component_name",
    value.name = "component_value"
  )

  # stack or fill the geom_col position
  col_position <- ifelse(relative_contributions, "fill", "stack")

  plot <- ggplot(scores, aes(y = .data[[x]])) +
    geom_col(
      position = col_position,
      aes(x = component_value, fill = wis_component_name)
    ) +
    theme_scoringutils() +
    scale_fill_discrete(type = c("#DF536B", "#61D04F", "#2297E6")) +
    guides(fill = guide_legend(title = "WIS component")) +
    xlab("WIS contributions")

  if (flip) {
    plot <- plot +
      theme(
        panel.spacing = unit(4, "mm"),
        axis.text.x = element_text(
          angle = 90,
          vjust = 1,
          hjust = 1
        )
      ) +
      coord_flip()
  }

  return(plot)
}

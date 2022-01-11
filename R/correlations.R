#' @title Correlation Between Metrics
#'
#' @description
#' Calculate the correlation between different metrics for a data.frame of
#' scores as produced by [score()]
#'
#' @param scores A data.frame of scores as produced by
#' [score()]
#' @param metrics A character vector with the metrics to show. If set to
#' `NULL` (default), all metrics present in `scores` will
#' be shown
#' @return A data.table with correlations for the different metrics
#' @importFrom data.table setDT
#' @importFrom stats cor na.omit
#' @export
#'
#' @examples
#' library(scoringutils)
#' scores <- score(example_quantile)
#' correlation(scores)
correlation <- function(scores,
                        metrics = NULL) {
  metrics <- check_metrics(metrics)

  # check metrics are present
  metrics <- names(scores)[names(scores) %in% metrics]

  # remove all non metrics and non-numeric columns
  df <- scores[, .SD, .SDcols = sapply(
    scores,
    function(x) {
      (all(is.numeric(x))) && all(is.finite(x))
    }
  )]
  df <- df[, .SD, .SDcols = names(df) %in% metrics]

  # define correlation matrix
  cor_mat <- round(cor(as.matrix(df)), 2)

  correlations <- setDT(as.data.frame((cor_mat)),
    keep.rownames = TRUE
  )[, metric := rn][, rn := NULL]

  return(correlations)
}



#' @title Plot Correlation Between Metrics
#'
#' @description
#' Plots a heatmap of correlations between different metrics
#'
#' @param correlations A data.table of correlations between scores as produced
#' by [correlation()]
#' @return A ggplot2 object showing a coloured matrix of correlations
#' between metrics
#' @importFrom ggplot2 ggplot geom_tile geom_text aes scale_fill_gradient2
#' element_text labs coord_cartesian theme theme_light
#' @importFrom data.table setDT melt
#' @export
#'
#' @examples
#' library(scoringutils)
#' scores <- score(example_quantile)
#' correlations <- correlation(scores)
#' plot_correlation(correlations)
plot_correlation <- function(correlations) {

  # define function to obtain upper triangle of matrix
  get_lower_tri <- function(cormat) {
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }

  metrics <- names(correlations)[names(correlations) %in% available_metrics()]

  lower_triangle <- get_lower_tri(correlations[, .SD, .SDcols = metrics])
  rownames(lower_triangle) <- colnames(lower_triangle)

  # get plot data.frame
  plot_df <- as.data.table(lower_triangle)[, metric := metrics]
  plot_df <- na.omit(data.table::melt(plot_df, id.vars = "metric"))

  # refactor levels according to the metrics
  # metrics <- unique(plot_df$metric)
  plot_df[, metric := factor(metric, levels = metrics)]
  plot_df[, variable := factor(variable, rev(metrics))]

  plot <- ggplot2::ggplot(plot_df, ggplot2::aes(
    x = variable, y = metric,
    fill = value
  )) +
    ggplot2::geom_tile(
      color = "white",
      width = 0.97, height = 0.97
    ) +
    ggplot2::geom_text(ggplot2::aes(y = metric, label = value)) +
    ggplot2::scale_fill_gradient2(
      low = "steelblue", mid = "white",
      high = "salmon",
      name = "Correlation",
      breaks = c(-1, -0.5, 0, 0.5, 1)
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 1,
        hjust = 1
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(title = "Correlation between metrics")

  return(plot)
}

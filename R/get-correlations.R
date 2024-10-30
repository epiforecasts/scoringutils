#' @title Calculate correlation between metrics
#'
#' @description
#' Calculate the correlation between different metrics for a data.frame of
#' scores as produced by [score()].
#'
#' @param metrics A character vector with the metrics to show. If set to
#'   `NULL` (default), all metrics present in `scores` will be shown.
#' @inheritParams get_pairwise_comparisons
#' @param ... Additional arguments to pass down to [cor()].
#' @return
#' An object of class `scores` (a data.table with an additional
#' attribute `metrics` holding the names of the scores) with correlations
#' between different metrics
#' @importFrom data.table setDT
#' @importFrom stats cor na.omit
#' @importFrom cli cli_warn
#' @importFrom checkmate assert_subset
#' @export
#' @keywords scoring
#' @examples
#' library(magrittr) # pipe operator
#'
#' scores <- example_quantile %>%
#'  as_forecast_quantile() %>%
#'  score()
#'
#' get_correlations(scores)
get_correlations <- function(scores,
                             metrics = get_metrics.scores(scores),
                             ...) {
  scores <- ensure_data.table(scores)
  assert_subset(metrics, colnames(scores), empty.ok = FALSE)
  df <- scores[, .SD, .SDcols = names(scores) %in% metrics]

  # define correlation matrix
  cor_mat <- cor(as.matrix(df), ...)

  correlations <- new_scores(
    as.data.frame((cor_mat)),
    metrics = metrics,
    keep.rownames = TRUE
  )
  correlations <- copy(correlations)[, metric := rn][, rn := NULL]

  return(correlations[])
}


#' @title Plot correlation between metrics
#'
#' @description
#' Plots a heatmap of correlations between different metrics.
#'
#' @param correlations A data.table of correlations between scores as produced
#'   by [get_correlations()].
#' @param digits A number indicating how many decimal places the correlations
#'   should be rounded to. By default (`digits = NULL`) no rounding takes place.
#' @return
#' A ggplot object showing a coloured matrix of correlations between metrics.
#' @importFrom ggplot2 ggplot geom_tile geom_text aes scale_fill_gradient2
#' element_text labs coord_cartesian theme element_blank
#' @importFrom data.table setDT melt
#' @importFrom checkmate assert_data_frame
#' @export
#' @returns A ggplot object with a visualisation of correlations between metrics
#' @examples
#' library(magrittr) # pipe operator
#' scores <- example_quantile %>%
#'   as_forecast_quantile %>%
#'   score()
#' correlations <- scores %>%
#'   summarise_scores() %>%
#'   get_correlations()
#' plot_correlations(correlations, digits = 2)

plot_correlations <- function(correlations, digits = NULL) {

  assert_data_frame(correlations)
  metrics <- get_metrics.scores(correlations, error = TRUE)

  lower_triangle <- get_lower_tri(correlations[, .SD, .SDcols = metrics])

  if (!is.null(digits)) {
    lower_triangle <- round(lower_triangle, digits)
  }


  # check correlations is actually a matrix of correlations
  col_present <- check_columns_present(correlations, "metric")
  if (any(lower_triangle > 1, na.rm = TRUE) || !isTRUE(col_present)) {
    #nolint start: keyword_quote_linter
    cli_abort(
      c(
        "Found correlations > 1 or missing `metric` column.",
        "i" = "Did you forget to call {.fn scoringutils::get_correlations}?"
      )
    )
    #nolint end
  }

  rownames(lower_triangle) <- colnames(lower_triangle)

  # get plot data.frame
  plot_df <- data.table::as.data.table(lower_triangle)[, metric := metrics]
  plot_df <- na.omit(data.table::melt(plot_df, id.vars = "metric"))

  # refactor levels according to the metrics
  plot_df[, metric := factor(metric, levels = metrics)]
  plot_df[, variable := factor(variable, rev(metrics))]

  plot <- ggplot(plot_df, aes(
    x = variable, y = metric,
    fill = value
  )) +
    geom_tile(
      color = "white",
      width = 0.97, height = 0.97
    ) +
    geom_text(aes(y = metric, label = value)) +
    scale_fill_gradient2(
      low = "steelblue", mid = "white",
      high = "salmon",
      name = "Correlation",
      breaks = c(-1, -0.5, 0, 0.5, 1)
    ) +
    theme_scoringutils() +
    theme(
      axis.text.x = element_text(
        angle = 90, vjust = 1,
        hjust = 1
      )
    ) +
    labs(x = "", y = "") +
    coord_cartesian(expand = FALSE)
  return(plot)
}


# helper function to obtain lower triangle of matrix
get_lower_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

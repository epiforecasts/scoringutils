#' @title Correlation Between Metrics
#'
#' @description
#' Calculate the correlation between different metrics for a data.frame of
#' scores as produced by [score()].
#'
#' @param metrics A character vector with the metrics to show. If set to
#' `NULL` (default), all metrics present in `scores` will
#' be shown
#' @inheritParams pairwise_comparison
#' @return A data.table with correlations for the different metrics
#' @importFrom data.table setDT
#' @importFrom stats cor na.omit
#' @export
#' @keywords scoring
#' @examples
#' scores <- score(example_quantile)
#' correlation(scores)
correlation <- function(scores,
                        metrics = NULL) {
  metrics <- check_metrics(metrics)

  # check metrics are present
  metrics <- names(scores)[names(scores) %in% metrics]

  # if quantile column is present, throw a warning
  if ("quantile" %in% names(scores)) {
    warning("There is a column called 'quantile' in the scores. Usually, you should call 'summarise_scores()' to summarise over quantiles and obtain one score per forecast before calculating correlations. You can ignore this warning if you know what you're doing.")
  }

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

  return(correlations[])
}

# define function to obtain upper triangle of matrix
get_lower_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

#' @title Calculate correlation between metrics
#'
#' @description
#' Calculate the correlation between different metrics for a data.frame of
#' scores as produced by [score()].
#'
#' @param metrics A character vector with the metrics to show. If set to
#'   `NULL` (default), all metrics present in `scores` will be shown.
#' @param digits A number indicating how many decimal places the result should
#'   be rounded to. By default (`digits = NULL`) no rounding takes place.
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
#' scores <- score(as_forecast(example_quantile))
#' get_correlations(scores, digits = 2)
get_correlations <- function(scores,
                             metrics = get_metrics(scores),
                             digits = NULL,
                             ...) {
  scores <- ensure_data.table(scores)
  assert_subset(metrics, colnames(scores), empty.ok = FALSE)
  df <- scores[, .SD, .SDcols = names(scores) %in% metrics]

  # define correlation matrix
  cor_mat <- cor(as.matrix(df), ...)

  if (!is.null(digits)) {
    cor_mat <- round(cor_mat, digits)
  }

  correlations <- new_scores(
    as.data.frame((cor_mat)),
    metrics = metrics,
    keep.rownames = TRUE
  )
  correlations <- copy(correlations)[, metric := rn][, rn := NULL]

  return(correlations[])
}

get_lower_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

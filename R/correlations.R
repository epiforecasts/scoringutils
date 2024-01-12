#' @title Correlation Between Scoring rules
#'
#' @description
#' Calculate the correlation between different rules for a data.frame of
#' scores as produced by [score()].
#'
#' @param rules A character vector with the rules to show. If set to
#' `NULL` (default), all rules present in `scores` will
#' be shown
#' @param digits A number indicating how many decimal places the result should
#' be rounded to. By default (`digits = NULL`) no rounding takes place.
#' @inheritParams pairwise_comparison
#' @return A data.table with correlations for the different rules
#' @importFrom data.table setDT
#' @importFrom stats cor na.omit
#' @export
#' @keywords scoring
#' @examples
#' scores <- score(example_quantile)
#' correlation(scores, digits = 2)
correlation <- function(scores,
                        rules = NULL,
                        digits = NULL) {
  rules <- get_score_names(scores)

  # if quantile column is present, throw a warning
  if ("quantile" %in% names(scores)) {
    warning(
      "There is a column called 'quantile' in the scores. Usually, you ",
      "should call 'summarise_scores()' to summarise over quantiles and ",
      "obtain one score per forecast before calculating correlations. You ",
      "can ignore this warning if you know what you're doing."
    )
  }

  # remove all non rules and non-numeric columns
  df <- scores[, .SD, .SDcols = sapply(
    scores,
    function(x) {
      (all(is.numeric(x))) && all(is.finite(x))
    }
  )]
  df <- df[, .SD, .SDcols = names(df) %in% rules]

  # define correlation matrix
  cor_mat <- cor(as.matrix(df))

  if (!is.null(digits)) {
    cor_mat <- round(cor_mat, digits)
  }

  correlations <- setDT(as.data.frame((cor_mat)),
    keep.rownames = TRUE
  )[, rule := rn][, rn := NULL]

  return(correlations[])
}

# define function to obtain upper triangle of matrix
get_lower_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

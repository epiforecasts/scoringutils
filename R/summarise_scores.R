
#' @title Summarise scores as produced by [score()]
#'
#' @description Summarise scores as produced by [score()]-
#'
#' @param scores a data.table of unsummarised scores as produced by
#' [score()]
#' @inheritParams score
#' @param by character vector with column names to summarise scores by.
#'
#' @examples
#' library(scoringutils)
#' data <- example_quantile
#' scores <- score(data)
#'
#' # get scores by model
#' summarise_scores(scores, by = c("model"))
#'
#' # get scores by model and target type
#' summarise_scores(scores, by = c("model", "target_type"))
#'
#' # get standard deviation
#' summarise_scores(scores, by = "model", FUN = sd)
#'
#' # get quantiles of scores
#' # make sure to aggregate over ranges first
#' summarise_scores(scores, by = "model", FUN = quantile,
#'                  probs = c(0.25, 0.5, 0.75))
#'
#' # get ranges
#' # summarise_scores(scores, by = "range")
#'
#' @export

summarise_scores <- function(scores,
                             by,
                             FUN = mean,
                             ...) {

  # get names of columns to summarise over
  cols_to_summarise <- paste0(available_metrics(), collapse = "|")

  # takes the mean over ranges and quantiles first, if neither range nor
  # quantile are in `by`. Reason to do this is that summaries may be
  # inaccurate if we treat individual quantiles as independent forecasts
  forecast_unit <- scoringutils:::get_unit_of_forecast(scores)
  scores <- scores[, lapply(.SD, mean, ...),
                   by = c(unique(c(forecast_unit, by))),
                   .SDcols = colnames(scores) %like% cols_to_summarise]


  scores <- scores[, lapply(.SD, FUN, ...),
                   by = c(by),
                   .SDcols = colnames(scores) %like% cols_to_summarise]

  # if neither quantile nor range are in by, remove coverage and quantile_coverage
  if (!("range" %in% by) & ("coverage" %in% colnames(scores))) {
    scores[, c("coverage") := NULL]
  }
  if (!("quantile" %in% by) & "quantile_coverage" %in% names(scores)) {
    scores[, c("quantile_coverage") := NULL]
  }

  return(scores[])
}

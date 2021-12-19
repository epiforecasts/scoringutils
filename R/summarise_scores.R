#' @title Summarise scores as produced by [score()]
#'
#' @description Summarise scores as produced by [score()]-
#'
#' @param scores a data.table of unsummarised scores as produced by
#' [score()]
#' @inheritParams score
#' @param by character vector with column names to summarise scores by.
#' @param FUN a function used for summarising scores. Default is `mean`.
#'
#' @examples
#' library(scoringutils)
#' data <- example_quantile
#' scores <- score(data,
#'                 summarise_by = c("model", "target_type", "location",
#'                                  "horizon", "range", "quantile"))
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
  forecast_unit <- get_unit_of_forecast(scores)
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


#' @title Add coverage of central prediction intervals
#'
#' @description Adds a column with the coverage of central prediction intervals
#' to unsummarised scores as produced by [score()]
#'
#' @details
#' The coverage values that are added are computed according to the values
#' specified in `by`. If, for example, `by = "model"`, then there will be one
#' coverage value for every model and [add_coverage()] will compute the coverage
#' for every model across the values present in all other columns which define
#' the unit of a single forecast.
#'
#' @param scores a data.table of unsummarised scores as produced by
#' [score()]
#' @inheritParams score
#' @param by character vector with column names to add the coverage for.
#' @param ranges numeric vector of the ranges of the central prediction intervals
#' for which coverage values shall be added.
#' @return a data.table with unsummarised scores with columns added for the
#' coverage of the central prediction intervals. While the overall data.table
#' is still unsummarised, note that for the coverage columns some level of
#' summary is present according to the value specified in `by`.
#' @examples
#' library(scoringutils)
#' data <- example_quantile
#' score(data)
#' scores <- score(data,
#'                 summarise_by = c("model", "target_type", "location",
#'                                  "horizon", "range", "quantile"))
#'
#' # add coverage
#' scores <- add_coverage(scores, by = c("model", "target_type"))
#' summarise_scores(scores, by = c("model", "target_type"))
#'
#'
#' @export


add_coverage <- function(scores,
                         by,
                         ranges = c(50, 90)) {

  summarised_scores <- summarise_scores(
    scores,
    by = c(by, "range")
  )[range %in% ranges]


  # create cast formula
  cast_formula <-
    paste(
      paste(by, collapse = "+"),
      "~",
      "paste0('coverage_', range)"
    )

  coverages <- dcast(
    summarised_scores,
    value.var = "coverage",
    formula = cast_formula
  )

  scores_with_coverage <- merge(scores, coverages, by = by)
  return(scores_with_coverage)
}


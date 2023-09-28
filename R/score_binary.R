#' @title Evaluate forecasts in a Binary Format
#'
#' @inheritParams score
#' @param forecast_unit A character vector with the column names that define
#' the unit of a single forecast, i.e. a forecast was made for a combination
#' of the values in `forecast_unit`.
#'
#' @return A data.table with appropriate scores. For more information see
#' [score()].
#'
#' @importFrom data.table ':='
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @keywords internal

score_binary <- function(data,
                         forecast_unit,
                         metrics, ...) {

  # get a list of the functions to be applied.
  # this should be done by some helper function / more elegantly
  metrics <- as.list(metrics)
  if ("brier_score" %in% metrics) {
    names(metrics)[metrics == "brier_score"] <- "brier_score"
    metrics["brier_score"] <- list(brier_score)
  }
  if ("log_score" %in% metrics) {
    names(metrics)[metrics == "log_score"] <- "log_score"
    metrics["log_score"] <- list(logs_binary)
  }
  metrics <- metrics[sapply(metrics, is.function)]

  # need to check the functions used here
  # i.e. check the function has the relevant arguments.


  lapply(seq_along(metrics), function(function_to_call, ...) {
    metric_name <- names(metrics[function_to_call])
    fun <- metrics[[function_to_call]]

    fun_accepts_ellipsis <- any(names(formals(fun)) == "...")

    if (fun_accepts_ellipsis) {
      data[, (metric_name) := fun(true_value, prediction, ...)]
    } else {
      data[, (metric_name) := fun(true_value, prediction)]
    }

    return()
  }, ...)

  return(data[])
}

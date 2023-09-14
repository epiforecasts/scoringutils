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
#' @export
#' @keywords scoring


score.scoringutils_binary <- function(x,
                                      forecast_unit,
                                      metrics = NULL,
                                      ...) {

  data <- as.data.table(x)
  forecast_unit <- attr(x, "forecast_unit")

  metrics <- check_metrics(metrics)
  if ("brier_score" %in% metrics) {
    data[, "brier_score" := brier_score(true_value, prediction),
      by = forecast_unit
    ]
  }

  if ("log_score" %in% metrics) {
    data[, "log_score" := logs_binary(true_value, prediction),
      by = forecast_unit
    ]
  }

  return(data[])
}

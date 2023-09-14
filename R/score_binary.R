#' @title Evaluate forecasts in a Binary Format
#'
#' @inheritParams score
#' @inheritParams score.scoringutils_quantile
#' @return A data.table with appropriate scores. For more information see
#' [score()].
#'
#' @importFrom data.table ':='
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @export
#' @keywords scoring


score.scoringutils_binary <- function(x,
                                      metrics = NULL,
                                      ...) {

  data <- as.data.table(score_data)
  forecast_unit <- attr(data, "forecast_unit")

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

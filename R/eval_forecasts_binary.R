#' @title Evaluate forecasts in a Binary Format
#'
#' @inheritParams eval_forecasts
#' @param forecast_unit A character vector with the column names that define
#' the unit of a single forecast, i.e. a forecast was made for a combination
#' of the values in `forecast_unit`
#'
#' @return A data.table with appropriate scores. For more information see
#' [eval_forecasts()]
#'
#' @importFrom data.table ':='
#'
#' @examples
#' # Probability Forecast for Binary Target
#' binary_example <- data.table::setDT(scoringutils::binary_example_data)
#' eval <- scoringutils::eval_forecasts(data = binary_example,
#'                                      summarise_by = c("model"),
#'                                      quantiles = c(0.5), sd = TRUE)
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}

eval_forecasts_binary <- function(data,
                                  forecast_unit,
                                  metrics){

  res <- data[, "brier_score" := scoringutils::brier_score(true_value, prediction),
              by = forecast_unit]

  return(res[])
}



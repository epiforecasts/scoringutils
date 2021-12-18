#' @title Evaluate forecasts in a Binary Format
#'
#' @inheritParams score
#' @param forecast_unit A character vector with the column names that define
#' the unit of a single forecast, i.e. a forecast was made for a combination
#' of the values in `forecast_unit`
#'
#' @return A data.table with appropriate scores. For more information see
#' [score()]
#'
#' @importFrom data.table ':='
#'
#' @examples
#' # Probability Forecast for Binary Target
#' binary_example <- data.table::setDT(scoringutils::example_binary)
#' eval <- scoringutils::score(data = binary_example,
#'                                      summarise_by = c("model"),
#'                                      quantiles = c(0.5), sd = TRUE)
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}

score_binary <- function(data,
                                  forecast_unit,
                                  metrics){

  res <- data[, "brier_score" := scoringutils::brier_score(true_value, prediction),
              by = forecast_unit]

  return(res[])
}



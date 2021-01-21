#' @title Evaluate forecasts in a Binary Format
#'
#' @inheritParams eval_forecasts
#' @return A data.table with appropriate scores. For more information see
#' \code{\link{eval_forecasts}}
#'
#' @importFrom data.table ':='
#'
#' @examples
#' # Probability Forecast for Binary Target
#' binary_example <- data.table::setDT(scoringutils::binary_example_data)
#' eval <- scoringutils::eval_forecasts(data = binary_example,
#'                                      summarise_by = c("model"),
#'                                      quantiles = c(0.5), sd = TRUE,
#'                                      verbose = FALSE)
#'
#' @author Nikos Bosse \email{nikosbosse@gmail.com}

eval_forecasts_binary <- function(data,
                                  by,
                                  summarise_by,
                                  metrics,
                                  quantiles,
                                  sd,
                                  summarised,
                                  verbose){

  res <- data[, "brier_score" := scoringutils::brier_score(true_value, prediction),
              by = by]

  if (summarised) {
    # add quantiles
    if (!is.null(quantiles)) {
      res <- add_quantiles(res, "brier_score", quantiles, by = summarise_by)
    }

    # add standard deviation
    if (sd) {
      res <- add_sd(res, "brier_score", by = c(summarise_by))
    }

    # summarise by taking the mean over all relevant columns
    res <- data[, lapply(.SD, mean, na.rm = TRUE),
                .SDcols = colnames(res) %like% "brier",
                by = summarise_by]

  }
}



#' @title Evaluate forecasts in a Sample-Based Format (Integer or Continuous)
#'
#' @inheritParams score
#' @param prediction_type character, should be either "continuous" or "integer"
#' @param forecast_unit A character vector with the column names that define
#' the unit of a single forecast, i.e. a forecast was made for a combination
#' of the values in `forecast_unit`
#'
#' @return A data.table with appropriate scores. For more information see
#' [score()]
#'
#' @importFrom data.table ':=' as.data.table rbindlist %like%
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @inherit score references
#' @keywords internal

score_sample <- function(data,
                         forecast_unit,
                         metrics,
                         prediction_type) {
  if (missing(prediction_type)) {
    if (isTRUE(all.equal(data$prediction, as.integer(data$prediction)))) {
      prediction_type <- "integer"
    } else {
      prediction_type <- "continuous"
    }
  }

  # calculate scores -----------------------------------------------------------
  # sharpness
  if (any(c("sharpness", "mad") %in% metrics)) {
    data[, mad := mad_sample(t(prediction)), by = forecast_unit]
  }
  # bias
  if ("bias" %in% metrics) {
    data[, bias := bias_sample(
      unique(true_value),
      t(prediction)
    ), by = forecast_unit]
  }
  # DSS
  if ("dss" %in% metrics) {
    data[, dss := scoringutils::dss(
      unique(true_value),
      t(prediction)
    ), by = forecast_unit]
  }
  # CRPS
  if ("crps" %in% metrics) {
    data[, crps := scoringutils::crps(
      unique(true_value),
      t(prediction)
    ), by = forecast_unit]
  }
  # Log Score
  if ("log_score" %in% metrics) {
    # only compute if prediction type is continuous
    if (prediction_type == "continuous") {
      data[, log_score := scoringutils::logs(
        unique(true_value),
        t(prediction)
      ), by = forecast_unit]
    }
  }
  # coverage
  if ("coverage" %in% metrics) {
  }

  res <- data.table::copy(data)

  # make scores unique to avoid redundancy.
  res <- res[, lapply(.SD, unique),
    .SDcols = colnames(res) %like% "pit_|bias|sharpness|dss|crps|log_score|pit",
    by = forecast_unit
  ]

  return(res[])
}

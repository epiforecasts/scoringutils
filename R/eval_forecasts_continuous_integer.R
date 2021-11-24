#' @title Evaluate forecasts in a Sample-Based Format (Integer or Continuous)
#'
#' @inheritParams eval_forecasts
#' @param prediction_type character, should be either "continuous" or "integer"
#'
#' @return A data.table with appropriate scores. For more information see
#' [eval_forecasts()]
#'
#' @importFrom data.table ':=' as.data.table rbindlist %like%
#'
#'
#' @examples
#'
#' ## Integer Forecasts
#' integer_example <- data.table::setDT(scoringutils::integer_example_data)
#' eval <- scoringutils::eval_forecasts(integer_example,
#'                                      summarise_by = c("model"),
#'                                      quantiles = c(0.1, 0.9),
#'                                      sd = TRUE,
#'                                      pit_plots = TRUE)
#' eval <- scoringutils::eval_forecasts(integer_example)
#'
#' ## Continuous Forecasts
#' continuous_example <- data.table::setDT(scoringutils::continuous_example_data)
#' eval <- scoringutils::eval_forecasts(continuous_example)#'
#'
#' eval <- scoringutils::eval_forecasts(continuous_example,
#'                                      quantiles = c(0.5, 0.9),
#'                                      sd = TRUE,
#'                                      summarise_by = c("model"))
#'
#' @author Nikos Bosse \email{nikosbosse@@gmail.com}
#' @inherit eval_forecasts references


eval_forecasts_sample <- function(data,
                                  by,
                                  summarise_by,
                                  metrics,
                                  prediction_type,
                                  quantiles,
                                  sd,
                                  pit_plots) {

  if (missing(prediction_type)) {
    if (all.equal(data$prediction, as.integer(data$prediction)) == TRUE) {
      prediction_type <- "integer"
    } else {
      prediction_type <- "continuous"
    }
  }

  # calculate scores -----------------------------------------------------------
  # sharpness
  if ("sharpness" %in% metrics) {
    data[, sharpness := scoringutils::sharpness(t(prediction)), by = c(by)]
  }
  # bias
  if ("bias" %in% metrics) {
    data[, bias := scoringutils::bias(unique(true_value),
                                      t(prediction)), by = c(by)]
  }
  # DSS
  if ("dss" %in% metrics) {
    data[, dss := scoringutils::dss(unique(true_value),
                                    t(prediction)), by = c(by)]
  }
  # CRPS
  if ("crps" %in% metrics) {
    data[, crps := scoringutils::crps(unique(true_value),
                                      t(prediction)), by = c(by)]
  }
  # Log Score
  if ("log_score" %in% metrics) {
    # only compute if prediction type is continuous
    if (prediction_type == "continuous") {
      data[, log_score := scoringutils::logs(unique(true_value),
                                             t(prediction)), by = c(by)]
    }
  }
  # coverage
  if ("coverage" %in% metrics) {
  }

  res <- data.table::copy(data)

  # make scores unique to avoid redundancy.
  res <- res[, lapply(.SD, unique),
             .SDcols = colnames(res) %like% "pit_|bias|sharpness|dss|crps|log_score|pit",
             by = c(by)]

  # summarise output if desired ------------------------------------------------
  # add quantiles
  if (!is.null(quantiles)) {
    quantile_vars <- c("crps", "dss", "log_score", "pit_p_val", "bias", "sharpness")
    res <- add_quantiles(res, quantile_vars, quantiles, by = c(summarise_by))
  }

  if (sd) {
    # add standard deviations
    sd_vars <- c("crps", "dss", "log_score", "bias", "sharpness")
    res <- add_sd(res, sd_vars, by = c(summarise_by))
  }

  # take mean
  res <- res[, lapply(.SD, mean, na.rm = TRUE),
             .SDcols = colnames(res) %like% "pit_|bias|sharpness|dss|crps|log_score",
             by = summarise_by]

  return(res[])
}

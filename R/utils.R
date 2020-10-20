globalVariables(c(".",
                  ".SD",
                  "boundary",
                  "Brier_score",
                  "count",
                  "coverage_deviation",
                  "CRPS",
                  "DSS",
                  "identif",
                  "Interval_Score",
                  "overprediction",
                  "underprediction",
                  "LogS",
                  "calibration",
                  "coverage",
                  "hist",
                  "id",
                  "log_score",
                  "lower",
                  "metric",
                  "metrics_select",
                  "model",
                  "pit_p_val",
                  "predictions",
                  "quantile",
                  "rn",
                  "true_values",
                  "type",
                  "upper",
                  "value",
                  "value_scaled",
                  "variable",
                  "x"))


list_of_avail_metrics <- function() {
  available_metrics <- c("log_score", "sharpness", "bias", "dss", "crps",
                         "calibration", "coverage_deviation",
                         "pit_p_val", "pit_sd","interval_score",
                         "is_underprediction", "is_overprediction")

  return(available_metrics)
}

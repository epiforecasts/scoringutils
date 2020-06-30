globalVariables(c(".",
                  ".SD",
                  "boundary",
                  "Brier_score",
                  "CRPS",
                  "DSS",
                  "Interval_Score",
                  "LogS",
                  "calibration",
                  "hist",
                  "id",
                  "lower",
                  "model",
                  "predictions",
                  "true_values",
                  "type",
                  "upper"))



quantile_to_wide <- function(data) {
  data.table::dcast(data, ... ~ boundary + range,
                    value.var = "predictions")
}




quantile_to_long <- function(data) {
  colnames <- colnames(data)
  ranges <- colnames[grepl("lower", colnames) | grepl("upper", colnames)]

  data <- data.table::melt(data,
                           id.vars = c("id", "true_values", "model"),
                           measure.vars = ranges,
                           variable.name = "range",
                           value.name = "predictions")
  data[, boundary := gsub("_.*", "", range)]
  data[, range := as.numeric(gsub("^.*?_","", range))]

  return(data)
}

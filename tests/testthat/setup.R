# load common required test packages
library(ggplot2, quietly = TRUE)
library(data.table)
suppressMessages(library(magrittr))
data.table::setDTthreads(2) # restricts number of cores used on CRAN

metrics_no_cov <- metrics_quantile(
  exclude = c("interval_coverage_50", "interval_coverage_90",
              "interval_coverage_deviation")
)
metrics_no_cov_no_ae <- metrics_quantile(
  exclude = c("interval_coverage_50", "interval_coverage_90",
              "interval_coverage_deviation", "ae_median")
)


# compute scores
scores_quantile <- suppressMessages(score(as_forecast(example_quantile)))
scores_continuous <- suppressMessages(score(as_forecast(example_continuous)))
scores_point <- suppressMessages(score(as_forecast(example_point)))
scores_binary <- suppressMessages(score(as_forecast(example_binary)))


# load common required test packages
library(ggplot2, quietly = TRUE)
library(data.table)
suppressMessages(library(magrittr))

metrics_no_cov <- rules_quantile(
  exclude = c("interval_coverage_50", "interval_coverage_90",
              "interval_coverage_deviation")
)
metrics_no_cov_no_ae <- rules_quantile(
  exclude = c("interval_coverage_50", "interval_coverage_90",
              "interval_coverage_deviation", "ae_median")
)


# compute scores
scores_quantile <- suppressMessages(score(example_quantile))
scores_continuous <- suppressMessages(score(data = example_continuous))
scores_point <- suppressMessages(score(example_point))
scores_binary <- suppressMessages(score(example_binary))

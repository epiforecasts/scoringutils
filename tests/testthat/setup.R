# load common required test packages
library(ggplot2, quietly = TRUE)
library(data.table)
suppressMessages(library(magrittr))

metrics_no_cov <- metrics_quantile[!grepl("coverage", names(metrics_quantile))]
metrics_no_cov_no_ae <- metrics_no_cov[!grepl("ae", names(metrics_no_cov))]


# compute scores
scores_quantile <- suppressMessages(score(example_quantile))
scores_continuous <- suppressMessages(score(data = example_continuous))
scores_point <- suppressMessages(score(example_point))
scores_binary <- suppressMessages(score(example_binary))

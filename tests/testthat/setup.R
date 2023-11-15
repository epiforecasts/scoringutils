# load common required test packages
library(ggplot2, quietly = TRUE)
library(data.table)
suppressMessages(library(magrittr))

# compute quantile scores
scores_quantile <- suppressMessages(score(example_quantile))
scores_continuous <- suppressMessages(score(data = example_continuous))
scores_point <- suppressMessages(score(example_point))
scores_binary <- suppressMessages(score(example_binary))

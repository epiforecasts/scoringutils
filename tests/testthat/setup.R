# load common required test packages
library(ggplot2, quietly = TRUE)
suppressMessages(library(magrittr))

# compute quantile scores
scores <- suppressMessages(score(example_quantile))
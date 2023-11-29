# load common required test packages
library(ggplot2, quietly = TRUE)
suppressMessages(library(magrittr))
data.table::setDTthreads(2) # only needed to avoid issues on CRAN

# compute quantile scores
scores <- suppressMessages(score(example_quantile))

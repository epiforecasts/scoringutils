% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise-comparisons.R
\name{plot_pairwise_comparisons}
\alias{plot_pairwise_comparisons}
\title{Plot heatmap of pairwise comparisons}
\usage{
plot_pairwise_comparisons(
  comparison_result,
  type = c("mean_scores_ratio", "pval")
)
}
\arguments{
\item{comparison_result}{A data.frame as produced by
\code{\link[=get_pairwise_comparisons]{get_pairwise_comparisons()}}.}

\item{type}{Character vector of length one that is either
"mean_scores_ratio" or "pval". This denotes whether to
visualise the ratio or the p-value of the pairwise comparison.
Default is "mean_scores_ratio".}
}
\value{
A ggplot object with a heatmap of mean score ratios from pairwise
comparisons.
}
\description{
Creates a heatmap of the ratios or pvalues from a pairwise comparison
between models.
}
\examples{
library(ggplot2)
library(magrittr) # pipe operator
scores <- example_quantile \%>\%
  as_forecast_quantile \%>\%
  score()
pairwise <- get_pairwise_comparisons(scores, by = "target_type")
plot_pairwise_comparisons(pairwise, type = "mean_scores_ratio") +
  facet_wrap(~target_type)
}

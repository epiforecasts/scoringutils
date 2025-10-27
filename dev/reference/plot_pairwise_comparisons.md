# Plot heatmap of pairwise comparisons

Creates a heatmap of the ratios or pvalues from a pairwise comparison
between models.

## Usage

``` r
plot_pairwise_comparisons(
  comparison_result,
  type = c("mean_scores_ratio", "pval")
)
```

## Arguments

- comparison_result:

  A data.frame as produced by
  [`get_pairwise_comparisons()`](https://epiforecasts.io/scoringutils/dev/reference/get_pairwise_comparisons.md).

- type:

  Character vector of length one that is either "mean_scores_ratio" or
  "pval". This denotes whether to visualise the ratio or the p-value of
  the pairwise comparison. Default is "mean_scores_ratio".

## Value

A ggplot object with a heatmap of mean score ratios from pairwise
comparisons.

## Examples

``` r
library(ggplot2)
library(magrittr) # pipe operator
scores <- example_quantile %>%
  as_forecast_quantile %>%
  score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
pairwise <- get_pairwise_comparisons(scores, by = "target_type")
plot_pairwise_comparisons(pairwise, type = "mean_scores_ratio") +
  facet_wrap(~target_type)
```

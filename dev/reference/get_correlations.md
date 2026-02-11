# Calculate correlation between metrics

Calculate the correlation between different metrics for a data.frame of
scores as produced by
[`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md).

## Usage

``` r
get_correlations(scores, metrics = get_metrics.scores(scores), ...)
```

## Arguments

- scores:

  An object of class `scores` (a data.table with scores and an
  additional attribute `metrics` as produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md)).

- metrics:

  A character vector with the metrics to show. If set to `NULL`
  (default), all metrics present in `scores` will be shown.

- ...:

  Additional arguments to pass down to
  [`cor()`](https://rdrr.io/r/stats/cor.html).

## Value

An object of class `scores` (a data.table with an additional attribute
`metrics` holding the names of the scores) with correlations between
different metrics

## Examples

``` r
scores <- example_quantile |>
 as_forecast_quantile() |>
 score()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.

get_correlations(scores)
#>           wis overprediction underprediction  dispersion        bias
#>         <num>          <num>           <num>       <num>       <num>
#> 1:  1.0000000     0.94297565      0.28377361  0.45566303  0.10545891
#> 2:  0.9429757     1.00000000     -0.03310356  0.32493799  0.21532161
#> 3:  0.2837736    -0.03310356      1.00000000  0.14580143 -0.35123801
#> 4:  0.4556630     0.32493799      0.14580143  1.00000000  0.11118365
#> 5:  0.1054589     0.21532161     -0.35123801  0.11118365  1.00000000
#> 6: -0.2076649    -0.14556039     -0.21392764 -0.09400664  0.01338140
#> 7: -0.4075613    -0.31824017     -0.35756699 -0.08614678  0.09802725
#> 8:  0.9886108     0.90326672      0.33589892  0.53809741  0.09578751
#>    interval_coverage_50 interval_coverage_90   ae_median               metric
#>                   <num>                <num>       <num>               <char>
#> 1:          -0.20766492          -0.40756133  0.98861080                  wis
#> 2:          -0.14556039          -0.31824017  0.90326672       overprediction
#> 3:          -0.21392764          -0.35756699  0.33589892      underprediction
#> 4:          -0.09400664          -0.08614678  0.53809741           dispersion
#> 5:           0.01338140           0.09802725  0.09578751                 bias
#> 6:           1.00000000           0.37245118 -0.24559356 interval_coverage_50
#> 7:           0.37245118           1.00000000 -0.41079097 interval_coverage_90
#> 8:          -0.24559356          -0.41079097  1.00000000            ae_median
```

# Dawid-Sebastiani score

Wrapper around the
[`dss_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_univ.html)
function from the scoringRules package.

## Usage

``` r
dss_sample(observed, predicted, ...)
```

## Arguments

- observed:

  A vector with observed values of size n

- predicted:

  nxN matrix of predictive samples, n (number of rows) being the number
  of data points and N (number of columns) the number of Monte Carlo
  samples. Alternatively, if n = 1, `predicted` can just be a vector of
  size n.

- ...:

  Additional arguments passed to
  [dss_sample()](https://rdrr.io/pkg/scoringRules/man/scores_sample_univ.html)
  from the scoringRules package.

## Value

Vector with scores.

## Input format

![](figures/metrics-sample.png)

Overview of required input format for sample-based forecasts

## References

Alexander Jordan, Fabian Krüger, Sebastian Lerch, Evaluating
Probabilistic Forecasts with scoringRules,
<https://www.jstatsoft.org/article/view/v090i12>

## Examples

``` r
observed <- rpois(30, lambda = 1:30)
predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
dss_sample(observed, predicted)
#>  [1] -0.06179111  2.81530809  2.46742176  1.79360855  1.61740613  2.39081466
#>  [7]  1.92865409 15.58374079  3.88246189  4.97070116  2.24551342  2.71528477
#> [13]  2.79485162  2.62249405  2.80770087  4.30015607  3.23096351  3.16329747
#> [19]  3.23001192  7.67628290  2.92052357  5.37555852  4.78857945  3.12865561
#> [25]  3.61999703  4.32028466  3.38593521  6.87116360  4.71995903  4.99107110
```

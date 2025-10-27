# Absolute error of the median (sample-based version)

Absolute error of the median calculated as \$\$ \|\text{observed} -
\text{median prediction}\| \$\$ where the median prediction is
calculated as the median of the predictive samples.

## Usage

``` r
ae_median_sample(observed, predicted)
```

## Arguments

- observed:

  A vector with observed values of size n

- predicted:

  nxN matrix of predictive samples, n (number of rows) being the number
  of data points and N (number of columns) the number of Monte Carlo
  samples. Alternatively, if n = 1, `predicted` can just be a vector of
  size n.

## Value

Numeric vector of length n with the absolute errors of the median.

## Input format

![](figures/metrics-sample.png)

Overview of required input format for sample-based forecasts

## See also

[`ae_median_quantile()`](https://epiforecasts.io/scoringutils/dev/reference/ae_median_quantile.md)

## Examples

``` r
observed <- rnorm(30, mean = 1:30)
predicted_values <- matrix(rnorm(30, mean = 1:30))
ae_median_sample(observed, predicted_values)
#>  [1] 2.52982645 0.98458168 0.94495454 0.65538891 0.56511146 0.09373061
#>  [7] 1.31110818 0.61226219 0.75386115 0.08959962 0.39077113 1.56818369
#> [13] 0.84567980 1.24260044 0.27781917 0.65054779 1.18084954 0.45036469
#> [19] 0.05976767 0.14675942 0.60583332 0.19442459 0.21123533 0.28585022
#> [25] 0.64582375 1.78993469 1.20347916 0.67902801 1.47081575 1.26862726
```

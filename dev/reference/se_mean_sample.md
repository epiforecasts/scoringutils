# Squared error of the mean (sample-based version)

Squared error of the mean calculated as

\$\$ \textrm{mean}(\textrm{observed} - \textrm{mean prediction})^2 \$\$
The mean prediction is calculated as the mean of the predictive samples.

## Usage

``` r
se_mean_sample(observed, predicted)
```

## Arguments

- observed:

  A vector with observed values of size n

- predicted:

  nxN matrix of predictive samples, n (number of rows) being the number
  of data points and N (number of columns) the number of Monte Carlo
  samples. Alternatively, if n = 1, `predicted` can just be a vector of
  size n.

## Input format

![](figures/metrics-sample.png)

Overview of required input format for sample-based forecasts

## Examples

``` r
observed <- rnorm(30, mean = 1:30)
predicted_values <- matrix(rnorm(30, mean = 1:30))
se_mean_sample(observed, predicted_values)
#>  [1]  0.541860930  9.800234601  0.517696633  1.551587018  4.105700572
#>  [6]  0.012761648  1.907995082  0.025050300  0.452778345  0.191431693
#> [11] 15.568627620  0.172518749  1.291851568  3.101428037  0.837992802
#> [16]  1.460835744  0.071220889  0.229046104  2.407846715  1.770079557
#> [21]  0.864783947  0.009872220  0.702369741  3.232846492  0.009438926
#> [26]  1.573558319  7.414888478  0.216364592  4.113449709  3.146478871
```

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
#>  [1] 4.546623e+00 5.575020e-01 3.935505e+00 9.437016e-01 2.431634e-03
#>  [6] 2.094508e-01 3.115281e+00 4.288333e-01 1.944089e+00 1.679750e-04
#> [11] 3.051100e-01 1.809854e-01 1.889211e+00 3.757465e-01 4.818431e-05
#> [16] 1.385961e+00 2.840632e+00 5.637887e-01 3.563894e-01 8.583315e-01
#> [21] 3.606269e+00 1.578801e-01 6.666549e+00 2.816836e+00 5.620241e-01
#> [26] 4.845099e-01 3.809361e+00 3.687691e-01 1.570164e+00 2.714351e+00
```

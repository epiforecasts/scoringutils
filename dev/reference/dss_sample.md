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
#>  [1]  0.8642468  0.4946967  1.1019401  1.4991881  1.5861693 15.8448553
#>  [7]  2.9985045  5.1285158  3.7527144  2.3461893  2.4050764  2.5469256
#> [13]  2.6793194  2.6363501  4.6505855  3.0048800  3.2424225  3.4583532
#> [19]  8.4819106  3.1878380  4.9319517  4.8247972  3.2960804  3.5101142
#> [25]  4.2467462  3.3790835  5.7542379  4.9187633  4.7918042  3.4923608
```

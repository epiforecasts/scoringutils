# Logarithmic score (sample-based version)

This function is a wrapper around the
[`logs_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_univ.html)
function from the scoringRules package.

The log score is the negative logarithm of the predictive density
evaluated at the observed value.

The function should be used to score continuous predictions only. While
the Log Score is in theory also applicable to discrete forecasts, the
problem lies in the implementation: The function uses a kernel density
estimation, which is not well defined with integer-valued Monte Carlo
Samples. See the scoringRules package for more details and alternatives,
e.g. calculating scores for specific discrete probability distributions.

## Usage

``` r
logs_sample(observed, predicted, ...)
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
  [logs_sample()](https://rdrr.io/pkg/scoringRules/man/scores_sample_univ.html)
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

## See also

Other log score functions:
[`logs_categorical()`](https://epiforecasts.io/scoringutils/dev/reference/scoring-functions-nominal.md),
[`scoring-functions-binary`](https://epiforecasts.io/scoringutils/dev/reference/scoring-functions-binary.md)

## Examples

``` r
observed <- rpois(30, lambda = 1:30)
predicted <- replicate(200, rpois(n = 30, lambda = 1:30))
logs_sample(observed, predicted)
#>  [1] 0.9880967 1.9405218 2.3609820 1.9448132 2.3972189 3.4000585 1.8824050
#>  [8] 1.9686091 3.0100234 2.1396645 2.1488704 2.4251551 3.0337112 3.3762610
#> [15] 2.2571433 2.5586526 2.3731309 2.8382683 2.8252143 2.4030810 2.6421347
#> [22] 2.8504512 2.8833280 3.8308886 3.0905242 2.9102331 2.6547945 2.6423248
#> [29] 2.7727979 2.6983418
```

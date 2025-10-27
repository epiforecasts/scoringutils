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
#>  [1] 1.308474 1.604634 1.682903 2.835490 2.018350 1.834829 2.721416 1.993444
#>  [9] 1.859838 2.259479 2.301326 3.479248 2.415809 2.088545 2.748643 2.735340
#> [17] 2.310007 2.469050 3.308508 3.915754 2.667890 4.424452 2.534516 2.858301
#> [25] 3.975788 3.067371 3.334775 3.794322 2.733746 2.668445
```

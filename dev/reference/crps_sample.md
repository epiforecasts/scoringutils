# (Continuous) ranked probability score

Wrapper around the
[`crps_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_univ.html)
function from the scoringRules package. Can be used for continuous as
well as integer valued forecasts

The Continuous ranked probability score (CRPS) can be interpreted as the
sum of three components: overprediction, underprediction and dispersion.
"Dispersion" is defined as the CRPS of the median forecast \$m\$. If an
observation \$y\$ is greater than \$m\$ then overprediction is defined
as the CRPS of the forecast for \$y\$ minus the dispersion component,
and underprediction is zero. If, on the other hand, \$y\<m\$ then
underprediction is defined as the CRPS of the forecast for \$y\$ minus
the dispersion component, and overprediction is zero.

The overprediction, underprediction and dispersion components correspond
to those of the
[`wis()`](https://epiforecasts.io/scoringutils/dev/reference/wis.md).

## Usage

``` r
crps_sample(observed, predicted, separate_results = FALSE, ...)

dispersion_sample(observed, predicted, ...)

overprediction_sample(observed, predicted, ...)

underprediction_sample(observed, predicted, ...)
```

## Arguments

- observed:

  A vector with observed values of size n

- predicted:

  nxN matrix of predictive samples, n (number of rows) being the number
  of data points and N (number of columns) the number of Monte Carlo
  samples. Alternatively, if n = 1, `predicted` can just be a vector of
  size n.

- separate_results:

  Logical. If `TRUE` (default is `FALSE`), then the separate parts of
  the CRPS (dispersion penalty, penalties for over- and
  under-prediction) get returned as separate elements of a list. If you
  want a `data.frame` instead, simply call
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on the
  output.

- ...:

  Additional arguments passed on to `crps_sample()` from functions
  `overprediction_sample()`, `underprediction_sample()` and
  `dispersion_sample()`.

## Value

Vector with scores.

`dispersion_sample()`: a numeric vector with dispersion values (one per
observation).

`overprediction_sample()`: a numeric vector with overprediction values
(one per observation).

`underprediction_sample()`: a numeric vector with underprediction values
(one per observation).

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
crps_sample(observed, predicted)
#>  [1] 0.397425 0.317700 1.115950 0.646575 1.945725 1.154800 0.596975 1.328250
#>  [9] 3.501850 1.136050 4.131775 4.132275 1.537200 0.873850 3.019275 0.988200
#> [17] 2.479175 1.415625 1.809875 2.786350 1.063300 1.138375 2.396075 2.989700
#> [25] 1.977950 1.525400 2.511375 1.938875 3.006150 3.177950
```

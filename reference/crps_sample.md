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
[`wis()`](https://epiforecasts.io/scoringutils/reference/wis.md).

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
#>  [1] 0.231225 0.329850 1.060100 0.412625 0.991550 0.793850 2.847025 1.126200
#>  [9] 0.690675 0.992875 1.118300 4.911975 4.135800 1.411050 0.967825 3.425125
#> [17] 0.839600 2.404700 1.376075 1.665525 2.771375 1.168550 1.244700 2.377250
#> [25] 2.847875 2.019175 1.827975 2.074850 1.868775 3.171950
```

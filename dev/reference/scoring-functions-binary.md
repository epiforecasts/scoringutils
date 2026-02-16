# Metrics for binary outcomes

**Brier score**

The Brier Score is the mean squared error between the probabilistic
prediction and the observed outcome. The Brier score is a proper scoring
rule. Small values are better (best is 0, the worst is 1).

\$\$ \textrm{Brier\\Score} = (\textrm{prediction} - \textrm{outcome})^2,
\$\$ where \\\textrm{outcome} \in \\0, 1\\\\, and \\\textrm{prediction}
\in \[0, 1\]\\ represents the probability that the outcome is equal to
1.

**Log score for binary outcomes**

The Log Score is the negative logarithm of the probability assigned to
the observed value. It is a proper scoring rule. Small values are better
(best is zero, worst is infinity).

## Usage

``` r
brier_score(observed, predicted)

logs_binary(observed, predicted)
```

## Arguments

- observed:

  A factor of length n with exactly two levels, holding the observed
  values. The highest factor level is assumed to be the reference level.
  This means that `predicted` represents the probability that the
  observed value is equal to the highest factor level.

- predicted:

  A numeric vector of length n, holding probabilities. Values represent
  the probability that the corresponding outcome is equal to the highest
  level of the factor `observed`.

## Value

A numeric vector of size n with the Brier scores

A numeric vector of size n with log scores

## Details

The functions require users to provide observed values as a factor in
order to distinguish its input from the input format required for
scoring point forecasts. Internally, however, factors will be converted
to numeric values. A factor `observed = factor(c(0, 1, 1, 0, 1)` with
two levels (`0` and `1`) would internally be coerced to a numeric vector
(in this case this would result in the numeric vector
`c(1, 2, 2, 1, 1)`). After subtracting 1, the resulting vector
(`c(0, 1, 1, 0)` in this case) is used for internal calculations. All
predictions are assumed represent the probability that the outcome is
equal of the last/highest factor level (in this case that the outcome is
equal to 1).

You could alternatively also provide a vector like
`observed = factor(c("a", "b", "b", "a"))` (with two levels, `a` and
`b`), which would result in exactly the same internal representation.
Probabilities then represent the probability that the outcome is equal
to "b". If you want your predictions to be probabilities that the
outcome is "a", then you could of course make `observed` a factor with
levels swapped, i.e.
`observed = factor(c("a", "b", "b", "a"), levels = c("b", "a"))`

## Input format

![](figures/metrics-binary-point.png)

Overview of required input format for binary and point forecasts

## See also

Other log score functions:
[`logs_categorical()`](https://epiforecasts.io/scoringutils/dev/reference/scoring-functions-nominal.md),
[`logs_sample()`](https://epiforecasts.io/scoringutils/dev/reference/logs_sample.md)

## Examples

``` r
observed <- factor(sample(c(0, 1), size = 30, replace = TRUE))
predicted <- runif(n = 30, min = 0, max = 1)

brier_score(observed, predicted)
#>  [1] 0.015132274 0.827754385 0.082469422 0.190462642 0.059759406 0.029541112
#>  [7] 0.001885541 0.017224811 0.050140573 0.456732140 0.098934872 0.077556431
#> [13] 0.038141163 0.074036862 0.014049223 0.172046569 0.086978529 0.260165114
#> [19] 0.231315325 0.893765882 0.153587342 0.490323668 0.031733325 0.156106300
#> [25] 0.427641857 0.018208153 0.086841448 0.155751831 0.850220038 0.239924492
logs_binary(observed, predicted)
#>  [1] 0.1312635 2.4058376 0.3385192 0.5734464 0.2803191 0.1885915 0.0443938
#>  [8] 0.1406922 0.2535008 1.1264551 0.3776639 0.3264084 0.2172828 0.3175877
#> [15] 0.1261637 0.5357759 0.3494457 0.7134802 0.6557599 2.9075734 0.4974196
#> [22] 1.2047436 0.1961834 0.5026969 1.0611527 0.1449536 0.3491161 0.5019552
#> [29] 2.5519927 0.6729934
```

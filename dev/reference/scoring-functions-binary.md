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
#>  [1] 1.171431e-02 2.833029e-01 2.709027e-01 1.913235e-02 2.247761e-01
#>  [6] 7.932857e-01 1.277878e-01 8.776563e-01 4.531539e-01 4.317316e-01
#> [11] 4.127999e-01 6.221770e-01 1.530832e-02 9.713887e-02 2.452834e-01
#> [16] 4.169802e-05 9.783106e-02 2.673219e-02 3.988662e-01 5.607356e-01
#> [21] 3.212415e-08 1.551775e-01 1.841034e-01 2.795083e-03 7.423630e-01
#> [26] 8.203480e-01 2.824196e-01 7.303245e-01 4.375116e-01 7.763407e-01
logs_binary(observed, predicted)
#>  [1] 0.1145500163 0.7598471133 0.7349763128 0.1488710053 0.6426549049
#>  [6] 2.2133465996 0.4423482533 2.7619753973 1.1183060639 1.0702081736
#> [11] 1.0286061427 1.5548644399 0.1320773455 0.3734884852 0.6837136070
#> [16] 0.0064783399 0.3751001804 0.1785285273 0.9984735674 1.3815966337
#> [21] 0.0001792482 0.5007528282 0.5604934350 0.0543173790 1.9776427074
#> [26] 2.3615990286 0.7580733555 1.9281994912 1.0830733232 2.1294889880
```

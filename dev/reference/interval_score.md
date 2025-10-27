# Interval score

Proper Scoring Rule to score quantile predictions, following Gneiting
and Raftery (2007). Smaller values are better.

The score is computed as

\$\$ \textrm{score} = (\textrm{upper} - \textrm{lower}) +
\frac{2}{\alpha}(\textrm{lower} - \textrm{observed}) \*
\mathbf{1}(\textrm{observed} \< \textrm{lower}) +
\frac{2}{\alpha}(\textrm{observed} - \textrm{upper}) \*
\mathbf{1}(\textrm{observed} \> \textrm{upper}) \$\$ where
\\\mathbf{1}()\\ is the indicator function and indicates how much is
outside the prediction interval. \\\alpha\\ is the decimal value that
indicates how much is outside the prediction interval.

To improve usability, the user is asked to provide an interval range in
percentage terms, i.e. interval_range = 90 (percent) for a 90 percent
prediction interval. Correspondingly, the user would have to provide the
5% and 95% quantiles (the corresponding alpha would then be 0.1). No
specific distribution is assumed, but the interval has to be symmetric
around the median (i.e you can't use the 0.1 quantile as the lower bound
and the 0.7 quantile as the upper bound). Non-symmetric quantiles can be
scored using the function
[`quantile_score()`](https://epiforecasts.io/scoringutils/dev/reference/quantile_score.md).

## Usage

``` r
interval_score(
  observed,
  lower,
  upper,
  interval_range,
  weigh = TRUE,
  separate_results = FALSE
)
```

## Arguments

- observed:

  A vector with observed values of size n

- lower:

  Vector of size n with the prediction for the lower quantile of the
  given interval range.

- upper:

  Vector of size n with the prediction for the upper quantile of the
  given interval range.

- interval_range:

  Numeric vector (either a single number or a vector of size n) with the
  range of the prediction intervals. For example, if you're forecasting
  the 0.05 and 0.95 quantile, the interval range would be 90. The
  interval range corresponds to \\(100-\alpha)/100\\, where \\\alpha\\
  is the decimal value that indicates how much is outside the prediction
  interval (see e.g. Gneiting and Raftery (2007)).

- weigh:

  Logical. If `TRUE` (the default), weigh the score by \\\alpha / 2\\,
  so it can be averaged into an interval score that, in the limit (for
  an increasing number of equally spaced quantiles/prediction
  intervals), corresponds to the CRPS. \\\alpha\\ is the value that
  corresponds to the (\\\alpha/2\\) or (\\1 - \alpha/2\\), i.e. it is
  the decimal value that represents how much is outside a central
  prediction interval (E.g. for a 90 percent central prediction
  interval, alpha is 0.1).

- separate_results:

  Logical. If `TRUE` (default is `FALSE`), then the separate parts of
  the interval score (dispersion penalty, penalties for over- and
  under-prediction get returned as separate elements of a list). If you
  want a `data.frame` instead, simply call
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on the
  output.

## Value

Vector with the scoring values, or a list with separate entries if
`separate_results` is `TRUE`.

## References

Strictly Proper Scoring Rules, Prediction,and Estimation, Tilmann
Gneiting and Adrian E. Raftery, 2007, Journal of the American
Statistical Association, Volume 102, 2007 - Issue 477

Evaluating epidemic forecasts in an interval format, Johannes Bracher,
Evan L. Ray, Tilmann Gneiting and Nicholas G. Reich,
<https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618>
\# nolint

## Examples

``` r
observed <- rnorm(30, mean = 1:30)
interval_range <- rep(90, 30)
alpha <- (100 - interval_range) / 100
lower <- qnorm(alpha / 2, rnorm(30, mean = 1:30))
upper <- qnorm((1 - alpha / 2), rnorm(30, mean = 11:40))

scoringutils:::interval_score(
  observed = observed,
  lower = lower,
  upper = upper,
  interval_range = interval_range
)
#>  [1] 0.6659581 0.6581834 1.2866256 0.7423596 0.6155816 0.6176548 0.6484069
#>  [8] 0.6584252 0.6929587 0.7558694 0.6724925 0.5716195 0.6251716 0.7431658
#> [15] 0.6906088 0.6524924 0.6274762 1.8011311 0.5840689 0.6602319 0.7076748
#> [22] 0.6060337 0.5491802 1.7128995 0.5762644 1.0630510 0.6635204 0.6410611
#> [29] 0.7354960 0.7408991

# gives a warning, as the interval_range should likely be 50 instead of 0.5
scoringutils:::interval_score(
  observed = 4, upper = 8, lower = 2, interval_range = 0.5
)
#> Warning: ! Found interval ranges between 0 and 1. Are you sure that's right? An interval
#>   range of 0.5 e.g. implies a (49.75%, 50.25%) prediction interval.
#> ℹ If you want to score a (25%, 75%) prediction interval, set `interval_range =
#>   50`.
#> This warning is displayed once per session.
#> [1] 2.985

# example with missing values and separate results
scoringutils:::interval_score(
  observed = c(observed, NA),
  lower = c(lower, NA),
  upper = c(NA, upper),
  separate_results = TRUE,
  interval_range = 90
)
#> $interval_score
#>  [1]        NA 0.6346291 1.1441141 0.7545579 0.6109357 0.6003346 0.5762499
#>  [8] 0.5621607 0.6826498 0.6576112 0.6649209 0.5101806 0.6064191 0.5613882
#> [15] 0.7192299 0.6075918 0.6736010 1.6335484 0.6182397 0.6036268 0.5671816
#> [22] 0.6245858 0.5639638 1.4869856 0.7336300 0.8655401 0.5912985 0.6734720
#> [29] 0.6317333 0.6911570        NA
#> 
#> $dispersion
#>  [1]        NA 0.6346291 0.5198196 0.7545579 0.6109357 0.6003346 0.5762499
#>  [8] 0.5621607 0.6826498 0.6576112 0.6649209 0.5101806 0.6064191 0.5613882
#> [15] 0.7192299 0.6075918 0.6736010 0.4719967 0.6182397 0.6036268 0.5671816
#> [22] 0.6245858 0.5639638 0.4529700 0.7336300 0.4398082 0.5912985 0.6734720
#> [29] 0.6317333 0.6911570        NA
#> 
#> $underprediction
#>  [1] NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#> [26]  0  0  0  0  0 NA
#> 
#> $overprediction
#>  [1] 0.0000000 0.0000000 0.6242945 0.0000000 0.0000000 0.0000000 0.0000000
#>  [8] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
#> [15] 0.0000000 0.0000000 0.0000000 1.1615517 0.0000000 0.0000000 0.0000000
#> [22] 0.0000000 0.0000000 1.0340156 0.0000000 0.4257319 0.0000000 0.0000000
#> [29] 0.0000000 0.0000000        NA
#> 
```

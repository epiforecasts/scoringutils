# Plot quantile coverage

Plot quantile coverage values (see
[`get_coverage()`](https://epiforecasts.io/scoringutils/dev/reference/get_coverage.md)
for more information).

## Usage

``` r
plot_quantile_coverage(coverage, colour = "model")
```

## Arguments

- coverage:

  A data frame of coverage values as produced by
  [`get_coverage()`](https://epiforecasts.io/scoringutils/dev/reference/get_coverage.md).

- colour:

  String, according to which variable shall the graphs be coloured?
  Default is "model".

## Value

A ggplot object with a plot of interval coverage

## Examples

``` r
example <- as_forecast_quantile(example_quantile)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
coverage <- get_coverage(example, by = "model")
plot_quantile_coverage(coverage)
```

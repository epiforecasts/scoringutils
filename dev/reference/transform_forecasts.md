# Transform forecasts and observed values

Function to transform forecasts and observed values before scoring.

## Usage

``` r
transform_forecasts(
  forecast,
  fun = log_shift,
  append = TRUE,
  label = "log",
  ...
)
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

- fun:

  A function used to transform both observed values and predictions. The
  default function is
  [`log_shift()`](https://epiforecasts.io/scoringutils/dev/reference/log_shift.md),
  a custom function that is essentially the same as
  [`log()`](https://rdrr.io/r/base/Log.html), but has an additional
  arguments (`offset`) that allows you add an offset before applying the
  logarithm. This is often helpful as the natural log transformation is
  not defined at zero. A common, and pragmatic solution, is to add a
  small offset to the data before applying the log transformation. In
  our work we have often used an offset of 1 but the precise value will
  depend on your application.

- append:

  Logical, defaults to `TRUE`. Whether or not to append a transformed
  version of the data to the currently existing data (`TRUE`). If
  selected, the data gets transformed and appended to the existing data,
  making it possible to use the outcome directly in
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md).
  An additional column, 'scale', gets created that denotes which rows or
  untransformed ('scale' has the value "natural") and which have been
  transformed ('scale' has the value passed to the argument `label`).

- label:

  A string for the newly created 'scale' column to denote the newly
  transformed values. Only relevant if `append = TRUE`.

- ...:

  Additional parameters to pass to the function you supplied. For the
  default option of
  [`log_shift()`](https://epiforecasts.io/scoringutils/dev/reference/log_shift.md)
  this could be the `offset` argument.

## Value

A forecast object with either a transformed version of the data, or one
with both the untransformed and the transformed data. includes the
original data as well as a transformation of the original data. There
will be one additional column, \`scale', present which will be set to
"natural" for the untransformed forecasts.

## Details

There are a few reasons, depending on the circumstances, for why this
might be desirable (check out the linked reference for more info). In
epidemiology, for example, it may be useful to log-transform incidence
counts before evaluating forecasts using scores such as the weighted
interval score (WIS) or the continuous ranked probability score (CRPS).
Log-transforming forecasts and observations changes the interpretation
of the score from a measure of absolute distance between forecast and
observation to a score that evaluates a forecast of the exponential
growth rate. Another motivation can be to apply a variance-stabilising
transformation or to standardise incidence counts by population.

Note that if you want to apply a transformation, it is important to
transform the forecasts and observations and then apply the score.
Applying a transformation after the score risks losing propriety of the
proper scoring rule.

## References

Transformation of forecasts for evaluating predictive performance in an
epidemiological context Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van
Leeuwen, Johannes Bracher, Sebastian Funk medRxiv 2023.01.23.23284722
[doi:10.1101/2023.01.23.23284722](https://doi.org/10.1101/2023.01.23.23284722)
<https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1>

## Author

Nikos Bosse <nikosbosse@gmail.com>

## Examples

``` r
library(magrittr) # pipe operator

# transform forecasts using the natural logarithm
# negative values need to be handled (here by replacing them with 0)
example_quantile %>%
  .[, observed := ifelse(observed < 0, 0, observed)] %>%
  as_forecast_quantile() %>%
# Here we use the default function log_shift() which is essentially the same
# as log(), but has an additional arguments (offset) that allows you add an
# offset before applying the logarithm.
  transform_forecasts(append = FALSE) %>%
  head()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#> Warning: ! Detected zeros in input values.
#> ℹ Try specifying offset = 1 (or any other offset).
#> Warning: ! Detected zeros in input values.
#> ℹ Try specifying offset = 1 (or any other offset).
#> Key: <location, target_end_date, target_type>
#>    location target_end_date target_type  observed location_name forecast_date
#>      <char>          <Date>      <char>     <num>        <char>        <Date>
#> 1:       DE      2021-01-02       Cases 11.754302       Germany          <NA>
#> 2:       DE      2021-01-02      Deaths  8.419360       Germany          <NA>
#> 3:       DE      2021-01-09       Cases 11.950677       Germany          <NA>
#> 4:       DE      2021-01-09      Deaths  8.718827       Germany          <NA>
#> 5:       DE      2021-01-16       Cases 11.609898       Germany          <NA>
#> 6:       DE      2021-01-16      Deaths  8.677099       Germany          <NA>
#>    quantile_level predicted  model horizon
#>             <num>     <num> <char>   <num>
#> 1:             NA        NA   <NA>      NA
#> 2:             NA        NA   <NA>      NA
#> 3:             NA        NA   <NA>      NA
#> 4:             NA        NA   <NA>      NA
#> 5:             NA        NA   <NA>      NA
#> 6:             NA        NA   <NA>      NA

# alternatively, integrating the truncation in the transformation function:
example_quantile %>%
  as_forecast_quantile() %>%
 transform_forecasts(
   fun = function(x) {log_shift(pmax(0, x))}, append = FALSE
 ) %>%
 head()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#> Warning: ! Detected zeros in input values.
#> ℹ Try specifying offset = 1 (or any other offset).
#> Warning: ! Detected zeros in input values.
#> ℹ Try specifying offset = 1 (or any other offset).
#> Key: <location, target_end_date, target_type>
#>    location target_end_date target_type  observed location_name forecast_date
#>      <char>          <Date>      <char>     <num>        <char>        <Date>
#> 1:       DE      2021-01-02       Cases 11.754302       Germany          <NA>
#> 2:       DE      2021-01-02      Deaths  8.419360       Germany          <NA>
#> 3:       DE      2021-01-09       Cases 11.950677       Germany          <NA>
#> 4:       DE      2021-01-09      Deaths  8.718827       Germany          <NA>
#> 5:       DE      2021-01-16       Cases 11.609898       Germany          <NA>
#> 6:       DE      2021-01-16      Deaths  8.677099       Germany          <NA>
#>    quantile_level predicted  model horizon
#>             <num>     <num> <char>   <num>
#> 1:             NA        NA   <NA>      NA
#> 2:             NA        NA   <NA>      NA
#> 3:             NA        NA   <NA>      NA
#> 4:             NA        NA   <NA>      NA
#> 5:             NA        NA   <NA>      NA
#> 6:             NA        NA   <NA>      NA

# specifying an offset for the log transformation removes the
# warning caused by zeros in the data
example_quantile %>%
  as_forecast_quantile() %>%
  .[, observed := ifelse(observed < 0, 0, observed)] %>%
  transform_forecasts(offset = 1, append = FALSE) %>%
  head()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#> Key: <location, target_end_date, target_type>
#>    location target_end_date target_type  observed location_name forecast_date
#>      <char>          <Date>      <char>     <num>        <char>        <Date>
#> 1:       DE      2021-01-02       Cases 11.754310       Germany          <NA>
#> 2:       DE      2021-01-02      Deaths  8.419580       Germany          <NA>
#> 3:       DE      2021-01-09       Cases 11.950683       Germany          <NA>
#> 4:       DE      2021-01-09      Deaths  8.718991       Germany          <NA>
#> 5:       DE      2021-01-16       Cases 11.609907       Germany          <NA>
#> 6:       DE      2021-01-16      Deaths  8.677269       Germany          <NA>
#>    quantile_level predicted  model horizon
#>             <num>     <num> <char>   <num>
#> 1:             NA        NA   <NA>      NA
#> 2:             NA        NA   <NA>      NA
#> 3:             NA        NA   <NA>      NA
#> 4:             NA        NA   <NA>      NA
#> 5:             NA        NA   <NA>      NA
#> 6:             NA        NA   <NA>      NA

# adding square root transformed forecasts to the original ones
example_quantile %>%
  .[, observed := ifelse(observed < 0, 0, observed)] %>%
  as_forecast_quantile() %>%
  transform_forecasts(fun = sqrt, label = "sqrt") %>%
  score() %>%
  summarise_scores(by = c("model", "scale"))
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#>                    model   scale          wis overprediction underprediction
#>                   <char>  <char>        <num>          <num>           <num>
#> 1: EuroCOVIDhub-ensemble natural  5796.064569   1828.5715014    2120.6402853
#> 2: EuroCOVIDhub-baseline natural 11124.930667   3884.4414062    5143.5356658
#> 3:  epiforecasts-EpiNow2 natural  7514.375476   2866.4071466    1697.2341137
#> 4:       UMass-MechBayes natural    52.651946      8.9786005      16.8009511
#> 5: EuroCOVIDhub-ensemble    sqrt    14.974344      5.5037665       5.1827454
#> 6: EuroCOVIDhub-baseline    sqrt    27.742316     10.4190016       9.5936380
#> 7:  epiforecasts-EpiNow2    sqrt    17.704899      6.5700431       5.7235785
#> 8:       UMass-MechBayes    sqrt     1.328653      0.3273746       0.4019195
#>      dispersion        bias interval_coverage_50 interval_coverage_90
#>           <num>       <num>                <num>                <num>
#> 1: 1846.8527819  0.00812500            0.6328125            0.9023438
#> 2: 2096.9535954  0.21816406            0.4960938            0.9101562
#> 3: 2950.7342158 -0.04336032            0.4453441            0.8461538
#> 4:   26.8723947 -0.02234375            0.4609375            0.8750000
#> 5:    4.2878323  0.00812500            0.6328125            0.9023438
#> 6:    7.7296761  0.21816406            0.4960938            0.9101562
#> 7:    5.4112770 -0.04336032            0.4453441            0.8461538
#> 8:    0.5993586 -0.02234375            0.4609375            0.8750000
#>       ae_median
#>           <num>
#> 1:  8880.542969
#> 2: 16156.871094
#> 3: 11208.072874
#> 4:    78.476562
#> 5:    22.458900
#> 6:    39.185406
#> 7:    25.585018
#> 8:     2.069103

# adding multiple transformations
example_quantile %>%
  as_forecast_quantile() %>%
  .[, observed := ifelse(observed < 0, 0, observed)] %>%
  transform_forecasts(fun = log_shift, offset = 1) %>%
  transform_forecasts(fun = sqrt, label = "sqrt") %>%
  head()
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#>    location target_end_date target_type observed location_name forecast_date
#>      <char>          <Date>      <char>    <num>        <char>        <Date>
#> 1:       DE      2021-01-02       Cases   127300       Germany          <NA>
#> 2:       DE      2021-01-02      Deaths     4534       Germany          <NA>
#> 3:       DE      2021-01-09       Cases   154922       Germany          <NA>
#> 4:       DE      2021-01-09      Deaths     6117       Germany          <NA>
#> 5:       DE      2021-01-16       Cases   110183       Germany          <NA>
#> 6:       DE      2021-01-16      Deaths     5867       Germany          <NA>
#>    quantile_level predicted  model horizon   scale
#>             <num>     <num> <char>   <num>  <char>
#> 1:             NA        NA   <NA>      NA natural
#> 2:             NA        NA   <NA>      NA natural
#> 3:             NA        NA   <NA>      NA natural
#> 4:             NA        NA   <NA>      NA natural
#> 5:             NA        NA   <NA>      NA natural
#> 6:             NA        NA   <NA>      NA natural
```

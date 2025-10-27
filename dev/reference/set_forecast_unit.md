# Set unit of a single forecast manually

Helper function to set the unit of a single forecast (i.e. the
combination of columns that uniquely define a single forecast) manually.
This simple function keeps the columns specified in `forecast_unit`
(plus additional protected columns, e.g. for observed values,
predictions or quantile levels) and removes duplicate rows.
`set_forecast_unit()` will mainly be called when constructing a
`forecast` object via the `forecast_unit` argument in
`as_forecast_<type>`.

If not done explicitly, `scoringutils` attempts to determine the unit of
a single forecast automatically by simply assuming that all column names
are relevant to determine the forecast unit. This may lead to unexpected
behaviour, so setting the forecast unit explicitly can help make the
code easier to debug and easier to read.

## Usage

``` r
set_forecast_unit(data, forecast_unit)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

- forecast_unit:

  Character vector with the names of the columns that uniquely identify
  a single forecast.

## Value

A data.table with only those columns kept that are relevant to scoring
or denote the unit of a single forecast as specified by the user.

## Examples

``` r
library(magrittr) # pipe operator
example_quantile %>%
  scoringutils:::set_forecast_unit(
    c("location", "target_end_date", "target_type", "horizon", "model")
  )
#> Forecast type: quantile
#> Forecast unit:
#> location, target_end_date, target_type, horizon, and model
#> 
#> Key: <location, target_end_date, target_type>
#>        observed quantile_level predicted location target_end_date target_type
#>           <num>          <num>     <int>   <char>          <Date>      <char>
#>     1:   127300             NA        NA       DE      2021-01-02       Cases
#>     2:     4534             NA        NA       DE      2021-01-02      Deaths
#>     3:   154922             NA        NA       DE      2021-01-09       Cases
#>     4:     6117             NA        NA       DE      2021-01-09      Deaths
#>     5:   110183             NA        NA       DE      2021-01-16       Cases
#>    ---                                                                       
#> 20541:       78          0.850       352       IT      2021-07-24      Deaths
#> 20542:       78          0.900       397       IT      2021-07-24      Deaths
#> 20543:       78          0.950       499       IT      2021-07-24      Deaths
#> 20544:       78          0.975       611       IT      2021-07-24      Deaths
#> 20545:       78          0.990       719       IT      2021-07-24      Deaths
#>        horizon                model
#>          <num>               <char>
#>     1:      NA                 <NA>
#>     2:      NA                 <NA>
#>     3:      NA                 <NA>
#>     4:      NA                 <NA>
#>     5:      NA                 <NA>
#>    ---                             
#> 20541:       2 epiforecasts-EpiNow2
#> 20542:       2 epiforecasts-EpiNow2
#> 20543:       2 epiforecasts-EpiNow2
#> 20544:       2 epiforecasts-EpiNow2
#> 20545:       2 epiforecasts-EpiNow2
```

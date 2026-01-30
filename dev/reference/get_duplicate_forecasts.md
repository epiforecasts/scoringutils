# Find duplicate forecasts

Internal helper function to identify duplicate forecasts, i.e. instances
where there is more than one forecast for the same prediction target.

## Usage

``` r
get_duplicate_forecasts(data, forecast_unit = NULL, counts = FALSE)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

- forecast_unit:

  (optional) Name of the columns in `data` (after any renaming of
  columns) that denote the unit of a single forecast. See
  [`get_forecast_unit()`](https://epiforecasts.io/scoringutils/dev/reference/get_forecast_unit.md)
  for details. If `NULL` (the default), all columns that are not
  required columns are assumed to form the unit of a single forecast. If
  specified, all columns that are not part of the forecast unit (or
  required columns) will be removed.

- counts:

  Should the output show the number of duplicates per forecast unit
  instead of the individual duplicated rows? Default is `FALSE`.

## Value

A data.frame with all rows for which a duplicate forecast was found

## Examples

``` r
example <- rbind(example_quantile, example_quantile[1000:1010])
get_duplicate_forecasts(example)
#>     location target_end_date target_type observed location_name forecast_date
#>       <char>          <Date>      <char>    <num>        <char>        <Date>
#>  1:       DE      2021-05-22      Deaths     1285       Germany    2021-05-17
#>  2:       DE      2021-05-22      Deaths     1285       Germany    2021-05-17
#>  3:       DE      2021-05-22      Deaths     1285       Germany    2021-05-17
#>  4:       DE      2021-05-22      Deaths     1285       Germany    2021-05-17
#>  5:       DE      2021-05-22      Deaths     1285       Germany    2021-05-17
#>  6:       DE      2021-05-22      Deaths     1285       Germany    2021-05-17
#>  7:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#>  8:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#>  9:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 10:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 11:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 12:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 13:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 14:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 15:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 16:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 17:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 18:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 19:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 20:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 21:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#> 22:       DE      2021-05-29       Cases    31653       Germany    2021-05-10
#>     location target_end_date target_type observed location_name forecast_date
#>       <char>          <Date>      <char>    <num>        <char>        <Date>
#>     quantile_level predicted                 model horizon
#>              <num>     <int>                <char>   <num>
#>  1:          0.950      1464  epiforecasts-EpiNow2       1
#>  2:          0.950      1464  epiforecasts-EpiNow2       1
#>  3:          0.975      1642  epiforecasts-EpiNow2       1
#>  4:          0.975      1642  epiforecasts-EpiNow2       1
#>  5:          0.990      1951  epiforecasts-EpiNow2       1
#>  6:          0.990      1951  epiforecasts-EpiNow2       1
#>  7:          0.010     28999 EuroCOVIDhub-ensemble       3
#>  8:          0.010     28999 EuroCOVIDhub-ensemble       3
#>  9:          0.025     32612 EuroCOVIDhub-ensemble       3
#> 10:          0.025     32612 EuroCOVIDhub-ensemble       3
#> 11:          0.050     36068 EuroCOVIDhub-ensemble       3
#> 12:          0.050     36068 EuroCOVIDhub-ensemble       3
#> 13:          0.100     41484 EuroCOVIDhub-ensemble       3
#> 14:          0.100     41484 EuroCOVIDhub-ensemble       3
#> 15:          0.150     47110 EuroCOVIDhub-ensemble       3
#> 16:          0.150     47110 EuroCOVIDhub-ensemble       3
#> 17:          0.200     50929 EuroCOVIDhub-ensemble       3
#> 18:          0.200     50929 EuroCOVIDhub-ensemble       3
#> 19:          0.250     54561 EuroCOVIDhub-ensemble       3
#> 20:          0.250     54561 EuroCOVIDhub-ensemble       3
#> 21:          0.300     57739 EuroCOVIDhub-ensemble       3
#> 22:          0.300     57739 EuroCOVIDhub-ensemble       3
#>     quantile_level predicted                 model horizon
#>              <num>     <int>                <char>   <num>
```

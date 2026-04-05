# Print information about a forecast object

This function prints information about a forecast object, including
"Forecast type", "Score columns", "Forecast unit".

## Usage

``` r
# S3 method for class 'forecast'
print(x, ...)
```

## Arguments

- x:

  A forecast object

- ...:

  Additional arguments for
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

Returns `x` invisibly.

## Examples

``` r
dat <- as_forecast_quantile(example_quantile)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
print(dat)
#> Forecast type: quantile
#> Forecast unit:
#> location, target_end_date, target_type, location_name, forecast_date, model,
#> and horizon
#> 
#> Key: <location, target_end_date, target_type>
#>        location target_end_date target_type observed location_name
#>          <char>          <Date>      <char>    <num>        <char>
#>     1:       DE      2021-01-02       Cases   127300       Germany
#>     2:       DE      2021-01-02      Deaths     4534       Germany
#>     3:       DE      2021-01-09       Cases   154922       Germany
#>     4:       DE      2021-01-09      Deaths     6117       Germany
#>     5:       DE      2021-01-16       Cases   110183       Germany
#>    ---                                                            
#> 20541:       IT      2021-07-24      Deaths       78         Italy
#> 20542:       IT      2021-07-24      Deaths       78         Italy
#> 20543:       IT      2021-07-24      Deaths       78         Italy
#> 20544:       IT      2021-07-24      Deaths       78         Italy
#> 20545:       IT      2021-07-24      Deaths       78         Italy
#>        forecast_date quantile_level predicted                model horizon
#>               <Date>          <num>     <int>               <char>   <num>
#>     1:          <NA>             NA        NA                 <NA>      NA
#>     2:          <NA>             NA        NA                 <NA>      NA
#>     3:          <NA>             NA        NA                 <NA>      NA
#>     4:          <NA>             NA        NA                 <NA>      NA
#>     5:          <NA>             NA        NA                 <NA>      NA
#>    ---                                                                    
#> 20541:    2021-07-12          0.850       352 epiforecasts-EpiNow2       2
#> 20542:    2021-07-12          0.900       397 epiforecasts-EpiNow2       2
#> 20543:    2021-07-12          0.950       499 epiforecasts-EpiNow2       2
#> 20544:    2021-07-12          0.975       611 epiforecasts-EpiNow2       2
#> 20545:    2021-07-12          0.990       719 epiforecasts-EpiNow2       2
```

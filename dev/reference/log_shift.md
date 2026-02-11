# Log transformation with an additive shift

Function that shifts a value by some offset and then applies the natural
logarithm to it.

## Usage

``` r
log_shift(x, offset = 0, base = exp(1))
```

## Arguments

- x:

  vector of input values to be transformed

- offset:

  Number to add to the input value before taking the natural logarithm.

- base:

  A positive number: the base with respect to which logarithms are
  computed. Defaults to e = exp(1).

## Value

A numeric vector with transformed values

## Details

The output is computed as log(x + offset)

## References

Transformation of forecasts for evaluating predictive performance in an
epidemiological context Nikos I. Bosse, Sam Abbott, Anne Cori, Edwin van
Leeuwen, Johannes Bracher, Sebastian Funk medRxiv 2023.01.23.23284722
[doi:10.1101/2023.01.23.23284722](https://doi.org/10.1101/2023.01.23.23284722)
<https://www.medrxiv.org/content/10.1101/2023.01.23.23284722v1> \#
nolint

## Examples

``` r
log_shift(1:10)
#>  [1] 0.0000000 0.6931472 1.0986123 1.3862944 1.6094379 1.7917595 1.9459101
#>  [8] 2.0794415 2.1972246 2.3025851
log_shift(0:9, offset = 1)
#>  [1] 0.0000000 0.6931472 1.0986123 1.3862944 1.6094379 1.7917595 1.9459101
#>  [8] 2.0794415 2.1972246 2.3025851

example_quantile[observed > 0, ] |>
  as_forecast_quantile() |>
  transform_forecasts(fun = log_shift, offset = 1)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
#> Forecast type: quantile
#> Forecast unit:
#> location, target_end_date, target_type, location_name, forecast_date, model,
#> horizon, and scale
#> 
#>        location target_end_date target_type     observed location_name
#>          <char>          <Date>      <char>        <num>        <char>
#>     1:       DE      2021-01-02       Cases 1.273000e+05       Germany
#>     2:       DE      2021-01-02      Deaths 4.534000e+03       Germany
#>     3:       DE      2021-01-09       Cases 1.549220e+05       Germany
#>     4:       DE      2021-01-09      Deaths 6.117000e+03       Germany
#>     5:       DE      2021-01-16       Cases 1.101830e+05       Germany
#>    ---                                                                
#> 40672:       IT      2021-07-24      Deaths 4.369448e+00         Italy
#> 40673:       IT      2021-07-24      Deaths 4.369448e+00         Italy
#> 40674:       IT      2021-07-24      Deaths 4.369448e+00         Italy
#> 40675:       IT      2021-07-24      Deaths 4.369448e+00         Italy
#> 40676:       IT      2021-07-24      Deaths 4.369448e+00         Italy
#>        forecast_date quantile_level predicted                model horizon
#>               <Date>          <num>     <num>               <char>   <num>
#>     1:          <NA>             NA        NA                 <NA>      NA
#>     2:          <NA>             NA        NA                 <NA>      NA
#>     3:          <NA>             NA        NA                 <NA>      NA
#>     4:          <NA>             NA        NA                 <NA>      NA
#>     5:          <NA>             NA        NA                 <NA>      NA
#>    ---                                                                    
#> 40672:    2021-07-12          0.850  5.866468 epiforecasts-EpiNow2       2
#> 40673:    2021-07-12          0.900  5.986452 epiforecasts-EpiNow2       2
#> 40674:    2021-07-12          0.950  6.214608 epiforecasts-EpiNow2       2
#> 40675:    2021-07-12          0.975  6.416732 epiforecasts-EpiNow2       2
#> 40676:    2021-07-12          0.990  6.579251 epiforecasts-EpiNow2       2
#>          scale
#>         <char>
#>     1: natural
#>     2: natural
#>     3: natural
#>     4: natural
#>     5: natural
#>    ---        
#> 40672:     log
#> 40673:     log
#> 40674:     log
#> 40675:     log
#> 40676:     log
```

# Get type-specific ID columns for a forecast

S3 generic that returns the column names (beyond the forecast unit) that
identify a unique row for a given forecast type. Each forecast type
method returns the columns specific to that type. The default returns
`character(0)` (no type-specific columns).

Custom forecast types should define a method returning the relevant
column names.

## Usage

``` r
# S3 method for class 'forecast_multivariate_sample'
get_forecast_type_ids(data)

# S3 method for class 'forecast_nominal'
get_forecast_type_ids(data)

# S3 method for class 'forecast_ordinal'
get_forecast_type_ids(data)

# S3 method for class 'forecast_quantile'
get_forecast_type_ids(data)

# S3 method for class 'forecast_sample'
get_forecast_type_ids(data)

get_forecast_type_ids(data)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

## Value

A character vector of column names.

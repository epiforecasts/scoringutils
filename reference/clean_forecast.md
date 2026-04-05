# Clean forecast object

The function makes it possible to silently validate an object. In
addition, it can return a copy of the data and remove rows with missing
values.

## Usage

``` r
clean_forecast(forecast, copy = FALSE, na.omit = FALSE)
```

## Arguments

- forecast:

  A forecast object (a validated data.table with predicted and observed
  values).

- copy:

  Logical, default is `FALSE`. If `TRUE`, a copy of the input data is
  created.

- na.omit:

  Logical, default is `FALSE`. If `TRUE`, rows with missing values are
  removed.

# Common functionality for `as_forecast_<type>` functions

Common functionality for `as_forecast_<type>` functions

## Usage

``` r
as_forecast_generic(data, forecast_unit = NULL, ...)
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

- ...:

  Named arguments that are used to rename columns. The names of the
  arguments are the names of the columns that should be renamed. The
  values are the new names.

## Details

This function splits out part of the functionality of
`as_forecast_<type>` that is the same for all `as_forecast_<type>`
functions. It renames the required columns, where appropriate, and sets
the forecast unit.

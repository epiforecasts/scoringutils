# Point forecast example data

A data set with predictions for COVID-19 cases and deaths submitted to
the European Forecast Hub. This data set is like the quantile example
data, only that the median has been replaced by a point forecast.

## Usage

``` r
example_point
```

## Format

An object of class `forecast_point` (see
[`as_forecast_point()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_point.md))
with the following columns:

- location:

  the country for which a prediction was made

- target_end_date:

  the date for which a prediction was made

- target_type:

  the target to be predicted (cases or deaths)

- observed:

  observed values

- location_name:

  name of the country for which a prediction was made

- forecast_date:

  the date on which a prediction was made

- predicted:

  predicted value

- model:

  name of the model that generated the forecasts

- horizon:

  forecast horizon in weeks

## Source

<https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/>

## Details

The data was created using the script create-example-data.R in the inst/
folder (or the top level folder in a compiled package).

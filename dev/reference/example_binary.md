# Binary forecast example data

A data set with binary predictions for COVID-19 cases and deaths
constructed from data submitted to the European Forecast Hub.

## Usage

``` r
example_binary
```

## Format

An object of class `forecast_binary` (see
[`as_forecast_binary()`](https://epiforecasts.io/scoringutils/dev/reference/as_forecast_binary.md))
with the following columns:

- location:

  the country for which a prediction was made

- location_name:

  name of the country for which a prediction was made

- target_end_date:

  the date for which a prediction was made

- target_type:

  the target to be predicted (cases or deaths)

- observed:

  A factor with observed values

- forecast_date:

  the date on which a prediction was made

- model:

  name of the model that generated the forecasts

- horizon:

  forecast horizon in weeks

- predicted:

  predicted value

## Source

<https://github.com/european-modelling-hubs/covid19-forecast-hub-europe_archive/commit/a42867b1ea152c57e25b04f9faa26cfd4bfd8fa6/>

## Details

Predictions in the data set were constructed based on the continuous
example data by looking at the number of samples below the mean
prediction. The outcome was constructed as whether or not the actually
observed value was below or above that mean prediction. This should not
be understood as sound statistical practice, but rather as a practical
way to create an example data set.

The data was created using the script create-example-data.R in the inst/
folder (or the top level folder in a compiled package).

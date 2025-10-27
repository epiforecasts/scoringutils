# Class constructor for `forecast` objects

Construct a class based on a data.frame or similar. The constructor

- coerces the data into a data.table

- assigns a class

## Usage

``` r
new_forecast(data, classname)
```

## Arguments

- data:

  A data.frame (or similar) with predicted and observed values. See the
  details section of for additional information on the required input
  format.

- classname:

  name of the class to be created

## Value

An object of the class indicated by `classname`

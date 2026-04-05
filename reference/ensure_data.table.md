# Ensure that an object is a `data.table`

This function ensures that an object is a `data table`. If the object is
not a data table, it is converted to one. If the object is a data table,
a copy of the object is returned.

## Usage

``` r
ensure_data.table(data)
```

## Arguments

- data:

  An object to ensure is a data table.

## Value

A data.table/a copy of an existing data.table.

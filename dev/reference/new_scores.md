# Construct an object of class `scores`

This function creates an object of class `scores` based on a data.table
or similar.

## Usage

``` r
new_scores(scores, metrics, ...)
```

## Arguments

- scores:

  A data.table or similar with scores as produced by
  [`score()`](https://epiforecasts.io/scoringutils/dev/reference/score.md).

- metrics:

  A character vector with the names of the scores (i.e. the names of the
  scoring rules used for scoring).

- ...:

  Additional arguments to
  [`data.table::as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html)

## Value

An object of class `scores`

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  model = "A",
  wis = "0.1"
)
new_scores(df, "wis")
} # }
```

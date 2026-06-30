# Handling missing forecasts

When comparing forecast models, not all models will have made
predictions for every target. Naively averaging scores across different
sets of targets can produce misleading summaries. This vignette walks
through diagnosing which targets each model covers, scoring the
forecasts, and then adjusting the scores using filtering and imputation
so that summaries are comparable.

The approaches here were initially inspired by [Kim, Ray & Reich
(2026)](https://doi.org/10.1016/j.ijforecast.2025.12.006), who discuss
the importance of handling missing forecasts when evaluating model
contributions beyond simple leaderboard rankings.

## Diagnosing missingness

Before adjusting scores we should aim to understand the patterns of
potential missingness. Models may have different coverage for legitimate
reasons (a model may only forecast deaths, not cases) or because of
operational failures (a missed submission deadline).

[`get_forecast_counts()`](https://epiforecasts.io/scoringutils/dev/reference/get_forecast_counts.md)
tabulates how many forecasts each model has, grouped by the columns you
choose.

``` r

library(scoringutils)
fc <- as_forecast_quantile(example_quantile)
#> ℹ Some rows containing NA values may be removed. This is fine if not
#>   unexpected.
get_forecast_counts(fc, by = c("model", "target_type"))
#> Key: <model, target_type>
#>                    model target_type count
#>                   <char>      <char> <int>
#> 1: EuroCOVIDhub-baseline       Cases   128
#> 2: EuroCOVIDhub-baseline      Deaths   128
#> 3: EuroCOVIDhub-ensemble       Cases   128
#> 4: EuroCOVIDhub-ensemble      Deaths   128
#> 5:       UMass-MechBayes       Cases     0
#> 6:       UMass-MechBayes      Deaths   128
#> 7:  epiforecasts-EpiNow2       Cases   128
#> 8:  epiforecasts-EpiNow2      Deaths   119
```

`UMass-MechBayes` does not forecast cases at all, and
`epiforecasts-EpiNow2` has fewer death forecasts than the other models.

To see exactly which death targets `epiforecasts-EpiNow2` is missing, we
can request counts at a finer level and filter to zero-count rows.

``` r

death_counts <- get_forecast_counts(
  fc,
  by = c("model", "target_type", "location", "target_end_date")
)
death_counts[
  model == "epiforecasts-EpiNow2" &
    target_type == "Deaths" &
    count == 0
]
#> Key: <model, target_type, location, target_end_date>
#>                   model target_type location target_end_date count
#>                  <char>      <char>   <char>          <Date> <int>
#> 1: epiforecasts-EpiNow2      Deaths       FR      2021-06-19     0
```

Note that
[`get_pairwise_comparisons()`](https://epiforecasts.io/scoringutils/dev/reference/get_pairwise_comparisons.md)
handles missingness internally, restricting each pair of models to their
shared set of targets before comparing. The functions below give you
explicit control over how to handle missingness when computing score
summaries.

## Scoring

We score all forecasts first, then adjust the resulting scores table.
Both
[`filter_scores()`](https://epiforecasts.io/scoringutils/dev/reference/filter_scores.md)
and
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md)
accept a `compare` argument (default `"model"`) that specifies which
column identifies the units being compared.

``` r

scores <- score(fc)
```

A naive summary averages over different numbers of targets per model,
which can make direct comparison misleading.

``` r

summarise_scores(scores, by = "model")
#>                    model         wis overprediction underprediction dispersion
#>                   <char>       <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  8992.62316    5025.130095      2120.64029 1846.85278
#> 2: EuroCOVIDhub-baseline 14321.48926    7081.000000      5143.53567 2096.95360
#> 3:  epiforecasts-EpiNow2 10827.40786    6179.439535      1697.23411 2950.73422
#> 4:       UMass-MechBayes    52.65195       8.978601        16.80095   26.87239
#>           bias interval_coverage_50 interval_coverage_90   ae_median
#>          <num>                <num>                <num>       <num>
#> 1:  0.00812500            0.6328125            0.9023438 12077.10156
#> 2:  0.21851562            0.4960938            0.9101562 19353.42969
#> 3: -0.04336032            0.4453441            0.8461538 14521.10526
#> 4: -0.02234375            0.4609375            0.8750000    78.47656
```

The sections below show two approaches to addressing this: filtering
scores to a common set of targets, and imputing scores for missing
targets.

## Filtering to a common set of targets

[`filter_scores()`](https://epiforecasts.io/scoringutils/dev/reference/filter_scores.md)
removes scores based on the supplied strategy. This can be appropriate
when a model legitimately does not cover certain targets and you want to
compare only on shared ground.

The default strategy,
[`filter_to_intersection()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_intersection.md),
keeps only targets covered by **all** models.

``` r

scores_filtered <- filter_scores(scores)
#> ℹ Filtered out 411 rows.
#> ℹ 476 of 887 rows remaining.
summarise_scores(scores_filtered, by = "model")
#>                    model       wis overprediction underprediction dispersion
#>                   <char>     <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  41.30642       7.366094        4.313117   29.62721
#> 2: EuroCOVIDhub-baseline 158.92682      66.312386        2.257216   90.35722
#> 3:       UMass-MechBayes  49.58008       7.935331       15.134819   26.50993
#> 4:  epiforecasts-EpiNow2  66.64282      18.892583       15.893314   31.85692
#>           bias interval_coverage_50 interval_coverage_90 ae_median
#>          <num>                <num>                <num>     <num>
#> 1:  0.06890756            0.8655462            1.0000000  53.86555
#> 2:  0.32941176            0.6638655            1.0000000 230.96639
#> 3: -0.01495798            0.4621849            0.8991597  74.17647
#> 4: -0.00512605            0.4201681            0.9075630 104.74790
```

This drops all case targets (since `UMass-MechBayes` has no case
forecasts) and the death targets `epiforecasts-EpiNow2` missed.

### Requiring partial coverage

The default requires every model to cover a target for it to be kept.
The `min_coverage` argument relaxes this by keeping targets covered by
at least a given proportion of models. With four models,
`min_coverage = 0.75` requires coverage by at least three.

``` r

scores_relaxed <- filter_scores(
  scores,
  strategy = filter_to_intersection(min_coverage = 0.75)
)
#> ℹ No rows filtered. Returning scores unchanged.
summarise_scores(scores_relaxed, by = "model")
#>                    model         wis overprediction underprediction dispersion
#>                   <char>       <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  8992.62316    5025.130095      2120.64029 1846.85278
#> 2: EuroCOVIDhub-baseline 14321.48926    7081.000000      5143.53567 2096.95360
#> 3:  epiforecasts-EpiNow2 10827.40786    6179.439535      1697.23411 2950.73422
#> 4:       UMass-MechBayes    52.65195       8.978601        16.80095   26.87239
#>           bias interval_coverage_50 interval_coverage_90   ae_median
#>          <num>                <num>                <num>       <num>
#> 1:  0.00812500            0.6328125            0.9023438 12077.10156
#> 2:  0.21851562            0.4960938            0.9101562 19353.42969
#> 3: -0.04336032            0.4453441            0.8461538 14521.10526
#> 4: -0.02234375            0.4609375            0.8750000    78.47656
```

In this example, case targets are covered by three of four models and
death targets by three or four, so `min_coverage = 0.75` retains all
targets. The default (`min_coverage = 1`) is stricter and drops all case
targets because `UMass-MechBayes` has no case forecasts. Between 0.75
and 1.0 no intermediate threshold changes the result here because no
target is covered by exactly three out of four models while being
missing for one that isn’t `UMass-MechBayes`.

### Filtering to a specific model’s targets

[`filter_to_include()`](https://epiforecasts.io/scoringutils/dev/reference/filter_to_include.md)
restricts to targets covered by named models. For example, to evaluate
all models only on the targets `epiforecasts-EpiNow2` covered:

``` r

scores_epinow2 <- filter_scores(
  scores,
  strategy = filter_to_include("epiforecasts-EpiNow2")
)
#> ℹ Filtered out 27 rows.
#> ℹ 860 of 887 rows remaining.
summarise_scores(scores_epinow2, by = "model")
#>                    model         wis overprediction underprediction dispersion
#>                   <char>       <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  9318.72435    5208.081676      2197.86217 1912.78050
#> 2: EuroCOVIDhub-baseline 14837.28683    7336.810069      5330.95195 2169.52482
#> 3:  epiforecasts-EpiNow2 10827.40786    6179.439535      1697.23411 2950.73422
#> 4:       UMass-MechBayes    49.58008       7.935331        15.13482   26.50993
#>            bias interval_coverage_50 interval_coverage_90   ae_median
#>           <num>                <num>                <num>       <num>
#> 1:  0.003967611            0.6194332            0.8987854 12515.57490
#> 2:  0.209473684            0.4898785            0.9068826 20049.01215
#> 3: -0.043360324            0.4453441            0.8461538 14521.10526
#> 4: -0.014957983            0.4621849            0.8991597    74.17647
```

This keeps both case and death targets where EpiNow2 submitted
forecasts, but `UMass-MechBayes` will still be missing case scores in
the result.

## Imputing missing scores

Instead of dropping data,
[`impute_missing_scores()`](https://epiforecasts.io/scoringutils/dev/reference/impute_missing_scores.md)
fills in scores for target combinations a model did not cover. Imputed
rows are marked with `.imputed = TRUE` so they can be identified later.

### NA

The simplest option: fill missing scores with `NA`. This preserves the
structure of the data without making assumptions about what the score
would have been. `NA` values propagate through summaries, so this can
also serve as a diagnostic check to confirm where missingness exists.

``` r

scores_na <- impute_missing_scores(
  scores,
  strategy = impute_na_score()
)
#> ℹ Imputing 137 missing score rows.
#> ℹ 2 model values affected.
summarise_scores(scores_na, by = "model")
#>                    model       wis overprediction underprediction dispersion
#>                   <char>     <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  8992.623        5025.13        2120.640   1846.853
#> 2: EuroCOVIDhub-baseline 14321.489        7081.00        5143.536   2096.954
#> 3:  epiforecasts-EpiNow2        NA             NA              NA         NA
#> 4:       UMass-MechBayes        NA             NA              NA         NA
#>         bias interval_coverage_50 interval_coverage_90 ae_median
#>        <num>                <num>                <num>     <num>
#> 1: 0.0081250            0.6328125            0.9023438  12077.10
#> 2: 0.2185156            0.4960938            0.9101562  19353.43
#> 3:        NA                   NA                   NA        NA
#> 4:        NA                   NA                   NA        NA
```

### Worst score

Fill each missing score with the worst (maximum) observed score for that
target across all models. This penalises models most heavily for missing
targets.

``` r

scores_worst <- impute_missing_scores(
  scores,
  strategy = impute_worst_score()
)
#> ℹ Imputing 137 missing score rows.
#> ℹ 2 model values affected.
summarise_scores(scores_worst, by = "model")
#>                    model       wis overprediction underprediction dispersion
#>                   <char>     <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  8992.623       5025.130        2120.640   1846.853
#> 2: EuroCOVIDhub-baseline 14321.489       7081.000        5143.536   2096.954
#> 3:  epiforecasts-EpiNow2 10452.972       5964.931        1638.933   2850.699
#> 4:       UMass-MechBayes 15389.614       7632.698        6020.870   3725.102
#>           bias interval_coverage_50 interval_coverage_90 ae_median
#>          <num>                <num>                <num>     <num>
#> 1:  0.00812500            0.6328125            0.9023438  12077.10
#> 2:  0.21851562            0.4960938            0.9101562  19353.43
#> 3: -0.02054687            0.4648438            0.8515625  14020.69
#> 4:  0.13046875            0.6015625            0.9101562  21026.91
```

We can check that the imputed rows match the models and targets we
identified as missing earlier.

``` r

scores_worst[
  (.imputed),
  .(n_imputed = .N),
  by = c("model", "target_type")
]
#>                   model target_type n_imputed
#>                  <char>      <char>     <int>
#> 1:      UMass-MechBayes       Cases       128
#> 2: epiforecasts-EpiNow2      Deaths         9
```

### Reference model

Fill with the scores of a named baseline model, treating a missing
forecast as performing no better than that baseline.

``` r

scores_ref <- impute_missing_scores(
  scores,
  strategy = impute_model_score("EuroCOVIDhub-baseline")
)
#> ℹ Imputing 137 missing score rows.
#> ℹ 2 model values affected.
summarise_scores(scores_ref, by = "model")
#>                    model       wis overprediction underprediction dispersion
#>                   <char>     <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  8992.623       5025.130        2120.640   1846.853
#> 2: EuroCOVIDhub-baseline 14321.489       7081.000        5143.536   2096.954
#> 3:  epiforecasts-EpiNow2 10452.583       5964.318        1637.566   2850.699
#> 4:       UMass-MechBayes 14268.113       7052.540        5150.887   2064.687
#>           bias interval_coverage_50 interval_coverage_90 ae_median
#>          <num>                <num>                <num>     <num>
#> 1:  0.00812500            0.6328125            0.9023438  12077.10
#> 2:  0.21851562            0.4960938            0.9101562  19353.43
#> 3: -0.02542969            0.4531250            0.8515625  14019.86
#> 4:  0.03781250            0.3945312            0.8476562  19276.04
```

This is a reasonable default when a suitable baseline exists, though
more research is needed on best practice for choosing the reference
model and understanding the impact of this choice.

### Mean score

Fill with the mean score across models that did forecast each target.
This is the least severe penalty as it assigns the average performance,
which may be close to the ensemble performance.

``` r

scores_mean <- impute_missing_scores(
  scores,
  strategy = impute_mean_score()
)
#> ℹ Imputing 137 missing score rows.
#> ℹ 2 model values affected.
summarise_scores(scores_mean, by = "model")
#>                    model       wis overprediction underprediction dispersion
#>                   <char>     <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  8992.623       5025.130        2120.640   1846.853
#> 2: EuroCOVIDhub-baseline 14321.489       7081.000        5143.536   2096.954
#> 3:  epiforecasts-EpiNow2 10450.295       5963.217        1638.036   2849.042
#> 4:       UMass-MechBayes 11236.152       6012.164        2972.151   2251.837
#>           bias interval_coverage_50 interval_coverage_90 ae_median
#>          <num>                <num>                <num>     <num>
#> 1:  0.00812500            0.6328125            0.9023438  12077.10
#> 2:  0.21851562            0.4960938            0.9101562  19353.43
#> 3: -0.03634115            0.4544271            0.8463542  14015.78
#> 4: -0.01739583            0.4283854            0.8398438  15122.32
```

## Combining filter and impute

Consider combining filtering and imputation when you want to focus on a
specific model’s targets but still need complete scores for all models.
For example, to evaluate on the targets `epiforecasts-EpiNow2` covered
and then impute scores for models that are missing forecasts within that
set:

``` r

result <- scores |>
  filter_scores(
    strategy = filter_to_include("epiforecasts-EpiNow2")
  ) |>
  impute_missing_scores(
    strategy = impute_worst_score()
  )
#> ℹ Filtered out 27 rows.
#> ℹ 860 of 887 rows remaining.
#> ℹ Imputing 128 missing score rows.
#> ℹ 1 model value affected.
summarise_scores(result, by = "model")
#>                    model       wis overprediction underprediction dispersion
#>                   <char>     <num>          <num>           <num>      <num>
#> 1: EuroCOVIDhub-ensemble  9318.724       5208.082        2197.862   1912.781
#> 2: EuroCOVIDhub-baseline 14837.287       7336.810        5330.952   2169.525
#> 3:  epiforecasts-EpiNow2 10827.408       6179.440        1697.234   2950.734
#> 4:       UMass-MechBayes 15946.971       7909.983        6238.839   3859.681
#>            bias interval_coverage_50 interval_coverage_90 ae_median
#>           <num>                <num>                <num>     <num>
#> 1:  0.003967611            0.6194332            0.8987854  12515.57
#> 2:  0.209473684            0.4898785            0.9068826  20049.01
#> 3: -0.043360324            0.4453441            0.8461538  14521.11
#> 4:  0.139595142            0.6072874            0.9230769  21788.14
```

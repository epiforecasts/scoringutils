# summarise_scores() works with point forecasts in a quantile format

    Code
      summarise_scores(scores, by = "model", relative_skill = TRUE,
        relative_skill_metric = "mae")
    Warning <simpleWarning>
      argument 'metric' must either be 'auto' or one of the metrics that can be computed. Relative skill will not be computed
    Output
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline             NA         NA              NA
      2: EuroCOVIDhub-ensemble             NA         NA              NA
      3:  epiforecasts-EpiNow2             NA         NA              NA
      4:       UMass-MechBayes             NA         NA              NA
         overprediction coverage_deviation  bias    ae_point     se_point ae_median
                  <num>              <num> <num>       <num>        <num>     <num>
      1:             NA                 NA    NA 19353.42969 2.883446e+09        NA
      2:             NA                 NA    NA 12077.10156 1.945118e+09        NA
      3:             NA                 NA    NA 14521.10526 2.680928e+09        NA
      4:             NA                 NA    NA    78.47656 1.170976e+04        NA

# metric is deprecated

    Code
      summarise_scores(scores, by = "model", metric = "auto")
    Warning <lifecycle_warning_deprecated>
      The `metric` argument of `summarise_scores()` is deprecated as of scoringutils 1.1.0.
      i Please use the `relative_skill_metric` argument instead.
    Output
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline             NA         NA              NA
      2: EuroCOVIDhub-ensemble             NA         NA              NA
      3:  epiforecasts-EpiNow2             NA         NA              NA
      4:       UMass-MechBayes             NA         NA              NA
         overprediction coverage_deviation  bias ae_point se_point ae_median
                  <num>              <num> <num>    <num>    <num>     <num>
      1:             NA                 NA    NA       NA       NA        NA
      2:             NA                 NA    NA       NA       NA        NA
      3:             NA                 NA    NA       NA       NA        NA
      4:             NA                 NA    NA       NA       NA        NA


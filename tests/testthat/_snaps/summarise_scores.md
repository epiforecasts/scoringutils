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
      summarise_scores(scores, metric = "auto")
    Warning <lifecycle_warning_deprecated>
      The `metric` argument of `summarise_scores()` is deprecated as of scoringutils 1.1.0.
      i Please use the `relative_skill_metric` argument instead.
    Output
      Key: <location, target_end_date, target_type, location_name, forecast_date, model, horizon>
           location target_end_date target_type location_name forecast_date
             <char>          <Date>      <char>        <char>        <Date>
        1:       DE      2021-05-08       Cases       Germany    2021-05-03
        2:       DE      2021-05-08       Cases       Germany    2021-05-03
        3:       DE      2021-05-08       Cases       Germany    2021-05-03
        4:       DE      2021-05-08      Deaths       Germany    2021-05-03
        5:       DE      2021-05-08      Deaths       Germany    2021-05-03
       ---                                                                 
      883:       IT      2021-07-24      Deaths         Italy    2021-07-05
      884:       IT      2021-07-24      Deaths         Italy    2021-07-12
      885:       IT      2021-07-24      Deaths         Italy    2021-07-12
      886:       IT      2021-07-24      Deaths         Italy    2021-07-12
      887:       IT      2021-07-24      Deaths         Italy    2021-07-12
                           model horizon interval_score dispersion underprediction
                          <char>   <num>          <num>      <num>           <num>
        1: EuroCOVIDhub-baseline       1             NA         NA              NA
        2: EuroCOVIDhub-ensemble       1             NA         NA              NA
        3:  epiforecasts-EpiNow2       1             NA         NA              NA
        4: EuroCOVIDhub-baseline       1             NA         NA              NA
        5: EuroCOVIDhub-ensemble       1             NA         NA              NA
       ---                                                                        
      883:  epiforecasts-EpiNow2       3             NA         NA              NA
      884: EuroCOVIDhub-baseline       2             NA         NA              NA
      885: EuroCOVIDhub-ensemble       2             NA         NA              NA
      886:       UMass-MechBayes       2             NA         NA              NA
      887:  epiforecasts-EpiNow2       2             NA         NA              NA
           overprediction coverage_deviation  bias ae_point se_point ae_median
                    <num>              <num> <num>    <num>    <num>     <num>
        1:             NA                 NA  0.95       NA       NA        NA
        2:             NA                 NA  0.50       NA       NA        NA
        3:             NA                 NA  0.90       NA       NA        NA
        4:             NA                 NA  0.30       NA       NA        NA
        5:             NA                 NA    NA       NA       NA        NA
       ---                                                                    
      883:             NA                 NA  0.50       NA       NA        NA
      884:             NA                 NA  0.20       NA       NA        NA
      885:             NA                 NA  0.40       NA       NA        NA
      886:             NA                 NA  0.80       NA       NA        NA
      887:             NA                 NA  0.90       NA       NA        NA


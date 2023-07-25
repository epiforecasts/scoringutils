# score() can support a sample column when a quantile forecast is used

    Code
      summarise_scores(summarise_scores(scores, by = "model"), by = "model", fun = signif,
      digits = 2)
    Output
                         model interval_score dispersion underprediction
      1: EuroCOVIDhub-baseline           8500        850               0
      2: EuroCOVIDhub-ensemble             NA         NA              NA
      3:  epiforecasts-EpiNow2          13000       4100               0
      4:       UMass-MechBayes            120         77              39
         overprediction coverage_deviation  bias ae_median
      1:           7600             -0.081  0.62     13000
      2:          11000                 NA  0.58     21000
      3:           8600              0.050  0.50     22000
      4:              0              0.050 -0.50       210


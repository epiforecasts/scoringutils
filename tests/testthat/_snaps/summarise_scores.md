# summarise_scores() can compute relative measures

    Code
      summarise_scores(summarise_scores(scores, by = "model", relative_skill = TRUE,
        fun = signif, digits = 2))
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14352.29688 2101.98047      5167.42207
      2: EuroCOVIDhub-ensemble     9014.75977 1842.54375      2117.93744
      3:       UMass-MechBayes       52.69375   26.84297        16.89448
      4:  epiforecasts-EpiNow2    10828.59271 2948.98259      1694.30872
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7123.576586        0.002742188  0.21851562 19326.50391           1.60
      2:    5040.330676        0.049246094  0.00812500 12147.88672           0.81
      3:       9.011062       -0.022812500 -0.02234375    78.73438           0.75
      4:    6174.094170       -0.054578947 -0.04336032 14476.35223           1.00
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA

---

    Code
      summarise_scores(summarise_scores(scores, by = "model", relative_skill = TRUE,
        relative_skill_metric = "ae_median", fun = signif, digits = 2))
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14352.29688 2101.98047      5167.42207
      2: EuroCOVIDhub-ensemble     9014.75977 1842.54375      2117.93744
      3:       UMass-MechBayes       52.69375   26.84297        16.89448
      4:  epiforecasts-EpiNow2    10828.59271 2948.98259      1694.30872
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7123.576586        0.002742188  0.21851562 19326.50391           1.60
      2:    5040.330676        0.049246094  0.00812500 12147.88672           0.78
      3:       9.011062       -0.022812500 -0.02234375    78.73438           0.77
      4:    6174.094170       -0.054578947 -0.04336032 14476.35223           1.00
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA

# summarise_scores(): metric is deprecated

    Code
      summarise_scores(summarise_scores(scores, by = "model", metric = "auto",
        relative_skill = TRUE, fun = signif, digits = 2))
    Warning <lifecycle_warning_deprecated>
      The `metric` argument of `summarise_scores()` is deprecated as of scoringutils 1.1.0.
      i Please use the `relative_skill_metric` argument instead.
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14352.29688 2101.98047      5167.42207
      2: EuroCOVIDhub-ensemble     9014.75977 1842.54375      2117.93744
      3:       UMass-MechBayes       52.69375   26.84297        16.89448
      4:  epiforecasts-EpiNow2    10828.59271 2948.98259      1694.30872
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7123.576586        0.002742188  0.21851562 19326.50391           1.60
      2:    5040.330676        0.049246094  0.00812500 12147.88672           0.81
      3:       9.011062       -0.022812500 -0.02234375    78.73438           0.75
      4:    6174.094170       -0.054578947 -0.04336032 14476.35223           1.00
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA


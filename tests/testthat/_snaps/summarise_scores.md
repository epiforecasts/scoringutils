# summarise_scores() can compute relative measures

    Code
      summarise_scores(summarise_scores(scores, by = "model", relative_skill = TRUE,
        fun = round, digits = 2))
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14321.48926 2096.95344      5143.53563
      2: EuroCOVIDhub-ensemble     8992.62320 1846.85262      2120.64035
      3:       UMass-MechBayes       52.65188   26.87234        16.80078
      4:  epiforecasts-EpiNow2    10827.40798 2950.73393      1697.23393
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7081.000000         0.00234375  0.21851562 19353.42969           1.60
      2:    5025.129844         0.04914063  0.00812500 12077.10156           0.81
      3:       8.978672        -0.02304687 -0.02234375    78.47656           0.75
      4:    6179.439271        -0.05485830 -0.04336032 14521.10526           1.03
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA

---

    Code
      summarise_scores(summarise_scores(scores, by = "model", relative_skill = TRUE,
        relative_skill_metric = "ae_median", fun = round, digits = 2))
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14321.48926 2096.95344      5143.53563
      2: EuroCOVIDhub-ensemble     8992.62320 1846.85262      2120.64035
      3:       UMass-MechBayes       52.65188   26.87234        16.80078
      4:  epiforecasts-EpiNow2    10827.40798 2950.73393      1697.23393
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7081.000000         0.00234375  0.21851562 19353.42969           1.60
      2:    5025.129844         0.04914063  0.00812500 12077.10156           0.78
      3:       8.978672        -0.02304687 -0.02234375    78.47656           0.77
      4:    6179.439271        -0.05485830 -0.04336032 14521.10526           1.04
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA

# summarise_scores(): metric is deprecated

    Code
      summarise_scores(summarise_scores(scores, by = "model", metric = "auto",
        relative_skill = TRUE, fun = round, digits = 2))
    Warning <lifecycle_warning_deprecated>
      The `metric` argument of `summarise_scores()` is deprecated as of scoringutils 1.1.0.
      i Please use the `relative_skill_metric` argument instead.
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14321.48926 2096.95344      5143.53563
      2: EuroCOVIDhub-ensemble     8992.62320 1846.85262      2120.64035
      3:       UMass-MechBayes       52.65188   26.87234        16.80078
      4:  epiforecasts-EpiNow2    10827.40798 2950.73393      1697.23393
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7081.000000         0.00234375  0.21851562 19353.42969           1.60
      2:    5025.129844         0.04914063  0.00812500 12077.10156           0.81
      3:       8.978672        -0.02304687 -0.02234375    78.47656           0.75
      4:    6179.439271        -0.05485830 -0.04336032 14521.10526           1.03
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA


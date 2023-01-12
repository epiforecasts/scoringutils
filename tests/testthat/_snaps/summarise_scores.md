# summarise_scores() can compute relative measures

    Code
      summarise_scores(scores, by = "model", relative_skill = TRUE)
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14321.48926 2096.95360      5143.53567
      2: EuroCOVIDhub-ensemble     8992.62316 1846.85278      2120.64029
      3:       UMass-MechBayes       52.65195   26.87239        16.80095
      4:  epiforecasts-EpiNow2    10827.40786 2950.73422      1697.23411
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7081.000000         0.00201087  0.21851562 19353.42969      1.6032604
      2:    5025.130095         0.04871603  0.00812500 12077.10156      0.8074916
      3:       8.978601        -0.02312500 -0.02234375    78.47656      0.7475873
      4:    6179.439535        -0.05516986 -0.04336032 14521.10526      1.0332277
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA

---

    Code
      summarise_scores(scores, by = "model", relative_skill = TRUE,
        relative_skill_metric = "ae_median")
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14321.48926 2096.95360      5143.53567
      2: EuroCOVIDhub-ensemble     8992.62316 1846.85278      2120.64029
      3:       UMass-MechBayes       52.65195   26.87239        16.80095
      4:  epiforecasts-EpiNow2    10827.40786 2950.73422      1697.23411
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7081.000000         0.00201087  0.21851562 19353.42969      1.6013866
      2:    5025.130095         0.04871603  0.00812500 12077.10156      0.7768187
      3:       8.978601        -0.02312500 -0.02234375    78.47656      0.7701946
      4:    6179.439535        -0.05516986 -0.04336032 14521.10526      1.0437192
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA

# summarise_scores(): metric is deprecated

    Code
      summarise_scores(scores, by = "model", metric = "auto", relative_skill = TRUE)
    Warning <lifecycle_warning_deprecated>
      The `metric` argument of `summarise_scores()` is deprecated as of scoringutils 1.1.0.
      i Please use the `relative_skill_metric` argument instead.
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14321.48926 2096.95360      5143.53567
      2: EuroCOVIDhub-ensemble     8992.62316 1846.85278      2120.64029
      3:       UMass-MechBayes       52.65195   26.87239        16.80095
      4:  epiforecasts-EpiNow2    10827.40786 2950.73422      1697.23411
         overprediction coverage_deviation        bias   ae_median relative_skill
                  <num>              <num>       <num>       <num>          <num>
      1:    7081.000000         0.00201087  0.21851562 19353.42969      1.6032604
      2:    5025.130095         0.04871603  0.00812500 12077.10156      0.8074916
      3:       8.978601        -0.02312500 -0.02234375    78.47656      0.7475873
      4:    6179.439535        -0.05516986 -0.04336032 14521.10526      1.0332277
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA


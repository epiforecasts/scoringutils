# summarise_scores() can compute relative measures

    Code
      summarise_scores(summarise_scores(scores, by = "model", relative_skill = TRUE,
        fun = signif, digits = 1))
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14531.32812 2063.28125      5228.14504
      2: EuroCOVIDhub-ensemble     8777.69531 1800.33203      2132.37828
      3:       UMass-MechBayes       51.60938   26.50781        15.79562
      4:  epiforecasts-EpiNow2    10703.32794 2783.59514      1674.83271
         overprediction coverage_deviation         bias   ae_median relative_skill
                  <num>              <num>        <num>       <num>          <num>
      1:    6772.927266       -0.003554687  0.218750000 18837.23438            2.0
      2:    4856.788438        0.039804688  0.007421875 12265.44141            0.8
      3:       8.868594       -0.029687500 -0.021875000    76.95312            0.7
      4:    5933.455587       -0.062631579 -0.043319838 14547.31174            1.0
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA

---

    Code
      summarise_scores(summarise_scores(scores, by = "model", relative_skill = TRUE,
        relative_skill_metric = "ae_median", fun = signif, digits = 1))
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14531.32812 2063.28125      5228.14504
      2: EuroCOVIDhub-ensemble     8777.69531 1800.33203      2132.37828
      3:       UMass-MechBayes       51.60938   26.50781        15.79562
      4:  epiforecasts-EpiNow2    10703.32794 2783.59514      1674.83271
         overprediction coverage_deviation         bias   ae_median relative_skill
                  <num>              <num>        <num>       <num>          <num>
      1:    6772.927266       -0.003554687  0.218750000 18837.23438            2.0
      2:    4856.788438        0.039804688  0.007421875 12265.44141            0.8
      3:       8.868594       -0.029687500 -0.021875000    76.95312            0.8
      4:    5933.455587       -0.062631579 -0.043319838 14547.31174            1.0
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA

# summarise_scores(): metric is deprecated

    Code
      summarise_scores(summarise_scores(scores, by = "model", metric = "auto",
        relative_skill = TRUE, fun = signif, digits = 1))
    Warning <lifecycle_warning_deprecated>
      The `metric` argument of `summarise_scores()` is deprecated as of scoringutils 1.1.0.
      i Please use the `relative_skill_metric` argument instead.
    Output
      Key: <model>
                         model interval_score dispersion underprediction
                        <char>          <num>      <num>           <num>
      1: EuroCOVIDhub-baseline    14531.32812 2063.28125      5228.14504
      2: EuroCOVIDhub-ensemble     8777.69531 1800.33203      2132.37828
      3:       UMass-MechBayes       51.60938   26.50781        15.79562
      4:  epiforecasts-EpiNow2    10703.32794 2783.59514      1674.83271
         overprediction coverage_deviation         bias   ae_median relative_skill
                  <num>              <num>        <num>       <num>          <num>
      1:    6772.927266       -0.003554687  0.218750000 18837.23438            2.0
      2:    4856.788438        0.039804688  0.007421875 12265.44141            0.8
      3:       8.868594       -0.029687500 -0.021875000    76.95312            0.7
      4:    5933.455587       -0.062631579 -0.043319838 14547.31174            1.0
         scaled_rel_skill
                    <num>
      1:               NA
      2:               NA
      3:               NA
      4:               NA


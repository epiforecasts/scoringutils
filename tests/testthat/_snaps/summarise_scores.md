# summarise_scores() can compute relative measures

    Code
      summarise_scores(summarise_scores(scores, by = "model", relative_skill = TRUE,
        fun = signif, digits = 2)[, .(model, relative_skill)])
    Output
      Key: <model>
                         model relative_skill
                        <char>          <num>
      1: EuroCOVIDhub-baseline           1.60
      2: EuroCOVIDhub-ensemble           0.81
      3:       UMass-MechBayes           0.75
      4:  epiforecasts-EpiNow2           1.00

---

    Code
      summarise_scores(summarise_scores(scores, by = "model", relative_skill = TRUE,
        relative_skill_metric = "ae_median", fun = signif, digits = 2)[, .(model,
        relative_skill)])
    Output
      Key: <model>
                         model relative_skill
                        <char>          <num>
      1: EuroCOVIDhub-baseline           1.60
      2: EuroCOVIDhub-ensemble           0.78
      3:       UMass-MechBayes           0.77
      4:  epiforecasts-EpiNow2           1.00

# summarise_scores(): metric is deprecated

    Code
      summarise_scores(summarise_scores(scores, by = "model", metric = "auto",
        relative_skill = TRUE, fun = signif, digits = 2)[, .(model, relative_skill)])
    Warning <lifecycle_warning_deprecated>
      The `metric` argument of `summarise_scores()` is deprecated as of scoringutils 1.1.0.
      i Please use the `relative_skill_metric` argument instead.
    Output
      Key: <model>
                         model relative_skill
                        <char>          <num>
      1: EuroCOVIDhub-baseline           1.60
      2: EuroCOVIDhub-ensemble           0.81
      3:       UMass-MechBayes           0.75
      4:  epiforecasts-EpiNow2           1.00


test_that("get_correlations() works as expected", {

  # expect all to go well in the usual case
  expect_no_condition(
    correlations <- scores_quantile %>%
      summarise_scores(by = get_forecast_unit(scores_quantile)) %>%
      get_correlations(digits = 2)
  )
  expect_equal(
    colnames(correlations), c(get_metrics(scores_quantile), "metric")
  )

  # expect no error even if scores are unsummarised
  # (meaning that coverage will be a logical vector instead of a numeric)
  expect_no_condition(
    correlations2 <- scores_quantile %>%
      get_correlations(digits = 2)
  )
  expect_equal(correlations, correlations2)
})

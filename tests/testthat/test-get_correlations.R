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

  # expect no error if scores are unsummarised
  # (meaning that coverage will be a logical vector instead of a numeric)
  expect_no_condition(
    correlations2 <- scores_quantile %>%
      get_correlations(digits = 2)
  )
  expect_equal(correlations, correlations2)

  expect_s3_class(
    get_correlations(scores_quantile, digits = 2),
    c("scores", "data.table", "data.frame"),
    exact = TRUE
  )

  # passing a data.frame works as long as the metrics attribute is still there
  expect_no_condition(
    get_correlations(as.data.frame(scores_quantile), digits = 2)
  )

  # check we get an error if metrics attribute is missing.
  expect_error(
    get_correlations(as.data.frame(as.matrix(scores_quantile))),
    "Assertion on 'metrics' failed: Must be a subset of"
  )
})

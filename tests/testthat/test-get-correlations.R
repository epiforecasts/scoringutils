test_that("get_correlations() works as expected", {
  # expect all to go well in the usual case
  correlations <- expect_no_condition(
    get_correlations(scores_quantile)
  )
  expect_identical(
    colnames(correlations), c(get_metrics.scores(scores_quantile), "metric")
  )

  # expect no error if scores are unsummarised
  # (meaning that coverage will be a logical vector instead of a numeric)
  correlations2 <- expect_no_condition(
    get_correlations(scores_quantile)
  )
  expect_identical(correlations, correlations2)

  expect_s3_class(
    get_correlations(scores_quantile),
    c("scores", "data.table", "data.frame"),
    exact = TRUE
  )

  # passing a data.frame works as long as the metrics attribute is still there
  expect_no_condition(
    get_correlations(as.data.frame(scores_quantile))
  )

  # check we get an error if metrics attribute is missing.
  expect_error(
    get_correlations(as.data.frame(as.matrix(scores_quantile))),
    "Assertion on 'metrics' failed: Must be a subset of"
  )
})

# ==============================================================================
# plot_correlation() # nolint: commented_code_linter
# ==============================================================================
test_that("plot_correlations() works as expected", {
  correlations <- get_correlations(
    summarise_scores(
      scores_quantile,
      by = get_forecast_unit(scores_quantile)
    )
  )
  p <- plot_correlations(correlations, digits = 2)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot__correlation", p)

  # expect an error if you forgot to compute correlations
  expect_error(
    plot_correlations(summarise_scores(scores_quantile)),
    "Did you forget to call `scoringutils::get_correlations()`?"
  )
})

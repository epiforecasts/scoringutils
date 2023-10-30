test_that("plot_correlation() works as expected", {
  correlations <- correlation(summarise_scores(scores_quantile))
  p <- plot_correlation(correlations)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot__correlation", p)
})

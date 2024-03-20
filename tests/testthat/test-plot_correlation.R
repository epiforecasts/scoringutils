test_that("plot_correlations() works as expected", {
  correlations <- get_correlations(
    summarise_scores(
      scores_quantile,
      by = get_forecast_unit(scores_quantile)
    ),
    digits = 2
  )
  p <- plot_correlations(correlations)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot__correlation", p)
})

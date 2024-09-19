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
    plot_correlations(summarise_scores(scores_quantile, by = "model")),
    "Did you forget to call `scoringutils::get_correlations()`?"
  )
})

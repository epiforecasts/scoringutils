test_that("plot.forecast_counts() works as expected", {
  available_forecasts <- get_forecast_counts(
    example_quantile,
    by = c("model", "target_type", "target_end_date")
  )
  p <- plot(available_forecasts,
    xvar = "target_end_date", show_numbers = FALSE
  ) +
    facet_wrap("target_type")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_available_forecasts", p)
})

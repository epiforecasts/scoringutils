test_that("plot.forecast_counts() works as expected", {
  available_forecasts <- na.omit(example_quantile) %>%
    as_forecast() %>%
    get_forecast_counts(
    by = c("model", "target_type", "target_end_date")
  )
  p <- plot_forecast_counts(available_forecasts,
    x = "target_end_date", show_counts = FALSE
  ) +
    facet_wrap("target_type")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_available_forecasts", p)
})

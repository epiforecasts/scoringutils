test_that("plot_avail_forecasts() works as expected", {
  avail_forecasts <- suppressMessages(
    avail_forecasts(example_quantile,
      by = c("model", "target_type", "target_end_date")
    )
  )
  p <- plot_avail_forecasts(avail_forecasts,
    x = "target_end_date", show_numbers = FALSE
  ) +
    facet_wrap("target_type")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_avail_forecasts", p)
})
test_that("plot_available_forecasts() works as expected", {
  available_forecasts <- suppressMessages(
    available_forecasts(example_quantile,
      by = c("model", "target_type", "target_end_date")
    )
  )
  p <- plot_available_forecasts(available_forecasts,
    x = "target_end_date", show_numbers = FALSE
  ) +
    facet_wrap("target_type")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  vdiffr::expect_doppelganger("plot_available_forecasts", p)
})

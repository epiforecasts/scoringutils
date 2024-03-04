test_that("plot_interval_coverage() works as expected", {
  coverage <- example_quantile %>%
    na.omit() %>%
    as_forecast() %>%
    get_coverage(by = c("model"))
  p <- plot_interval_coverage(coverage)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_interval_coverage", p))
})

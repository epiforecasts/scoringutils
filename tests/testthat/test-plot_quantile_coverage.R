test_that("plot_quantile_coverage() works as expected", {
  coverage <- get_coverage(example_quantile, by = c("model", "quantile_level"))

  p <- plot_quantile_coverage(coverage)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_quantile_coverage", p))
})

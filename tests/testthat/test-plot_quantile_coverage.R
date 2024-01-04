test_that("plot_quantile_coverage() works as expected", {
  coverage <- add_coverage(na.omit(example_quantile)) %>%
    summarise_scores(by = c("model", "quantile"))

  p <- plot_quantile_coverage(coverage)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  suppressWarnings(vdiffr::expect_doppelganger("plot_quantile_coverage", p))
})
